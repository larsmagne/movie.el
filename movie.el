;;; movie.el --- playing movies  -*- lexical-binding: t; -*-
;; Copyright (C) 2004-2024 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; movie.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; movie.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; apt install mpv mediainfo

;; query-assistant.el can be found in https://github.com/larsmagne/bookiez.el
;; It's needed for LLM-assisted actor identification only.

;;; Code:

(require 'time-date)
(require 'imdb)
(require 'mkv)
(require 'subr-x)
(require 'touchgrid)
(require 'url-cache)
(require 'vtable)

(defvar movie-order nil)
(defvar movie-limit nil)

(defvar movie-files "." ;"\\.\\(mpg\\|avi\\|wmv\\|ogm\\|rm\\|mpeg\\)$"
  "Regexp to match movie files.")

(defvar movie-player
  '("/usr/src/mpv/build/mpv"
    "--audio-device=alsa/hw:CARD=J65,DEV=0"
    "--vo=gpu"
    "--hwdec=vdpau"
    ;;"--vf=vdpaupp=denoise=1"
    ;;"--tone-mapping=clip" "--tone-mapping-param=1"
    "--input-ipc-server=/tmp/mpv-socket"
    "--fullscreen"
    "--stop-screensaver"
    )
  "Command to play a file.")

(defvar movie-image-scale 0.5
  "Scaling applied to screenshots.")

(defvar movie-genres nil
  "A list of strings that are names of genres.")

(defvar movie-crop '("-vf" "crop=700:420")
  "Parameters to crop a 4:3 aspect ratio program.")

(defvar movie-deinterlace-switch "--deinterlace=yes"
  "-Switch to pass to mpv to deinterlace films.")

(defvar movie-high-volume
  '("-af" "volume=15:1")
  "Parameters to boost the volume.")

(defvar movie-picture-directory nil
  "Directory where pictures are taken during movie playing.")

(defvar movie-positions-file "/tv/data/mplayer.positions"
  "Where viewing positions are stored.")

(defvar movie-inhibit-positions nil
  "If non-nil, don't use stored positons when playing files.")

(defvar movie-file-id nil)

(defvar movie-deletion-process nil)

(defvar movie-after-play-callback nil
  "Function called after MPV playback has ended.")

(defvar movie-before-play-callback nil
  "Function called before MPV playback has started.")

(defun movie-holiday ()
  "Start viewing movies when not at home."
  (interactive)
  (setq movie-positions-file "~/.emacs.d/mplayer.positions")
  (movie-browse "/dvd"))

(defun movie-browse (directory &optional order match)
  "Browse DIRECTORY."
  (interactive (list
		(let ((read-file-name-completion-ignore-case t))
		  (read-directory-name
		   "Directory: "
		   (cond
		    ((string-match "^/dvd/" default-directory)
		     "/dvd/")
		    ((string-match "^/tv/torrent/" default-directory)
		     "/tv/torrent/")
		    (t
		     default-directory))
		   nil t))))
					  
  ;; If called in the /dvd directory, just display films that are
  ;; unseen.
  (when (and (null match)
	     (equal (file-name-nondirectory (directory-file-name directory))
		    "dvd"))
    (setq match 
	  (lambda (stats)
	    (and (null (assoc "Seen" stats))
		 (null (assoc "Seen-Version" stats))))))
  (setq directory (file-truename directory))
  (let ((files (movie-get-files directory match)))
    (when (null order)
      (setq order 'alphabetical))
    (switch-to-buffer (directory-file-name directory))
    (erase-buffer)
    (movie-mode)
    (setq movie-order order
	  movie-limit match)
    (unless (string-match "/$" directory)
      (setq directory (concat directory "/")))
    (setq default-directory directory)
    (when files
      (movie-generate-buffer files order))
    (setq-local mode-line-misc-info (movie-buffer-identification directory))
    (unless movie-deletion-process
      (setq movie-deletion-process (run-at-time 1 10 'movie-delete-scheduled)))
    (let ((sleeve (expand-file-name "sleeve.jpg" directory)))
      (when (file-exists-p sleeve)
	(goto-char (point-max))
	(insert "\n\n")
	(insert-image (create-image sleeve nil nil
				    :max-height (/ (window-pixel-height) 2)))))
    (movie-goto-logical-line)))

(defun movie-goto-logical-line ()
  "If we're in a dvd directory, go to the last film we've seen in it.
Otherwise, goto the start of the buffer."
  (interactive)
  (let ((stats (movie-get-stats default-directory)))
    (if (not stats)
	(progn
	  (goto-char (point-min))
	  nil)
      ;; We want to go to the last "seen" track.
      (movie-goto-movie
       (or
	(cl-loop for track in (reverse (cdr (assoc 'tracks stats)))
		 when (plist-get (cdr track) :seen)
		 return (car track))
	(caar (cdr (assoc 'tracks stats)))))
      t)))

(defun movie-goto-movie (movie)
  (goto-char (point-min))
  (let (file found)
    (while (and (not (eobp))
		(or (not (setq file (cl-getf (vtable-current-object)
					     :file)))
		    (not (setq found
			       (equal movie (file-name-nondirectory file))))))
      (forward-line 1))
    found))

(defun movie-get-stats (directory)
  (let ((file (expand-file-name "stats" directory))
	data)
    (when (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(while (looking-at "\\([^:\n]+\\): \\(.*\\)")
	  (push (cons (match-string 1) (match-string 2)) data)
	  (forward-line 1))
	(forward-line 1)
	(let ((tracks nil))
	  (while (looking-at "(")
	    (push (delq 'list (read (current-buffer))) tracks)
	    (forward-line 1))
	  (push (cons 'tracks (nreverse tracks)) data))))
    (nreverse data)))	  

(defun movie-list-parts (match &optional only-unseen)
  "Limit the buffer to matching genres."
  (interactive
   (list
    (completing-read "Match: " (append movie-genres
				       (list "nostats" "unseen" "all"
					     "mostly-seen"
					     "seen-version"
					     "movies")))
    current-prefix-arg))
  (movie-browse
   default-directory movie-order
   (cond
    ((equal match "nostats")
     (lambda (stats)
       (null stats)))
    ((equal match "all")
     (lambda (_stats) t))
    ((equal match "unseen")
     (lambda (stats)
       (and (null (assoc "Seen" stats))
	    (null (assoc "Seen-Version" stats)))))
    ((equal match "movies")
     (lambda (stats)
       (let ((genres (cdr (assoc "Genre" stats))))
	 (and genres
	      (null (assoc "Seen" stats))
	      (null (assoc "Seen-Version" stats))
	      (assoc "Year" stats)
	      (not (member "eclipse" (split-string genres ",")))
	      (not (member "best22" (split-string genres ",")))
	      (not (member "tv" (split-string genres ",")))))))
    ((equal match "mostly-seen")
     (lambda (stats)
       (equal (cdr (assoc "Status" stats)) "mostly-seen")))
    ((equal match "seen-version")
     (lambda (stats)
       (equal (cdr (assoc "Status" stats)) "seen-version")))
    (t
     `(lambda (stats)
	(let ((genres (cdr (assoc "Genre" stats))))
	  (and genres
	       (or (not ,only-unseen)
		   (and (null (assoc "Seen" stats))
			(null (assoc "Seen-Version" stats))))
	       (member ,match (split-string genres ",")))))))))

(defun movie-list-by-year ()
  "List the movies by year."
  (interactive)
  (movie-browse default-directory 'year movie-limit))

(defun movie-list-by-rip-time ()
  "List the movies by year."
  (interactive)
  (movie-browse default-directory 'rip-time movie-limit))

(defun movie-list-by-size ()
  "List the movies by size."
  (interactive)
  (movie-browse default-directory 'size movie-limit))

(defun movie-list-by-country ()
  "List the movies by year."
  (interactive)
  (movie-browse default-directory 'country movie-limit))

(defun movie-list-by-director ()
  "List movies by element like directory or genre."
  (interactive)
  (movie-browse default-directory 'director movie-limit))

(defun movie-get-files (directory &optional match)
  (let ((files (directory-files directory t))
	(data nil)
	(stats (movie-get-stats directory))
	atts track)
    (dolist (file files)
      (setq atts (file-attributes file))
      (when (and
	     (not (string-match "^[.]" (file-name-nondirectory file)))
	     (not (equal (file-name-nondirectory file) "p"))
	     (or (null match)
		 (and (stringp match)
		      (let ((case-fold-search t))
			(string-match match file)))
		 (and (functionp match)
		      (eq (car atts) t)
		      (funcall match (movie-get-stats file))))
	     (not (string-match "\\.png\\'\\|\\.JPG\\'\\|\\.srt\\'\\|/stats\\|/seen-date\\|txt$\\|~$\\|[.]hidden\\'" file))
	     (not (file-exists-p (concat file ".hidden")))
	     (or (and (eq (car atts) nil)
		      (string-match movie-files (file-name-nondirectory file)))
		 (and (eq (car atts) t)
		      (not (member (file-name-nondirectory file)
				   '("." ".." "lost+found"))))))
	(setq track (assoc (file-name-nondirectory file)
			   (cdr (assoc 'tracks stats))))
	(push `(:file ,file
		      :time ,(nth 5 atts)
		      :recorded ,(cdr (assoc "Recorded" (movie-get-stats file)))
		      :size ,(nth 7 atts)
		      :directoryp ,(nth 0 atts)
		      :year ,(when-let ((year (cdr
					       (assoc "Year" (movie-get-stats
							      file)))))
			       (string-to-number year))
		      :genre ,(cdr (assoc "Genre" (movie-get-stats file)))
		      :title ,(cdr (assoc "Title" (movie-get-stats file)))
		      :imdb ,(cdr (assoc "IMDB" (movie-get-stats file)))
		      :director
		      ,(let ((director (cdr (assoc "Director"
						   (movie-get-stats file)))))
			 (or director ""))
		      :country
		      ,(let ((country (cdr (assoc "Country"
						  (movie-get-stats file)))))
			 (or country ""))
		      ,@(when track
			  (cdr track))
		      ,@(when (nth 0 atts)
			  (movie-dvd-directory-data file)))
	      data)))
    data))

(defun movie-dvd-directory-data (dir)
  (let ((stats (movie-get-stats dir))
	data max)
    (when stats
      (when (equal (cdr (assoc "Status" stats)) "seen")
	(setq data (list :seen '(t))))
      (when (equal (cdr (assoc "Status" stats)) "mostly-seen")
	(setq data (list :mostly-seen '(t))))
      (when (equal (cdr (assoc "Status" stats)) "seen-version")
	(setq data (list :seen-version '(t))))
      (when (assoc "Genre" stats)
	(nconc data (list :genre (cdr (assoc "Genre" stats)))))
      (dolist (track (cdr (assoc 'tracks stats)))
	(when (or (not max)
		  (> (plist-get (cdr track) :length)
		     (plist-get (cdr max) :length)))
	  (setq max track)))
      (setq data (nconc
		  (list :image (expand-file-name (concat (car max) ".png") dir)
			:length (cdr max))
		  data))
      data)))

(defun movie-biggest-file-data (dir)
  (when (file-exists-p (plist-get dir :file))
    (let ((files nil))
      (dolist (file (directory-files (plist-get dir :file) t))
	(push (cons (nth 7 (file-attributes file)) file) files))
      (car (sort files (lambda (f1 f2)
			 (> (car f1) (car f2))))))))

(defun movie-generate-buffer (files &optional order)
  (when (not order)
    (setq order 'chronological))
  (setq files (movie-sort files order))
  (make-vtable
   :columns `(( :name "Poster"
		:max-width ,(format "%dpx"
				    (* (if (string-match "/dvd/" default-directory)
					   70
					 100)
				       (image-compute-scaling-factor)))
		:displayer
		,(lambda (image max-width _table)
		   (propertize "*" 'display
			       (append image
				       `(:max-width ,max-width)))))
	      (:name "Time")
	      (:name "Info")
	      (:name "Director" :max-width 20)
	      (:name "Title"))
   :face (if (string-match "Futura" (face-font 'default))
	     'default
	   'vtable)
   :keymap (define-keymap
             "g" #'movie-rescan)
   :objects files
   :actions '("RET" (lambda (object)
		      (movie-find-file (plist-get object :file))))
   :separator-width 1
   :getter
   (lambda (object column table)
     (let ((dvdp (and (string-match "^/dvd/\\|^/flash/movies\\|^/mnt/dos\\|/home/larsi/dvd"
				    (plist-get object :file))
		      (plist-get object :directoryp))))
       (pcase (vtable-column table column)
	 ("Poster"
	  (and
	   (length< files 500)
	   (let ((sleeve (and dvdp
			      (expand-file-name
			       "sleeve.jpg" (plist-get object :file)))))
	     (if (and sleeve
		      (file-exists-p sleeve))
		 (create-image
		  sleeve nil nil 
		  :scale movie-image-scale
		  :max-height
		  (truncate (* 50 (image-compute-scaling-factor))))
	       (let ((png (or (plist-get object :image)
			      (concat (plist-get object :file) ".png"))))
		 (cond
		  ((file-exists-p png)
		   (create-image png nil nil
				 :scale movie-image-scale))
		  ((and
		    (plist-get object :directoryp)
		    (file-exists-p
		     (setq png
			   (format "%s.png"
				   (cdr (movie-biggest-file-data object))))))
		   (create-image png nil nil
				 :scale movie-image-scale))
		  (t
		   (create-image "~/src/movie.el/empty.png" nil nil
				 :scale movie-image-scale))))))))
	 ("Time"
	  (cond
	   ((or dvdp (memq order '(year director rip-time size country)))
	    (or (plist-get object :year) ""))
	   ((plist-get object :directoryp)
	    (round (/ (or (car (movie-biggest-file-data object)) -1)
		      1024 1024)))
	   ((plist-get object :length)
	    (let ((str (movie-format-length (plist-get object :length))))
	      (if (> (plist-get object :length) (* 30 60))
		  (propertize str 'face '(:background "#006000"))
		str)))
	   (t
	    (/ (or (plist-get object :size) -1) 1024 1024))))
	 ("Info"
	  (cond
	   ((eq order 'rip-time)
	    (format-time-string " %Y-%m-%d " (movie-rip-time object)))
	   ((eq order 'size)
	    (format " %d " (/ (movie--directory-size object) 1024.0 1024)))
	   ((eq order 'country)
	    (plist-get object :country))
	   (t "")))
	 ("Director"
	  (plist-get object :director))
	 ("Title"
	  (if (or (not (plist-get object :length))
		  (and (not (plist-get object :seen))
		       (not (plist-get object :mostly-seen))
		       (not (plist-get object :seen-version))))
	      (file-name-nondirectory (plist-get object :file))
	    (propertize
	     (file-name-nondirectory (plist-get object :file))
	     'face `(:foreground
		     ,(let ((seen (car (last (plist-get object :seen) 2)))
			    (length (plist-get object :length)))
			(cond
			 ((plist-get object :directoryp)
			  "#5050f0")
			 ((> (/ (setq seen (float seen)) length) 0.9)
			  "#000080")
			 ((> (/ seen length) 0.1)
			  "#808000")
			 (t
			  "#800000"))))))))))))

(defun movie-format-length (seconds)
  (if (< seconds (* 60 60))
      (format "%02d:%02dm" (truncate (/ seconds 60))
	      (mod seconds 60))
     (format "%02d:%02dh"
	     (truncate (/ seconds 60 60))
	     (mod (/ seconds 60) 60))))

(defun movie-limit (match)
  "Limit the buffer to matching files."
  (interactive
   (list
    (read-string "Match: "
		 (movie-prefix (file-name-nondirectory
				(movie-current-file))))))
  (setq movie-limit match)
  (movie-browse default-directory movie-order))

(defun movie-sortable-name (name)
  "Regularise the names so that \\='foo \"bar\"\\=' sorts the same as \\='foo.bar\\=' etc."
  (replace-regexp-in-string
   "[ .-]+" "."
   (replace-regexp-in-string
    "['\"]" ""
    (replace-regexp-in-string "/\\(the\\|a\\)[ .]" "/"
			      (downcase name)))))

(defun movie-sort (files order)
  (let ((predicate
	 (cond
	  ((eq order 'alphabetical)
	   (lambda (f1 f2)
	     (string< (movie-sortable-name (plist-get f1 :file))
		      (movie-sortable-name (plist-get f2 :file)))))
	  ((eq order 'chronological)
	   (lambda (f1 f2)
	     (cond
	      ((plist-get f1 :recorded)
	       (string< (plist-get f1 :recorded)
			(plist-get f2 :recorded)))
	      (t
	       (time-less-p (plist-get f1 :time)
			    (plist-get f2 :time))))))
	  ((eq order 'year)
	   (lambda (f1 f2)
	     (< (or (plist-get f1 :year) 9999)
		(or (plist-get f2 :year) 9999))))
	  ((eq order 'rip-time)
	   (lambda (f1 f2)
	     (< (movie-rip-time f1)
		(movie-rip-time f2))))
	  ((eq order 'size)
	   (lambda (f1 f2)
	     (< (movie--directory-size f1)
		(movie--directory-size f2))))
	  ((eq order 'director)
	   (lambda (f1 f2)
	     (string< (movie--director-sort (plist-get f1 :director))
		      (movie--director-sort (plist-get f2 :director)))))
	  ((eq order 'country)
	   (lambda (f1 f2)
	     (string< (or (plist-get f1 :country) "")
		      (or (plist-get f2 :country) ""))))
	  (t
	   (error "No such order %s" order)))))
    (when (eq order 'director)
      (setq files (sort files
			(lambda (f1 f2)
			  (time-less-p (or (plist-get f1 :year) 0)
				       (or (plist-get f2 :year) 0))))))
    (sort files predicate)))

(defun movie--director-sort (director)
  (if (zerop (length director))
      "ý"
    (downcase (string-join (reverse (split-string director)) " "))))

(defun movie-rip-time (elem)
  (let ((files (directory-files (cl-getf elem :file) t "[.]mkv$")))
    (if (not files)
	0
      (float-time (file-attribute-modification-time
		   (file-attributes
		    (car (cl-sort files #'<
				  :key (lambda (f)
					 (float-time
					  (file-attribute-modification-time
					   (file-attributes f))))))))))))

(defun movie--directory-size (elem)
  (let ((files (directory-files (cl-getf elem :file) t "[.]mkv$")))
    (if (not files)
	0
      (cl-reduce #'+ (mapcar (lambda (f)
			       (file-attribute-size (file-attributes f)))
			     files)))))

(defconst movie-mode-map 
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'movie-find-file)
    (define-key map " " 'movie-play-best-file)
    (define-key map [delete] 'movie-delete-file)
    (define-key map [del] 'movie-delete-file)
    (define-key map [backspace] 'movie-delete-file)
    (define-key map [deletechar] 'movie-delete-file)
    (define-key map "V" 'movie-play-vlc-dvd)
    (define-key map "F" 'movie-play-current-vob)
    (define-key map "T" 'movie-change-rate)
    (define-key map "r" 'movie-change-rate-current)
    ;;(define-key map "T" 'movie-thumbnails)
    (define-key map "q" 'bury-buffer)
    (define-key map "k" 'movie-browse)
    (define-key map "j" 'movie-jump-to-directory)
    (define-key map "c" 'movie-play-cropped)
    (define-key map "x" 'movie-prefixed-action)
    (define-key map "h" 'movie-play-high-volume)
    (define-key map "g" 'movie-rescan)
    (define-key map "G" 'movie-add-genre)
    (define-key map "t" 'movie-find-torrent)
    (define-key map "s" 'movie-toggle-sort)
    (define-key map "R" 'movie-reload)
    (define-key map [touchscreen-begin] 'ignore)
    (define-key map [touchscreen-end] 'movie-set-touch-point)
    (define-key map [touchscreen-update] 'ignore)
    (define-key map "C" 'movie-clear-screenshots)
    (define-key map "l" 'movie-last-seen)
    (define-key map "d" 'movie-download-bigger)
    (define-key map "-" 'movie-collapse)
    (define-key map "M" 'movie-move-to-movie)
    (define-key map "i" 'movie-mark-as-seen)
    (define-key map "I" 'movie-mark-as-unseen)
    (define-key map "A" 'movie-add-stats-query)
    (define-key map "a" 'movie-add-stats)
    (define-key map "u" 'movie-undo-delete)
    (define-key map "U" 'movie-update-stats-file)
    (define-key map "P" 'movie-rotate-screen)
    (define-key map "." 'end-of-buffer)
    (define-key map "," 'beginning-of-buffer)
    (define-key map "}" 'scroll-down-command)
    (define-key map "'" 'scroll-up-command)
    (define-key map "/" 'movie-limit)
    (define-key map "m" 'movie-list-parts)
    (define-key map "O" 'movie-list-by-rip-time)
    (define-key map "o" 'movie-change-orientation)
    (define-key map "Y" 'movie-list-by-year)
    (define-key map "L" 'movie-list-by-director)
    (define-key map "e" 'movie-goto-last-series)
    (define-key map "E" 'movie-play-current)
    (define-key map "*" 'movie-mark)
    map))

(defvar movie-mode nil
  "Mode for Movie buffers.")

(defvar movie-mode-hook nil
  "Hook run in Movie mode buffers.")

(defun movie-mode (&optional arg)
  "Mode for Movie mode buffers.

\\{movie-mode-map}"
  (interactive (list current-prefix-arg))
  (make-local-variable 'movie-mode)
  (setq movie-mode
	(if (null arg) (not movie-mode)
	  (> (prefix-numeric-value arg) 0)))
  (setq major-mode 'movie-mode)
  (setq mode-name "Movie")
  (use-local-map movie-mode-map)
  (set (make-local-variable 'movie-order) nil)
  (set (make-local-variable 'movie-limit) nil)
  (setq-local movie-marks nil)
  (setq mode-line-buffer-identification
	'("Movie: " default-directory))
  (setq truncate-lines t)
  (run-hooks 'movie-mode-hook))

(defun movie-buffer-identification (dir)
  (let ((stats (movie-get-stats dir)))
    (if (not stats)
	""
      (format "%s %s %s"
	      (or (cdr (assoc "Year" stats)) "")
	      (or (cdr (assoc "Director" stats)) "")
	      (or (cdr (assoc "Country" stats)) "")))))

(defun movie-find-file (file)
  "Find or play the file under point."
  (interactive (list (movie-current-file)))
  (if (file-directory-p file)
      (movie-browse file 'alphabetical)
    (movie-play file)
    (discard-input)))

(defun movie-play-best-file (file &optional _no-adjust)
  "Play the longest file in the directory/file under point.
If NO-ADJUST (the interactive prefix), don't change the frame rate."
  (interactive (list (movie-current-file) current-prefix-arg))
  (call-process "pkill" nil nil nil "touchegg")
  (when (file-directory-p file)
    (setf file (movie-best-file file)))
  (movie-find-file file))

(defun movie-best-file (dir)
  (cdar
   (cl-sort
    (mapcar
     (lambda (file)
       (cons (elt (file-attributes file) 7)
	     file))
     (movie-find-files dir "."))
    '>
    :key 'car)))

(defun movie-find-files (dir match &optional include-directories)
  "Return all files under DIR that have file names matching MATCH (a regexp).
This function works recursively.  Files are returned in \"depth first\"
and alphabetical order.
If INCLUDE-DIRECTORIES, also include directories that have matching names."
  (let ((result nil)
	(files nil))
    (dolist (file (directory-files dir t))
      (let ((leaf (file-name-nondirectory file)))
	(unless (member leaf '("." ".."))
	  (if (file-directory-p file)
	      (progn
		(when (and include-directories
			   (string-match match leaf))
		  (push file files))
		(setq result (nconc result (movie-find-files
					    file match
					    include-directories))))
	    (when (string-match match leaf)
	      (push file files))))))
    (nconc result (nreverse files))))

(defun movie-find-torrent ()
  "Find torrent dir"
  (interactive)
  (movie-find-file "/tv/torrent")
  (movie-rescan-1 'alphabetical)
  (goto-char (point-max)))

(defun movie-play-cropped (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player movie-crop (list file))))

(defun movie-prefixed-action ()
  (interactive)
  (let ((options nil)
	(command nil)
	(player (cl-copy-list movie-player)))
    (while (let ((char (read-char "")))
	     (cond
	      ((eq char ?r)
	       (setq options
		     (append options (list "--vf=format=colormatrix=bt.709"))))
	      ((eq char ?g)
	       (setq options (append options (list "--vo=gpu-next"))))
	      ((eq char ?a)
	       (setq options (append options (list "--aspect=16:9"))))
	      ((eq char ?p)
	       (setq movie-inhibit-positions (not movie-inhibit-positions)))
	      ((eq char ?h)
	       (setq options (append options
				     (list "--hdr-compute-peak=no"
					   "--tone-mapping=reinhard"
					   "--tone-mapping-param=0.6"))))
	      (t
	       (setq command
		     (lookup-key movie-mode-map (format "%c" char)))
	       nil))))
    (when (eq command 'movie-find-file)
      (setq command 'movie-play-simple))
    (let ((movie-player (append player options)))
      (call-interactively command))))

(defun movie-add-vf (options vf)
  (setq options (cl-copy-list options))
  (let ((old (member "--vf" options)))
    (if (not old)
	(append options (list "--vf" vf))
      (setcar (cdr old)
	      (concat vf "," (cadr old)))
      options)))

(defun movie-remove-vf (options vf)
  (setq options (cl-copy-list options))
  (let ((old (member "-vf" options)))
    (if (not old)
	options
      (setcar (cdr old)
	      (mapconcat
	       #'identity
	       (delq
		nil
		(mapcar
		 (lambda (elem)
		   (if (equal elem vf)
		       nil
		     elem))
		 (split-string (cadr old) ",")))
	       ","))
      options)))

(defun movie-play-high-volume (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player movie-high-volume (list file))))

(defun movie-possible-subs (file)
  (list
   (concat (file-name-sans-extension file) ".srt")
   (concat file ".srt")
   (concat (file-name-sans-extension file) "_eng.srt")
   (file-name-concat
    (file-name-directory file)
    "Subs"
    (concat (file-name-sans-extension (file-name-nondirectory file)) ".sub"))
   (let ((dir (file-name-concat
	       (file-name-directory file)
	       "Subs"
	       (file-name-sans-extension (file-name-nondirectory file)))))
     (and (file-exists-p dir)
	  (car (directory-files dir t "English.*srt$"))))
   (let ((dir (file-name-concat
	       (file-name-directory file)
	       "Subs")))
     (and (file-exists-p dir)
	  (car (directory-files dir t "English.*srt$"))))))

(defun movie-play (file)
  (interactive (list (movie-current-file)))
  (let ((subs (movie-possible-subs file))
	(movie-player (copy-sequence movie-player)))
    (dolist (sub subs)
      (when (and sub (file-exists-p sub))
	(setq movie-player (append movie-player
				   (list (concat "--sub-file=" sub))))))
    (let ((stats (movie--stats-data file)))
      (when-let ((interlaced (plist-get stats :interlaced)))
	(setq movie-player (append movie-player
				   (list movie-deinterlace-switch))))
      (if-let ((sub-index (movie--find-best-subtitle stats)))
	  (setq movie-player (append movie-player
				     (list (format "--sid=%s" sub-index))))
	;; If we have an SRT file, and no internal sub, then use the
	;; srt file.  This logic isn't quite right...
	(setq movie-player (append movie-player
				   (list "--sid=1"))))
      (when-let ((aid (movie--find-best-audio stats)))
	(setq movie-player (append movie-player
				   (list (format "--aid=%s" aid))))))
    (movie-play-1 (append movie-player (list file)))))

(defun movie--find-best-audio (stats)
  (let* ((case-fold-search t))
    (cl-loop for (aid . lang) in (plist-get stats :audio)
	     when (string-match "english" lang)
	     return aid)))  

(defun movie--find-best-subtitle (stats)
  (let* ((case-fold-search t)
	 (titles
	  (cl-loop for i from 1
		   for sub in (plist-get stats :subtitles)
		   when (string-match "^eng" sub)
		   collect (cons i (if (string-match "forced" sub) 2 1)))))
    (caar (sort titles
		(lambda (t1 t2)
		  (< (cdr t1) (cdr t2)))))))
  
(defun movie-play-simple (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player (list file))))

(defun movie-find-position (file &optional no-skip)
  (or (movie-find-position-from-stats file no-skip)
      (movie-find-position-from-mplayer file no-skip)))

(defun movie-find-position-from-stats (file &optional no-skip)
  (let ((pos
	 (car
	  (last
	   (plist-get (cdr
		       (assoc
			(file-name-nondirectory file)
			(cdr (assoc 'tracks (movie-get-stats
					     (file-name-directory file))))))
		      :seen)
	   2))))
    (cond
     ((null pos)
      nil)
     (no-skip
      (format "%s" pos))
     (t
      (format "%s" (max 0 (- pos 2)))))))

(defun movie-find-position-from-mplayer (file &optional no-skip)
  (when (and (file-exists-p movie-positions-file)
	     (or (not (equal (system-name) "quimbies"))
		 (string-match "/tv/\\|/dvd/\\|http:\\|^/run" file))
	     (not (string-match "/title_"  file))
	     (not (equal file "/tv/live")))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
	(insert-file-contents movie-positions-file)
	(goto-char (point-max))
	(when (search-backward
	       (concat " " (file-name-nondirectory file) "\n") nil t)
	  (beginning-of-line)
	  (and (looking-at "[^ \n]+ \\([0-9]+\\)")
	       ;; Skip backwards two seconds to avoid missing a second.
	       (format "%d" (max (- (string-to-number (match-string 1))
				    (if no-skip 0 2))
				 0))))))))

(defun movie-find-geometry ()
  (let ((total-width (x-display-pixel-width))
	(this-width (nth 2 (frame-monitor-geometry))))
    (when (> total-width this-width)
      this-width)))

(defvar movie-rotate-audio nil)

(defun movie-start-mpv (command &optional wait)
  (interactive (list (append movie-player
			     (list (read-file-name "File: ")))))
  ;; Kill any mpv that may be playing in case there's one hanging.
  (call-process "pkill" nil nil nil "mpv")
  (let ((start-x (movie-find-geometry)))
    (when start-x
      (setq command (append command (list "-geometry"
					  (format "+%d+0" start-x))))))
  (setq movie-rotate-audio 0)
  (with-environment-variables (("MPV_VERBOSE" "1"))
    (with-current-buffer (get-buffer-create "*mplayer*")
      (when (file-exists-p "/tmp/mpv-socket")
	(delete-file "/tmp/mpv-socket"))
      (when movie-before-play-callback
	(with-current-buffer (get-buffer-create "*mpv*")
	  (funcall movie-before-play-callback)))
      (let ((mpv (apply 'start-process "mpv" (current-buffer) command)))
	(while (not (file-exists-p "/tmp/mpv-socket"))
	  (sleep-for 0.1))
	(make-network-process
	 :name "talk-mpv"
	 :service "/tmp/mpv-socket"
	 :buffer (get-buffer-create "*mpv*")
	 :family 'local
	 :filter 'movie--mpv-filter)
	(movie-send-mpv-command
	 '((command . ["observe_property" 1 "time-pos"])))
	(when wait
	  (while (process-live-p mpv)
	    (sleep-for 0.1))
	  (when movie-after-play-callback
	    (with-current-buffer (get-buffer-create "*mpv*")
	      (funcall movie-after-play-callback))))))))

(defun movie--mpv-filter (process string)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert string)))))


(defun movie-send-mpv-command (command)
  (with-current-buffer "*mpv*"
    (when (get-buffer-process (current-buffer))
      (process-send-string
       (get-buffer-process (current-buffer))
       (format "%s\n" (json-encode command))))))

(defvar movie-anim-state nil)

(defun movie-record-gif ()
  "Start/stop recording an animation."
  (interactive)
  (movie-send-mpv-command
   `((command . ["set"
		 "screenshot-template"
		 ,(if movie-anim-state
		      "mpv-shot%n"
		    (format "%s-%%n" (movie-find-anim-name)))])))
  (setq movie-anim-state (not movie-anim-state))
  (movie-send-mpv-command
   `((command . ["screenshot" "video" "each-frame"]))))

(defvar movie-recording-directory "/tmp")
(defvar movie--current-title "unknown")
(defvar movie--file-currently-playing "/tmp")

(defun movie-find-anim-name ()
  (let ((num 1)
	result)
    (while (directory-files movie-recording-directory
			    nil (setq result (format "anim%02d" num)))
      (cl-incf num))
    result))

(defconst movie-audio-devices
  '(("alsa/hw:CARD=J65,DEV=0" "0.250")	; Headphones
    ("alsa/plughw:CARD=NVidia,DEV=7" "0")	; TV
    ("alsa/plughw:CARD=PCH,DEV=0" "0.250"))) ; Speakers

(defvar movie-current-audio-device 0)

(defun movie-rotate-audio ()
  "Change the audio output."
  (interactive)
  (let ((elem (elt movie-audio-devices
		   (mod (cl-incf movie-current-audio-device)
			(length movie-audio-devices)))))
    (movie-send-mpv-command
     `((command . ["set_property" "audio-device" ,(car elem)])))
    (movie-send-mpv-command
     `((command . ["set_property" "audio-delay" ,(cadr elem)])))))

(defun movie--file-title (file)
  (let ((stats-file (expand-file-name "stats" (file-name-directory file)))
	(title nil))
    (when (file-exists-p stats-file)
      (let ((stats (movie-get-stats (file-name-directory stats-file))))
	(setq title (cdr (assoc "Title" stats)))))
    (unless title
      (setq title (file-name-sans-extension (file-name-nondirectory file)))
      (setq title (replace-regexp-in-string "-title.*" "" title))
      (setq title (movie-prefix title)))
    title))

(defun movie-play-1 (player)
  (setq movie-current-audio-device 0
	movie-anim-state nil
	movie--file-currently-playing (car (last player)))
  (when-let ((skip (and (not movie-inhibit-positions)
			(movie-find-position
			 (or movie-file-id
			     (car (last player)))))))
    (setq player (cons (pop player)
		       (append (list (format "--start=%s" skip))
			       player))))
  (if (not movie-picture-directory)
      (with-current-buffer (get-buffer-create "*mplayer*")
	(buffer-disable-undo)
	(erase-buffer)
	(movie-start-mpv player t)
	(movie-update-mplayer-position (car (last player))))
    (let* ((path (file-truename (car (last player))))
	   (file (file-name-nondirectory
		  (directory-file-name (file-name-directory path))))
	   (dir (format
		 "/var/tmp/screenshots/%s/"
		 (if (zerop (length file))
		     "unknown"
		   file)))
	   (highest (movie-find-highest-image)))
      (when (file-symlink-p "~/.movie-current")
	(delete-file "~/.movie-current"))
      (make-symbolic-link dir "~/.movie-current")
      (unless (file-exists-p dir)
	(make-directory dir t))
      ;; And record the file.
      (when (file-symlink-p "~/.movie-current-file")
	(delete-file "~/.movie-current-file"))
      (make-symbolic-link path "~/.movie-current-file")
      (let ((stats (expand-file-name "stats" (file-name-directory path))))
	(when (file-exists-p stats)
	  (copy-file stats (expand-file-name "stats" dir) t)))
      (with-temp-buffer
	(insert path)
	(let ((coding-system-for-write 'utf-8))
	  (when (file-exists-p "/tv/data/")
	    (write-region (point-min) (point-max) "/tv/data/current-file"
			  nil 'silent))))
      (with-current-buffer (get-buffer-create "*mplayer*")
	(buffer-disable-undo)
	(erase-buffer)
	(insert (format "%s\n" player))
	;; mplayer will store the screenshots in the current directory.
	(setq default-directory dir
	      movie-recording-directory dir)
	(movie-start-mpv player t)
	(movie-update-mplayer-position (car (last player)))
	(movie-copy-images-higher-than highest dir))
      (let ((stats (expand-file-name "stats" (file-name-directory path))))
	(when (file-exists-p stats)
	  (copy-file stats (expand-file-name "stats" dir) t)))))
  (movie-update-stats-position (car (last player)))
  (movie-update-line))

(defun movie-update-line ()
  (when-let ((object (vtable-current-object))
	     (seen (movie-find-position (plist-get object :file))))
    (plist-put object :seen
	       (append (plist-get object :seen)
		       (list (string-to-number seen) "")))
    (vtable-update-object (vtable-current-table) object object)))

(defun movie-update-mplayer-position (file)
  (ignore-errors
    (unless (file-exists-p movie-positions-file)
      (unless (file-exists-p (file-name-directory movie-positions-file))
	(make-directory (file-name-directory movie-positions-file)))
      (write-region 1 1 movie-positions-file)))
  (with-current-buffer "*mpv*"
    (goto-char (point-max))
    (when (and (file-exists-p movie-positions-file)
	       (re-search-backward "time-pos.*[0-9]" nil t))
      (beginning-of-line)
      (let* ((json (json-read))
	     (position (cdr (assq 'data json)))
	     (coding-system-for-read 'utf-8)
	     (coding-system-for-write 'utf-8))
	(with-temp-buffer
	  (insert (format "%s %s %s\n"
			  (format-time-string "%FT%T")
			  position (file-name-nondirectory file)))
	  (write-region (point-min) (point-max) movie-positions-file
			'append 'nomessage))))))

(defun movie-find-highest-image ()
  (car
   (sort (mapcar 'movie-image-number
		 (directory-files movie-picture-directory nil "IMG_[0-9]+.JPG"))
	 '>)))

(defun movie-image-number (file)
  (if (string-match "IMG_\\([0-9]+\\).JPG" file)
      (string-to-number (match-string 1 file))
    0))

(defun movie-copy-images-higher-than (highest dir)
  (dolist (file (directory-files movie-picture-directory t "IMG_[0-9]+.JPG"))
    (when (> (movie-image-number file) highest)
      (let ((new-file (expand-file-name (file-name-nondirectory file) dir)))
	(call-process "convert" nil nil nil "-resize" "1000x" file new-file)))))

(defvar movie-scheduled-deletions nil)

(defun movie-delete-file (file)
  "Delete the file under point."
  (interactive (list (movie-current-file)))
  (unless file
    (error "No file on the current line"))
  (unless (file-exists-p file)
    (error "%s doesn't exist" file))
  (when (and (file-directory-p file)
	     (not (string-match "/torrent" file))
	     (not (string-match "/future/series" file))
	     (not (string-match "/future/rainy-day" file))
	     (not (string-match "/future/films" file))
	     (not (= (length (directory-files-recursively file ".")) 2)))
    (error "Directory not empty"))
  (beginning-of-line)
  (let ((new-name (expand-file-name
		   (concat ".deleted-" (file-name-nondirectory file))
		   (file-name-directory file))))
    (push
     (list :name file
	   :deletion-name new-name
	   :time (float-time)
	   :display (buffer-substring (point) (line-beginning-position 2)))
     movie-scheduled-deletions)
    (rename-file file new-name))
  (delete-region (point) (line-beginning-position 2))
  (when (and (not (bobp))
	     (eq movie-order 'chronological))
    (forward-line -1))
  (unless (eobp)
    (forward-char 1)))

(defun movie-undo-delete ()
  "Undo a scheduled deletion."
  (interactive)
  (let ((elem (pop movie-scheduled-deletions)))
    (unless elem
      (error "No further deletions scheduled"))
    (unless (file-exists-p (plist-get elem :deletion-name))
      (error "File %s has been completely deleted"
	     (plist-get elem :deletion-name)))
    (beginning-of-line)
    (insert (plist-get elem :display))
    (forward-line -1)
    (rename-file (plist-get elem :deletion-name)
		 (plist-get elem :name))))

(defun movie-delete-scheduled ()
  (let ((deleted nil))
    (dolist (elem movie-scheduled-deletions)
      ;; Delete files that were scheduled for deletion more than ten
      ;; minutes ago.
      (when (> (- (float-time) (* 10 60))
	       (plist-get elem :time))
	;; More than ten minutes has passed, so delete.
	(let ((file (plist-get elem :deletion-name)))
	  (when (file-exists-p file)
	    (if (file-directory-p file)
		(delete-directory file t)
	      (delete-file file))
	    (let ((png (concat (plist-get elem :name) ".png")))
	      (when (file-exists-p png)
		(delete-file png)))
	    ;; We need both of these because of the apparent process
	    ;; that's fixing .png files with missing movie files.
	    (let ((png (concat (plist-get elem :deletion-name) ".png")))
	      (when (file-exists-p png)
		(delete-file png)))))
	(push elem deleted)))
    (dolist (elem deleted)
      (setq movie-scheduled-deletions
	    (delq elem movie-scheduled-deletions)))))

(defun movie-add-stats (dir &optional no-director)
  "Add a stats file to the directory under point."
  (interactive (list (movie-current-file) current-prefix-arg))
  (unless (file-directory-p dir)
    (setq dir (directory-file-name (file-name-directory dir))))
  (movie-make-stats-file dir no-director)
  (message "Made a stats file for %s" dir))

(defun movie-add-stats-query (dir title)
  "Add a stats file to the directory under point."
  (interactive (list (movie-current-file)
		     (read-string "Title: " (file-name-nondirectory
					     (movie-current-file)))))
  (unless (file-directory-p dir)
    (setq dir (directory-file-name (file-name-directory dir))))
  (movie-make-stats-file dir nil title)
  (message "Made a stats file for %s" dir))

(defun movie-current-file ()
  (or
   (save-excursion
     (beginning-of-line)
     (cl-getf (vtable-current-object) :file))
   (and (not (equal default-directory "/dvd/"))
	(save-excursion
	  (goto-char (point-min))
	  (cl-getf (vtable-current-object) :file)))))

(defun movie-rescan (&optional order)
  "Update the current buffer."
  (interactive)
  (movie-rescan-1 order))

(defun movie-rescan-1 (&optional order)
  (unless order
    (setq order movie-order))
  (when (and (bolp)
	     (not (eobp)))
    (forward-char 1))
  (let ((lines (count-lines (point-min) (point))))
    (movie-browse default-directory order movie-limit)
    (goto-char (point-min))
    (forward-line (1- lines))))

(defun movie-toggle-sort ()
  "Toggle sorting by time."
  (interactive)
  (while (and (not (vtable-current-table))
	      (not (bobp)))
    (forward-line -1))
  (if (eq movie-order 'alphabetical)
      (setq movie-order 'chronological)
    (setq movie-order 'alphabetical))
  (setf (vtable-objects (vtable-current-table))
	(sort (vtable-objects (vtable-current-table))
	      (lambda (o1 o2)
		(movie-compare-lines movie-order o1 o2))))
  (vtable-revert-command))

(defun movie-compare-lines (order d1 d2)
  (if (eq order 'alphabetical)
      (string< (movie--canonicalize-name (cl-getf d1 :file))
	       (movie--canonicalize-name (cl-getf d2 :file)))
    (time-less-p (cl-getf d1 :time) (cl-getf d2 :time))))

(defun movie--canonicalize-name (name)
  (replace-regexp-in-string "\\`\\(the\\|an\\|a\\)[. ]" ""
			    (downcase (file-name-nondirectory name))))

(defun movie-rename (to)
  "Rename the current movie."
  (interactive "FNew name: ")
  (rename-file (movie-current-file) to))

(defun movie-collapse ()
  "Move all files matching a prefix to the same directory."
  (interactive)
  (let* ((prefix (movie-prefix (file-name-nondirectory (movie-current-file))))
	 (dir (expand-file-name prefix "/tv/future/series/")))
    (when (zerop (length prefix))
      (error "No prefix"))
    (unless (file-exists-p dir)
      (setq dir (downcase dir))
      (unless (file-exists-p dir)
	(make-directory dir)))
    (dolist (file (directory-files default-directory t))
      (when (and (not (equal (file-name-nondirectory file) "."))
		 (not (equal (file-name-nondirectory file) ".."))
		 (string-equal
		  (downcase (or (movie-prefix (file-name-nondirectory file))
				""))
		  (downcase prefix)))
	(save-excursion
	  (when (movie-goto-movie (file-name-nondirectory file))
	    (delete-line)))
	(rename-file file (concat dir "/"))))))

(defun movie-move-to-movie (from)
  "Move film to future/films."
  (interactive (list (movie-current-file)))
  (rename-file from (expand-file-name (file-name-nondirectory from)
				      "/tv/future/films/"))
  (let ((png (concat from ".png")))
    (when (file-exists-p png)
      (rename-file png (expand-file-name (file-name-nondirectory png)
					 "/tv/future/films/"))))
  (delete-line))

(defun movie-remove-seen ()
  "Delete the lines of seen movies."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (get-text-property (1- (line-end-position)) 'face)
	  (delete-region (point) (line-beginning-position 2))
	(forward-line 1)))))

(defun movie-prefix (file)
  (setq file (replace-regexp-in-string " " "." file))
  (catch 'end
    (let ((prefix ""))
      (dolist (part (split-string file "[.]"))
	(if (string-match "S[0-9]+E[0-9]+" part)
	    (throw 'end prefix)
	  (setq prefix
		(concat
		 prefix
		 (if (zerop (length prefix))
		     part
		   (concat "." part)))))))))

(defun movie-find-previous-vob (data)
  (with-temp-buffer
    (insert-file-contents movie-positions-file)
    (goto-char (point-max))
    (when (re-search-backward (format " %s#[0-9]+#\\([0-9]+\\)\n"
				      (regexp-quote (car data)))
			      nil t)
      (string-to-number (match-string 1)))))

(defun movie-play-dvd-vob (data number)
  ;; DVD VOBs don't have natural "file names", so create a file ID
  ;; and use that to look up whether we want to skip into it
  ;; (because we've already seen parts of it)...
  (let ((movie-file-id (format "/dvd/%s#%d#%d"
			       (car data)
			       (elt (cadr data) (1- number))
			       number)))
    (movie-play (format "dvd://%d" (elt (cadr data) (1- number))))
    ;; And after playing the movie, update the data from the
    ;; .positions file to be this file ID.
    (when (file-exists-p movie-positions-file)
      (with-temp-buffer
	(insert-file-contents movie-positions-file)
	(goto-char (point-max))
	(forward-line -1)
	(when (looking-at "[0-9.]+ \\([0-9]+\\)\n")
	  (goto-char (match-beginning 1))
	  (delete-region (point) (line-end-position))
	  (insert (file-name-nondirectory movie-file-id))
	  (write-region (point-min) (point-max) movie-positions-file
			nil 'silent))))))

(defun movie-dvd-data ()
  (let (big-files index title)
    (with-temp-buffer
      (call-process "lsdvd" nil (current-buffer) nil "-Op")
      (goto-char (point-min))
      (when (re-search-forward "title => '\\(.*\\)'" nil t)
	(setq title (match-string 1)))
      (while (re-search-forward "ix => \\([0-9]+\\)" nil t)
	(setq index (string-to-number (match-string 1)))
	(when (re-search-forward "length => \\([0-9]+\\)" nil t)
	  (when (> (string-to-number (match-string 1)) 500)
	    (push index big-files)))))
    (list title (nreverse big-files))))

(defun movie-play-whole-dvd (number)
  "Play the DVD."
  (interactive "p")
  (movie-play (format "dvd://%d" number)))

(defun movie-play-total-dvd (&optional number)
  "Play the DVD."
  (interactive "P")
  (movie-play (format "dvd://%s" (or number ""))))

(defun movie-play-vlc-dvd (number)
  "Play the DVD."
  (interactive "p")
  (movie-play (format "dvd://%d" number)))

(defun movie-emacsclient (command)
  (call-process "emacsclient" nil t nil
		"--server-file=potato" 
		"--eval" command))

(defun movie-thumbnails ()
  "Create missing thumbnails."
  (interactive)
  (call-process "~/src/movie.el/thumbnail-movies"))

(defun movie-eject ()
  "Eject the cd."
  (interactive)
  (call-process "eject" nil nil nil "/dev/dvd"))

(require 'server)

(defun movie-start-server ()
  (setq server-use-tcp t
	server-host (system-name)
	server-name "quimbies-mov")
  (server-start))

(defun movie-direct-url (url)
  (with-temp-buffer
    (call-process "youtube-dl" nil (list (current-buffer) nil) nil "-g"
		  "-f" (movie-best-youtube-format url) url)
    (goto-char (point-min))
    (buffer-substring (point) (line-end-position))))

(defun movie-best-youtube-format (url)
  (with-temp-buffer
    (call-process "youtube-dl" nil (list (current-buffer) nil) nil
		  "-F" url)
    (goto-char (point-min))
    (re-search-forward "^format")
    (forward-line 1)
    (delete-region (point-min) (point))
    (while (re-search-forward "audio only\\|video only" nil t)
      (delete-region (line-beginning-position)
		     (line-beginning-position 2)))
    (goto-char (point-min))
    (caar
     (sort
      (nreverse
       (cl-loop while (not (eobp))
		for (format _code resolution _note) =
		(split-string (buffer-substring (point) (line-end-position)))
		collect (cons format resolution)
		do (forward-line 1)))
      'movie-resolution-predicate))))

(defun movie-resolution-predicate (e1 e2)
  (> (movie-resolution-predicate-1 e1)
     (movie-resolution-predicate-1 e2)))

(defun movie-resolution-predicate-1 (elem)
  (let ((x (string-to-number (car (split-string (cdr elem) "x")))))
    (if (> x 3840)
	-1
      x)))

(defun movie-play-youtube (url &optional aspect)
  (message "Got youtube url %s" url)
  (movie-play-1
   (append movie-player
	   (and aspect
		(list "-aspect" "4:3"))
	   (list
	    (movie-direct-url url)))))

(defun movie-get-mkv-info (file)
  "Output pertinent information about MKV FILE."
  (interactive "fMKV File: ")
  (let* ((dom (mkv-information file))
	 (subtitles
	  (cl-loop for track in (dom-by-tag dom 'Track)
		   when (equal (dom-attr track :Track-type) "subtitles")
		   collect (dom-attr track :Language))))
    `(:length ,(movie-mkv-length
		(dom-attr (dom-by-tag dom 'Segment-information) :Duration))
	      :audio-tracks
	      ,(cl-loop for track in (dom-by-tag dom 'Track)
			when (equal (dom-attr track :Track-type) "audio")
			collect (or (dom-attr track :Language)
				    (dom-attr track :Name)))
	      ,@(when subtitles
		  (list :subtitles subtitles)))))

(defun movie-mkv-length (string)
  (let ((time (iso8601-parse-time (replace-regexp-in-string
				   "^.*(" ""
				   (replace-regexp-in-string ")" ""
							     string)))))
    (+ (* (decoded-time-hour time) 60 60)
       (* (decoded-time-minute time) 60)
       (decoded-time-second time))))

(defun movie--stats-data (file)
  ;; DVD files have stats already; just return those.
  (or (when-let ((stats (movie-get-stats (file-name-directory file))))
	(cdr (assoc (file-name-nondirectory file)
		    (cdr (plist-get stats 'tracks)))))
      ;; Otherwise, use mediainfo to synthesize them.
      (with-temp-buffer
	(with-environment-variables (("LC_ALL" (getenv "LANG")))
	  (call-process "mediainfo" nil t nil file))
	(goto-char (point-min))
	(list :interlaced
	      (save-excursion
		(re-search-forward "^Scan type.*Interlace" nil t))
	      :subtitles
	      (save-excursion
		(cl-loop
		 while (re-search-forward "\n\nText" nil t)
		 collect
		 (let ((lang
			(save-excursion
			  (and
			   (re-search-forward "^Language.*: \\(.*\\)"
					      (movie--mediainfo-block-end) t)
			   (match-string 1))))
		       (title
			(save-excursion
			  (and
			   (re-search-forward "^Title.*: \\(.*\\)"
					      (movie--mediainfo-block-end) t)
			   (match-string 1)))))
		   (string-join (list lang title) " "))))
	      :audio
	      (save-excursion
		(cl-loop
		 while (re-search-forward "\n\nAudio #\\([0-9]+\\)" nil t)
		 collect
		 (cons (match-string 1)
		       (save-excursion
			 (and
			  (re-search-forward "^Language.*: \\(.*\\)"
					     (movie--mediainfo-block-end) t)
			  (match-string 1))))))))))

(defun movie--mediainfo-block-end ()
  (save-excursion
    (or (search-forward "\n\n" nil t)
	(point-max))))

(defun movie-interlaced-p (file)
  (let ((stats (movie-get-stats (file-name-directory file))))
    (if stats
	(cl-getf (cdr (assoc (file-name-nondirectory file)
			     (cdr
			      (assq 'tracks stats))))
		 :interlaced)
      (and (not (member (system-name) '("mouse" "sandy" "quimbies")))
	   (with-temp-buffer
	     (with-environment-variables (("LC_ALL" (getenv "LANG")))
	       (call-process "mediainfo" nil t nil file))
	     (goto-char (point-min))
	     (re-search-forward "^Scan type.*Interlace" nil t))))))

(defun movie-make-stats-file (directory &optional no-directory title)
  "Create a stats file for DIRECTORY."
  (interactive "dDirectory: \nP")
  (with-temp-file (expand-file-name "stats" directory)
    (let* ((title (replace-regexp-in-string
		   " +([0-9]+)$" ""
		   (or title
		       (file-name-nondirectory directory))))
	   (imdb (if (not no-directory)
		     (imdb-query title)))
	   (files (directory-files directory t "mkv$")))
      (insert (format "Title: %s\n" title))
      (when imdb
	(insert (format "Director: %s\nYear: %s\nCountry: %s\nIMDB: %s\n"
			(string-replace " + " ", " (plist-get imdb :director))
			(plist-get imdb :year)
			(plist-get imdb :country)
			(or (plist-get imdb :id) ""))))
      (insert
       (format
	"Genre: %s\nRecorded: %s\n"
	(completing-read
	 "Genre: "
	 movie-genres)
	(replace-regexp-in-string
	 "[-:]" ""
	 (format-time-string
	  "%FT%T"
	  (nth 5 (file-attributes (car files)))))))
      (let ((seen (expand-file-name "seen-date" directory)))
	(when (file-exists-p seen)
	  (let ((date
		 (with-temp-buffer
		   (insert-file-contents seen)
		   (replace-regexp-in-string "[^0-9T]" "" (buffer-string)))))
	    (insert (format "Status: seen\nSeen: %s\n" date)))))
      (insert "\n")
      (dolist (file files)
	(let ((position (movie-find-position file t)))
	  (insert (format
		   "%S\n"
		   `(,(file-name-nondirectory file)
		     ,@(movie-get-mkv-info file)
		     ,@(when (movie-interlaced-p file)
			 `(list :interlaced t))
		     ,@(when position
			 `(:seen (,(string-to-number position)
				  "19700101T010000"))))))))))
  (movie--get-missing-posters directory))

(defun movie-parse-stats (directory)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "stats" directory))
    (let ((data nil))
      (while (looking-at "\\([^:\n]+\\): ?\\(.*\\)")
	(setq data (plist-put data
			      (intern (concat ":" (downcase (match-string 1))))
			      (match-string 2)))
	(forward-line 1))
      data)))

(defun movie-update-stats-file (directory)
  "Update the country/id in the stats file for DIRECTORY."
  (interactive (list (movie-current-file)))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "stats" directory))
    (when (re-search-forward "^IMDB:" nil t)
      (error "IMDB id already registered"))
    (let* ((stats (movie-parse-stats directory))
	   (data (imdb-query-full (plist-get stats :title)))
	   imdb)
      (when data
	(cl-loop for elem in data
		 when (or (equal (plist-get stats :director)
				 (plist-get elem :director))
			  (equal (plist-get stats :year)
				 (plist-get elem :year)))
		 do (setq imdb elem))
	(unless imdb
	  (setq imdb (imdb-query (plist-get stats :title))))
	(when imdb
	  (re-search-forward "^$")
	  (insert (format "Country: %s\nIMDB: %s\n"
			  (plist-get imdb :country)
			  (plist-get imdb :id)))
	  (write-region (point-min) (point-max)
			(expand-file-name "stats" directory))
	  (message "The country is %s" (plist-get imdb :country))))))
  (forward-line 1))

(defun movie-one-directory ()
  "Move files from (2)-like subdirectories to the current directory."
  (interactive)
  (dolist (sub (directory-files default-directory t " ([0-9]+)$"))
    (let ((part (and (string-match "([0-9]+)$" sub)
		     (match-string 0 sub))))
      (dolist (file (directory-files sub t))
	(when (file-regular-p file)
	  (let ((leaf (file-name-nondirectory file)))
	    (unless (cl-search part leaf)
	      (setq leaf (concat part " " leaf)))
	    (let ((new (expand-file-name leaf default-directory)))
	      (unless (file-exists-p new)
		(rename-file file new))))))
      (delete-directory sub))))

(defun movie-mark-as-seen (&optional mostly file)
  "Mark the current DVD directory as seen in the stats file."
  (interactive (list current-prefix-arg (movie-current-file)))
  (let ((stats (expand-file-name
		"stats"
		(if (file-directory-p file)
		    file
		  default-directory))))
    (unless (file-exists-p stats)
      (error "No stats file"))
    (with-temp-file stats
      (insert-file-contents stats)
      (if (re-search-forward "^Status:" nil t)
	  (delete-region (match-beginning 0) (progn (forward-line 1) (point)))
	(search-forward "\n\n")
	(forward-line -1))
      (insert (format "Status: %s\n"
		      (if mostly (if (equal mostly 1)
				     "seen-version"
				   "mostly-seen")
			"seen")))
      (insert (format "Seen: %s\n" (format-time-string "%Y%m%dT%H%M%S"))))
    (message "Marked as seen")))

(defun movie-mark-as-unseen (file)
  "Mark the current DVD directory as unseen in the stats file."
  (interactive (list (movie-current-file)))
  (let ((stats (expand-file-name
		"stats"
		(if (file-directory-p file)
		    file
		  default-directory))))
    (unless (file-exists-p stats)
      (error "No stats file"))
    (with-temp-file stats
      (insert-file-contents stats)
      (when (search-forward (concat "\n(\"" (file-name-nondirectory file) "\"")
			    nil t)
	(while (search-forward " :seen" (line-end-position) t)
	  (goto-char (match-beginning 0))
	  (kill-sexp 2)))
      (message "Marked as unseen"))))

(defun movie-update-stats-position (file)
  (let* ((dir (file-name-directory file))
	 (stats (movie-get-stats dir))
	 (stats-file (expand-file-name "stats" dir)))
    (when stats
      (let ((position (movie-find-position-from-mplayer file t))
	    (track (assoc (file-name-nondirectory file)
			  (cdr (assoc 'tracks stats)))))
	(when (and position track)
	  (plist-put (delq 'list (cdr track)) :seen
		     (append (plist-get (cdr track) :seen)
			     (list (string-to-number position)
				   (format-time-string "%Y%m%dT%H%M%S"))))
	  (with-temp-file stats-file
	    (insert-file-contents stats-file)
	    (when (search-forward "\n\n" nil t)
	      (delete-region (point) (point-max))
	      (dolist (track (cdr (assoc 'tracks stats)))
		(insert (format "%S\n" track))))))))))

(defun movie-create-unseen-directory ()
  "Create a directory of symlinks to the unseen films for easier rsyncing."
  (interactive)
  (dolist (file (directory-files "/tv/links/unseen" t))
    (when (file-symlink-p file)
      (delete-file file)))
  (dolist (movie (movie-get-files "/dvd"))
    (let ((genre (split-string (or (plist-get movie :genre) "") ",")))
      (when (and (not (plist-get movie :seen))
		 (plist-get movie :genre)
		 (plist-get movie :year)
		 (not (plist-get movie :mostly-seen))
		 (not (plist-get movie :seen-version))
		 ;;(member (plist-get movie :country) '("" "fr" "us" "gb" "ca"))
		 (plist-get movie :genre)
		 (not (string-match "Star Trek" (plist-get movie :file)))
		 (not (string-match "Allen"
				    (plist-get movie :director)))
		 (not (member "tv" genre))
		 (not (member "best" genre))
		 (not (member "best22" genre))
		 (not (member "Amazon" genre))
		 (not (member "comics" genre))
		 (not (member "eclipse" genre))
		 )
	(let ((file (plist-get movie :file)))
	  (make-symbolic-link
	   file (expand-file-name (file-name-nondirectory file)
				  "/tv/links/unseen")))))))

(defun movie-split-unseen (size)
  "Move some files from /tv/links/unseen until we have SIZE MiB.
In /tv/links/other-unseen."
  (dolist (file (directory-files "/tv/links/other-unseen" t))
    (when (file-symlink-p file)
      (delete-file file)))
  (let ((films
	 (sort
	  (cl-loop for film in (directory-files "/tv/links/unseen" t)
		   unless (string-match "^[.]" (file-name-nondirectory film))
		   collect (cons (movie-film-size film) film))
	  (lambda (_f1 _f2)
	    (< (random) (random))))))
    (cl-loop for (s . link) in films
	     do (cl-decf size (/ (float s) 1000 1000 1000))
	     while (cl-plusp size)
	     do (rename-file link (expand-file-name
				   (file-name-nondirectory link)
				   "/tv/links/other-unseen")))))

(defun movie-move-small-unseen ()
  "Create a directory of the smallest films from the unseen directory."
  (interactive)
  (dolist (file (directory-files "/tv/links/smallunseen" t))
    (when (file-symlink-p file)
      (delete-file file)))
  (let ((films
	 (sort
	  (cl-loop for film in (directory-files "/tv/links/unseen" t)
		   unless (string-match "^[.]" (file-name-nondirectory film))
		   collect (cons (movie-film-size film) film))
	  (lambda (f1 f2)
	    (< (movie-small-rank f1) (movie-small-rank f2)))))
	;; 1.4TB
	(total (* 1000 1000 1000 1000 1.4)))
    (cl-loop for (size . film) in films
	     while (cl-plusp total)
	     do
	     (cl-decf total size)
	     (rename-file film
			  (expand-file-name (file-name-nondirectory film)
					    "/tv/links/smallunseen")))))

(defvar movie-special-small-genre nil)

(defun movie-small-rank (f)
  (let ((size (car f))
	(file (cdr f)))
    (or
     (with-temp-buffer
       (insert-file-contents (expand-file-name "stats" file))
       (and movie-special-small-genre
	    (re-search-forward movie-special-small-genre nil t)
	    0)
       size))))

(defun movie-limit-unseen-directory ()
  "Limit the unseem directory to a specific size by removing largest movies."
  (interactive)
  (let ((films
	 (sort
	  (cl-loop for film in (directory-files "/tv/links/unseen" t)
		   unless (string-match "^[.]" (file-name-nondirectory film))
		   collect (cons (movie-film-size film) film))
	  (lambda (f1 f2)
	    (< (car f1) (car f2)))))
	;; 4TB
	(total (* 1000 1000 1000 4000)))
    ;; Peel off the smallest films.
    (cl-loop for elem in (cl-copy-list films)
	     for (size . film) = elem
	     while (cl-plusp total)
	     do
	     (cl-decf total size)
	     (setq films (delq elem films)))
    ;; Remove the rest.
    (cl-loop for (_ . film) in films
	     do (delete-file film))))

(defun movie-create-unseen-and-small-directory ()
  (interactive)
  (movie-create-unseen-directory)
  (movie-move-small-unseen)
  (movie-limit-unseen-directory))

(defun movie-film-size (film)
  (cl-loop for file in (directory-files-recursively film ".")
	   sum (* 1.0 (file-attribute-size (file-attributes file)))))

;; "Bitchin Rides S01E06 The Juice Is Worth the Squeeze HDTV XviD-AF"
;; "Naild.It.S01E01.Nail.Pride.HDTV.x264-DaViEW"
;; "The Daily Show 2014 10 07 Wyatt Cenac HDTV x264-W4F [GloDLS]"
(defun movie-parse-description (desc)
  (let ((case-fold-search t))
    (cond
     ((string-match "^\\(.*\\)[. ]+\\(s\\([0-9]+\\)e\\([0-9]+\\)\\)" desc)
      (list :name (match-string 1 desc)
	    :season (movie-clean-number (match-string 3 desc))
	    :episode (movie-clean-number (match-string 4 desc))
	    :epspec (match-string 2 desc)))
     ((string-match "^\\(.*?\\)[. ]+\\([-0-9 ]+\\)" desc)
      (list :name (match-string 1 desc)
	    :season "0"
	    :episode (movie-clean-number (match-string 2 desc))
	    :epspec (match-string 2 desc)))
     (t
      (message "Unable to parse %s" desc)
      nil))))

(defun movie-clean-number (string)
  (save-match-data
    (replace-regexp-in-string
     " +" "-"
     (replace-regexp-in-string
      "^0+\\|^ +\\| $" "" string))))

(autoload 'kickass-download-bigger "kickass")

(defun movie-download-bigger (file)
  "Download bigger versions of the file under point."
  (interactive (list (movie-current-file)))
  (let ((data (movie-parse-description (file-name-nondirectory file))))
    (unless data
      (error "Couldn't parse %s" (file-name-nondirectory file)))
    (let ((names
	   (kickass-download-bigger (concat (plist-get data :name)
					    " "
					    (plist-get data :epspec)))))
      (message "Downloading %s" (string-join names "\n")))))

(defun movie-last-seen (file &optional edit)
  "Say when the series under point was last seen.
If EDIT (the prefix), allow editing"
  (interactive (list (movie-current-file)
		     current-prefix-arg))
  (let* ((data (movie-parse-description (file-name-nondirectory file)))
	 (name (plist-get data :name))
	 (case-fold-search t)
	 results)
    (unless data
      (error "Couldn't parse %s" (file-name-nondirectory file)))
    (when edit
      (setq name (read-string "Look for: " name)))
    (with-temp-buffer
      (insert-file-contents movie-positions-file)
      (goto-char (point-max))
      (while (and (re-search-backward
		   (replace-regexp-in-string "[^a-z0-9]" ".*" name)
		   nil t)
		  (< (length results) 5))
	(beginning-of-line)
	(when (looking-at "[^ \n]+ \\([0-9.]+\\) \\(.*\\)")
	  (let ((length (string-to-number (match-string 1)))
		(show (match-string 2)))
	    (when (and (not (member show results))
		       (> length 400))
	      (push show results))))))
    (if (not results)
	(message "Not seen %s" name)
      (message "%s" (mapconcat 'identity (nreverse results) "\n")))))

(defun movie-title (movie)
  (replace-regexp-in-string "\\`\\(The\\|A\\) " ""
			    (or (plist-get movie :title) "")))

(defun movie-create-unseen-html ()
  "Create a simple HTML page for unseen films."
  (interactive)
  (with-temp-file "/tmp/unseen.html"
    (insert "<html>")
    (insert "<meta charset=utf-8><body>")
    (dolist (movie (sort (movie-get-files "/dvd")
			 (lambda (m1 m2)
			   (string< (movie-title m1) (movie-title m2)))))
      (when (and (not (plist-get movie :seen))
		 (not (plist-get movie :mostly-seen))
		 (not (plist-get movie :seen-version))
		 (plist-get movie :genre)
		 (not (string-match "Star Trek" (plist-get movie :file)))
		 (not (string-match "Allen" (plist-get movie :director)))
		 (not (string-match "tv" (plist-get movie :genre)))
		 (not (string-match "comics" (plist-get movie :genre))))
	(if (plist-get movie :imdb)
	    (insert
	     (format "<a href=\"http://www.imdb.com/title/%s/?ref_=nv_sr_1\">%s</a>"
		     (plist-get movie :imdb)
		     (plist-get movie :title)))
	  (insert (plist-get movie :title)))
	(insert "<br>\n")))
    (message "%s unseen" (count-lines (point-min) (point-max))))
  (call-process "scp" nil nil nil "/tmp/unseen.html" "www@quimby:html/s/"))

(defvar movie-upload-file-command '("ncftpput" "host" "/sdcard"))

(defun movie-upload-file (file)
  "Upload the file under point to the Galaxy View."
  (interactive (list (movie-current-file)))
  (when (file-directory-p file)
    (setf file (movie-best-file file)))
  (message "Uploading %s..." file)
  (apply 'start-process
	 "uploading" (get-buffer-create "*upload*")
	 (car movie-upload-file-command)
	 (append (cdr movie-upload-file-command)
		 (list file))))

(defun movie-delete-superfluous-thumbnails ()
  (interactive)
  (let ((tail "[.]png\\'"))
    (dolist (png (directory-files-recursively "/tv/torrent" tail))
      (let ((base (replace-regexp-in-string tail "" png)))
	(unless (file-exists-p base)
	  (message "%s" png)
	  (delete-file png))))))

(defvar movie-prime-directory "/media/sdd1")

(defun movie-concatenate-prime ()
  (interactive)
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (file (directory-files movie-prime-directory nil "Encode"))
      (when (string-match "Encode_1080P_\\([0-9]+\\)" file)
	(setf (gethash (match-string 1 file) ids) t)))
    (dolist (id (hash-table-keys ids))
      (movie-concatenate-id id))))

(defun movie-concatenate-id-1 (id)
  (cl-loop for file in (cons (expand-file-name
			      (format "Encode_1080P_%s.mp4" id)
			      movie-prime-directory)
			     (sort (directory-files
				    movie-prime-directory
				    t
				    (format "Encode_1080P_%s_.*.mp4" id))
				   'string-version-lessp))
	   for size = (file-attribute-size (file-attributes file))
	   while (or t ;; With the extension cord the recorder switches
		     ;; itself off so we don't get all these small files.
		     (not (= (/ size 1024 1024) 120)))
	   collect file))

(defun movie-concatenate-id (id)
  (let ((files (movie-concatenate-id-1 id))
	(default-directory movie-prime-directory)
	(output (format "/dvd/prime/%s.mp4" id)))
    (when (file-exists-p output)
      (delete-file output))
    (message "concatting %s" files)
    (apply 'call-process "vconcat-video" nil (get-buffer-create "*concat*") nil
	   output
	   files)))

(defvar movie-rotation nil)

(defun movie-rotate-screen ()
  "Change screen rotation."
  (interactive)
  (call-process "xrandr" nil nil nil "--output" "eDP-1" "--rotate"
		(if movie-rotation
		    "normal"
		  "inverted"))
  (setq movie-rotation (not movie-rotation)))

(defun movie-split-collection (files)
  "Split the dir under point into separate directories."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (dolist (dir files)
    (let ((films (mapcar 'string-trim
			 (split-string
			  (replace-regexp-in-string
			   "^[^:]+?: " "" (file-name-nondirectory dir))
			  ",")))
	  (files (directory-files dir t ".mkv$")))
      (cl-loop for film = (pop films)
	       while film
	       for new-dir = (movie-find-split-name film)
	       do
	       (make-directory new-dir)
	       (cl-loop for file in (if (zerop (length films))
					files
				      (movie--files-until-over-gb files))
			do
			(progn
			  (setq files (delete file files))
			  (rename-file file (expand-file-name
					     (file-name-nondirectory file)
					     new-dir))
			  (let ((png (concat file ".png")))
			    (when (file-exists-p png)
			      (rename-file png
					   (expand-file-name
					    (file-name-nondirectory png)
					    new-dir)))))))
      (delete-directory dir))))

(defun movie--files-until-over-gb (files)
  (cl-loop for file in files
	   collect file
	   while (< (file-attribute-size (file-attributes file))
		    (* 3 1000 1000 1000))))

(defun movie-find-split-name (film)
  (cl-loop for dir = (expand-file-name film "/dvd")
	   while (file-exists-p dir)
	   do (setq film (concat film " "))
	   finally (cl-return dir)))

(defun movie-goto-last-series ()
  "Go to the /dvd last series directory."
  (interactive)
  (let ((mkvs (cl-loop for path in
		       (movie-directory-files-recursively "/dvd" "[.]mkv\\'")
		       collect (list
				(file-name-nondirectory path)
				path)))
	found)
    (with-temp-buffer
      (insert-file-contents movie-positions-file)
      (goto-char (point-max))
      (while (and (not (bobp))
		  (not found))
	(forward-line -1)
	(let* ((current (save-restriction
			  (narrow-to-region (point) (line-end-position))
			  (search-forward " " nil t 2)
			  (buffer-substring (point) (point-max))))
	       (dir (cadr (assoc current mkvs))))
	  (when (and dir
		     (movie-tv-series-p (file-name-directory dir)))
	    (setq found dir)))))
    (if (not found)
	(error "Can't find a tv series")
      (movie-find-file (file-name-directory found)))))

(defun movie-tv-series-p (dir)
  (let ((tracks (cdr (assq 'tracks (movie-get-stats dir)))))
    (and (> (length tracks) 5)
	 (> (cl-loop for track in tracks
		     when (> (cl-getf (cdr track) :length) (* 20 60))
		     sum 1)
	    5))))

(defun movie-directory-files-recursively (dir regexp &optional include-directories)
  "Return list of all files under DIR that have file names matching REGEXP.
This function works recursively.  Files are returned in \"depth first\"
order, and files from each directory are sorted in alphabetical order.
Each file name appears in the returned list in its absolute form.
Optional argument INCLUDE-DIRECTORIES non-nil means also include in the
output directories whose names match REGEXP."
  (let ((result nil)
	(files nil)
	;; When DIR is "/", remote file names like "/method:" could
	;; also be offered.  We shall suppress them.
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (and (file-readable-p dir)
		       (sort (file-name-all-completions "" dir)
			     'string<)))
      (unless (member file '("./" "../"))
	(if (directory-name-p file)
	    (let* ((leaf (substring file 0 (1- (length file))))
		   (full-file (expand-file-name leaf dir)))
	      ;; Don't follow symlinks to other directories.
	      (unless (file-symlink-p full-file)
		(setq result
		      (nconc result (movie-directory-files-recursively
				     full-file regexp include-directories))))
	      (when (and include-directories
			 (string-match regexp leaf))
		(setq result (nconc result (list full-file)))))
	  (when (string-match regexp file)
	    (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defvar movie-marks nil)

(defun movie-symlink-marks-to-dir (dir)
  (dolist (file movie-marks)
    (let ((target (expand-file-name (file-name-nondirectory file) dir)))
      (unless (file-exists-p target)
	(make-symbolic-link file target)))))

(defun movie-mark (movie)
  "Toggle the mark on the current movie."
  (interactive (list (movie-current-file)))
  (let ((mark ?*))
    (if (member movie movie-marks)
	(setq movie-marks (delete movie movie-marks)
	      mark ? )
      (push movie movie-marks))
    (save-excursion
      (beginning-of-line)
      (forward-char 1)
      (delete-char 1)
      (insert mark))
    (forward-line 1)))

(defun movie-copy-marked (dir)
  "Copy the marked movies to DIR."
  (interactive "DCopy to directory: ")
  (dolist (movie movie-marks)
    (message "Copying %s..." movie)
    (call-process "rsync" nil nil nil "-av" movie dir))
  (message "Done"))

(defun movie-reload ()
  "Reload movie.el."
  (interactive)
  (load "~/src/movie.el/movie.el")
  (load "~/.emacs")
  (message "Reloaded")
  ;;(movie-reload-play)
  ;;(raise-frame)
  )

(defun movie-reload-play ()
  (call-process "/usr/src/mpv/build/mpv" nil (get-buffer-create "*foo*") nil
		"--audio-device=alsa/plughw:CARD=J75,DEV=0"
		"--vo=gpu"
		"--hwdec=vdpau"
		"--demuxer-max-back-bytes=2147483647"
		;;"--vf=vdpaupp=denoise=1"
		;;"--tone-mapping=clip" "--tone-mapping-param=1"
		"--input-ipc-server=/tmp/mpv-socket"
		"--fullscreen"
		"--stop-screensaver"
		;;"/dev/video0"
		"/tv/tmp/output.mkv"
		))

(defun movie-clear-screenshots ()
  "Delete the screenshots from the screenshots directory."
  (interactive)
  (let ((dir (format
	      "/var/tmp/screenshots/%s/"
	      (file-name-nondirectory
	       (directory-file-name default-directory)))))
    (dolist (file (directory-files dir t "mpv-shot"))
      (rename-file file
		   (replace-regexp-in-string
		    "mpv-shot"
		    (format "old-shot-%s" (format-time-string "%FT%T"))
		    file)))))

(defun movie-add-genre (dir genre)
  (interactive (list (movie-current-file)
		     (read-string "Genre: ")))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "stats" dir))
    (if (re-search-forward "^Genre: ")
	(progn
	  (end-of-line)
	  (insert "," genre))
      (re-search-forward "^$")
      (insert "Genre: " genre "\n"))
    (write-region (point-min) (point-max) (expand-file-name "stats" dir))))

(defun movie-remove-genre (dir)
  (interactive (list (movie-current-file)))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "stats" dir))
    (when (re-search-forward "^Genre: ")
      (when (search-forward ",eclipse")
	(replace-match "" t t))
      (write-region (point-min) (point-max) (expand-file-name "stats" dir)))))

(defun movie-find-display ()
  "Return the highest display."
  (with-temp-buffer
    (call-process "xrandr" nil t)
    (goto-char (point-min))
    (let ((disp 0))
      (while (re-search-forward " connected" nil t)
	(cl-incf disp))
      disp)))

(defun movie-play-current ()
  "Start playing the last recorded play from the .movie.positions file."
  (interactive)
  (let ((file (with-temp-buffer
		(let ((coding-system-for-read 'utf-8))
		  (insert-file-contents "/tv/data/current-file")
		  (buffer-string)))))
    (movie-find-file file)))

(defun movie-change-rate-current (file)
  "Change the frame rate to the file under point."
  (interactive (list (movie-current-file)))
  (movie-change-rate (movie--find-closest-fps (movie--fps file))))

(defun movie-change-rate (rate)
  "Change frame rates."
  (interactive
   (list
    (cadr
     (read-multiple-choice
      "Set frame rate: "
      '((?5 "50")
	(?6 "59.94")
	(?4 "23.98")
	(?2 "25.00")
	(?9 "29.97"))))))
  (call-process "xrandr" nil nil nil
		"--output" "HDMI-0"
		"--mode" "3840x2160"
		"--rate" rate)
  (with-temp-buffer
    (call-process "xrandr" nil t)
    (write-region (point-min) (point-max) "/tmp/nxr"))
  (message "Set frame rate to %s" rate))

(defun movie--fps (file)
  (with-temp-buffer
    (with-environment-variables (("LC_ALL" (getenv "LANG")))
      (call-process "mediainfo" nil t nil file))
    (let ((scale 1))
      (goto-char (point-min))
      ;; Interlaced films have twice the frame rate.
      (when (save-excursion
	      (re-search-forward "Scan type.*: Interlaced" nil t))
	(setq scale 2))
      (when (re-search-forward "Frame rate.*: \\([0-9.]+\\)" nil t)
	(* (string-to-number (match-string 1)) scale)))))

(defvar movie-valid-fps
  '("59.94" "50.00" "29.97" "25.00" "23.98"))

(defun movie--find-closest-fps (fps)
  (cdar
   (cl-stable-sort
    (cl-loop for cand in movie-valid-fps
	     append (cl-loop for mult from 1 upto 1
			     collect (cons (abs (- (* mult fps)
						   (string-to-number cand)))
					   cand)))
    (lambda (f1 f2)
      (< (car f1) (car f2))))))

(defvar movie--rotation nil)

(defun movie-change-orientation ()
  (interactive)
  (setq movie--rotation (not movie--rotation))
  (call-process "xrandr" nil nil nil
		"--output" "eDP-1"
		"--rotate"
		(if (not movie--rotation)
		    "normal"
		  "inverted"))
  (call-process "gsettings" nil nil nil
		"set" "org.gnome.settings-daemon.plugins.xsettings"
		"overrides" "{'Gdk/WindowScalingFactor': <2>}"))

(defun movie--get-poster (id sleeve)
  (when-let ((image (imdb-get-image-and-country id nil t)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert image)
      (write-region (point-min) (point-max) sleeve))))

(defun movie-get-missing-posters ()
  (dolist (dir (directory-files "/dvd/" t))
    (movie--get-missing-posters dir)))

(defun movie--get-missing-posters (dir)
  (let ((sleeve (expand-file-name "sleeve.jpg" dir)))
    (unless (file-exists-p sleeve)
      (when-let ((id (cdr (assoc "IMDB" (movie-get-stats dir)))))
	(movie--get-poster id sleeve)))))

(defun movie-get-missing-imdb-id-and-poster ()
  (dolist (dir (directory-files "/dvd/" t))
    (let ((stats (movie-get-stats dir)))
      (when (and (assoc "Director" stats)
		 (assoc "Title" stats)
		 (not (assoc "IMDB" stats)))
	(pop-to-buffer "*Query*")
	(erase-buffer)
	(insert-file-contents (expand-file-name "stats" dir))
	(goto-char (point-min))
	(sit-for 0.1)
	(let ((imdb (imdb-query (concat (cdr (assoc "Title" stats))
					" "
					(cdr (assoc "Year" stats)))
				5)))
	  (with-temp-buffer
	    (insert-file-contents (expand-file-name "stats" dir))
	    (forward-line 1)
	    (insert "IMDB: " (plist-get imdb :id) "\n")
	    (unless (re-search-forward "^Country:" nil t)
	      (insert "Country: " (plist-get imdb :country) "\n"))
	    (write-region (point-min) (point-max)
			  (expand-file-name "stats" dir)))
	  (movie--get-missing-posters dir))))))

(defun movie-jump-to-directory (dir)
  (interactive
   (list
    (cadr
     (read-multiple-choice
      "Jump to dir: "
      '((?b "/tv/Buffy/")
	(?d "/dvd/")
	(?t "/tv/torrent/")
	(?s "/tv/Series/")
	(?i "/tv/future/films/")
	(?r "/tv/future/rainy-day/")
	(?f "/tv/future/series/")
	(?j "/tv/Fishing With John/"))))))
  (catch 'done
    (progn
      (cl-loop for buffer in (buffer-list)
	       when (with-current-buffer buffer
		      (and (equal default-directory dir)
			   (eq major-mode 'movie-mode)))
	       do
	       (switch-to-buffer buffer)
	       (throw 'done nil))
      (movie-browse dir))))

(defun movie-set-touch-point (event)
  (interactive "e")
  (goto-char (nth 2 (plist-get  event 'touchscreen-end))))

(defun movie-swap-file-names (files)
  "Change the names of two /dvd directories."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)))
  (when (or (not (length= files 2))
	    (not (seq-every-p #'file-directory-p files)))
    (error "Only mark two directories"))
  (let ((first (split-string
		(with-temp-buffer
		  (insert-file-contents
		   (expand-file-name "stats" (car files)))
		  (buffer-string))
		"\n\n"))
	(second (split-string
		 (with-temp-buffer
		   (insert-file-contents
		    (expand-file-name "stats" (cadr files)))
		   (buffer-string))
		 "\n\n")))
    (rename-file (car files) (concat (car files) ".tmp"))
    (rename-file (cadr files) (car files))
    (rename-file (concat (car files) ".tmp") (cadr files))

    (with-temp-buffer
      (insert (car first) "\n\n" (cadr second))
      (write-region (point-min) (point-max)
		    (expand-file-name "stats" (car files))))
		    
    (with-temp-buffer
      (insert (car second) "\n\n" (cadr first))
      (write-region (point-min) (point-max)
		    (expand-file-name "stats" (cadr files))))

    (when (and (file-exists-p (expand-file-name "sleeve.jpg" (car files)))
	       (file-exists-p (expand-file-name "sleeve.jpg" (cadr files))))
      (rename-file (expand-file-name "sleeve.jpg" (cadr files))
		   (expand-file-name "sleeve.jpg.tmp" (car files)))
      (rename-file (expand-file-name "sleeve.jpg" (car files))
		   (expand-file-name "sleeve.jpg" (cadr files)))
      (rename-file (expand-file-name "sleeve.jpg.tmp" (car files))
		   (expand-file-name "sleeve.jpg" (cadr files))))))

(defun movie-link-to-smallunseen (directory)
  (interactive (list (movie-current-file)))
  (let ((target (expand-file-name (file-name-nondirectory directory)
				  "/tv/links/smallunseen")))
    (when (file-exists-p target)
      (user-error "Already exists in smallunseen"))
    (make-symbolic-link directory target)
    (message "Made link")))

(defun movie-convert-video-ts-to-mkv (dir name)
  "Convert the movie in DIR to an mkv in /dvd/NAME."
  (interactive "DVideo TS dir:\nsName: ")
  (let ((to-dir (expand-file-name name "/var/tmp/dvd/")))
    (when (and (file-exists-p to-dir)
	       (length> (directory-files to-dir) 2))
      (error "%s already exists" to-dir))
    (unless (file-exists-p to-dir)
      (make-directory to-dir t))
    (let ((vobs (directory-files (expand-file-name "VIDEO_TS" dir)
				 t "[.]VOB\\'"))
	  (mkv 0))
      (while vobs
	(let ((vob
	       (if (not (= (file-attribute-size (file-attributes (car vobs)))
			   1073739776))
		   (pop vobs)
		 (apply #'call-process
			"cat" nil '(:file "/tmp/concat.vob") nil
			(cl-loop collect (car vobs)
				 while (and vobs
					    (= (file-attribute-size
						(file-attributes (pop vobs)))
					       1073739776))))
		 "/tmp/concat.vob")))
	  (call-process
	   "ffmpeg" nil (get-buffer-create "*errors*") nil
	   "-fflags" "+genpts" "-i" vob "-c" "copy"
	   (expand-file-name (format "title%02d.mkv" (cl-incf mkv))
			     to-dir)))))))

(defvar movie--actor-timer nil)

(defun movie--mpv-osd (string)
  (movie-send-mpv-command
   `((command . ["show-text"
		 ,(format "%S"
			  (string-replace "\"" "'"
					  (string-replace "\n" " " string)))
		 5000]))))

(defun movie-query-and-display ()
  "Query an LLM about the actor currently on the screen."
  (interactive)
  (when movie--actor-timer
    (cancel-timer movie--actor-timer))
  (let* ((movie (movie--file-title movie--file-currently-playing))
	 (is-movie (file-exists-p
		    (expand-file-name "stats" (file-name-directory movie))))
	 (old-files (directory-files movie-recording-directory t))
	 func)
    (setq func
	  (lambda ()
	    (let ((new (seq-difference
			(directory-files movie-recording-directory t)
			old-files)))
	      (if new
		  (let ((names (movie-query-actor movie (car new) is-movie)))
		    (message "Got names %s"  names)
		    (movie--mpv-osd names))
		(setq movie--actor-timer (run-at-time 0.1 nil func))))))
    (movie-send-mpv-command
     `((command . ["screenshot" "video"])))
    (setq movie--actor-timer (run-at-time 0.1 nil func))
    nil))

(defun movie-query-actor (movie image &optional is-movie)
  (message "Querying %s" movie)
  (require 'query-assistant)
  (let ((text
	 (concat "What is the name of the character in this "
		 (if is-movie "movie" "tv series")
		 " screenshot?  The "
		 (if is-movie "movie" "tv series")
		 " is " movie ". "
		 "And what's the name of the actor/actreess "
		 "that portrayed this character in this "
		 (if is-movie "movie" "tv series")
		 "?  If you don't recognise the person, say so."))
	(data
	 (with-temp-buffer
	   (set-buffer-multibyte nil)
	   (call-process
	    "convert" nil t nil (expand-file-name image)
	    "-resize" "800x" "jpg:-")
	   (base64-encode-region (point-min) (point-max) t)
	   (buffer-string))))
    ;; Gemini seems to give better results than OpenAI.
    (if t
	(query-assistant
	 'gemini
	 (vector
	  (query-assistant--hash
	   (list "text" text))
	  (query-assistant--hash
	   (list "inline_data"
		 (query-assistant--hash
		  (list "mime_type" "image/jpeg")
		  (list "data" data))))))
      (query-assistant
       'openai
       (vector
	(query-assistant--hash
	 (list "role" "user")
	 (list "content"
	       (vector
		(query-assistant--hash
		 (list "type" "input_text")
		 (list "text" text))
		(query-assistant--hash
		 (list "type" "input_image")
		 (list "image_url"
		       (concat "data:image/jpg;base64," data)))))))))))

(provide 'movie)

;;; movie.el ends here
