;;; movie.el --- playing movies
;; Copyright (C) 2004-2018 Lars Magne Ingebrigtsen

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

;;; Code:

(require 'pvr)
(require 'time-date)
(require 'imdb)
(require 'mkv)
(require 'subr-x)
(require 'touchgrid)

(defvar movie-order nil)
(defvar movie-limit nil)

(defvar movie-files "." ;"\\.\\(mpg\\|avi\\|wmv\\|ogm\\|rm\\|mpeg\\)$"
  "Regexp to match movie files.")

(defvar movie-player
  '("/usr/src/mpv/build/mpv"
    "--audio-device=alsa/plughw:CARD=J75,DEV=0"
    "--vo=gpu"
    "--hwdec=vdpau"
    ;;"--vf=vdpaupp=denoise=1"
    ;;"--tone-mapping=clip" "--tone-mapping-param=1"
    "--input-ipc-server=/tmp/mpv-socket"
    "--fullscreen"
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

(defvar movie-file-id nil)

(defvar movie-deletion-process nil)

(defvar movie-after-play-callback nil
  "Function called after MPV playback has ended.")

(defun movie-holiday ()
  "Start viewing movies when not at home."
  (interactive)
  (setq movie-positions-file "~/.emacs.d/mplayer.positions")
  (movie-browse "/dvd"))

(defun movie-browse (directory &optional order match)
  "Browse DIRECTORY."
  (interactive "DDirectory: ")
  ;; If called interactively in the /dvd directory, just display films
  ;; that are unseen.
  (when (and (called-interactively-p 'interactive)
	     (null match)
	     (equal (file-name-nondirectory
		     (directory-file-name directory)) "dvd"))
    (setq match 
	  (lambda (stats)
	    (null (assoc "Seen" stats)))))
  (setq directory (file-truename directory))
  (let ((files (movie-get-files directory match)))
    (when (null order)
      (setq order 'alphabetical))
    (switch-to-buffer (directory-file-name directory))
    (erase-buffer)
    (movie-mode)
    (setq movie-order order
	  movie-limit match)
    (movie-generate-buffer files order)
    (unless (string-match "/$" directory)
      (setq directory (concat directory "/")))
    (setq default-directory directory)
    (setq-local mode-line-misc-info (movie-buffer-identification directory))
    (unless movie-deletion-process
      (setq movie-deletion-process (run-at-time 1 10 'movie-delete-scheduled)))
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
	(loop for track in (reverse (cdr (assoc 'tracks stats)))
	      when (plist-get (cdr track) :seen)
	      return (car track))
	(caar (cdr (assoc 'tracks stats)))))
      t)))

(defun movie-goto-movie (movie)
  (goto-char (point-min))
  (let (file)
    (while (and (not (eobp))
		(or (not (setq file (getf (get-text-property
					   (point) 'movie-data)
					  :file)))
		    (not (equal movie (file-name-nondirectory file)))))
      (forward-line 1))))

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
					     "movies")))
    current-prefix-arg))
  (movie-browse
   default-directory movie-order
   (cond
    ((equal match "nostats")
     (lambda (stats)
       (null stats)))
    ((equal match "all")
     nil)
    ((equal match "unseen")
     (lambda (stats)
       (null (assoc "Seen" stats))))
    ((equal match "movies")
     (lambda (stats)
       (let ((genres (cdr (assoc "Genre" stats))))
	 (and genres
	      (null (assoc "Seen" stats))
	      (assoc "Year" stats)
	      (not (member "tv" (split-string genres ",")))
	      (not (member "best" (split-string genres ",")))))))
    ((equal match "mostly-seen")
     (lambda (stats)
       (equal (cdr (assoc "Status" stats)) "mostly-seen")))
    (t
     `(lambda (stats)
	(let ((genres (cdr (assoc "Genre" stats))))
	  (and genres
	       (or (not ,only-unseen)
		   (null (assoc "Seen" stats)))
	       (member ,match (split-string genres ",")))))))))

(defun movie-list-by-year ()
  "List the movies by year."
  (interactive)
  (movie-browse default-directory 'year movie-limit))

(defun movie-list-by-rip-time ()
  "List the movies by year."
  (interactive)
  (movie-browse default-directory 'rip-time movie-limit))

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
	     (not (string-match "\\.png\\'\\|\\.JPG\\'\\|\\.srt\\'\\|/stats\\|/seen-date\\|txt$\\|~$" file))
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
		      :year ,(let ((year (cdr (assoc "Year"
						     (movie-get-stats file)))))
			       (if year
				   (string-to-number year)
				 9999))
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
      (when (assoc "Genre" stats)
	(nconc data (list :genre (cdr (assoc "Genre" stats)))))
      (dolist (track (cdr (assoc 'tracks stats)))
	(when (or (not max)
		  (> (plist-get (cdr track) :length)
		     (plist-get (cdr max) :length)))
	  (setq max track)))
      (setq data (nconc
		  (list :image (expand-file-name (concat (car max) ".png") dir))
		  data))
      data)))

(defun movie-biggest-file-data (dir)
  (let ((files nil))
    (dolist (file (directory-files (plist-get dir :file) t))
      (push (cons (nth 7 (file-attributes file)) file) files))
    (car (sort files (lambda (f1 f2)
		       (> (car f1) (car f2)))))))

(defun movie--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(defun movie-generate-buffer (files &optional order)
  (when (not order)
    (setq order 'chronological))
  (setq files (movie-sort files order))
  (dolist (file files)
    (let ((subtitles (length (plist-get file :subtitles)))
	  (dir-data (and (plist-get file :directoryp)
			 (movie-biggest-file-data file)))
	  (system-name (system-name))
	  (dvdp (string-match "^/dvd/" (plist-get file :file))))
      (when (eq order 'year)
	(insert (format "%04d " (or (plist-get file :year) 9999))))
      (when (eq order 'rip-time)
	(insert (format-time-string " %Y-%m-%d" (movie-rip-time file))))
      (when (eq order 'country)
	(insert (format "%04d %02s " (or (plist-get file :year) 9999)
			(or (plist-get file :country) ""))))
      (when (eq order 'director)
	(insert (format "%4s %-20s "
			(plist-get file :year)
			(let ((string (or (plist-get file :director) "")))
			  (if (> (length string) 20)
			      (substring string 0 20)
			    string)))))
      (insert
       (format
	" %5s%s %s%s\n"
	(if (plist-get file :directoryp)
	    (if dvdp
		""
	      (format "%s"
		      (round (/ (or (car dir-data) -1) 1024 1024))))
	  (format
	   " %s"
	   (if (plist-get file :length)
	       (movie-format-length (plist-get file :length))
	     (round
	      (/ (or (plist-get file :size) -1) 1024 1024)))))
	(if (member system-name '("sandy" "quimbies"))
	    (propertize " " 'display `(space :align-to (600)))
	  "")
	(if (and (not (plist-get file :seen))
		 (not (plist-get file :mostly-seen)))
	    (file-name-nondirectory (plist-get file :file))
	  (propertize
	   (file-name-nondirectory (plist-get file :file))
	   'face `(:foreground
		   ,(let ((seen (car (last (plist-get file :seen) 2)))
			  (length (plist-get file :length)))
		      (if (or (plist-get file :directoryp)
			      (> (/ seen length) 0.9))
			  (if (plist-get file :directoryp)
			      "#5050f0"
			    "#000080")
			"#800000")))))
	(if (plist-get file :directoryp)
	    (if dvdp
		""
	      (format "/ (%s)"
		      (round (/ (or (car dir-data) -1) 1024 1024))))
	  (format
	   " (%s)%s%s"
	   (if (plist-get file :length)
	       (movie-format-length (plist-get file :length))
	     (round
	      (/ (or (plist-get file :size) -1) 1024 1024)))
	   (if (> (length (plist-get file :audio-tracks)) 1)
	       (format " %s" (mapconcat
			      'identity
			      (plist-get file :audio-tracks) ","))
	     "")
	   (if (> subtitles 0)
	       (format " %s sub%s" subtitles
		       (if (= subtitles 1) "" "s"))
	     "")))))
      (save-excursion
	(forward-line -1)
	(when (> (or (plist-get file :length) 0)
		 (* 30 60))
	  (add-face-text-property (line-beginning-position)
				  (1+ (line-end-position))
				  '(:background "#006000")
				  t))
	(let ((png (or (plist-get file :image)
		       (concat (plist-get file :file) ".png"))))
	  (cond
	   ((file-exists-p png)
	    (insert-image (create-image png (movie--image-type) nil
					:scale movie-image-scale)))
	   ((and (plist-get file :directoryp)
		 (file-exists-p (format "%s.png" (cdr dir-data))))
	    (insert-image (create-image (format "%s.png" (cdr dir-data))
					(movie--image-type) nil
					:scale movie-image-scale)))
	   (t
	    (insert-image (create-image "~/src/movie.el/empty.png"
					(movie--image-type) nil
					:scale movie-image-scale)))))
	(beginning-of-line)
	(put-text-property (point) (1+ (point)) 'movie-data file)))))
			   

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
  (replace-regexp-in-string
   "['\"]" ""
   (replace-regexp-in-string "/\\(the\\|a\\)[ .]" "/"
			     (downcase name))))

(defun movie-sort (files order)
  (let ((predicate
	 (cond
	  ((eq order 'alphabetical)
	   (lambda (f1 f2)
	     (string< (movie-sortable-name (plist-get f1 :file))
		      (movie-sortable-name (plist-get f2 :file)))))
	  ((eq order 'chronological)
	   (lambda (f1 f2)
	     (if (plist-get f1 :recorded)
		 (string< (plist-get f1 :recorded)
			  (plist-get f2 :recorded))
	       (time-less-p (plist-get f1 :time)
			    (plist-get f2 :time)))))
	  ((eq order 'year)
	   (lambda (f1 f2)
	     (< (or (plist-get f1 :year) 0)
		(or (plist-get f2 :year) 0))))
	  ((eq order 'rip-time)
	   (lambda (f1 f2)
	     (< (movie-rip-time f1)
		(movie-rip-time f2))))
	  ((eq order 'director)
	   (lambda (f1 f2)
	     (string< (or (plist-get f1 :director) 0)
		      (or (plist-get f2 :director) 0))))
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

(defun movie-rip-time (elem)
  (let ((files (directory-files (getf elem :file) t "[.]mkv$")))
    (if (not files)
	0
      (float-time (file-attribute-modification-time
		   (file-attributes
		    (car (cl-sort files #'<
				  :key (lambda (f)
					 (float-time
					  (file-attribute-modification-time
					   (file-attributes f))))))))))))

(defvar movie-mode-map 
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'movie-find-file)
    (define-key map " " 'movie-play-best-file)
    (define-key map [delete] 'movie-delete-file)
    (define-key map [del] 'movie-delete-file)
    (define-key map [backspace] 'movie-delete-file)
    (define-key map [deletechar] 'movie-delete-file)
    (define-key map "d" 'movie-play-dvd)
    (define-key map "D" 'movie-play-whole-dvd)
    (define-key map "W" 'movie-play-total-dvd)
    (define-key map "V" 'movie-play-vlc-dvd)
    (define-key map "f" 'movie-play-next-vob)
    (define-key map "F" 'movie-play-current-vob)
    (define-key map "T" 'movie-thumbnails)
    (define-key map "q" 'bury-buffer)
    (define-key map "k" 'movie-browse)
    (define-key map "c" 'movie-play-cropped)
    (define-key map "x" 'movie-prefixed-action)
    (define-key map "h" 'movie-play-high-volume)
    (define-key map "g" 'movie-rescan)
    (define-key map "t" 'movie-find-torrent)
    (define-key map "s" 'movie-toggle-sort)
    (define-key map "r" 'movie-rename)
    (define-key map "R" 'movie-reload)
    (define-key map "C" 'movie-clear-screenshots)
    (define-key map "l" 'movie-last-seen)
    (define-key map "-" 'movie-collapse)
    (define-key map "i" 'movie-mark-as-seen)
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
    (define-key map "Y" 'movie-list-by-year)
    (define-key map "L" 'movie-list-by-director)
    (define-key map "e" 'movie-goto-last-series)
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

(defun movie-play-best-file (file)
  "Play the longest file in the directory/file under point."
  (interactive (list (movie-current-file)))
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
	(files nil)
	(directories nil))
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
  (movie-rescan-1 'chronological)
  (goto-char (point-max)))

(defun movie-play-cropped (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player movie-crop (list file))))

(defun movie-prefixed-action ()
  (interactive)
  (let ((options nil)
	(command nil)
	(player (copy-list movie-player)))
    (while (let ((char (read-char "")))
	     (cond
	      ((eq char ?n)
	       (setq options
		     (movie-add-vf options "dctdnoiz=15")))
	      ((eq char ?c)
	       (setq options
		     (movie-add-vf options "crop=700:420")))
	      ((eq char ?w)
	       (setq options
		     (movie-add-vf options "crop=700:300")))
	      ((eq char ?i)
	       (setq options (append options (list movie-deinterlace-switch))))
	      ((eq char ?I)
	       (setq options (delete movie-deinterlace-switch options)))
	      (t
	       (setq command
		     (lookup-key movie-mode-map (format "%c" char)))
	       nil))))
    (when (eq command 'movie-find-file)
      (setq command 'movie-play-simple))
    (let ((movie-player (append player options)))
      (call-interactively command))))

(defun movie-add-vf (options vf)
  (setq options (copy-list options))
  (let ((old (member "--vf" options)))
    (if (not old)
	(append options (list "--vf" vf))
      (setcar (cdr old)
	      (concat vf "," (cadr old)))
      options)))

(defun movie-remove-vf (options vf)
  (setq options (copy-list options))
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

(defun movie-play (file)
  (interactive (list (movie-current-file)))
  (let ((subs (list
	       (concat (replace-regexp-in-string "[.][^.]+\\'" "" file) ".srt")
	       (concat (replace-regexp-in-string
			"[.][^.]+\\'" "" file) "_eng.srt")))
	(movie-player (copy-sequence movie-player)))
    (dolist (sub subs)
      (when (file-exists-p sub)
	(setq movie-player (append movie-player
				   (list "--sub-file" sub)))))
    (if (movie-interlaced-p file)
	(movie-play-1 (append movie-player (list movie-deinterlace-switch)
			      (list file)))
      (movie-play-1 (append movie-player (list file))))))

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
  (with-current-buffer (get-buffer-create "*mplayer*")
    (when (file-exists-p "/tmp/mpv-socket")
      (delete-file "/tmp/mpv-socket"))
    (let ((mpv (apply 'start-process "mpv" (current-buffer) command))
	  (count 0)
	  (request-id 0)
	  socket)
      (while (not (file-exists-p "/tmp/mpv-socket"))
	(sleep-for 0.1))
      (setq socket
	    (make-network-process
	     :name "talk-mpv"
	     :service "/tmp/mpv-socket"
	     :buffer (get-buffer-create "*mpv*")
	     :family 'local))
      (movie-send-mpv-command '((command . ["observe_property" 1 "time-pos"])))
      (when wait
	(while (process-live-p mpv)
	  ;; Once a second, get the bitrate.
	  (when (zerop (mod (incf count) 10))
	    (movie-send-mpv-command
	     '((command . ["get_property" "video-bitrate"])
	       (request_id . ,(incf request-id)))))
	  (sleep-for 0.1))
	(when movie-after-play-callback
	  (with-current-buffer (get-buffer-create "*mpv*")
	    (funcall movie-after-play-callback)))))))

(defun movie-send-mpv-command (command)
  (with-current-buffer "*mpv*"
    (when (get-buffer-process (current-buffer))
      (process-send-string
       (get-buffer-process (current-buffer))
       (format "%s\n" (json-encode command))))))

(defun movie-anim-state nil)

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

(defun movie-find-anim-name ()
  (let ((num 1)
	result)
    (while (directory-files movie-recording-directory
			    nil (setq result (format "anim%02d" num)))
      (incf num))
    result))

(defconst movie-audio-devices
  '("alsa/plughw:CARD=J75,DEV=0"	; Headphones
    "alsa/plughw:CARD=NVidia,DEV=7"	; TV
    "alsa/plughw:CARD=PCH,DEV=0")) ; Speakers

(defvar movie-current-audio-device 0)

(defun movie-rotate-audio ()
  "Change the audio output."
  (interactive)
  (movie-send-mpv-command
   `((command . ["set_property" "audio-device"
		 ,(elt movie-audio-devices
		       (mod (incf movie-current-audio-device)
			    (length movie-audio-devices)))]))))
  
(defun movie-play-1 (player)
  (setq movie-current-audio-device 0
	movie-anim-state nil)
  (let ((skip (movie-find-position
	       (or movie-file-id
		   (car (last player))))))
    (when skip
      (setq player (cons (pop player)
			 (append (list (format "--start=%s" skip))
				 player)))))
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
  (movie-update-stats-position (car (last player))))

(defun movie-update-mplayer-position (file)
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
  (when (and (file-directory-p file)
	     (not (string-match "/torrent" file))
	     (not (= (length (directory-files-recursively file ".") 2))))
    (error "Directory not empty"))
  (beginning-of-line)
  (let ((new-name (expand-file-name
		   (concat ".deleted-" (file-name-nondirectory file))
		   (file-name-directory file))))
    (push (list :name file
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
      (error "File %s has been completely deleted"))
    (beginning-of-line)
    (insert (plist-get elem :display))
    (forward-line -1)
    (rename-file (plist-get elem :deletion-name)
		 (plist-get elem :name))))

(defun movie-delete-scheduled ()
  (dolist (elem movie-scheduled-deletions)
    (when (> (- (float-time) 60)
	     (plist-get elem :time))
      ;; More than a minute has passed, so delete.
      (let ((file (plist-get elem :deletion-name)))
	(when (file-exists-p file)
	  (if (file-directory-p file)
	      (delete-directory file t)
	    (delete-file file)
	    (let ((png (concat (plist-get elem :name) ".png")))
	      (when (file-exists-p png)
		(delete-file png))))))
      (setq movie-scheduled-deletions
	    (delete elem movie-scheduled-deletions)))))

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
  (save-excursion
    (beginning-of-line)
    (getf (get-text-property (point) 'movie-data) :file)))

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
    (unless (movie-browse default-directory order movie-limit)
      (forward-line (1- lines)))))

(defun movie-toggle-sort ()
  "Toggle sorting by time."
  (interactive)
  (let ((current (movie-current-file)))
    (if (eq movie-order 'alphabetical)
	(setq movie-order 'chronological)
      (setq movie-order 'alphabetical))
    (goto-char (point-min))
    (sort-subr nil 'forward-line 'end-of-line nil nil
	       (lambda (l1 l2)
		 (movie-compare-lines
		  movie-order
		  (get-text-property (car l1) 'movie-data)
		  (get-text-property (car l2) 'movie-data))))
    (if (not current)
	(goto-char (point-max))
      (goto-char (point-min))
      (search-forward (file-name-nondirectory current) nil t)
      (beginning-of-line))))

(defun movie-compare-lines (order d1 d2)
  (if (eq order 'alphabetical)
      (string< (downcase (getf d1 :file)) (downcase (getf d2 :file)))
    (time-less-p (getf d1 :time) (getf d2 :time))))

(defun movie-rename (to)
  "Rename the current movie."
  (interactive "FNew name: ")
  (rename-file (movie-current-file) to))

(defun movie-collapse ()
  "Move all files matching a prefix to the same directory."
  (interactive)
  (let* ((prefix
	  (read-string "Prefix to collapse: "
		       (movie-prefix (file-name-nondirectory
				      (movie-current-file)))))
	 (dir (expand-file-name prefix default-directory)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (dolist (file (directory-files default-directory t))
      (when (and (not (equal (file-name-nondirectory file) "."))
		 (not (equal (file-name-nondirectory file) ".."))
		 (string-equal
		  (downcase (or (movie-prefix (file-name-nondirectory file))
				""))
		  (downcase prefix)))
	(rename-file file (concat dir "/"))))))

(defun movie-prefix (file)
  (let ((prefix ""))
    (dolist (part (split-string file "[.]"))
      (if (string-match "[0-9]" part)
	  (return prefix)
	(setq prefix
	      (concat
	       prefix
	       (if (zerop (length prefix))
		   part
		 (concat "." part))))))))

(defun movie-play-dvd (number)
  "Play 'large' VOB NUMBER from the DVD."
  (interactive "p")
  (let ((data (movie-dvd-data)))
    (if (> number (length (cadr data)))
	(message "Just %d big files" (length (cadr data)))
      (movie-play-dvd-vob data number))))

(defun movie-play-next-vob ()
  "Play the next 'large' VOB on the DVD."
  (interactive)
  (let* ((data (movie-dvd-data))
	 (number (movie-find-previous-vob data)))
    (if (not number)
	(message "No previous VOBs for %s" (car data))
      (movie-play-dvd (1+ number)))))

(defun movie-play-current-vob ()
  "Play current 'large' VOB on the DVD."
  (interactive)
  (let* ((data (movie-dvd-data))
	 (number (movie-find-previous-vob data)))
    (if (not number)
	(message "No previous VOBs for %s" (car data))
      (movie-play-dvd number))))

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

(defun movie-list-channels ()
  "List channels that can be viewed."
  (interactive)
  (let ((channels (pvr-read-channel-file)))
    (switch-to-buffer "*channels*")
    (erase-buffer)
    (dolist (spec channels)
      (insert (car spec) "\n"))
    (goto-char (point-min))
    (movie-channel-mode 1)))

(defvar movie-channel-process nil)

(defun movie-channel-kill ()
  (when (and movie-channel-process
	     (memq (process-status movie-channel-process)
		   '(open run)))
    (delete-process movie-channel-process)))

(defun movie-channel-play ()
  "Play the channel under point."
  (interactive)
  (movie-channel-kill)
  (let ((channel (buffer-substring (line-beginning-position)
				   (line-end-position)))
	device)
    (with-temp-buffer
      (movie-emacsclient (format "(pvr-choose-channel %S)" channel))
      (goto-char (point-min))
      (when (re-search-forward "/dev/video\\([0-9]+\\)" nil t)
	(setq device (string-to-number (match-string 1)))))
    (when device
      (setq movie-channel-process
	    (start-process "cat" nil "bash" "-c"
			   (format "nc potato %d > /tv/live" (+ 8040 device))))
      (while (not (file-exists-p "/tv/live"))
	(sleep-for 0.1))
      (movie-play-1 (append movie-player movie-crop
			    (list "-loop" "0" "/tv/live")))
      (movie-channel-kill))))

(defun movie-emacsclient (command)
  (call-process "emacsclient" nil t nil
		"--server-file=potato" 
		"--eval" command))

(defvar movie-channel-mode-map nil)
(unless movie-channel-mode-map
  (setq movie-channel-mode-map (make-sparse-keymap))
  (suppress-keymap movie-channel-mode-map)
  (define-key movie-channel-mode-map "\r" 'movie-channel-play)
  (define-key movie-channel-mode-map "q" 'bury-buffer))

(defvar movie-channel-mode nil
  "Mode for Movie Channel buffers.")

(defun movie-channel-mode (&optional arg)
  "Mode for Movie Channel mode buffers.

\\{movie-mode-map}"
  (interactive (list current-prefix-arg))
  (make-local-variable 'movie-channel-mode)
  (setq movie-channel-mode
	(if (null arg) (not movie-channel-mode)
	  (> (prefix-numeric-value arg) 0)))
  (setq major-mode 'movie-channel-mode)
  (setq mode-name "Channel")
  (use-local-map movie-channel-mode-map))

(defun movie-thumbnails ()
  "Create missing thumbnails."
  (interactive)
  (call-process "~/src/movie.el/thumbnail-movies"))

(defun movie-eject ()
  "Eject the cd."
  (interactive)
  (call-process "eject" nil nil nil "/dev/dvd"))

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
       (loop while (not (eobp))
	     for (format code resolution note) =
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
	 (subtitles (loop for track in (dom-by-tag dom 'A-track)
			  when (equal (dom-attr track :Track-type) "subtitles")
			  collect (dom-attr track :Language))))
    `(:length ,(movie-mkv-length
		(dom-attr (dom-by-tag dom 'Segment-information) :Duration))
	      :audio-tracks ,(loop for track in (dom-by-tag dom 'A-track)
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

(defun movie-interlaced-p (file)
  (let ((stats (movie-get-stats (file-name-directory file))))
    (if stats
	(getf (cdr (assoc (file-name-nondirectory file)
			  (cdr
			   (assq 'tracks stats))))
	      :interlaced)
      (and (not (member (system-name) '("mouse" "sandy")))
	   (with-temp-buffer
	     (call-process "mediainfo" nil t nil file)
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
			(plist-get imdb :director)
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
				  "19700101T010000")))))))))))

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
	(loop for elem in data
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
	    (unless (search part leaf)
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
		      (if mostly "mostly-seen" "seen")))
      (insert (format "Seen: %s\n" (format-time-string "%Y%m%dT%H%M%S"))))
    (message "Marked as seen")))

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
  (dolist (file (directory-files "/tv/unseen" t))
    (when (file-symlink-p file)
      (delete-file file)))
  (dolist (movie (movie-get-files "/dvd"))
    (let ((genre (split-string (or (plist-get movie :genre) "") ",")))
      (when (and (not (plist-get movie :seen))
		 (plist-get movie :genre)
		 (plist-get movie :year)
		 (not (plist-get movie :mostly-seen))
		 ;;(member (plist-get movie :country) '("" "fr" "us" "gb" "ca"))
		 (plist-get movie :genre)
		 (not (string-match "Star Trek" (plist-get movie :file)))
		 (not (string-match "Allen" (plist-get movie :director)))
		 (not (member "tv" genre))
		 (not (member "best" genre))
		 (not (member "Amazon" genre))
		 (not (member "comics" genre))
		 )
	(let ((file (plist-get movie :file)))
	  (make-symbolic-link
	   file (expand-file-name (file-name-nondirectory file)
				  "/tv/unseen")))))))

(defun movie-split-unseen (size)
  "Move some files from /tv/unseen until we have SIZE GB."
  (dolist (file (directory-files "/tv/other-unseen" t))
    (when (file-symlink-p file)
      (delete-file file)))
  (let ((films
	 (sort
	  (loop for film in (directory-files "/tv/unseen" t)
		unless (string-match "^[.]" (file-name-nondirectory film))
		collect (cons (movie-film-size film) film))
	  (lambda (f1 f2)
	    (< (random) (random))))))
    (loop for (s . link) in films
	  do (decf size (/ (float s) 1000 1000 1000))
	  while (plusp size)
	  do (rename-file link (expand-file-name (file-name-nondirectory link)
						 "/tv/other-unseen")))))

(defun movie-move-small-unseen ()
  "Create a directory of the smallest films from the unseen directory."
  (interactive)
  (dolist (file (directory-files "/tv/smallunseen" t))
    (when (file-symlink-p file)
      (delete-file file)))
  (let ((films
	 (sort
	  (loop for film in (directory-files "/tv/unseen" t)
		unless (string-match "^[.]" (file-name-nondirectory film))
		collect (cons (movie-film-size film) film))
	  (lambda (f1 f2)
	    (< (car f1) (car f2)))))
	;; 200GB
	(total (* 1000 1000 1000 205)))
    (loop for (size . film) in films
	  while (plusp total)
	  do
	  (decf total size)
	  (rename-file film
		       (expand-file-name (file-name-nondirectory film)
					 "/tv/smallunseen")))))

(defun movie-film-size (film)
  (loop for file in (directory-files-recursively film ".")
	sum (* 1.0 (file-attribute-size (file-attributes file)))))

;; "Bitchin Rides S01E06 The Juice Is Worth the Squeeze HDTV XviD-AF"
;; "Naild.It.S01E01.Nail.Pride.HDTV.x264-DaViEW"
;; "The Daily Show 2014 10 07 Wyatt Cenac HDTV x264-W4F [GloDLS]"
(defun movie-parse-description (desc)
  (let ((case-fold-search t))
    (cond
     ((string-match "^\\(.*\\)[. ]+s\\([0-9]+\\)e\\([0-9]+\\)" desc)
      (list :name (match-string 1 desc)
	    :season (movie-clean-number (match-string 2 desc))
	    :episode (movie-clean-number (match-string 3 desc))))
     ((string-match "^\\(.*?\\)[. ]+\\([-0-9 ]+\\)" desc)
      (list :name (match-string 1 desc)
	    :season "0"
	    :episode (movie-clean-number (match-string 2 desc))))
     (t
      (message "Unable to parse %s" desc)
      nil))))

(defun movie-clean-number (string)
  (save-match-data
    (replace-regexp-in-string
     " +" "-"
     (replace-regexp-in-string
      "^0+\\|^ +\\| $" "" string))))

(defun movie-last-seen (file)
  "Say when the series under point was last seen."
  (interactive (list (movie-current-file)))
  (let ((data (movie-parse-description (file-name-nondirectory file)))
	(case-fold-search t)
	results)
    (unless data
      (error "Couldn't parse %s" (file-name-nondirectory file)))
    (with-temp-buffer
      (insert-file-contents movie-positions-file)
      (goto-char (point-max))
      (while (and (re-search-backward
		   (replace-regexp-in-string "[^a-z0-9]" ".*"
					     (downcase (plist-get data :name)))
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
	(message "Not seen %s" (plist-get data :name))
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
  (loop for file in (cons (expand-file-name (format "Encode_1080P_%s.mp4" id)
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
      (loop for film = (pop films)
	    while film
	    for new-dir = (expand-file-name film "/dvd")
	    unless (file-exists-p new-dir)
	    do
	    (make-directory new-dir)
	    (loop for file in (if (zerop (length films))
				  files
				(list (pop files)))
		  do (rename-file file (expand-file-name
					(file-name-nondirectory file)
					new-dir))
		  (let ((png (concat ".png" file)))
		    (when (file-exists-p png)
		      (rename-file png new-dir)))))
      (delete-directory dir))))

(defun movie-goto-last-series ()
  "Go to the /dvd last series directory."
  (interactive)
  (let ((mkvs (loop for path in
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
	(let* ((current (caddr
			 (split-string
			  (buffer-substring (point) (line-end-position)))))
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
	 (> (loop for track in tracks
		  when (> (getf (cdr track) :length) (* 20 60))
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
      (delete-forward-char 1)
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
  (message "Reloaded"))

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

(defun movie-add-genre (dir)
  (interactive (list (movie-current-file)))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "stats" dir))
    (if (re-search-forward "^Genre: ")
	(progn
	  (end-of-line)
	  (insert ",best"))
      (re-search-forward "^$")
      (insert "Genre: best\n"))
    (write-region (point-min) (point-max) (expand-file-name "stats" dir))))

(defun movie-find-display ()
  "Return the highest display."
  (with-temp-buffer
    (call-process "xrandr" nil t)
    (goto-char (point-min))
    (let ((disp 0))
      (while (re-search-forward " connected" nil t)
	(incf disp))
      disp)))

(provide 'movie)

;;; movie.el ends here
