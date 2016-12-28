;;; movie.el --- playing movies
;; Copyright (C) 2004-2015 Lars Magne Ingebrigtsen

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

(defvar movie-order nil)
(defvar movie-limit nil)
(defvar movie-dvdnav-p nil)

(defvar movie-files "." ;"\\.\\(mpg\\|avi\\|wmv\\|ogm\\|rm\\|mpeg\\)$"
  "Regexp to match movie files.")

(defvar movie-player
  '("mplayer"
    "-vf" "screenshot"
    ;;"-framedrop" "-hardframedrop"
    "-volume" "100"
    "-vo" "xv"
    "-fs"
    "-quiet"
    "-softvol"
    "-ao" "alsa:device=hw=2.0"
    "-heartbeat-cmd" "/home/larsi/src/movie.el/xscreensave-off"
    "-delay" "-0.1"
    ;;"-ss" "1"
    ;; Pause at the end of files.
    ;;"-loop" "0"
    "-mouse-movements"
    "-cache-min" "99"
    "-cache" "10000"
    "-utf8"
    "-subfont-text-scale" "3"
     )
  "Command to play a file.")

(defvar movie-genres nil
  "A list of strings that are names of genres.")

(defvar movie-crop '("-vf" "crop=700:420")
  "Parameters to crop a 4:3 aspect ratio program.")

(defvar movie-deinterlace-switch "yadif=3"
  "-vf switch t opass to mplayer to deinterlace films.
Valid values include yadif=3 (CPU intensive), pp=lb (results in
double images when things are moving fast) and pp=ci (use half
the lines in the image and lots of stairing).")

(defvar movie-high-volume
  '("-af" "volume=15:1")
  "Parameters to boost the volume.")

(defvar movie-picture-directory nil
  "Directory where pictures are taken during movie playing.")

(defvar movie-positions-file "/tv/data/mplayer.positions"
  "Where viewing positions are stored.")

(defvar movie-file-id nil)

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
  (while (and (not (eobp))
	      (not (equal movie (get-text-property (point) 'file-name))))
    (forward-line 1)))

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
	    (push (read (current-buffer)) tracks)
	    (forward-line 1))
	  (push (cons 'tracks (nreverse tracks)) data))))
    (nreverse data)))	  

(defun movie-list-parts (match)
  "Limit the buffer to matching genres."
  (interactive
   (list
    (completing-read "Match: " (append movie-genres
				       (list "nostats" "unseen" "all"
					     "mostly-seen")))))
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
    ((equal match "mostly-seen")
     (lambda (stats)
       (equal (cdr (assoc "Status" stats)) "mostly-seen")))
    (t
     `(lambda (stats)
	(let ((genres (cdr (assoc "Genre" stats))))
	  (and genres
	       (member ,match (split-string genres ",")))))))))

(defun movie-list-by-year ()
  "List the movies by year."
  (interactive)
  (movie-browse default-directory 'year movie-limit))

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
	     (or (null match)
		 (and (stringp match)
		      (let ((case-fold-search t))
			(string-match match file)))
		 (and (functionp match)
		      (eq (car atts) t)
		      (funcall match (movie-get-stats file))))
	     (not (string-match "\\.png\\'\\|\\.JPG\\'\\|/stats\\|/seen-date\\|txt$\\|~$" file))
	     (or (and (eq (car atts) nil)
		      (string-match movie-files (file-name-nondirectory file)))
		 (and (eq (car atts) t)
		      (not (member (file-name-nondirectory file)
				   '("." ".." "lost+found"))))))
	(setq track (assoc (file-name-nondirectory file)
			   (cdr (assoc 'tracks stats))))
	(push `(:file ,file
		      :time ,(nth 5 atts)
		      :size ,(nth 7 atts)
		      :directoryp ,(nth 0 atts)
		      :year ,(let ((year (cdr (assoc "Year"
						     (movie-get-stats file)))))
			       (if year
				   (string-to-number year)
				 9999))
		      :genre ,(cdr (assoc "Genre" (movie-get-stats file)))
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

(defun movie-generate-buffer (files &optional order)
  (when (not order)
    (setq order 'chronological))
  (setq files (movie-sort files order))
  (dolist (file files)
    (let ((subtitles (length (plist-get file :subtitles)))
	  (dir-data (and (plist-get file :directoryp)
			 (movie-biggest-file-data file)))
	  (dvdp (string-match "^/dvd/" (plist-get file :file))))
      (when (eq order 'year)
	(insert (format "%04d " (or (plist-get file :year) 9999))))
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
	" %s%s\n"
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
	    (insert-image (create-image png)))
	   ((and (plist-get file :directoryp)
		 (file-exists-p (format "%s.png" (cdr dir-data))))
	    (insert-image (create-image (format "%s.png" (cdr dir-data)))))
	   (t
	    (insert-image (create-image "~/tmp/empty.png")))))
	(beginning-of-line)
	(put-text-property
	 (point) (1+ (point))
	 'file-name (file-name-nondirectory (plist-get file :file)))))))

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
	     (time-less-p (plist-get f1 :time)
			  (plist-get f2 :time))))
	  ((eq order 'year)
	   (lambda (f1 f2)
	     (< (or (plist-get f1 :year) 0)
		(or (plist-get f2 :year) 0))))
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
    (define-key map "e" 'movie-eject)
    (define-key map "k" 'movie-browse)
    (define-key map "c" 'movie-play-cropped)
    (define-key map "x" 'movie-prefixed-action)
    (define-key map "h" 'movie-play-high-volume)
    (define-key map "g" 'movie-rescan)
    (define-key map "t" 'movie-find-torrent)
    (define-key map "s" 'movie-toggle-sort)
    (define-key map "r" 'movie-rename)
    (define-key map "l" 'movie-last-seen)
    (define-key map "-" 'movie-collapse)
    (define-key map "i" 'movie-mark-as-seen)
    (define-key map "a" 'movie-add-stats)
    (define-key map "U" 'movie-update-stats-file)
    (define-key map "." 'end-of-buffer)
    (define-key map "," 'beginning-of-buffer)
    (define-key map "}" 'scroll-down-command)
    (define-key map "'" 'scroll-up-command)
    (define-key map "/" 'movie-limit)
    (define-key map "m" 'movie-list-parts)
    (define-key map "Y" 'movie-list-by-year)
    (define-key map "L" 'movie-list-by-director)
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
	      (or (cdr (assoc "Director" stats)))
	      (or (cdr (assoc "Country" stats)))))))

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
  (let ((options '("-vf" "screenshot"))
	(command nil)
	(movie-dvdnav-p nil))
    (while (let ((char (read-char "")))
	     (cond
	      ((eq char ?c)
	       (setq options
		     (movie-add-vf options "crop=700:420")))
	      ((eq char ?w)
	       (setq options
		     (movie-add-vf options "crop=700:300")))
	      ((eq char ?i)
	       (setq options (movie-add-vf options movie-deinterlace-switch)))
	      ((eq char ?x)
	       (setq options (append options (list "-vo" "xv"))))
	      ((eq char ?4)
	       (setq options (append options (list "--aspectratio=4:3"))))
	      ((eq char ?a)
	       (setq options
		     (movie-add-vf options "-monitoraspect=4:3")))
	      ((eq char ?n)
	       (setq movie-dvdnav-p t))
	      (t
	       (setq command
		     (lookup-key movie-mode-map (format "%c" char)))
	       nil))))
    (when (eq command 'movie-find-file)
      (setq command 'movie-play-simple))
    (let ((movie-player (append movie-player options)))
      (call-interactively command))))

(defun movie-add-vf (options vf)
  (setq options (copy-list options))
  (let ((old (member "-vf" options)))
    (if (not old)
	(append options (list "-vf" vf))
      (setcar (cdr old)
	      (concat vf "," (cadr old)))
      options)))

(defun movie-play-high-volume (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player movie-high-volume (list file))))

(defun movie-play (file)
  (interactive (list (movie-current-file)))
  (if (movie-interlaced-p file)
      (movie-play-1 (append (movie-add-vf movie-player movie-deinterlace-switch)
			    (list file)))
    (movie-play-1 (append movie-player (list file)))))

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
	     (not (equal file "/tv/live")))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
	(insert-file-contents movie-positions-file)
	(goto-char (point-max))
	(when (search-backward
	       (concat " " (file-name-nondirectory file) "\n") nil t)
	  (beginning-of-line)
	  (and (looking-at "[0-9]+")
	       ;; Skip backwards two seconds to avoid missing a second.
	       (format "%d" (max (- (string-to-number (match-string 0))
				    (if no-skip 0 2))
				 0))))))))
  
(defun movie-play-1 (player)
  ;; Kill off the glibc malloc checks that would make mplayer hang on
  ;; exit some times (due to double free()s).
  (setenv "MALLOC_CHECK" "0")
  (let ((skip (movie-find-position
	       (or movie-file-id
		   (car (last player))))))
    (when skip
      (setq player (cons (pop player)
			 (append (list "-ss" skip)
				 player)))))
  ;; The prefix command has been used to switch on libdvdnav playing.
  (when movie-dvdnav-p
    (let ((file (last movie-player)))
      (when (string-match "^dvd:" (car file))
	(setcar file (concat "dvdnav:" (substring (car file) 4)))))
    (unless (member "-ss" player)
      (setq player (cons (pop player)
			 (append (list "-ss" "1")
				 player)))))
  (if (not movie-picture-directory)
      (with-current-buffer (get-buffer-create "*mplayer*")
	(buffer-disable-undo)
	(erase-buffer)
	(apply 'call-process (car player) nil
	       (current-buffer)
	       nil (cdr player))
	(movie-update-mplayer-position (car (last player))))
    (let* ((path (file-truename (car (last player))))
	   (file (file-name-nondirectory
		  (directory-file-name (file-name-directory path))))
	   (dir (format "~/.emacs.d/screenshots/%s/"
			(if (zerop (length file))
			    "unknown"
			  file)))
	   (highest (movie-find-highest-image)))
      ;; For /dvd playing, we store the screenshots in the DVD
      ;; directory.
      (when (string-match "^/dvd/" path)
	(setq dir (file-name-directory path)))
      (when (file-symlink-p "~/.movie-current")
	(delete-file "~/.movie-current"))
      (make-symbolic-link dir "~/.movie-current")
      (unless (file-exists-p dir)
	(make-directory dir t))
      (with-current-buffer (get-buffer-create "*mplayer*")
	(buffer-disable-undo)
	(erase-buffer)
	;; mplayer will store the screenshots in the current directory.
	(setq default-directory dir)
	(apply 'call-process (car player) nil
	       (current-buffer)
	       nil (cdr player))
	(movie-update-mplayer-position (car (last player)))
	(movie-copy-images-higher-than highest dir))))
  (movie-update-stats-position (car (last player))))

(defun movie-update-mplayer-position (file)
  (goto-char (point-max))
  (when (re-search-backward "^@p \\([0-9.]+\\)" nil t)
    (let ((position (match-string 1))
	  (coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8))
      (with-temp-buffer
	(insert-file-contents movie-positions-file)
	(goto-char (point-max))
	(insert (format "%s %s\n" position (file-name-nondirectory file)))
	(write-region (point-min) (point-max) movie-positions-file
		      nil 'nomessage)))))

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

(defun movie-delete-file (file)
  "Delete the file under point."
  (interactive (list (movie-current-file)))
  (when (y-or-n-p (format "Really delete %s? " file))
    (if (file-directory-p file)
	(progn
	  (let ((files (directory-files file t)))
	    (cond
	     ((= (length files) 2)
	      (delete-directory file))
	     ((string-match "/torrent" file)
	      (delete-directory file t))
	     (t
	      (error "Directory not empty")))))
      (delete-file file)
      (let ((png (concat file ".png")))
	(when (file-exists-p png)
	  (delete-file png))))
    (beginning-of-line)
    (delete-region (point) (line-beginning-position 2))
    (unless (bobp)
      (forward-line -1))))

(defun movie-add-stats (dir &optional no-director)
  "Add a stats file to the directory under point."
  (interactive (list (movie-current-file) current-prefix-arg))
  (unless (file-directory-p dir)
    (error "Must be called on a directory"))
  (movie-make-stats-file dir no-director)
  (message "Made a stats file for %s" dir))

(defun movie-current-file ()
  (save-excursion
    (beginning-of-line)
    (concat default-directory "/"
	    (get-text-property (point) 'file-name))))

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
    (movie-rescan movie-order)
    (if (not current)
	(goto-char (point-max))
      (goto-char (point-min))
      (search-forward (file-name-nondirectory current) nil t)
      (beginning-of-line))))

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
	(rename-file file dir)))))

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
  (movie-play (format "dvdnav://%s" (or number ""))))

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
    (loop while (not (eobp))
	  for (format code resolution note) =
	  (split-string (buffer-substring (point) (line-end-position)))
	  if (string-match "(best)" (or note ""))
	  return format
	  collect format into formats
	  finally (return (car (last formats)))
	  do (forward-line 1))))

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
  (and (string-match "\\([0-9.]+\\)s" string)
       (string-to-number (match-string 1 string))))

(defun movie-interlaced-p (file)
  (with-temp-buffer
    (call-process "mediainfo" nil t nil file)
    (goto-char (point-min))
    (re-search-forward "^Scan type.*Interlace" nil t)))

(defun movie-make-stats-file (directory &optional no-directory)
  "Create a stats file for DIRECTORY."
  (interactive "dDirectory: \nP")
  (with-temp-file (expand-file-name "stats" directory)
    (let* ((title (replace-regexp-in-string " +([0-9]+)$" ""
					    (file-name-nondirectory directory)))
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
	  (plist-put (cdr track) :seen
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
    (when (and (not (plist-get movie :seen))
	       (not (plist-get movie :mostly-seen))
	       (plist-get movie :genre)
	       (not (string-match "Allen" (plist-get movie :director)))
	       (not (string-match "tv\\|comics" (plist-get movie :genre)))
	       (not (string-match "James Bond" (plist-get movie :genre))))
      (let ((file (plist-get movie :file)))
	(make-symbolic-link file (expand-file-name (file-name-nondirectory file)
						   "/tv/unseen"))))))

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
	(let ((show (buffer-substring
		     (progn
		       (beginning-of-line)
		       (search-forward " " (line-end-position) t)
		       (point))
		     (line-end-position))))
	  (unless (member show results)
	    (push show results)))
	(beginning-of-line)))
    (if (not results)
	(message "Not seen %s" (plist-get data :name))
      (message "%s" (mapconcat 'identity (nreverse results) "\n")))))

(provide 'movie)

;;; movie.el ends here
