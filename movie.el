;;; movie.el --- playing movies
;; Copyright (C) 2004-2011 Lars Magne Ingebrigtsen

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
(load "time-date.el")

(defvar movie-order nil)
(defvar movie-limit nil)

(defvar movie-files "." ;"\\.\\(mpg\\|avi\\|wmv\\|ogm\\|rm\\|mpeg\\)$"
  "Regexp to match movie files.")

(defvar movie-player
  '("mplayer"
    "-framedrop" "-hardframedrop"
    "-volume" "2"
    "-vo" "gl2:yuv=0"
    "-fs" "-quiet"
    "-softvol" "-softvol-max" "200"
    "-ao" "alsa:device=hw=0.7"
    "-heartbeat-cmd" "/home/larsi/src/movie.el/xscreensave-off"
    "-delay" "-0.1"
    ;; Pause at the end of files.
    "-loop" "0")
  "Command to play a file.")

(defvar movie-crop '("-vf" "crop=700:420")
  "Parameters to crop a 4:3 aspect ratio program.")

(defvar movie-high-volume
  '("-af" "volume=15:1")
  "Parameters to boost the volume.")

(defvar movie-aplayer
  '("mplayer" "-fs" "-monitoraspect" "4:3" "-softvol")
  "Player alternative for 4:3 monitors.")

(defvar movie-file-id nil)

(defun movie-browse (directory &optional order match)
  "Browse DIRECTORY."
  (interactive "DDirectory: ")
  (setq directory (file-truename directory)
	match (or match movie-limit))
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
    (goto-char (point-min))))

(defun movie-get-files (directory &optional match)
  (let ((files (directory-files directory t))
	(data nil)
	stats)
    (dolist (file files)
      (setq stats (file-attributes file))
      (when (and
	     (or (null match)
		 (let ((case-fold-search t))
		   (string-match match file)))
	     (not (string-match "\\.png\\'" file))
	     (or (and (eq (car stats) nil)
		      (string-match movie-files (file-name-nondirectory file)))
		 (and (eq (car stats) t)
		      (not (member (file-name-nondirectory file)
				   '("." ".."))))))
	(push (list file (nth 5 stats) (nth 7 stats) (nth 0 stats))
	      data)))
    data))

(defun movie-generate-buffer (files &optional order)
  (when (not order)
    (setq order 'chronological))
  (setq files (movie-sort files order))
  (dolist (file files)
    (insert (format " %s%s%s\n"
		    (file-name-nondirectory (nth 0 file))
		    (if (eq (nth 3 file) t) "/" "")
		    (if (eq (nth 3 file) t) ""
		      (format " (%d)"
			      (round (/ (or (nth 2 file) -1) 1024 1024))))))
    (save-excursion
      (forward-line -1)
      (let ((png (concat (nth 0 file) ".png")))
	(if (file-exists-p png)
	    (insert-image (create-image png))
	  (insert-image (create-image "~/tmp/empty.png"))))
      (beginning-of-line)
      (put-text-property (point) (1+ (point))
			 'file-name (file-name-nondirectory (nth 0 file))))))

(defun movie-limit (match)
  "Limit the buffer to matching files."
  (interactive
   (list
    (read-string "Match: "
		 (movie-prefix (file-name-nondirectory
				(movie-current-file))))))
  (setq movie-limit match)
  (movie-browse default-directory movie-order))

(defun movie-sort (files order)
  (let ((predicate
	 (cond
	  ((eq order 'alphabetical)
	   (lambda (f1 f2)
	     (string< (downcase (car f1)) (downcase (car f2)))))
	  ((eq order 'chronological)
	   (lambda (f1 f2)
	     (time-less-p (nth 1 f1) (nth 1 f2))))
	  (t
	   (error "No such order %s" order)))))
    (sort files predicate)))

(defvar movie-mode-map nil)
(unless movie-mode-map
  (setq movie-mode-map (make-sparse-keymap))
  (suppress-keymap movie-mode-map)
  (define-key movie-mode-map "\r" 'movie-find-file)
  (define-key movie-mode-map [delete] 'movie-delete-file)
  (define-key movie-mode-map [del] 'movie-delete-file)
  (define-key movie-mode-map [backspace] 'movie-delete-file)
  (define-key movie-mode-map [deletechar] 'movie-delete-file)
  (define-key movie-mode-map "d" 'movie-play-dvd)
  (define-key movie-mode-map "D" 'movie-play-whole-dvd)
  (define-key movie-mode-map "f" 'movie-play-next-vob)
  (define-key movie-mode-map "F" 'movie-play-current-vob)
  (define-key movie-mode-map "T" 'movie-thumbnails)
  (define-key movie-mode-map "q" 'bury-buffer)
  (define-key movie-mode-map "e" 'movie-eject)
  (define-key movie-mode-map "k" 'movie-browse)
  (define-key movie-mode-map "c" 'movie-play-cropped)
  (define-key movie-mode-map "h" 'movie-play-high-volume)
  (define-key movie-mode-map "a" 'movie-aplay)
  (define-key movie-mode-map "g" 'movie-rescan)
  (define-key movie-mode-map "t" 'movie-find-torrent)
  (define-key movie-mode-map "s" 'movie-toggle-sort)
  (define-key movie-mode-map "r" 'movie-rename)
  (define-key movie-mode-map "l" 'movie-list-channels)
  (define-key movie-mode-map "-" 'movie-collapse)
  (define-key movie-mode-map "." 'end-of-buffer)
  (define-key movie-mode-map "," 'beginning-of-buffer)
  (define-key movie-mode-map "}" 'scroll-down-command)
  (define-key movie-mode-map "'" 'scroll-up-command)
  (define-key movie-mode-map "/" 'movie-limit))

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

(defun movie-find-file (file)
  "Find or play the file under point."
  (interactive (list (movie-current-file)))
  (if (file-directory-p file)
      (movie-browse file 'alphabetical)
    (movie-play file)
    (discard-input)))

(defun movie-find-torrent ()
  "Find torrent dir"
  (interactive)
  (movie-find-file "/tv/torrent")
  (movie-rescan-1 'chronological)
  (goto-char (point-max)))

(defun movie-play-cropped (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player movie-crop (list file))))

(defun movie-play-high-volume (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player movie-high-volume (list file))))

(defun movie-aplay (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-aplayer (list file))))

(defun movie-play (file)
  (interactive (list (movie-current-file)))
  (movie-play-1 (append movie-player (list file))))

(defun movie-find-position (file)
  (when (and (file-exists-p "~/.mplayer.positions")
	     (string-match "^/tv\\|^/dvd" file))
    (with-temp-buffer
      (insert-file-contents "~/.mplayer.positions")
      (goto-char (point-max))
      (when (search-backward
	     (concat " " (file-name-nondirectory file) "\n") nil t)
	(beginning-of-line)
	(and (looking-at "[0-9]+")
	     ;; Skip backwards two seconds to avoid missing a second.
	     (format "%d" (max (- (string-to-number (match-string 0)) 2)
			       0)))))))
  
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
  (apply 'call-process (car player) nil
	 (get-buffer-create "*mplayer*")
	 nil (cdr player)))

(defun movie-delete-file (file)
  "Delete the file under point."
  (interactive (list (movie-current-file)))
  (when (y-or-n-p (format "Really delete %s? " file))
    (if (file-directory-p file)
	(delete-directory file)
      (delete-file file)
      (let ((png (concat file ".png")))
	(when (file-exists-p png)
	  (delete-file png))))
    (beginning-of-line)
    (movie-rescan-1)))

(defun movie-current-file ()
  (save-excursion
    (beginning-of-line)
    (concat default-directory "/"
	    (get-text-property (point) 'file-name))))

(defun movie-rescan (&optional order)
  "Update the current buffer."
  (interactive)
  (setq movie-limit nil)
  (movie-rescan-1 order))

(defun movie-rescan-1 (&optional order)
  (unless order
    (setq order movie-order))
  (let ((lines (count-lines (point-min) (point))))
    (movie-browse default-directory order movie-limit)
    (forward-line lines)))

(defun movie-toggle-sort ()
  "Toggle sorting by time."
  (interactive)
  (let ((current (movie-current-file)))
    (if (eq movie-order 'alphabetical)
	(setq movie-order 'chronological)
      (setq movie-order 'alphabetical))
    (movie-rescan movie-order)
    (goto-char (point-min))
    (search-forward (file-name-nondirectory current) nil t)
    (beginning-of-line)))

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
      (when (and (not (file-directory-p file))
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
    (insert-file-contents "~/.mplayer.positions")
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
    (when (file-exists-p "~/.mplayer.positions")
      (with-temp-buffer
	(insert-file-contents "~/.mplayer.positions")
	(goto-char (point-max))
	(forward-line -1)
	(when (looking-at "[0-9.]+ \\([0-9]+\\)\n")
	  (goto-char (match-beginning 1))
	  (delete-region (point) (line-end-position))
	  (insert (file-name-nondirectory movie-file-id))
	  (write-region (point-min) (point-max) "~/.mplayer.positions"
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
	  (when (> (string-to-number (match-string 1)) 1000)
	    (push index big-files)))))
    (list title (nreverse big-files))))

(defun movie-play-whole-dvd (number)
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

(defun movie-play-youtube (url)
  (let* ((default-directory "/tmp/")
	 (file "/tmp/youtube.flv")
	 (tmp (concat file ".part"))
	 (sleeps 40))
    (when (file-exists-p tmp)
      (delete-file tmp))
    (let ((process (start-process
		    "youtube" (get-buffer-create "*youtube*")
		    "youtube-dl"
		    "-q"
		    "-o" file
		    url)))
      (while (and (not (file-exists-p tmp))
		  (> (decf sleeps) 0))
	(sleep-for 0 100))
      (if (not (file-exists-p tmp))
	  (set-process-sentinel process 'movie-youtube-sentinel)
	(while (and (< (nth 7 (file-attributes tmp))
		       100000)
		    (> (decf sleeps) 0))
	  (sleep-for 0 100))
	(movie-play tmp)
	(delete-process process)))))

(defun movie-youtube-sentinel (process status)
  (when (equal status "finished\n")
    (movie-play "/tmp/youtube.flv")))

(provide 'movie)
;;; movie.el ends here
