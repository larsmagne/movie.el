;;; movie.el --- playing movies
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <lmi@gnus.org>
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

(load "time-date.el")

(defvar movie-order nil)
(defvar movie-limit nil)

(defvar movie-files "." ;"\\.\\(mpg\\|avi\\|wmv\\|ogm\\|rm\\|mpeg\\)$"
  "Regexp to match movie files.")

(defvar movie-player
  '("mplayer"
    "-framedrop" "-hardframedrop"
    "-volume" "2"
    "-vo" "xv" "-fs" "-quiet" "-softvol"
    "-ao" "alsa:device=hw=1.7"
    "-heartbeat-cmd" "/home/larsi/src/movie.el/xscreensave-off"
    "-delay" "-0.1")
  "Command to play a file.")

(defvar movie-crop '("-vf" "crop=700:420")
  "Parameters to crop a 4:3 aspect ratio program.")

(defvar movie-high-volume
  '("-af" "volume=15")
  "Parameters to boost the volume.")

(defvar movie-aplayer
  '("mplayer" "-fs" "-monitoraspect" "4:3" "-softvol")
  "Player alternative for 4:3 monitors.")

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
  (interactive "sMatch: ")
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
  (define-key movie-mode-map "q" 'bury-buffer)
  (define-key movie-mode-map "k" 'movie-browse)
  (define-key movie-mode-map "c" 'movie-play-cropped)
  (define-key movie-mode-map "h" 'movie-play-high-volume)
  (define-key movie-mode-map "a" 'movie-aplay)
  (define-key movie-mode-map "g" 'movie-rescan)
  (define-key movie-mode-map "t" 'movie-find-torrent)
  (define-key movie-mode-map "s" 'movie-toggle-sort)
  (define-key movie-mode-map "r" 'movie-rename)
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

(defun movie-play-1 (player)
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
  (if (eq movie-order 'alphabetical)
      (setq movie-order 'chronological)
    (setq movie-order 'alphabetical))
  (movie-rescan movie-order))

(defun movie-rename (to)
  "Rename the current movie."
  (interactive "FNew name: ")
  (rename-file (movie-current-file) to))

(defun movie-play-svideo (file)
  "Play a file."
  (interactive (list (movie-current-file)))
  (let ((process (start-process "pvplay"
				(get-buffer-create "*pvplay*")
				"/usr/local/src/pvr350player_1.0/pvr350player" file))
	(command nil))
    (unwind-protect
	(while (not (equal command ?q))
	  (setq command (read-char ""))
	  (process-send-string process (format "%c" command)))
      (ignore-errors
	(delete-process process)))))

(defun movie-play-dvd (number)
  "Play the DVD."
  (interactive "p")
  (if t
      (call-process "/home/larsi/src/movie.el/vc"
		    nil (get-buffer-create "*mplayer*") nil
		    (format "%d" number))
    (apply 'call-process
	   (car movie-player)
	   nil
	   (get-buffer-create "*mplayer*")
	   nil
	   (append (cdr movie-player)
		   (list (format "dvd://%d" number))))))

;;; movie.el ends here
