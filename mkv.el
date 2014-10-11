;;; mkv.el --- get information from MKV files
;; Copyright (C) 2014 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; mkv.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mkv.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'dom)
(require 'shr)

(defun mkv-information (file)
  "Output pertinent information about MKV FILE."
  (interactive "fMKV File: ")
  (with-temp-buffer
    (call-process "mkvinfo" nil t nil file)
    (goto-char (point-min))
    (mkv-parse)))

(defun mkv-parse ()
  (cons 'mkv-data (loop while (not (eobp))
			collect (mkv-parse-1))))

(defun mkv-parse-1 ()
  (let* ((line (mkv-parse-line))
	 (level (car line)))
    (forward-line 1)
    (cons (intern (replace-regexp-in-string
		   "[^-a-zA-Z90-9_]" "-" (cadr line)) obarray)
	  (loop while (not (eobp))
		for line = (mkv-parse-line)
		while (= (1+ level) (car line))
		collect (if (null (nth 2 line))
			    (mkv-parse-1)
			  (forward-line 1)
			  (cons (intern (concat ":"
						(replace-regexp-in-string
						 "[^-a-zA-Z90-9_]" "-"
						 (cadr line)))
					obarray)
				(nth 2 line)))))))

(defun mkv-parse-line ()
  (and (looking-at "\\([+| ]+\\)\\([^:\n]+\\)\\(: \\(.+\\)\\)?")
       (list (length (match-string 1))
	     (match-string 2)
	     (match-string 4))))
  

(provide 'mkv)

;;; mkv.el ends here

