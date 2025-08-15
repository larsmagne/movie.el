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

(require 'cl-lib)
(require 'dom)
(require 'shr)

(defun mkv-information (file)
  "Output pertinent information about MKV FILE."
  (interactive "fMKV File: ")
  (with-temp-buffer
    (with-environment-variables (("LC_ALL" (getenv "LANG")))
      (call-process "mkvinfo" nil t nil file))
    (goto-char (point-min))
    (mkv-parse)))

(defun mkv-parse ()
  (apply 'dom-node 'mkv-data nil
	 (cl-loop while (not (eobp))
		  collect (mkv-parse-1))))

(defun mkv-parse-1 ()
  (let* ((line (mkv-parse-line))
	 (level (car line))
	 (tag (and line
		   (intern (replace-regexp-in-string
			    "[^-a-zA-Z90-9_]" "-" (cadr line))
			   obarray)))
	 attributes children)
    (forward-line 1)
    (while (and (not (eobp))
		line
		(= (1+ level) (or (car (setq line (mkv-parse-line))) 0)))
      (when line
	(if (null (nth 2 line))
	    (push (mkv-parse-1) children)
	  (forward-line 1)
	  (push
	   (cons (intern (concat ":"
				 (replace-regexp-in-string
				  "[^-a-zA-Z90-9_]" "-"
				  (cadr line)))
			 obarray)
		 (nth 2 line))
	   attributes))))
    (apply 'dom-node tag (nreverse attributes) (nreverse children))))

(defun mkv-parse-line ()
  (and (looking-at "\\([+| ]+\\)\\([^:\n]+\\)\\(: \\(.+\\)\\)?")
       (list (length (match-string 1))
	     (match-string 2)
	     (match-string 4))))
  
(provide 'mkv)

;;; mkv.el ends here

