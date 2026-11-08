;;; tmdb.el --- get information from TMDB files  -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; tmdb.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; tmdb.el is distributed in the hope that it will be useful,
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

(defvar tmdb-read-access-token nil
  "API read access token from TMDB.")

(defun tmdb-search (name)
  "Return search results for NAME."
  (unless tmdb-read-access-token
    (user-error "`tmdb-read-access-token' has not been set"))
  (let* ((url-request-extra-headers
          `(("Connection" . "close")
            ("Content-Type" ."application/json")
	    ("Accept" . "application/json")
	    ("Authorization" . ,(concat "Bearer " tmdb-read-access-token)))))
    (with-current-buffer (url-retrieve-synchronously
			  (concat
			   "https://api.themoviedb.org/3/search/person?query="
			   (browse-url-encode-url name)))
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (plist-get (json-parse-buffer :object-type 'plist) :results))
	(kill-buffer (current-buffer))))))

(defun tmdb-actor-image (name)
  (cl-loop for elem across (tmdb-search name)
	   when (equal (plist-get elem :known_for_department) "Acting")
	   return (let ((image (plist-get elem :profile_path)))
		    (and (stringp image)
			 (tmdb--fetch
			  (concat "https://image.tmdb.org/t/p/h632" image))))))

(defun tmdb--fetch (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (unwind-protect
	(and (search-forward "\n\n" nil t)
	     (buffer-substring (point) (point-max)))
      (kill-buffer (current-buffer)))))

(provide 'tmdb)

;;; tmdb.el ends here

