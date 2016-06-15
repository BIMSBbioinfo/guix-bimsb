;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build hg)
  #:use-module (guix build utils)
  #:export (hg-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix hg-download).  It allows a
;;; Mercurial repository to be cloned and checked out at a specific changeset
;;; identifier.
;;;
;;; Code:

(define* (hg-fetch url changeset directory
                   #:key (hg-command "hg"))
  "Fetch CHANGESET from URL into DIRECTORY.  CHANGESET must be a valid
Mercurial changeset identifier.  Return #t on success, #f otherwise."

  (and (zero? (system* hg-command
                       "clone" url
                       "--rev" changeset
                       ;; Disable TLS certificate verification.  The hash of
                       ;; the checkout is known in advance anyway.
                       "--insecure"
                       directory))
       (with-directory-excursion directory
         (begin
           ;; The contents of '.hg' vary as a function of the current
           ;; status of the Mercurial repo.  Since we want a fixed
           ;; output, this directory needs to be taken out.
           (delete-file-recursively ".hg")
           #t))))

;;; hg.scm ends here
