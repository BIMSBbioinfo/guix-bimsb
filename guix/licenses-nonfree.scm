;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;;
;;; This file is NOT part of GNU Guix but as it is supposed to be used with
;;; Guix it has the same license.
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

(define-module (guix licenses-nonfree)
  #:use-module (srfi srfi-9)
  #:use-module (guix licenses)
  #:export (artistic1.0
            non-free))

;;; Commentary:
;;;
;;; Non-free licenses.
;;;
;;;
;;; Code:

(define license (@@ (guix licenses) license))

(define artistic1.0
  (license "Artistic License 1.0"
           "http://www.perlfoundation.org/artistic_license_1_0"
           "http://www.gnu.org/licenses/license-list.html#ArtisticLicense"))

(define* (non-free uri #:optional (comment ""))
  "Return a license that does not fit any of the ones above or a collection of
licenses, not approved as free by the FSF.  More details can be found at URI."
  (license "non-free"
           uri
           comment))

;;; licenses-nonfree.scm ends here
