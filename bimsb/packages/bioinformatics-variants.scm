;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
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

(define-module (bimsb packages bioinformatics-variants)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics))

;; A different version of MACS2 for Rebecca
(define-public macs/rebecca
  (package (inherit macs)
    (version "2.1.0.20150420.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MACS2" version))
       (sha256
        (base32
         "1zpxn9nxmfwj8zr8b4gjrx8j81mijgjmagl4prpkghphfbziwraw"))))))

(define-public bedtools-2.23.0
  (package
    (inherit bedtools)
    (version "2.23.0")
    (name "bedtools")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/arq5x/bedtools2/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mk0lg3bl6k7kbn675hinwby3jrb17mml7nms4srikhi3mbamb4x"))))))

(define-public samtools-1.1
  (package
    (inherit samtools-0.1)
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1y5p2hs4gif891b4ik20275a8xf3qrr1zh9wpysp4g8m0g1jckf2"))))))

;; Fixed version of ParDRe for Harm.
(define-public pardre/fixed
  (package (inherit pardre)
    (name "pardre-fixed")
    (source (origin (inherit (package-source pardre))
                    (patches (search-patches "pardre-fix-utils.patch"))))))
