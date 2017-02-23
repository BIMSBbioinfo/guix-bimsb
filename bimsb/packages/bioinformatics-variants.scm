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
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages statistics))

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

;; Patched version of bedtools 2.25.0, which segfaults on some
;; RCAS/dorina test datasets in Bed6+ format.
(define-public bedtools/patched
  (package
    (inherit bedtools)
    (name "bedtools")
    (version "2.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/arq5x/bedtools2/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ywcy3yfwzhl905b51l0ffjia55h75vv3mw5xkvib04pp6pj548m"))
       (patches (list (search-patch "bedtools-fix-null-segfault.patch")))))))

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

(define-public samtools-0
  (package (inherit samtools)
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "16js559vg13zz7rxsj4kz2l96gkly8kdk8wgj9jhrqwgdh7jq9iv"))))
    (arguments
     `(#:tests? #f ;no "check" target
       ,@(substitute-keyword-arguments `(#:modules ((guix build gnu-build-system)
                                                    (guix build utils)
                                                    (srfi srfi-1)
                                                    (srfi srfi-26))
                                         ,@(package-arguments samtools))
           ((#:make-flags flags)
            `(cons "LIBCURSES=-lncurses" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'patch-tests)
               (delete 'configure)
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append
                               (assoc-ref outputs "out") "/bin")))
                     (mkdir-p bin)
                     (copy-file "samtools" (string-append bin "/samtools-" ,version)))
                   #t)))))))))

;; Fixed version of ParDRe for Harm.
(define-public pardre/fixed
  (package (inherit pardre)
    (name "pardre-fixed")
    (source (origin (inherit (package-source pardre))
                    (patches (search-patches "pardre-fix-utils.patch"))))))

;; This is needed for MEDICC
(define-public python2-biopython-1.62
  (package
    (inherit python2-biopython)
    (version "1.62")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "biopython" version))
              (sha256
               (base32
                "1gbq54l0nlqyjxxhq6nrqjfdx98x039akcxxrr5y3j7ccjw47wm2"))))))

(define-public infernal-1.0
  (package (inherit infernal)
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://eddylab.org/software/infernal/"
                                  "infernal-" version ".tar.gz"))
              (sha256
               (base32
                "1ba3av8xg4309dpy1ls73nxk2v7ri0yp0ix6ad5b1j35x319my64"))))
    (arguments
     ;; We need to disable tests because we don't seem to have
     ;; getopts.pl.
     `(#:tests? #f))))

(define-public r-3.3.1
  (package (inherit r)
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qm9znh8akfy9fkzzi6f1vz2w1dd0chsr6qn7kw80lqzhgjrmi9x"))))))

;; (define-public r-3.3.2
;;   (package (inherit r)
;;     (version "3.3.2")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "mirror://cran/src/base/R-"
;;                                   (version-prefix version 1) "/R-"
;;                                   version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "0k2i9qdd83g09fcpls2198q4ykxkii5skczb514gnx7mx4hsv56j"))))))
