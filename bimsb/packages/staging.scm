;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (bimsb packages staging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages java))

(define-public bedtools/patched
  (package (inherit bedtools)
    (version (string-append (package-version bedtools) "-1"))
    (source
     (origin (inherit (package-source bedtools))
       (patches (list (search-patch "bedtools-fix-null-segfault.patch")))))))

(define-public samtools-0
  (package (inherit samtools)
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "16js559vg13zz7rxsj4kz2l96gkly8kdk8wgj9jhrqwgdh7jq9iv"))))
    (arguments
     (substitute-keyword-arguments `(#:modules ((guix build gnu-build-system)
                                                (guix build utils)
                                                (srfi srfi-1)
                                                (srfi srfi-26))
                                               ,@(package-arguments samtools))
       ((#:tests? tests) #f) ;no "check" target
       ((#:phases phases)
        `(alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append
                        (assoc-ref outputs "out") "/bin")))
            (mkdir-p bin)
            (copy-file "samtools" (string-append bin "/samtools-" ,version))))
          (alist-delete 'patch-tests ,phases)))))))

;; This package cannot yet be added to Guix because it bundles an as
;; yet unpackaged third-party library, namely "commons-cli-1.1.jar"
(define-public f-seq
  (let ((commit "d8cdf18")
        (revision "1"))
    (package
      (name "f-seq")
      (version (string-append "1.85." revision "." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aboyle/F-seq.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1rz305g6ikan5w9h7rl4a072qsb6h3371cmgppg9ribjnivqh3v7"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (replace 'build
            (lambda _
              (setenv "JAVA_HOME" (assoc-ref %build-inputs "jdk"))
              (zero? (system* "ant"))))
           (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((target (assoc-ref outputs "out"))
                     (doc (string-append target "/share/doc/f-seq/")))
                (mkdir-p target)
                (mkdir-p doc)
                (system* "tar" "-xvf" "dist~/fseq.tar")
                (install-file "fseq/README.txt" doc)
                (copy-recursively "fseq/bin" (string-append target "/bin"))
                (copy-recursively "fseq/lib" (string-append target "/lib"))
                #t))))))
      (native-inputs
       `(("ant" ,ant)
         ("jdk" ,icedtea7 "jdk")))
      (home-page "http://fureylab.web.unc.edu/software/fseq/")
      (synopsis "Feature density estimator for high-throughput sequence tags")
      (description
       "F-Seq is a software package that generates a continuous tag sequence
density estimation allowing identification of biologically meaningful sites
whose output can be displayed directly in the UCSC Genome Browser.")
      (license license:gpl3+))))
