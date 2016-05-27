;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages perl))

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
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((target (assoc-ref outputs "out"))
                      (doc (string-append target "/share/doc/f-seq/")))
                 (mkdir-p target)
                 (mkdir-p doc)
                 (substitute* "bin/linux/fseq"
                   (("java") (which "java")))
                 (install-file "README.txt" doc)
                 (install-file "bin/linux/fseq" (string-append target "/bin"))
                 (install-file "build~/fseq.jar" (string-append target "/lib"))
                 (copy-recursively "lib" (string-append target "/lib"))
                 #t))))))
      (inputs
       `(("perl" ,perl)))
      (home-page "http://fureylab.web.unc.edu/software/fseq/")
      (synopsis "Feature density estimator for high-throughput sequence tags")
      (description
       "F-Seq is a software package that generates a continuous tag sequence
density estimation allowing identification of biologically meaningful sites
whose output can be displayed directly in the UCSC Genome Browser.")
      (license license:gpl3+))))
