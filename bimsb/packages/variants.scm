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

(define-module (bimsb packages variants)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics))

(define-public rsem-latest
  (package (inherit rsem)
    (name "rsem")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/deweylab/RSEM/archive/v"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17y0496qar4hjs15pvrwp7d3svjdapl6akcp0iakg4564np6lxx6"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled copy of boost.  We cannot remove the
           ;; bundled version of samtools because rsem uses a modified
           ;; version of it.
           (delete-file-recursively "boost")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:make-flags
       (list (string-append "BOOST="
                            (assoc-ref %build-inputs "boost")
                            "/include/"))
       #:phases
       (modify-phases %standard-phases
         ;; No "configure" script.
         (replace 'configure
           (lambda _
             (substitute* '("samtools-1.3/configure"
                            "samtools-1.3/htslib-1.3/configure")
               (("/bin/sh") (which "sh")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (string-append (assoc-ref outputs "out")))
                    (bin (string-append out "/bin/"))
                    (perl (string-append out "/lib/perl5/site_perl")))
               (mkdir-p bin)
               (mkdir-p perl)
               (for-each (lambda (file)
                           (copy-file file
                                      (string-append bin (basename file))))
                         (find-files "." "rsem-.*"))
               (copy-file "rsem_perl_utils.pm"
                          (string-append perl "/rsem_perl_utils.pm")))
             #t))
         (add-after 'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each (lambda (prog)
                          (wrap-program (string-append out "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(string-append out "/lib/perl5/site_perl")))))
                        '("rsem-plot-transcript-wiggles"
                          "rsem-calculate-expression"
                          "rsem-generate-ngvector"
                          "rsem-run-ebseq"
                          "rsem-prepare-reference")))
            #t)))))
    (inputs
     `(("boost" ,boost)
       ("ncurses" ,ncurses)
       ("r" ,r)
       ("perl" ,perl)
       ("zlib" ,zlib)))))
