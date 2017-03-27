;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages wxwidgets))

;; An older version of Perl is required for Bcl2Fastq version 1.x
(define-public perl-5.14
  (package (inherit perl)
    (name "perl")
    (version "5.14.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://cpan/src/5.0/perl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1js47zzna3v38fjnirf2vq6y0rjp8m86ysj5vagzgkig956d8gw0"))
             (patches (map search-patch
                           '("perl-5.14-no-sys-dirs.patch"
                             "perl-5.14-autosplit-default-time.patch"
                             "perl-5.14-module-pluggable-search.patch")))))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc")))
               ;; Use the right path for `pwd'.
               (substitute* "dist/Cwd/Cwd.pm"
                 (("/bin/pwd")
                  (which "pwd")))

               (zero?
                (system* "./Configure"
                         (string-append "-Dprefix=" out)
                         (string-append "-Dman1dir=" out "/share/man/man1")
                         (string-append "-Dman3dir=" out "/share/man/man3")
                         "-de" "-Dcc=gcc"
                         "-Uinstallusrbinperl"
                         "-Dinstallstyle=lib/perl5"
                         "-Duseshrplib"
                         (string-append "-Dlocincpth=" libc "/include")
                         (string-append "-Dloclibpth=" libc "/lib")

                         ;; Force the library search path to contain only libc
                         ;; because it is recorded in Config.pm and
                         ;; Config_heavy.pl; we don't want to keep a reference
                         ;; to everything that's in $LIBRARY_PATH at build
                         ;; time (Binutils, bzip2, file, etc.)
                         (string-append "-Dlibpth=" libc "/lib")
                         (string-append "-Dplibpth=" libc "/lib"))))))

         (add-before 'strip 'make-shared-objects-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'lib/perl5' directory contains ~50 MiB of .so.  Make them
             ;; writable so that 'strip' actually strips them.
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each (lambda (dso)
                           (chmod dso #o755))
                         (find-files lib "\\.so$"))))))))))

(define (perl-5.14-package-name name)
  "Return NAME with a \"perl5.14-\" prefix instead of \"perl-\", when
applicable."
  (if (string-prefix? "perl-" name)
      (string-append "perl5.14-"
                     (string-drop name
                                  (string-length "perl-")))
      name))

(define-public (package-for-perl-5.14 pkg)
  ;; This is a procedure to replace PERL by PERL-5.14, recursively.
  ;; It also ensures that rewritten packages are built with PERL-5.14.
  (let* ((rewriter (package-input-rewriting `((,perl . ,perl-5.14))
                                            perl-5.14-package-name))
         (new (rewriter pkg)))
    (package (inherit new)
      (arguments `(#:perl ,perl-5.14
                   #:tests? #f
                   ,@(package-arguments new))))))

(define-public boost-1.58
  (package (inherit boost)
    (name "boost")
    (version "1.58.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1rfkqxns60171q62cppiyzj8pmsbwp1l8jd7p6crriryqd7j1z7x"))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("tcsh" ,tcsh)))
    (arguments
     `(#:tests? #f ; TODO
       #:make-flags
       (list "threading=multi" "link=shared"

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")

             ;; Boost's 'context' library is not yet supported on mips64, so
             ;; we disable it.  The 'coroutine' library depends on 'context',
             ;; so we disable that too.
             ,@(if (string-prefix? "mips64" (or (%current-target-system)
                                                (%current-system)))
                   '("--without-context"
                     "--without-coroutine" "--without-coroutine2")
                   '()))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/doc/bjam.qbk"
                              "tools/build/src/engine/execunix.c"
                              "tools/build/src/engine/Jambase"
                              "tools/build/src/engine/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (zero? (system* "./bootstrap.sh"
                               (string-append "--prefix=" out)
                               "--with-toolset=gcc")))))
         (replace 'build
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (zero? (apply system* "./b2"
                           (format #f "-j~a" (parallel-job-count))
                           make-flags))))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (zero? (apply system* "./b2" "install" make-flags)))))))))

(define-public boost-1.55
  (package (inherit boost-1.58)
    (name "boost")
    (version "1.55.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "0lkv5dzssbl5fmh2nkaszi8x9qbj80pr4acf9i26sj3rvlih1w7z"))))
    (arguments
     `(#:tests? #f ; TODO
       #:make-flags
       (list "threading=multi" "link=shared"

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")

             ;; Boost's 'context' library is not yet supported on mips64, so
             ;; we disable it.  The 'coroutine' library depends on 'context',
             ;; so we disable that too.
             ,@(if (string-prefix? "mips64" (or (%current-target-system)
                                                (%current-system)))
                   '("--without-context"
                     "--without-coroutine" "--without-coroutine2")
                   '()))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/v2/doc/bjam.qbk"
                              "tools/build/v2/engine/execunix.c"
                              "tools/build/v2/engine/Jambase"
                              "tools/build/v2/engine/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (zero? (system* "./bootstrap.sh"
                               (string-append "--prefix=" out)
                               "--with-toolset=gcc")))))
         (replace 'build
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (zero? (apply system* "./b2"
                           (format #f "-j~a" (parallel-job-count))
                           make-flags))))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (zero? (apply system* "./b2" "install" make-flags)))))))))

(define-public boost-1.44
  (package (inherit boost-1.55)
    (name "boost")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1nvq36mvzr1fr85q0jh86rk3bk65s1y55jgqgzfg3lcpkl12ihs5"))))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "threading=multi" "link=shared"
             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; See https://svn.boost.org/trac/boost/ticket/6165
         (add-after 'unpack 'fix-threads-detection
           (lambda _
             (substitute* "boost/config/stdlib/libstdcpp3.hpp"
               (("_GLIBCXX_HAVE_GTHR_DEFAULT")
                "_GLIBCXX_HAS_GTHREADS"))
             #t))
         ;; See https://svn.boost.org/trac/boost/ticket/6940
         (add-after 'unpack 'fix-TIME_UTC
           (lambda _
             (substitute* '("libs/interprocess/test/util.hpp"
                            "libs/interprocess/test/condition_test_template.hpp"
                            "libs/spirit/classic/test/grammar_mt_tests.cpp"
                            "libs/spirit/classic/test/owi_mt_tests.cpp"
                            "libs/thread/src/pthread/thread.cpp"
                            "libs/thread/src/pthread/timeconv.inl"
                            "libs/thread/src/win32/timeconv.inl"
                            "libs/thread/test/util.inl"
                            "libs/thread/test/test_xtime.cpp"
                            "libs/thread/example/xtime.cpp"
                            "libs/thread/example/tennis.cpp"
                            "libs/thread/example/starvephil.cpp"
                            "libs/thread/example/thread.cpp"
                            "boost/thread/xtime.hpp")
               (("TIME_UTC") "TIME_UTC_"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/jam/doc/bjam.qbk"
                              "tools/jam/src/execunix.c"
                              "tools/jam/src/Jambase"
                              "tools/jam/src/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (zero? (system* "./bootstrap.sh"
                               (string-append "--prefix=" out)
                               "--with-toolset=gcc")))))
         (replace 'build
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (zero? (apply system* "./bjam"
                           (format #f "-j~a" (parallel-job-count))
                           make-flags))))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (zero? (apply system* "./bjam" "install" make-flags)))))))))

;; Guix commit 6f9ba4c91c096a2fb95da111be0657d99ef2b683 removed this
;; for security reasons.  However, rapidstorm from staging depends on
;; this version.
(define-public wxwidgets-2
  (package
    (inherit wxwidgets)
    (version "2.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wxWidgets/wxWidgets/"
                           "releases/download/v" version
                           "/wxGTK-" version ".tar.gz"))
       (sha256
        (base32 "1gjs9vfga60mk4j4ngiwsk9h6c7j22pw26m3asxr1jwvqbr8kkqk"))))
    (inputs
     `(("gtk" ,gtk+-2)
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("libmspack" ,libmspack)
       ("sdl" ,sdl)
       ("unixodbc" ,unixodbc)))
    (arguments
     `(#:configure-flags
       '("--enable-unicode" "--with-regex=sys" "--with-sdl")
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; No 'check' target.
       #:tests? #f))))

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

;; For Benedikt
(define-public python-pysam-0.9
  (package (inherit python-pysam)
    (name "python-pysam")
    (version "0.9.1.4")
    (source (origin
              (method url-fetch)
              ;; Test data is missing on PyPi.
              (uri (string-append
                    "https://github.com/pysam-developers/pysam/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y41ssbg6nvn2jgcbnrvkzblpjcwszaiv1rgyd8dwzjkrbfsgsmc"))
              (modules '((guix build utils)))
              (snippet
               ;; Drop bundled htslib. TODO: Also remove samtools and bcftools.
               '(delete-file-recursively "htslib"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
          (add-before 'build 'set-flags
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "HTSLIB_MODE" "external")
              (setenv "HTSLIB_LIBRARY_DIR"
                      (string-append (assoc-ref inputs "htslib") "/lib"))
              (setenv "HTSLIB_INCLUDE_DIR"
                      (string-append (assoc-ref inputs "htslib") "/include"))
              (setenv "LDFLAGS" "-lncurses")
              (setenv "CFLAGS" "-D_CURSES_LIB=1")
              #t))
          (delete 'check))))))

(define-public python2-pysam-0.9
  (package-with-python2 python-pysam-0.9))
