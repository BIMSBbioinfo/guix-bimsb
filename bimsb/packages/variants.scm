;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2019 Marcel Schilling <marcel.schilling@mdc-berlin.de>
;;; Copyright © 2020 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (past packages boost)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (past packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages wxwidgets))

(define (other-perl-package-name other-perl)
  "Return a procedure that returns NAME with a new prefix for
OTHER-PERL instead of \"perl-\", when applicable."
  (let ((other-perl-version (version-major+minor
                             (package-version other-perl))))
    (lambda (name)
      (if (string-prefix? "perl-" name)
          (string-append "perl" other-perl-version "-"
                         (string-drop name
                                      (string-length "perl-")))
          name))))

(define-public (package-for-other-perl other-perl pkg)
  ;; This is a procedure to replace PERL by OTHER-PERL, recursively.
  ;; It also ensures that rewritten packages are built with OTHER-PERL.
  (let* ((rewriter (package-input-rewriting `((,perl . ,other-perl))
                                            (other-perl-package-name other-perl)))
         (new (rewriter pkg)))
    (package (inherit new)
      (arguments `(#:perl ,other-perl
                   #:tests? #f ; oh well...
                   ,@(package-arguments new))))))

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

(define-public htslib-1.0
  (package (inherit htslib)
    (name "htslib")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/htslib/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a483cqlhs9k8gjdyb78jkrpilhz2qyyvgrgr8bhy11mpki8vflk"))))
    (arguments
     `(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               (("/bin/bash") (which "bash")))
             #t)))))))

;; Needed for pacbio-htslib
(define-public htslib-1.1
  (package (inherit htslib-1.0)
    (name "htslib")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/htslib/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gwj4mzrys5bs7r8h3fl7w2qfzwbvbby6qmgzj552di3hqc7j2pb"))))))

(define-public htslib-1.2.1
  (package (inherit htslib)
    (name "htslib")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/samtools/htslib/"
                                  "releases/download/"
                                  version "/htslib-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1c32ssscbnjwfw3dra140fq7riarp2x990qxybh34nr1p5r17nxx"))))))

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
         "0mk0lg3bl6k7kbn675hinwby3jrb17mml7nms4srikhi3mbamb4x"))))
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*")))
             #t)))))))

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
       (patches (list (search-patch "bedtools-fix-null-segfault.patch")))))
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*")))
             #t)))))))

(define-public samtools-1.1
  (package
    (inherit samtools-0.1)
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1y5p2hs4gif891b4ik20275a8xf3qrr1zh9wpysp4g8m0g1jckf2"))))))

(define-public htslib-1.3
  (package
    (inherit htslib)
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1rja282fwdc25ql6izkhdyh8ppw8x2fs0w0js78zgkmqjlikmma9"))))))

(define-public htslib-1.4
  (package
    (inherit htslib)
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1crkk79kgxcmrkmh5f58c4k93w4rz6lp97sfsq3s6556zxcxvll5"))))))

(define-public samtools-1.3
  (package
    (inherit samtools)
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "0znnnxc467jbf1as2dpskrjhfh8mbll760j6w6rdkwlwbqsp8gbc"))))
    (inputs
     `(("htslib" ,htslib-1.3)
       ,@(package-inputs samtools)))))

(define-public samtools-1.4
  (package (inherit samtools)
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "0vzxjm5vkgvzynl7cssm1l560rqs2amdaib1x8sp2ch9b7bxx9xx"))))
    (inputs
     `(("htslib" ,htslib-1.4)
       ,@(package-inputs samtools)))))

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
           ((#:make-flags flags ''())
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

(define-public bamtools-2.0
  (package (inherit bamtools)
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pezmaster31/bamtools/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1s95gjhnlfd91mr692xp14gmirfwq55dsj1jir9fa06m1lk4iw1n"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "src/third_party/")
                  (substitute* "src/CMakeLists.txt"
                    (("add_subdirectory \\(third_party\\)") ""))
                  (substitute* "src/toolkit/bamtools_filter.cpp"
                    (("jsoncpp/json.h") "json/json.h"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments bamtools)
       ((#:phases phases)
        `(modify-phases ,phases
           ;; See https://github.com/pezmaster31/bamtools/pull/150
           (add-after 'unpack 'fix-building-with-c++11
             (lambda _
               (substitute* "src/toolkit/bamtools_resolve.cpp"
                 (("make_pair<string, ?(bool|ReadGroupResolver)>")
                  "make_pair"))))
           (add-after 'unpack 'add-install-target-for-utils-library
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/utils/CMakeLists.txt"
                 (("target_link_libraries.*" line)
                  (string-append line "\ninstall(TARGETS BamTools-utils \
LIBRARY DESTINATION \"lib/bamtools\")")))))))))
    (inputs
     `(("jsoncpp" ,jsoncpp)
       ,@(package-inputs bamtools)))))

(define-public htslib-latest
  (package (inherit htslib)
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "0bcjmnbwp2bib1z1bkrp95w9v2syzdwdfqww10mkb1hxlmg52ax0"))))))

(define-public bcftools-latest
  (package (inherit bcftools)
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/bcftools/releases/download/"
                    version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "0093hkkvxmbwfaa7905s6185jymynvg42kq6sxv7fili11l5mxwz"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled htslib.
               '(delete-file-recursively "htslib-1.5"))))
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (list "--enable-libgsl"
             (string-append "--with-htslib="
                            (assoc-ref %build-inputs "htslib")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               (("/bin/bash") (which "bash")))
             #t)))))
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("htslib" ,htslib-latest)
       ("gsl" ,gsl)
       ("zlib" ,zlib)))))

;; This version is not a master release and it is a separate branch.
;; It need to be removed from here when an official release will be  announced.
(define-public r-archr
  (let ((commit "3075f1f034d8c5a9d7dcdfd6b6990814733fbf87")
        (revision "2"))
    (package
      (name "r-archr")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/GreenleafLab/ArchR")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zkjr60hi0p6abszp4g2zn80b1xp52a20vkdd8dcchbpyjm515ar"))))
      (properties `((upstream-name . "ArchR")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-biocgenerics
             r-biostrings
             r-chromvar
             r-complexheatmap
             r-data-table
             r-genomicranges
             r-ggplot2
             r-ggrepel
             r-gridextra
             r-gtable
             r-gtools
             r-magrittr
             r-matrix
             r-matrixstats
             r-motifmatchr
             r-nabor
             r-plyr
             r-rcpp
             r-rhdf5
             r-rsamtools
             r-s4vectors
             r-stringr
             r-summarizedexperiment
             r-uwot))
      (home-page "https://github.com/GreenleafLab/ArchR")
      (synopsis "Analyze single-cell regulatory chromatin in R")
      (description
       "This package is designed to streamline scATAC analyses in R.")
      (license license:gpl2+))))

(define-public r-devel
  (package (inherit r-minimal)
    (name "r-devel")
    (version "2017-04-27_r72634")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base-prerelease/"
                                  "R-devel_" version ".tar.gz"))
              (sha256
               (base32
                "02x5dqps8v31djr38s4akq8q3hk6xf8x5knk5454awyvjipkry2j"))))))

(define-public r-methylkit-devel
  (let ((commit "ab60a62e0bd610718fcbb76e9505957c8c3ec15f")
        (revision "1"))
    (package (inherit r-methylkit)
      (name "r-methylkit-devel")
      (version (git-version "1.17.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/al2na/methylKit.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1a8rrjvm9nrzbbbkawixhq4vdy3q72b43h0xb28q43p0fl2j0jz9")))))))

(define-public star-2.5.2
  (package (inherit star)
    (name "star")
    (version "2.5.2b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexdobin/STAR.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0ipxr7mnc5ckanvis74ypfybd2ai0k12y6nai0mmmk4igfxd2zry"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "source/Makefile"
                    (("/bin/rm") "rm"))
                  ;; Remove pre-built binaries and bundled htslib sources.
                  (delete-file-recursively "bin/MacOSX_x86_64")
                  (delete-file-recursively "bin/Linux_x86_64")
                  (delete-file-recursively "bin/Linux_x86_64_static")
                  (delete-file-recursively "source/htslib")
                  #t))))
    (arguments
     '(#:tests? #f                      ;no check target
       #:make-flags '("STAR")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-dir
           (lambda _ (chdir "source") #t))
         (add-after 'enter-source-dir 'make-reproducible
           (lambda _
             (substitute* "Makefile"
               (("(COMPILATION_TIME_PLACE=\")(.*)(\")" _ pre mid post)
                (string-append pre "Built with Guix" post)))
             #t))
         (add-after 'enter-source-dir 'do-not-use-bundled-htslib
           (lambda _
             (substitute* "Makefile"
               (("(Depend.list: \\$\\(SOURCES\\) parametersDefault\\.xxd) htslib"
                 _ prefix) prefix))
             (substitute* '("BAMfunctions.cpp"
                            "signalFromBAM.h"
                            "bam_cat.h"
                            "bam_cat.c"
                            "STAR.cpp"
                            "bamRemoveDuplicates.cpp")
               (("#include \"htslib/([^\"]+\\.h)\"" _ header)
                (string-append "#include <" header ">")))
             (substitute* "IncludeDefine.h"
               (("\"htslib/(htslib/[^\"]+.h)\"" _ header)
                (string-append "<" header ">")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "STAR" bin))
             #t))
         (delete 'configure))))))

(define-public cmake-3.7.2
  (package (inherit cmake)
    (version "3.7.2")
    (source (origin
              (inherit (package-source cmake))
              (uri (string-append "https://www.cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "1q6a60695prpzzsmczm2xrgxdb61fyjznb04dr6yls6iwv24c4nw"))))))

(define-public r-seurat-next
  (let ((commit "3acf1655fa12f0ecd74b97c22f649c68eecab1b0")
        (revision "1"))
    (package (inherit r-seurat)
      (name "r-seurat-next")
      (version (git-version "2.9.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/satijalab/seurat.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ri4y776fpkh1w4ndbq03w3bqmv2jm6m325cgxrrjpi56g2cmi78"))))
      (propagated-inputs
       `(("r-future" ,r-future)
         ("r-future-apply" ,r-future-apply)
         ("r-ggrepel" ,r-ggrepel)
         ("r-rsvd" ,r-rsvd)
         ,@(package-propagated-inputs r-seurat))))))

(define-public openmpi-with-gcc8
  (package (inherit openmpi)
    (name "openmpi-with-gcc8")
    (native-inputs
     `(("gcc" ,gcc-8)
       ,@(package-native-inputs openmpi)))))

(define-public dune-grid-agfalcke
  (package (inherit dune-grid)
    (name "dune-grid-agfalcke")
    (version "2.6.0")
    (source (origin (inherit (package-source dune-grid))
                    (patches (search-patches "dune-grid-agfalcke.patch"))))))

(define-public dune-grid-agfalke
  (deprecated-package "dune-grid-agfalke" dune-grid-agfalcke))

(define-public python2-dill
  (package-with-python2 python-dill))

(define-public python2-multiprocess
  (package (inherit python-multiprocess)
    (name "python2-multiprocess")
    (build-system python-build-system)
    ;; FIXME: it's not clear how to run the tests.
    (arguments
     `(#:python ,python-2
       #:tests? #f))
    (propagated-inputs
     `(("python-dill" ,python2-dill)))))

(define-public python2-pox
  (package-with-python2 python-pox))

(define-public python2-ppft
  (package-with-python2 python-ppft))

(define-public python2-pathos
  (package (inherit python-pathos)
           (name "python2-pathos")
           (build-system python-build-system)
           (arguments
            `(#:python ,python-2
              #:phases
              (modify-phases %standard-phases
                (replace 'check
                  (lambda* (#:key tests? inputs outputs #:allow-other-keys)
                    (when tests?
                      (add-installed-pythonpath inputs outputs)
                      (invoke "python2" "./tests/__main__.py")))))))
           (propagated-inputs
            `(("python-dill" ,python2-dill)
              ("python-multiprocess" ,python2-multiprocess)
              ("python-pox" ,python2-pox)
              ("python-ppft" ,python2-ppft)))
           (native-inputs
            `(("python-pytest" ,python2-pytest)))))

(define-public python2-argh
  (package
    (inherit (package-with-python2 python-argh))
    (propagated-inputs
     `(("python-iocapture" ,(package-with-python2 python-iocapture))
       ("python-mock" ,python2-mock)
       ("python-pytest" ,python2-pytest)
       ("python-pytest-cov" ,python2-pytest-cov)))))

(define-public python2-pyfaidx
  (package-with-python2 python-pyfaidx))

(define-public python2-gffutils
  (let ((gffutils
         (package-with-python2
          (strip-python2-variant python-gffutils))))
    (package
      (inherit gffutils)
      (propagated-inputs
       `(("python2-argcomplete" ,python2-argcomplete)
         ("python2-argh" ,python2-argh)
         ("python2-functools32" ,python2-functools32)
         ("python2-biopython" ,python2-biopython)
         ("python2-pybedtools" ,python2-pybedtools)
         ("python2-pyfaidx" ,python2-pyfaidx)
         ("python2-simplejson" ,python2-simplejson)
         ("python2-six" ,python2-six))))))

(define-public jamm-prerelease
  (package (inherit jamm)
    (name "jamm-prerelease")
    (version "1.0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mahmoudibrahim/JAMM.git")
             (commit (string-append "JAMMv" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pn3nkxfpq6h04p63rj06gzhp36q2g3j4xl5fh941d94kkynqqpc"))))))
