;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages base) #:hide (which))
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (bimsb packages variants)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public rstudio-server
  (package
    (name "rstudio-server")
    (version "1.1.220")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rstudio/rstudio/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "00aqkz392l754n9h6z23f4yd5z7s5bfj57c0v0c5zf0y828fiawq"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DRSTUDIO_TARGET=Server")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-java-home
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
            #t))
         (add-after 'unpack 'fix-dependencies
           (lambda _
             ;; Disable checks for bundled dependencies.  We take care of them by other means.
             (substitute* "src/cpp/session/CMakeLists.txt"
               (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rmarkdown\"\\)") "if (FALSE)")
               (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rsconnect\"\\)") "if (FALSE)"))
             #t))
         (add-after 'unpack 'copy-clang
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "dependencies/common"
               (let ((clang   (assoc-ref inputs "clang"))
                     (dir     "libclang")
                     (lib     "libclang/3.5")
                     (headers "libclang/builtin-headers"))
                 (mkdir-p dir)
                 (mkdir-p lib)
                 (mkdir-p headers)
                 (for-each (lambda (file)
                             (install-file file lib))
                           (find-files (string-append clang "/lib") ".*"))
                 (install-file (string-append clang "/include") dir)
                 #t))))
         (add-after 'unpack 'unpack-dictionaries
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "dependencies/common"
               (mkdir "dictionaries")
               (mkdir "pandoc") ; TODO: only to appease the cmake stuff
               (zero? (system* "unzip" "-qd" "dictionaries"
                               (assoc-ref inputs "dictionaries"))))))
         (add-after 'unpack 'unpack-mathjax
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "dependencies/common"
               (mkdir "mathjax-26")
               (zero? (system* "unzip" "-qd" "mathjax-26"
                               (assoc-ref inputs "mathjax"))))))
         (add-after 'unpack 'unpack-gin
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/gwt"
               (install-file (assoc-ref inputs "junit") "lib")
               (mkdir-p "lib/gin/1.5")
               (zero? (system* "unzip" "-qd" "lib/gin/1.5"
                               (assoc-ref inputs "gin"))))))
         (add-after 'unpack 'unpack-gwt
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/gwt"
               (mkdir-p "lib/gwt")
	       (system* "unzip" "-qd" "lib/gwt"
			(assoc-ref inputs "gwt"))
               (rename-file "lib/gwt/gwt-2.7.0" "lib/gwt/2.7.0"))
	     #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("unzip" ,unzip)
       ("ant" ,ant)
       ("jdk" ,icedtea "jdk")
       ("gin"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-buildtools/gin-1.5.zip")
           (sha256
            (base32 "155bjrgkf046b8ln6a55x06ryvm8agnnl7l8bkwwzqazbpmz8qgm"))))
       ("gwt"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-buildtools/gwt-2.7.0.zip")
           (sha256
            (base32 "1cs78z9a1jg698j2n35wsy07cy4fxcia9gi00x0r0qc3fcdhcrda"))))
       ("junit"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-buildtools/junit-4.9b3.jar")
           (sha256
            (base32 "0l850yfbq0cgycp8n0r0a1b7xznd37pgfd656vzdwim4blznqmnw"))))
       ("mathjax"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-buildtools/mathjax-26.zip")
           (sha256
            (base32 "0wbcqb9rbfqqvvhqr1pbqax75wp8ydqdyhp91fbqfqp26xzjv6lk"))))
       ("dictionaries"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-dictionaries/core-dictionaries.zip")
           (sha256
            (base32 "153lg3ai97qzbqp6zjg10dh3sfvz80v42cjw45zwz7gv1risjha3"))))))
    (inputs
     `(("r" ,r)
       ("r-rmarkdown" ,r-rmarkdown) ; TODO: must be linked to another location
       ;;("r-rsconnect" ,r-rsconnect) ; TODO: must be linked to another location
       ("clang" ,clang-3.5)
       ("boost" ,boost)
       ("libuuid" ,util-linux)
       ("pandoc" ,ghc-pandoc)
       ("openssl" ,openssl)
       ("pam" ,linux-pam)
       ("zlib" ,zlib)))
    (home-page "http://www.rstudio.org/")
    (synopsis "Integrated development environment (IDE) for R")
    (description
     "RStudio is an integrated development environment (IDE) for the R
programming language. Some of its features include: Customizable workbench
with all of the tools required to work with R in one place (console, source,
plots, workspace, help, history, etc.); syntax highlighting editor with code
completion; execute code directly from the source editor (line, selection, or
file); full support for authoring Sweave and TeX documents.  RStudio can also
be run as a server, enabling multiple users to access the RStudio IDE using a
web browser.")
    (license license:agpl3+)))

(define-public rstudio-server-bimsb
  (let ((commit "v1.1.272-bimsb")
        (revision "1"))
    (package
      (inherit rstudio-server)
      (name "rstudio-server-bimsb")
      (version "1.1.272-bimsb")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BIMSBbioinfo/rstudio.git")
               (commit commit)))
         (sha256
          (base32
           "0x22bk2kkj5w9qarhvjklz62h79fpfg0ll2bsy8by588vbh9svzd"))))
      (description
       "This is the BIMSB fork of RStudio Server, a web IDE for the R
programming language.  The fork adds a single feature: it allows users
to switch R versions by placing a script in
@code{~/.rstudio/rsession-wrapper} which preloads a different version
of the @code{libR} shared library."))))

(define-public rstudio
  (package (inherit rstudio-server)
    (name "rstudio")
    (arguments
     (substitute-keyword-arguments (package-arguments rstudio-server)
       ((#:configure-flags flags)
        '(list "-DRSTUDIO_TARGET=Desktop"
               (string-append "-DQT_QMAKE_EXECUTABLE="
                              (assoc-ref %build-inputs "qtbase")
                              "/bin/qmake")))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'relax-qt-version
             (lambda _
               (substitute* "src/cpp/desktop/CMakeLists.txt"
                 (("5\\.4") "5.7"))
               #t))))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtsvg" ,qtsvg)
       ("qtsensors" ,qtsensors)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("qtwebkit" ,qtwebkit)
       ,@(package-inputs rstudio-server)))
    (synopsis "Integrated development environment (IDE) for R (desktop version)")))

;; This version of WebLogo is required by MEDICC.
(define-public weblogo-3.3
  (package
    (name "weblogo")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/WebLogo/weblogo/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0sxrrr0ybp22zdy0ii5qa89pryxryiavgbfxc6zi5hr7wr31wwv8"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; there is no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-svn-variables
           (lambda _
             (substitute* "corebio/_version.py"
               (("date =.*") "date = \"Guix\"\n")
               (("revision =.*") ""))
             (substitute* "weblogolib/__init__.py"
               (("release_build = .*") "")
               (("release_date =.*") "release_date = \"Guix\"\n"))
             #t)))))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)))
    (inputs
     `(;; TODO: ("pdf2svg" ,pdf2svg)
       ("ghostscript" ,ghostscript)))
    (home-page "http://weblogo.threeplusone.com/")
    (synopsis "Generate nucleotide sequence logos")
    (description
     "WebLogo is an application designed to make the generation of
nucleotide sequence logos easy.  It can create output in several
common graphics formats, including the bitmap formats GIF and PNG,
suitable for on-screen display, and the vector formats EPS and PDF,
more suitable for printing, publication, and further editing.
Additional graphics options include bitmap resolution, titles,
optional axis, and axis labels, antialiasing, error bars, and
alternative symbol formats.

A sequence logo is a graphical representation of an amino acid or
nucleic acid multiple sequence alignment.  Each logo consists of
stacks of symbols, one stack for each position in the sequence.  The
overall height of the stack indicates the sequence conservation at
that position, while the height of symbols within the stack indicates
the relative frequency of each amino or nucleic acid at that position.
The width of the stack is proportional to the fraction of valid
symbols in that position.")
    (license license:bsd-3)))

;; This exact version of OpenFST is required by MEDICC
;; Move this to machine-learning.scm
(define-public openfst
  (package
    (name "openfst")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.openfst.org/twiki/pub/"
                           "FST/FstDownload/openfst-"
                           version ".tar.gz"))
       (sha256
        (base32
         "00j647hsgldpn3dagpqlfalj48f739hh2krzy5fg3bjldi7jw72f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-pdt"
             "--enable-far")))
    (home-page "http://www.openfst.org/")
    (synopsis "Library for handling finite-state transducers (FST)")
    (description
     "OpenFst is a library for constructing, combining, optimizing,
and searching weighted finite-state transducers (FSTs).  Weighted
finite-state transducers are automata where each transition has an
input label, an output label, and a weight.  The more familiar
finite-state acceptor is represented as a transducer with each
transition's input and output label equal.  Finite-state acceptors are
used to represent sets of strings (specifically, regular or rational
sets); finite-state transducers are used to represent binary relations
between pairs of strings (specifically, rational transductions).  The
weights can be used to represent the cost of taking a particular
transition.")
    (license license:asl2.0)))

(define-public python-cmd2
  (package
    (name "python-cmd2")
    (version "0.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cmd2" version))
       (sha256
        (base32
         "0a5k7d5lxd8vmcgzkfpkmcpvyi68lkgg90bdvd237hfvj5f782gg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-pyparsing" ,python-pyparsing)))
    (home-page "http://packages.python.org/cmd2/")
    (synopsis "Extra features for Python's cmd module")
    (description
     "This package provides extra features for the Python standard
library's @code{cmd} module.")
    (license license:expat)))

(define-public python2-cmd2
  (package-with-python2 python-cmd2))

(define-public python-sortedcontainers
  (package
    (name "python-sortedcontainers")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sortedcontainers" version))
       (sha256
        (base32
         "1fyg7dwhyka53gzizxbq9w4bib9irkk40n4njpgpywxm70qdhdgp"))))
    (build-system python-build-system)
    ;; Tests require python-tox, but running the tests fails because
    ;; there is no configuration file for tox.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://www.grantjenks.com/docs/sortedcontainers/")
    (synopsis "Sorted container types for Python")
    (description
     "This package provides sorted container types for Python,
including @code{SortedList}, @{SortedDict}, and @code{SortedSet}")
    (license license:asl2.0)))

(define-public python2-sortedcontainers
  (package-with-python2 python-sortedcontainers))

(define-public python-intervaltree
  (package
    (name "python-intervaltree")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "intervaltree" version))
       (sha256
        (base32
         "02w191m9zxkcjqr1kv2slxvhymwhj3jnsyy3a28b837pi15q19dc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; TODO: they fail, probably because the own lib dir is not in the PYTHONPATH?
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check)))))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-sortedcontainers" ,python-sortedcontainers)))
    (home-page "https://github.com/chaimleib/intervaltree")
    (synopsis "Editable interval tree data structure for Python")
    (description
     "This package provides a module for editable interval tree data
structures.")
    (license license:asl2.0)))

(define-public python2-intervaltree
  (package-with-python2 python-intervaltree))

(define-public python-progressbar
  (package
    (name "python-progressbar")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "progressbar" version))
       (sha256
        (base32
         "0m0j93yfvbd8pw8cz2vdb9hyk9d0zkkd509k69jrw545jxr8mlxj"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://code.google.com/p/python-progressbar")
    (synopsis "Text progress bar library for Python")
    (description "The @code{ProgressBar} class manages the current
progress of a long running operation, and the format of a line
providing a visual cue that processing is underway.")
    ;; Either license may be used.
    (license (list license:lgpl2.1+ license:bsd-3))))

(define-public python2-progressbar
  (package-with-python2 python-progressbar))

(define-public python2-parcel
  (package
    (name "python2-parcel")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/LabAdvComp/parcel/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append "python2-parcel-" version ".tar.gz"))
       (sha256
        (base32
         "1k9wy7dx7frxz9cliahxri74nfc19lh97iqlwsgbq469bd8iai73"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ;; TODO: make this robust!
               (("0.6.8") ,(package-version python2-cmd2))
               (("2.0.4") ,(package-version python2-intervaltree))
               (("2.5.1") ,(package-version python2-requests)))
             #t))
         (add-before 'build 'set-HOME
           (lambda _
             ;; This is needed to build the parcel library, which is
             ;; placed in $HOME/.parcel/lib.
             (setenv "HOME" "/tmp")
             #t)))))
    (propagated-inputs
     `(("python2-cmd2" ,python2-cmd2)
       ("python2-intervaltree" ,python2-intervaltree)
       ("python2-flask" ,python2-flask)
       ("python2-progressbar" ,python2-progressbar)
       ("python2-requests" ,python2-requests)
       ("python2-termcolor" ,python2-termcolor)))
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (home-page "https://github.com/LabAdvComp/parcel")
    (synopsis "HTTP download client with the speed of UDP")
    (description "Parcel is a high performance HTTP download client
that leverages the speed of UDP without sacrificing reliability.
Parcel is written on top of the UDT protocol and bound to a Python
interface.  Parcel's software is comprised of a @code{parcel-server}
and a @{parcel} client.")
    (license license:asl2.0)))

(define python2-parcel-for-gdc-client
  (let ((commit "fddae5c09283ee5058fb9f43727a97a253de31fb")
        (revision "1"))
    (package
      (inherit python2-parcel)
      (version (string-append "0.1.13-" revision "."
                              (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/LabAdvComp/parcel.git")
               (commit commit)))
         (file-name (string-append (package-name python2-parcel) "-" version))
         (sha256
          (base32
           "0z1jnbdcyn571b1md51z13ivyl7l5mypwdkbjxk0klk70dskrs7i")))))))

(define-public gdc-client
  (package
    (name "gdc-client")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/NCI-GDC/gdc-client/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append "gdc-client-" version ".tar.gz"))
       (sha256
        (base32
         "0i24zlj764r16rn6jw82l9cffndm2ixw7d91s780vk5ihrmmwd3h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ;; TODO: make this robust!
               (("19.2") ,(package-version python2-setuptools))
               (("3.5.0b1") ,(package-version python2-lxml)))
             #t)))))
    (inputs
     `(("python2-parcel" ,python2-parcel-for-gdc-client)
       ("python2-jsonschema" ,python2-jsonschema)
       ("python2-pyyaml" ,python2-pyyaml)
       ("python2-lxml" ,python2-lxml)
       ("python2-functools32" ,python2-functools32)
       ("python2-setuptools" ,python2-setuptools)))
    (home-page "TODO")
    (synopsis "TODO")
    (description "TODO")
    (license license:asl2.0)))

(define-public umi-tools
  (package
  (name "umi-tools")
  (version "0.2.3")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "umi_tools" version))
      (sha256
       (base32
        "0ms8jfql7ypivrs9wwn3nn33cfkav706min1k3fjn3n6zhx58dab"))))
  (build-system python-build-system)
  (inputs
   `(("python-pandas" ,python-pandas)
     ("python-numpy" ,python-numpy)
     ("python-future" ,python-future)
     ("python-pysam" ,python-pysam)))
  (native-inputs
   `(("python-setuptools" ,python-setuptools)
     ("python-cython" ,python-cython)))
  (home-page "https://github.com/CGATOxford/UMI-tools")
  (synopsis "Tools for analyzing unique modular identifiers")
  (description "This package provides tools for dealing with
@dfn{Unique Molecular Identifiers} (UMIs) and @dfn{Random Molecular
Tags} (RMTs) in genetic sequences.  Currently, there are two tools:

@enumerate
@item extract: flexible removal of UMI sequences from fastq reads;
@item dedup: implementation of various UMI deduplication methods.
@end enumerate\n")
  (license license:expat)))

(define-public metal
  (package
    (name "metal")
    (version "2011-03-25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://csg.sph.umich.edu/abecasis/Metal/"
                           "download/generic-metal-" version ".tar.gz"))
       (sha256
        (base32
         "1bk00hc0xagmq0mabmbb8bykl75qd4kfyirba869h4x6hmn4a0f3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "INSTALLDIR="
                            (assoc-ref %outputs "out") "/bin")
             "all")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://csg.sph.umich.edu/abecasis/Metal")
    (synopsis "Facilitate meta-analysis of large datasets")
    (description "METAL is a tool for meta-analysis genomewide
association scans.  METAL can combine either test statistics and
standard errors or p-values across studies (taking sample size and
direction of effect into account).  METAL analysis is a convenient
alternative to a direct analysis of merged data from multiple studies.
It is especially appropriate when data from the individual studies
cannot be analyzed together because of differences in ethnicity,
phenotype distribution, gender or constraints in sharing of individual
level data imposed.  Meta-analysis results in little or no loss of
efficiency compared to analysis of a combined dataset including data
from all individual studies.")
    (license license:bsd-3)))

(define-public transat
  (package
    (name "transat")
    (version "2.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       ;; FIXME: without this field "url-fetch/tarbomb" fails
       (file-name (string-append name "-" version ".tgz"))
       (uri (string-append "http://www.e-rna.org/transat/download/"
                           "transat_v" version ".tgz"))
       (sha256
        (base32
         "0z754slkl6ijz3g8d7wjyqwaq1gb4cx3csfx4gyrvlg14wagjpm6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-dir
           (lambda _ (chdir "src") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "Transat" bin)
               #t))))))
    (home-page "http://www.e-rna.org/transat/")
    (synopsis "Detect conserved helices of functional RNA structures")
    (description "Transat detects conserved helices of high
statistical significance in functional RNA, including pseudo-knotted,
transient and alternative structures.  Given a multiple sequence
alignment, Transat will recover all possible helices and determine the
statistical probability of the helix existing based on its
phylogeny-based evolutionary likelihood.")
    (license license:gpl3+)))

(define-public rnadecoder
  (package
    (name "rnadecoder")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       ;; FIXME: there are no versioned tarballs
       (uri (string-append "http://www.e-rna.org/rnadecoder/download/"
                           "rnadecoder.tar.bz2"))
       (sha256
        (base32
         "1ml1bbil0gm2w9knx85kvkjwm7598313a5jy9fmavbsmwx7p3v0j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'patch-source-shebangs)
         (delete 'configure)
         (delete 'build)
         (delete 'patch-shebangs)
         (add-after 'unpack 'change-dir
           (lambda _ (chdir "bin") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let*  ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "RNA-decoder" bin)
               #t))))))
    (home-page "http://www.e-rna.org/rnadecoder/")
    (synopsis "Find and fold RNA secondary structures in protein-coding regions")
    (description "RNA-Decoder is a comparative method to find and fold
RNA secondary structures in protein-coding regions.  It explicitly
takes the known protein-coding context of an RNA-sequence alignment
into account in order to predict evolutionarily conserved
secondary-structure elements, which may span both coding and
non-coding regions.")
    (license license:gpl3+)))

(define-public nucleoatac
  (package
  (name "nucleoatac")
  (version "0.3.1")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://github.com/GreenleafLab/NucleoATAC/"
                          "archive/v" version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "1r2r4f5c9mhl722gkyxq0yb1pmk7jqgvjqn8bwlcr7jbbys51afh"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2
     #:phases
     (modify-phases %standard-phases
       (add-before 'check 'set-HOME
         ;; The tests need a valid HOME directory
         (lambda _ (setenv "HOME" (getcwd)) #t)))))
  (inputs
   `(("python-pandas" ,python2-pandas)
     ("python-numpy" ,python2-numpy)
     ("python-scipy" ,python2-scipy)
     ("python-matplotlib" ,python2-matplotlib)
     ("python-pysam" ,python2-pysam)))
  (native-inputs
   `(("python-setuptools" ,python2-setuptools)
     ("python-pytz" ,python2-pytz)
     ("python-mock" ,python2-mock)
     ("python-cython" ,python2-cython)))
  (home-page "https://github.com/GreenleafLab/NucleoATAC")
  (synopsis "Nucleosome calling using ATAC-seq data")
  (description "This package provides tools for calling nucleosomes
using ATAC-Seq data.  It also includes general scripts for working
with paired-end ATAC-Seq data (or potentially other paired-end
data).")
  (license license:expat)))

(define-public lammps
  (package
    (name "lammps")
    (version "r15407")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lammps/lammps/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1z2h7ja6h8m5q2vd8six4m6iv1nk2i2vrhfpdv20b53cjxpinfsz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags (list "CC=mpicc" "mpi"
                          "LMP_INC=-DLAMMPS_GZIP \
-DLAMMPS_JPEG -DLAMMPS_PNG -DLAMMPS_FFMPEG -DLAMMPS_MEMALIGN=64"
                          "LIB=-gz -ljpeg -lpng -lavcodec")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "MAKE/Makefile.mpi"
               (("SHELL =.*")
                (string-append "SHELL=" (which "bash") "\n"))
               (("cc ") "mpicc "))
             (substitute* "Makefile"
               (("SHELL =.*")
                (string-append "SHELL=" (which "bash") "\n")))
             #t))
         (add-after 'configure 'configure-modules
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "../lib/h5md"
               (system* "make" (string-append "HDF5_PATH="
                                              (assoc-ref inputs "hdf5"))))
             (zero? (system* "make"
                             "yes-molecule"
                             "yes-granular"
                             "yes-user-h5md"
                             "yes-user-misc"))))
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "src") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let*  ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "lmp_mpi" bin)
               #t))))))
    (inputs
     `(("python" ,python-2)
       ("gfortran" ,gfortran)
       ("openmpi" ,openmpi)
       ("ffmpeg" ,ffmpeg)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("hdf5" ,hdf5)
       ("gzip" ,gzip)))
    (native-inputs
     `(("bc" ,bc)
       ("gcc" ,gcc-4.9)))
    (home-page "http://lammps.sandia.gov/")
    (synopsis "Classical molecular dynamics simulator")
    (description "LAMMPS is a classical molecular dynamics simulator
designed to run efficiently on parallel computers.  LAMMPS has
potentials for solid-state materials (metals, semiconductors), soft
matter (biomolecules, polymers), and coarse-grained or mesoscopic
systems.  It can be used to model atoms or, more generically, as a
parallel particle simulator at the atomic, meso, or continuum scale.")
    (license license:gpl2+)))

(define-public lammps-serial
  (package (inherit lammps)
    (name "lammps-serial")
    (arguments
     (substitute-keyword-arguments (package-arguments lammps)
       ((#:make-flags flags)
        `(list "CC=gcc" "serial"
               "LMP_INC=-DLAMMPS_GZIP \
-DLAMMPS_JPEG -DLAMMPS_PNG -DLAMMPS_FFMPEG -DLAMMPS_MEMALIGN=64"
               "LIB=-gz -ljpeg -lpng -lavcodec"))
       ((#:phases phases)
        `(modify-phases  ,phases
           (replace 'configure
             (lambda _
               (substitute* "MAKE/Makefile.serial"
                 (("SHELL =.*")
                  (string-append "SHELL=" (which "bash") "\n"))
                 (("cc ") "gcc "))
               (substitute* "Makefile"
                 (("SHELL =.*")
                  (string-append "SHELL=" (which "bash") "\n")))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let*  ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (install-file "lmp_serial" bin)
                 #t)))))))
    (inputs
     (alist-delete "openmpi" (package-inputs lammps)))))

(define-public rapidstorm
  (package
    (name "rapidstorm")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://idefix.biozentrum.uni-wuerzburg.de/"
                           "software/rapidSTORM/source/rapidstorm-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1kp0z7xllx3krdph5ch23grh25133sj7vhf18shpxnia3fyh6y0a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "XSLT_FLAGS=--nonet --novalid"
                          "BIB2XML=touch")
       #:configure-flags
       (list "--enable-documentation=no")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-guilabels
           (lambda _
             (with-directory-excursion "doc"
               (zero? (system "xsltproc --nonet --novalid --path .: --xinclude -o guilabels.h rapidstorm_guilabels.xsl ./rapidstorm.xml")))))
         (add-after 'unpack 'fix-doc
           (lambda _
             ;; TODO: something's wrong with entities.  Not sure what,
             ;; so I'll just remove them for now.
             (substitute* '("doc/rapidstorm.xml"
                            "doc/ui.xml"
                            "doc/usage-examples.xml"
                            "doc/input-options.xml"
                            "doc/engine_options.xml"
                            "doc/output-options.xml"
                            "doc/fundamentals.xml")
               (("%version;") "")
               (("&marketingversion;") "TODO")
               (("&publicationyear;") "2016")
               (("&publicationdate;") "2016")
               (("&auml;") "a")
               (("&uuml;") "u")
               (("&eacute;") "e")
               (("%isopub;") "")
               (("%isonum;") "")
               (("%isogrk1;") "")
               (("%isolat1;") "")
               (("&lgr;") "l")
               (("&mgr;") "m")
               (("&Dgr;") "D")
               (("&sgr;") "s")
               (("&ohgr;") "oh")
               (("&hellip;") "...")
               (("&middot;") ""))
             #t)))))
    (inputs
     `(("boost" ,boost-1.55)
       ;; FIXME: the build fails when protobuf is used:
       ;; tsf/Output.cpp: In member function ?virtual dStorm::output::Output::RunRequirements dStorm::tsf::{anonymous}::Output::announce_run(const dStorm::output::Output::RunAnnouncement&)?:
       ;; tsf/Output.cpp:85:56: error: variable ?google::protobuf::io::CodedOutputStream coded_output? has initializer but incomplete type
       ;;   google::protobuf::io::CodedOutputStream coded_output(file.get());
       ;;("protobuf" ,protobuf)
       ("eigen" ,eigen)
       ("gsl" ,gsl)
       ("tinyxml" ,tinyxml)
       ("libtiff" ,libtiff)
       ("wxwidgets" ,wxwidgets-2)
       ("graphicsmagick" ,graphicsmagick)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ;; For documentation
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ;;("doxygen" ,doxygen)
       ("libxslt" ,libxslt)
       ("texlive" ,texlive-minimal)
       ("zip" ,zip)))
    (home-page "http://www.super-resolution.biozentrum.uni-wuerzburg.de/research_topics/rapidstorm/")
    (synopsis "")
    (description "")
    ;; Documentation is under fdl1.3+, most of rapidstorm is released
    ;; under GPL; parts are released under LGPL.
    (license (list license:gpl3+
                   license:lgpl3+
                   license:fdl1.3+))))

(define-public circ-explorer
  (package
    (name "circ-explorer")
    (version "1.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/YangLab/CIRCexplorer/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0945cbg4w2m148f0invni8gbkxrsxap0hy2yhjfy1qw63sncn2ag"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-import-in-executable
           (lambda _
             (substitute* "circ/CIRCexplorer.py"
               (("from genomic_interval import Interval")
                "from circ.genomic_interval import Interval"))
             #t)))))
    (inputs
     `(("python-pysam" ,python-pysam)
       ("python-docopt" ,python-docopt)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/YangLab/CIRCexplorer")
    (synopsis "Tool to identify circular RNAs")
    (description "CIRCexplorer implements a combined strategy to
identify genomic junction reads from back spliced exons and intron
lariats.")
    (license license:expat)))

(define-public flash
  (package
    (name "flash")
    (version "1.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/flashpage/FLASH-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1b1ns9ghbcxy92xwa2a53ikqacvnyhvca0zfv0s7986xzvvscp38"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CC=gcc")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; No configure script
         (delete 'configure)
         ;; No install target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (install-file "flash" bin)
               #t))))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://ccb.jhu.edu/software/FLASH/")
    (synopsis "Merge paired-end nucleotide reads from NGS experiments")
    (description "FLASH (Fast Length Adjustment of SHort reads) is a
tool to merge paired-end reads from next-generation sequencing
experiments.  FLASH is designed to merge pairs of reads when the
original DNA fragments are shorter than twice the length of reads.
The resulting longer reads can significantly improve genome
assemblies.  They can also improve transcriptome assembly when FLASH
is used to merge RNA-seq data.")
    (license license:gpl3+)))

(define-public python-argparse
  (package
    (name "python-argparse")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "argparse" version))
       (sha256
        (base32
         "1r6nznp64j68ih1k537wms7h57nvppq0szmwsaf99n71bfjqkc32"))))
    (properties `((python2-variant . ,(delay python2-argparse))))
    (build-system python-build-system)
    (home-page "https://pypi.python.org/pypi/argparse/")
    (synopsis "Command-line parsing library")
    (description "The @code{argparse} module makes it easy to write
user friendly command line interfaces.  The program defines what
arguments it requires, and @code{argparse} will figure out how to
parse those out of @code{sys.argv}.  The @code{argparse} module also
automatically generates help and usage messages and issues errors when
users give the program invalid arguments.")
    (license (package-license python))))

;; This is really needed for crispresso.  If the argparse module isn't
;; available at runtime the tool will fail, even though it builds fine
;; without it.
(define-public python2-argparse
  (package (inherit (package-with-python2
                     (strip-python2-variant python-argparse)))
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))))

(define-public crispresso
  (let ((commit "9c53ef0af863833013b88592d0c7118c5d5d5c33")
        (revision "1"))
    (package
      (name "crispresso")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lucapinello/CRISPResso.git")
               (commit commit)))
         (sha256
          (base32
           "0lbsrwkmnwcix3yvy234js4gkfv236g0kdsnm02q3v0n865hf1j7"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'do-not-install-dependencies
             (lambda _
               (substitute* "setup.py"
                 (("=='install'") "=='skip_this'"))
               #t))
           (add-after 'unpack 'use-full-paths-to-tools
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "CRISPResso/CRISPRessoCORE.py"
                 (("'flash %s")
                  (string-append "'" (which "flash") " %s"))
                 ((" needle -")
                  (string-append " " (which "needle") " -"))
                 (("'java -")
                  (string-append "'" (which "java") " -"))
                 (("^check_program\\(" line)
                  (string-append "#" line)))
               #t)))))
      ;; FIXME: this package includes the trimmomatic binary (jar)
      (inputs
       `(("emboss" ,emboss)
         ("flash" ,flash)
         ("jre" ,icedtea)
         ("python2-numpy" ,python2-numpy)
         ("python2-pandas" ,python2-pandas)
         ("python2-matplotlib" ,python2-matplotlib)
         ("python2-biopython" ,python2-biopython)
         ("python2-argparse" ,python2-argparse)))
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)
         ("python2-mock" ,python2-mock)
         ("python2-pytz" ,python2-pytz)))
      (home-page "https://github.com/lucapinello/CRISPResso")
      (synopsis "Analysis tool for CRISPR-Cas9 genome editing outcomes")
      (description "CRISPResso is a software pipeline for the analysis
of targeted CRISPR-Cas9 sequencing data.  This algorithm allows for
the quantification of both @dfn{non-homologous end joining} (NHEJ) and
@dfn{homologous directed repair} (HDR) occurrences.

CRISPResso automatizes and performs the following steps:

@itemize
@item filters low quality reads,
@item trims adapters,
@item aligns the reads to a reference amplicon,
@item quantifies the proportion of HDR and NHEJ outcomes,
@item quantifies frameshift/inframe mutations (if applicable) and
 identifies affected splice sites,
@item produces a graphical report to visualize and quantify the indels
 distribution and position.
@end itemize\n")
      (license license:agpl3+))))

(define-public isolator
  (let ((commit "24bafc0a102dce213bfc2b5b9744136ceadaba03")
        (revision "1"))
    (package
      (name "isolator")
      (version (string-append "0.0.2-"
                              revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dcjones/isolator.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "12mbcfqhiggcjvzizf2ff7b05z31i47njcyzcivpw5j74pfbr3dv"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f)) ; no check target
      (inputs
       `(("boost" ,boost)
         ("hdf5" ,hdf5)
         ("zlib" ,zlib)))
      (home-page "https://github.com/dcjones/isolator")
      (synopsis "Tools for the analysis of RNA-Seq experiments")
      (description "Isolator analyzes RNA-Seq experiments.  Isolator
has a particular focus on producing stable, consistent estimates.  It
implements a full hierarchical Bayesian model of an entire RNA-Seq
experiment.  It saves all the samples generated by the sampler, which
can be processed to compute posterior probabilities for arbitrarily
complex questions, far beyond the confines of pairwise tests.  It
aggressively corrects for technical effects, such as random priming
bias, GC-bias, 3' bias, and fragmentation effects.  Compared to other
MCMC approaches, it is exceedingly efficient, though generally slower
that modern maximum likelihood approaches.")
      (license license:expat))))

(define-public iclipro
  (package
    (name "iclipro")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.biolab.si/iCLIPro/dist/"
                           "iCLIPro-" version ".tar.gz"))
       (sha256
        (base32
         "05sql0150rq1w0d6pn5jcq0ag0rrlsg1wfgc9b5la6zvsp43xwxz"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python-matplotlib" ,python2-matplotlib)
       ("python-pysam" ,python2-pysam)))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)))
    (home-page "http://www.biolab.si/iCLIPro")
    (synopsis "Control for systematic misassignments in iCLIP data")
    (description "A typical (i)CLIP experiment may result in the
detection of RNA fragments of different lengths.  Under the
assumptions of conventional iCLIP, the start sites of iCLIP fragments
should coincide at the cross-linking position in a fragment
length-independent fashion.  This interpretation may not hold for some
iCLIP libraries (e.g., substantial read-through, binding to long RNA
stretches etc).  In summary, we identified a previously unrecognized
effect of iCLIP fragment length on the position of fragment start
sites and thus assigned binding sites for some RBPs.

iCLIPro is a robust analysis approach that examines this effect and
thus can improve the assignment of binding sites from iCLIP
data. iCLIPro’s main function is to visualize coinciding and
non-coinciding fragment start sites in order to examine the best way
how to analyze iCLIP data.")
    (license license:gpl3+)))

(define-public python-fastcluster
  (package
    (inherit r-fastcluster)
    (name "python-fastcluster")
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "src/python") #t)))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))))

(define-public python2-fastcluster
  (package-with-python2 python-fastcluster))

(define-public r-bsgenome-dmelanogaster-ucsc-dm3-masked
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm3-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Dmelanogaster.UCSC.dm3.masked_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1756csb09f1br9rj1l3f08qyh4hlymdbd0cfn8x3fq39dn45m5ap"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm3.masked")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-dmelanogaster-ucsc-dm3"
        ,r-bsgenome-dmelanogaster-ucsc-dm3)))
    (home-page "http://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm3.masked/")
    (synopsis "Full masked genome sequences for Fly")
    (description
     "This package provides full masked genome sequences for
Drosophila melanogaster (Fly) as provided by UCSC (dm3, April 2006)
and stored in Biostrings objects.  The sequences are the same as in
BSgenome.Dmelanogaster.UCSC.dm3, except that each of them has the 4
following masks on top: (1) the mask of assembly gaps (AGAPS
mask), (2) the mask of intra-contig ambiguities (AMB mask), (3) the
mask of repeats from RepeatMasker (RM mask), and (4) the mask of
repeats from Tandem Repeats Finder (TRF mask).  Only the AGAPS and AMB
masks are \"active\" by default.")
    (license license:artistic2.0)))

(define-public r-bsgenome-hsapiens-ucsc-hg19-masked
  (package
    (name "r-bsgenome-hsapiens-ucsc-hg19-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Hsapiens.UCSC.hg19.masked_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0452pyah0kv1vsrsjbrqw4k2rm8lc2vc771dzib45gnnfz86qxrr"))))
    (properties
     `((upstream-name . "BSgenome.Hsapiens.UCSC.hg19.masked")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-hsapiens-ucsc-hg19"
        ,r-bsgenome-hsapiens-ucsc-hg19)))
    (home-page "http://bioconductor.org/packages/BSgenome.Hsapiens.UCSC.hg19.masked/")
    (synopsis "Full masked genome sequences for Homo sapiens")
    (description
     "This package provides full genome sequences for Homo
sapiens (Human) as provided by UCSC (hg19, Feb. 2009) and stored in
Biostrings objects.  The sequences are the same as in
BSgenome.Hsapiens.UCSC.hg19, except that each of them has the 4
following masks on top: (1) the mask of assembly gaps (AGAPS
mask), (2) the mask of intra-contig ambiguities (AMB mask), (3) the
mask of repeats from RepeatMasker (RM mask), and (4) the mask of
repeats from Tandem Repeats Finder (TRF mask).  Only the AGAPS and AMB
masks are \"active\" by default.")
    (license license:artistic2.0)))

(define-public r-bsgenome-mmusculus-ucsc-mm9-masked
  (package
    (name "r-bsgenome-mmusculus-ucsc-mm9-masked")
    (version "1.3.99")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Mmusculus.UCSC.mm9.masked_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00bpbm3havqcxr4g63zhllsbpd9q6svgihks7qp7x73nm4gvq7fn"))))
    (properties
     `((upstream-name . "BSgenome.Mmusculus.UCSC.mm9.masked")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)
       ("r-bsgenome-mmusculus-ucsc-mm9"
        ,r-bsgenome-mmusculus-ucsc-mm9)))
    (home-page "http://bioconductor.org/packages/BSgenome.Mmusculus.UCSC.mm9.masked/")
    (synopsis "Full masked genome sequences for Mouse")
    (description
     "Full genome sequences for Mus musculus (Mouse) as provided by
UCSC (mm9, Jul. 2007) and stored in Biostrings objects.  The sequences
are the same as in BSgenome.Mmusculus.UCSC.mm9, except that each of
them has the 4 following masks on top: (1) the mask of assembly
gaps (AGAPS mask), (2) the mask of intra-contig ambiguities (AMB
mask), (3) the mask of repeats from RepeatMasker (RM mask), and (4)
the mask of repeats from Tandem Repeats Finder (TRF mask).  Only the
AGAPS and AMB masks are \"active\" by default."  )
    (license license:artistic2.0)))

(define-public r-genomicfiles
  (package
    (name "r-genomicfiles")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "07pd9vq3qigkh28mw33nicsy90ijsp01kcdgzfvnhzww5r32jfcd"))))
    (properties `((upstream-name . "GenomicFiles")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "http://bioconductor.org/packages/GenomicFiles")
    (synopsis "Distributed computing by file or by range")
    (description
     "This package provides infrastructure for parallel computations
distributed by file or by range.  User defined mapper and reducer
functions provide added flexibility for data combination and
manipulation.")
    (license license:artistic2.0)))

(define-public bbmap
  (package
    (name "bbmap")
    (version "36.92")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/bbmap/BBMap_"
                           version ".tar.gz"))
       (sha256
        (base32
         "0sa2mqpviwdhp114qyyrx1nk1ib4dp578msyjvzqcbi6s93wcjn4"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "." "\\.class$"))
           #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "dist"
       #:tests? #f ; there are no tests
       ;; Java 1.7 is supported according to the documentation, but it
       ;; won't build with icedtea-7.
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-ecj-to-classpath
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH" (assoc-ref inputs "ecj"))
             #t))
         (add-after 'unpack 'embed-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (find-files "." ".*\\.sh$")
                 (("^CP=.*$")
                  (string-append "CP=" out "/lib/BBTools.jar\n"))
                 (("^NATIVELIBDIR=.*$")
                  (string-append "NATIVELIBDIR=" out "/lib/\n"))
                 (("\\$DIR\"\"docs")
                  (string-append out "/share/doc/bbmap"))
                 (("CMD=\"java ")
                  (string-append "CMD=\"" (assoc-ref inputs "jre")
                                 "/bin/java "))))
             #t))
         (add-after 'build 'build-jni-extension
           (lambda _
             (with-directory-excursion "jni"
               (zero? (system* "make" "-f" "makefile.linux")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (doc (string-append out "/share/doc/bbmap")))
               (for-each mkdir-p
                         (list bin lib doc))
               (for-each (lambda (script)
                           (install-file script bin))
                         (find-files "." ".*\\.sh$"))
               (for-each (lambda (file)
                           (install-file file lib))
                         '("dist/lib/BBTools.jar"
                           "jni/libbbtoolsjni.so"))
               (copy-recursively "docs" doc)
               #t))))))
    (inputs
     `(("jre" ,icedtea-8)))
    (native-inputs
     `(("ecj"
        ,(origin
           (method url-fetch)
           (uri "http://central.maven.org/maven2/org/eclipse/\
jdt/core/compiler/ecj/4.6.1/ecj-4.6.1.jar")
           (sha256
            (base32
             "1q5dxv28izkg23wrfiyzazvd15z8ldhpnkplffg4dd51yisxmpcw"))))))
    (home-page "http://jgi.doe.gov/data-and-tools/bbtools/")
    (synopsis "Aligner and other tools for short genomic reads")
    (description
     "BBTools is a suite of fast, multithreaded bioinformatics tools
designed for analysis of DNA and RNA sequence data.  BBTools can
handle common sequencing file formats such as fastq, fasta, sam,
scarf, fasta+qual, compressed or raw, with autodetection of quality
encoding and interleaving.

The BBTools suite includes programs such as:
@itemize
@item bbduk: filters or trims reads for adapters and contaminants
  using k-mers;
@item bbmap: short-read aligner for DNA and RNA-seq data;
@item bbmerge: merges overlapping or nonoverlapping pairs into a
  single reads;
@item reformat: converts sequence files between different formats such
  as fastq and fasta.
@end itemize\n")
    ;; This package includes adapter sequences for Illumina machines.
    ;; I don't know if redistribution and use for any purpose is
    ;; actually permitted.
    (license license:bsd-3)))

;; FIXME: This package includes pre-built Java classes.
(define-public fastqc
  (package
    (name "fastqc")
    (version "0.11.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bioinformatics.babraham.ac.uk/"
                           "projects/fastqc/fastqc_v"
                           version "_source.zip"))
       (sha256
        (base32
         "18rrlkhcrxvvvlapch4dpj6xc6mpayzys8qfppybi8jrpgx5cc5f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:build-target "build"
       #:phases
       (modify-phases %standard-phases
         ;; There is no installation target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/fastqc/")))
               (for-each mkdir-p (list bin share))
               (copy-recursively "bin" share)
               (chmod (string-append share "/fastqc") #o555)
               (symlink (string-append share "/fastqc")
                        (string-append bin "/fastqc"))
               #t))))))
    (inputs
     `(("perl" ,perl)))  ; needed for the wrapper script
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/")
    (synopsis "Quality control tool for high throughput sequence data")
    (description
     "FastQC aims to provide a simple way to do some quality control
checks on raw sequence data coming from high throughput sequencing
pipelines.  It provides a modular set of analyses which you can use to
give a quick impression of whether your data has any problems of which
you should be aware before doing any further analysis.

The main functions of FastQC are:

@itemize
@item Import of data from BAM, SAM or FastQ files (any variant);
@item Providing a quick overview to tell you in which areas there may
  be problems;
@item Summary graphs and tables to quickly assess your data;
@item Export of results to an HTML based permanent report;
@item Offline operation to allow automated generation of reports
  without running the interactive application.
@end itemize\n")
    (license license:gpl3+)))

(define-public htslib-for-sambamba
  (let ((commit "2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5"))
    (package
      (inherit htslib)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lomereiter/htslib.git")
               (commit commit)))
         (file-name (string-append "htslib-"
                                   (string-take commit 9)
                                   "-checkout"))
         (sha256
          (base32
           "0g38g8s3npr0gjm9fahlbhiskyfws9l5i0x1ml3rakzj7az5l9c9"))))
      (arguments
       (substitute-keyword-arguments (package-arguments htslib)
         ((#:phases phases)
          `(modify-phases  ,phases
             (add-before 'configure 'bootstrap
               (lambda _
                 (zero? (system* "autoreconf" "-vif"))))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ,@(package-native-inputs htslib))))))

(define-public sambamba
  (package
    (name "sambamba")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lomereiter/sambamba/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17076gijd65a3f07zns2gvbgahiz5lriwsa6dq353ss3jl85d8vy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:make-flags
       '("D_COMPILER=ldc2"
         ;; Override "--compiler" flag only.
         "D_FLAGS=--compiler=ldc2 -IBioD -g -d"
         "sambamba-ldmd2-64")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'place-biod
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "biod") "BioD")
             #t))
         (add-after 'unpack 'unbundle-prerequisites
           (lambda _
             (substitute* "Makefile"
               ((" htslib-static lz4-static") ""))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "build/sambamba" bin)
               #t))))))
    (native-inputs
     `(("ldc" ,ldc)
       ("rdmd" ,rdmd)
       ("biod"
        ,(let ((commit "1248586b54af4bd4dfb28ebfebfc6bf012e7a587"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit commit)))
             (file-name (string-append "biod-"
                                       (string-take commit 9)
                                       "-checkout"))
             (sha256
              (base32
               "1m8hi1n7x0ri4l6s9i0x6jg4z4v94xrfdzp7mbizdipfag0m17g3")))))))
    (inputs
     `(("lz4" ,lz4)
       ("htslib" ,htslib-for-sambamba)))
    (home-page "http://lomereiter.github.io/sambamba")
    (synopsis "Tools for working with SAM/BAM data")
    (description "Sambamba is a high performance modern robust and
fast tool (and library), written in the D programming language, for
working with SAM and BAM files.  Current parallelised functionality is
an important subset of samtools functionality, including view, index,
sort, markdup, and depth.")
    (license license:gpl2+)))

(define-public nlopt
  (package
    (name "nlopt")
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ab-initio.mit.edu/nlopt/"
                                  "nlopt-" version ".tar.gz"))
              (sha256
               (base32
                "12cfkkhcdf4zmb6h7y6qvvdvqjs2xf9sjpa3rl3bq76px4yn76c0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-shared")))
    (home-page "http://ab-initio.mit.edu/wiki/index.php/NLopt")
    (synopsis "Library for nonlinear optimization")
    (description "NLopt is a library for nonlinear optimization,
providing a common interface for a number of different optimization
routines available online as well as original implementations of
various other algorithms.")
    (license license:lgpl2.1+)))

(define-public r-nloptr
  (package
    (name "r-nloptr")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nloptr" version))
       (sha256
        (base32
         "1cypz91z28vhvwq2rzqjrbdc6a2lvfr2g16vid2sax618q6ai089"))))
    (build-system r-build-system)
    (inputs
     `(("nlopt" ,nlopt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://cran.r-project.org/web/packages/nloptr")
    (synopsis "R interface to optimization library NLopt")
    (description
     "The nloptr package is an R interface to the NLopt nonlinear
optimization library.")
    (license license:lgpl3)))

(define-public r-minqa
  (package
    (name "r-minqa")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "minqa" version))
       (sha256
        (base32
         "036drja6xz7awja9iwb76x91415p26fb0jmg7y7v0p65m6j978fg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://optimizer.r-forge.r-project.org")
    (synopsis "Optimization algorithms by quadratic approximation")
    (description
     "This package implements derivative-free optimization algorithms
by quadratic approximation based on an interface to Fortran
implementations by M.J.D. Powell.")
    (license license:gpl2)))

(define-public r-misha
  (package
    (name "r-misha")
    (version "3.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.wisdom.weizmann.ac.il/~aviezerl/"
                           "gpatterns/misha_" version ".tar.gz"))
       (sha256
        (base32
         "1df4i0cisqj3szg08didmzk99awgvzjmzi55kasji5fw21z8qan6"))
       ;; Delete bundled executable.
       (snippet
        '(begin
           (delete-file "exec/bigWigToWig") #t))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-bundled-bigWigToWig
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "R/misha.R"
               (("get\\(\".GLIBDIR\"\\), \"/exec/bigWigToWig")
                (string-append "\""
                               (assoc-ref inputs "kentutils")
                               "/bin/bigWigToWig")))
             #t))
         (add-after 'unpack 'fix-isnan-error
           (lambda _
             (substitute* '("src/IncrementalWilcox.cpp"
                            "src/BinFinder.h"
                            "src/GenomeTrackImportWig.cpp"
                            "src/GenomeTrackSparse.h"
                            "src/GenomeTrackSmooth.cpp"
                            "src/GenomeTrackPartition.cpp"
                            "src/GenomeTrackArrays.cpp"
                            "src/TrackExpressionVars.cpp"
                            "src/GenomeTrackWilcox.cpp"
                            "src/GenomeTrackSegmentation.cpp"
                            "src/GTrackLiftover.cpp"
                            "src/rdbutils.h"
                            "src/GenomeTrackArrayImport.cpp"
                            "src/GenomeTrackFixedBin.cpp"
                            "src/GenomeTrackSummary.cpp"
                            "src/BinsManager.h"
                            "src/GenomeTrackQuantiles.cpp"
                            "src/GenomeTrackArrays.h"
                            "src/rdbinterval.cpp"
                            "src/GenomeTrackBinnedTransform.cpp")
               (("#define isnan ::isnan")
                "#define isnan std::isnan"))
             #t)))))
    (inputs
     `(("kentutils" ,kentutils)))
    (home-page "http://www.wisdom.weizmann.ac.il")
    (synopsis "Toolkit for analysis of genomic data")
    (description "This package is intended to help users to
efficiently analyze genomic data resulting from various experiments.")
    (license license:gpl2)))

(define-public r-laeken
  (package
    (name "r-laeken")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "laeken" version))
       (sha256
        (base32
         "1rhkv1kk508pwln1d325iq4fink2ncssps0ypxi52j9d7wk78la6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-mass" ,r-mass)))
    (home-page "http://cran.r-project.org/web/packages/laeken")
    (synopsis "Estimation of indicators on social exclusion and poverty")
    (description
     "This package provides tools for the estimation of indicators on
social exclusion and poverty, as well as an implementation of Pareto
tail modeling for empirical income distributions.")
    (license license:gpl2+)))

(define-public r-vcd
  (package
    (name "r-vcd")
    (version "1.4-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vcd" version))
       (sha256
        (base32
         "05azric2w8mrsdk7y0484cjygcgcmbp96q2v500wvn91fj98kkhp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)
       ("r-lmtest" ,r-lmtest)
       ("r-mass" ,r-mass)))
    (home-page "http://cran.r-project.org/web/packages/vcd")
    (synopsis "Visualizing categorical data")
    (description
     "This package provides visualization techniques, data sets,
summary and inference procedures aimed particularly at categorical
data.  Special emphasis is given to highly extensible grid graphics.
The package was package was originally inspired by the book
\"Visualizing Categorical Data\" by Michael Friendly and is now the
main support package for a new book, \"Discrete Data Analysis with R\"
by Michael Friendly and David Meyer (2015).")
    (license license:gpl2)))

(define-public r-sp
  (package
    (name "r-sp")
    (version "1.2-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sp" version))
       (sha256
        (base32
         "0crba3j00mb2xv2yk60rpa57gn97xq4ql3a6p9cjzqjxzv2cknk2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "http://cran.r-project.org/web/packages/sp")
    (synopsis "Classes and methods for spatial data")
    (description
     "This package provides classes and methods for spatial data; the
classes document where the spatial location information resides, for
2D or 3D data.  Utility functions are provided, e.g.  for plotting
data as maps, spatial selection, as well as methods for retrieving
coordinates, for subsetting, print, summary, etc.")
    (license license:gpl2+)))

(define-public r-vim
  (package
    (name "r-vim")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "VIM" version))
       (sha256
        (base32
         "1yl6bsdigwjfkdn5mbfrw10ip1xr7scw1mvykdwgzkyfnlb8z86w"))))
    (properties `((upstream-name . "VIM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-colorspace" ,r-colorspace)
       ("r-data-table" ,r-data-table)
       ("r-e1071" ,r-e1071)
       ("r-laeken" ,r-laeken)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-rcpp" ,r-rcpp)
       ("r-robustbase" ,r-robustbase)
       ("r-sp" ,r-sp)
       ("r-vcd" ,r-vcd)))
    (home-page "https://github.com/alexkowa/VIM")
    (synopsis "Visualization and imputation of missing values")
    (description
     "This package provides tools for the visualization of missing
and/or imputed values are introduced, which can be used for exploring
the data and the structure of the missing and/or imputed values.
Depending on this structure of the missing values, the corresponding
methods may help to identify the mechanism generating the missing
values and allows to explore the data including missing values.  In
addition, the quality of imputation can be visually explored using
various univariate, bivariate, multiple and multivariate plot methods.
A graphical user interface available in the separate package VIMGUI
allows an easy handling of the implemented plot methods.")
    (license license:gpl2+)))

(define-public r-fnn
  (package
    (name "r-fnn")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FNN" version))
       (sha256
        (base32
         "1kncmiaraq1mrykb9fj3fsxswabk3l71fnp1vks0x9aay5xfk8mj"))))
    (properties `((upstream-name . "FNN")))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/FNN")
    (synopsis "Fast nearest neighbor search algorithms and applications")
    (description
     "This package provides cover-tree and kd-tree fast k-nearest
neighbor search algorithms.  Related applications including KNN
classification, regression and information measures are implemented.")
    ;; The DESCRIPTION file erroneously states that GPL version 2.1 or
    ;; later can be used.
    (license license:gpl2+)))

(define-public r-scatterplot3d
  (package
    (name "r-scatterplot3d")
    (version "0.3-38")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scatterplot3d" version))
       (sha256
        (base32
         "1mw1v725cdq24lfygjjmx3vmxzr2zpd332v0lham7g0p4r7lc8pb"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/scatterplot3d")
    (synopsis "3D scatter plot")
    (description
     "This package provides tools to plot a three dimensional (3D)
point cloud.")
    (license license:gpl2)))

(define-public r-xts
  (package
    (name "r-xts")
    (version "0.9-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xts" version))
       (sha256
        (base32
         "163hzcnxrdb4lbsnwwv7qa00h4qlg4jm289acgvbg4jbiywpq7zi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zoo" ,r-zoo)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://r-forge.r-project.org/projects/xts/")
    (synopsis "Extensible time series")
    (description
     "This package provides for uniform handling of R's different
time-based data classes by extending zoo, maximizing native format
information preservation and allowing for user level customization and
extension, while simplifying cross-class interoperability.")
    (license license:gpl2+)))

(define-public r-ttr
  (package
    (name "r-ttr")
    (version "0.23-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TTR" version))
       (sha256
        (base32
         "1bmj0ngd3i3a9l2zsanifq3irz3rhsyd2rvvlhyndsgadkq9i5v9"))))
    (properties `((upstream-name . "TTR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-xts" ,r-xts)
       ("r-zoo" ,r-zoo)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://github.com/joshuaulrich/TTR")
    (synopsis "Technical trading rules")
    (description
     "This package provides functions and data to construct technical
trading rules with R.")
    (license license:gpl2)))

(define-public r-smoother
  (package
    (name "r-smoother")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "smoother" version))
       (sha256
        (base32
         "0nqr1bvlr5bnasqg74zmknjjl4x28kla9h5cxpga3kq5z215pdci"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ttr" ,r-ttr)))
    (home-page "http://cran.r-project.org/web/packages/smoother")
    (synopsis "Functions relating to the smoothing of numerical data")
    (description
     "This package provides a collection of methods for smoothing
numerical data, commencing with a port of the Matlab gaussian window
smoothing function.  In addition, several functions typically used in
smoothing of financial data are included.")
    (license license:gpl2)))

(define-public r-destiny
  (package
    (name "r-destiny")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "destiny" version))
       (sha256
        (base32
         "18rf73c65ni0769a3x00hryrvmz1bkdskm8x45lwbbssraaj8ffk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-fnn" ,r-fnn)
       ("r-hmisc" ,r-hmisc)
       ("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)
       ("r-proxy" ,r-proxy)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-scales" ,r-scales)
       ("r-scatterplot3d" ,r-scatterplot3d)
       ("r-smoother" ,r-smoother)
       ("r-vim" ,r-vim)))
    (home-page "http://bioconductor.org/packages/destiny")
    (synopsis "Create and plot diffusion maps")
    (description "This package provides tools to create and plot
diffusion maps.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-aroma-light
  (package
    (name "r-aroma-light")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "aroma.light" version))
       (sha256
        (base32
         "10snykmmx36qaymyf5s1n1km8hsscyzpykcpf0mzsrcv8ml9rp8a"))))
    (properties `((upstream-name . "aroma.light")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrixstats" ,r-matrixstats)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/aroma.light")
    (synopsis "Methods for normalization and visualization of microarray data")
    (description
     "This package provides methods for microarray analysis that take basic
data types such as matrices and lists of vectors.  These methods can be used
standalone, be utilized in other packages, or be wrapped up in higher-level
classes.")
    (license license:gpl2+)))

(define-public r-deseq
  (package
    (name "r-deseq")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq" version))
       (sha256
        (base32
         "0j3dgcxd64m9qknmlcbdzvg4xhp981xd6nbwsvnqjfn6yypslgyw"))))
    (properties `((upstream-name . "DESeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-genefilter" ,r-genefilter)
       ("r-geneplotter" ,r-geneplotter)
       ("r-lattice" ,r-lattice)
       ("r-locfit" ,r-locfit)
       ("r-mass" ,r-mass)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "http://www-huber.embl.de/users/anders/DESeq")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides tools for estimating variance-mean dependence in
count data from high-throughput genetic sequencing assays and for testing for
differential expression based on a model using the negative binomial
distribution.")
    (license license:gpl3+)))

(define-public r-edaseq
  (package
    (name "r-edaseq")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "EDASeq" version))
       (sha256
        (base32
         "0f25dfc8hdii9fjm3bf89vy9jkxv23sa62fkcga5b4gkipwrvm9a"))))
    (properties `((upstream-name . "EDASeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-aroma-light" ,r-aroma-light)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq" ,r-deseq)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-shortread" ,r-shortread)))
    (home-page "https://github.com/drisso/EDASeq")
    (synopsis "Exploratory data analysis and normalization for RNA-Seq")
    (description
     "This package provides support for numerical and graphical summaries of
RNA-Seq genomic read data.  Provided within-lane normalization procedures to
adjust for GC-content effect (or other gene-level effects) on read counts:
loess robust local regression, global-scaling, and full-quantile
normalization.  Between-lane normalization procedures to adjust for
distributional differences between lanes (e.g., sequencing depth):
global-scaling and full-quantile normalization.")
    (license license:artistic2.0)))

(define-public r-catch
  (let ((commit "60c995e34a6b709832c96bc82128218ecfebd108")
        (release "1.0")
        (revision "1"))
    (package
      (name "r-catch")
      (version (string-append release "-" revision "." (string-take commit 9)))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://raw.githubusercontent.com/zhanyinx/CaTCH_R/"
                                    commit "/CaTCH_" release ".tar.gz"))
                (sha256
                 (base32
                  "052s1ysyvg307q85h4v9flcvfilxw6wpgll417gp5jsygh3kgcpm"))))
      (build-system r-build-system)
      (home-page "https://github.com/zhanyinx/CaTCH_R")
      (synopsis "Call a hierarchy of domains based on Hi-C data")
      (description "This package allows building the hierarchy of
domains starting from Hi-C data.  Each hierarchical level is
identified by a minimum value of physical insulation between
neighboring domains.")
      (license license:gpl2))))

(define-public pacbio-htslib
  (let ((commit "6b6c81388e699c0c0cf2d1f7fe59c5da60fb7b9a")
        (revision "1"))
    (package (inherit htslib-1.1)
      (name "pacbio-htslib")
      (version (string-append "1.1-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PacificBiosciences/htslib.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1zynj7na8iypzzaryhpsczd2603facj8nskvfk6il0zcjrgn4rnj"))))
      (arguments
       (substitute-keyword-arguments (package-arguments htslib-1.1)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'install 'install-cram-headers
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((cram (string-append (assoc-ref outputs "out")
                                            "/include/cram")))
                   (mkdir-p cram)
                   (for-each (lambda (file)
                               (install-file file cram))
                             (find-files "cram" "\\.h$"))
                   #t))))))))))

(define-public pbbam
  (let ((commit "2db9abe7d979b5642a97446b9f9601e387315585")
        (revision "1"))
    (package
      (name "pbbam")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PacificBiosciences/pbbam.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1z9p8zhf59pbrx63js64n35gmhy9gbihs1drvx04kh33p45aab6m"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
         (list "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE"
               "-DPacBioBAM_build_shared=ON"
               "-DHTSLIB_LIBRARIES=-lhts"
               (string-append "-DHTSLIB_INCLUDE_DIRS="
                              (assoc-ref %build-inputs "pacbio-htslib")
                              "/include")
               (string-append "-DGTEST_SRC_DIR="
                              (getcwd) "/source/"
                              "googletest-release-"
                              ,(package-version googletest)
                              "/googletest"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'unpack-googletest
             (lambda* (#:key inputs #:allow-other-keys)
               (zero? (system* "tar" "xf"
                               (assoc-ref inputs "googletest-source")))))
           (add-after 'unpack 'patch-tests
             (lambda _
               (substitute* "tests/scripts/cram.py"
                 (("/bin/sh") (which "bash")))
               #t))
           ;; The install target only installs the tests.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out     (assoc-ref outputs "out"))
                      (bin     (string-append out "/bin"))
                      (lib     (string-append out "/lib"))
                      (include (string-append out "/include")))
                 (mkdir-p out)
                 (copy-recursively "bin" bin)
                 (copy-recursively "lib" lib)
                 (copy-recursively "../source/include" include)
                 #t)))
           ;; Run tests after installation.  This is needed to make
           ;; sure that the executable finds the libpbbam library.
           (delete 'check)
           (add-after 'install 'check
             (assoc-ref %standard-phases 'check)))))
      (inputs
       `(("boost" ,boost)
         ("pacbio-htslib" ,pacbio-htslib)
         ("zlib" ,zlib)))
      (native-inputs
       `(("doxygen" ,doxygen)
         ("python" ,python-2)
         ("googletest-source" ,(package-source googletest))
         ("patch" ,patch)))
      (home-page "https://github.com/PacificBiosciences/pbbam")
      (synopsis "BAM library for PacBio software")
      (description "The pbbam software package provides components to
create, query, and edit PacBio BAM files and associated indices.
These components include a core C++ library, bindings for additional
languages, and command-line utilities.")
      (license license:bsd-3))))

(define-public lsgkm
  (let ((commit "164a4a48f6387e67b5d955db932f8a403de6c321")
        (revision "1"))
    (package
      (name "lsgkm")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Dongwon-Lee/lsgkm.git")
               (commit commit)))
         (sha256
          (base32
           "0zxji94c30q4zg5lw35n4jpwfx1p6rwdsi7h0bi5ldarh6jfpn52"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "-C" "src")
         #:tests? #f ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out")
                                         "/bin")))
                 (mkdir-p bin)
                 (for-each (lambda (file)
                             (install-file file bin))
                           '("src/gkmtrain"
                             "src/gkmpredict"))
                 #t))))))
      (home-page "https://github.com/Dongwon-Lee/lsgkm")
      (synopsis "Predict regulatory DNA elements in large-scale data")
      (description "gkm-SVM, a sequence-based method for predicting
regulatory DNA elements, is a useful tool for studying gene regulatory
mechanisms.  LS-GKM is an effort to improve the method.  It offers
much better scalability and provides further advanced gapped k-mer
based kernel functions.  As a result, LS-GKM achieves considerably
higher accuracy than the original gkm-SVM.")
      (license license:gpl3+))))

(define-public python-fcsparser
  (package
    (name "python-fcsparser")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fcsparser" version ".tar.gz"))
       (sha256
        (base32
         "0jgxny74ppy7va96szvh04nrzkvy866hygki3imivg9kb9pzjdm2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/eyurtsev/fcsparser")
    (synopsis "Tools for reading raw fcs files")
    (description
     "This package provides a Python module for reading raw fcs
files.")
    (license license:expat)))

;; python-magic already exists, so we need to give this a different
;; name.
(define-public python-biomagic
  (package
    (name "python-biomagic")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pkathail/magic/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14v2ykxlc6yscj3s7bxgmbz7lc6pr2ab469w3qcxzh4lgvh3mq0m"))))
    (build-system python-build-system)
    ;; No tests included.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-scipy" ,python-scipy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-seaborn" ,python-seaborn)
       ("python-scikit-learn" ,python-scikit-learn)
       ("python-networkx" ,python-networkx)
       ("python-fcsparser" ,python-fcsparser)
       ("python-statsmodels" ,python-statsmodels)))
    (home-page "https://github.com/pkathail/magic")
    (synopsis "Markov affinity-based graph imputation of cells")
    (description "MAGIC is an interactive tool to impute missing
values in single-cell sequencing data and to restore the structure of
the data.  It also provides data pre-processing functionality such as
dimensionality reduction and gene expression visualization.")
    (license license:gpl2+)))

(define-public r-keggrest
  (package
    (name "r-keggrest")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "KEGGREST" version))
       (sha256
        (base32
         "00mbnsrh0xc6c9wm9szv55jyh1habxq6d448wk3bi2hizg60cdcw"))))
    (properties `((upstream-name . "KEGGREST")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-httr" ,r-httr)
       ("r-png" ,r-png)))
    (home-page
     "http://bioconductor.org/packages/KEGGREST")
    (synopsis "Client-side REST access to KEGG")
    (description
     "This package provides a package that provides a client
interface to the KEGG REST server.  Based on KEGGSOAP by J.  Zhang, R.
Gentleman, and Marc Carlson, and KEGG (python package) by Aurelien
Mazurie.")
    (license license:artistic2.0)))

(define-public r-complexheatmap
  (package
    (name "r-complexheatmap")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ComplexHeatmap" version))
       (sha256
        (base32
         "0rz99swi2s2wqyfws4642xkd23dar2vapiw86xsjqcc845032p9x"))))
    (properties
     `((upstream-name . "ComplexHeatmap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circlize" ,r-circlize)
       ("r-colorspace" ,r-colorspace)
       ("r-dendextend" ,r-dendextend)
       ("r-getoptlong" ,r-getoptlong)
       ("r-globaloptions" ,r-globaloptions)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page
     "https://github.com/jokergoo/ComplexHeatmap")
    (synopsis "Making Complex Heatmaps")
    (description
     "Complex heatmaps are efficient to visualize associations
between different sources of data sets and reveal potential
structures.  Here the ComplexHeatmap package provides a highly
flexible way to arrange multiple heatmaps and supports self-defined
annotation graphics.")
    (license license:gpl2+)))

(define-public r-dirichletmultinomial
  (package
    (name "r-dirichletmultinomial")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DirichletMultinomial" version))
       (sha256
        (base32
         "06mpj57fr08kgr9d87izwdbri930qfxr6kw6sg6qnfgx71lcdy1h"))))
    (properties
     `((upstream-name . "DirichletMultinomial")))
    (build-system r-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-iranges" ,r-iranges)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "http://bioconductor.org/packages/DirichletMultinomial")
    (synopsis "Dirichlet-Multinomial mixture models for microbiome data")
    (description
     "Dirichlet-multinomial mixture models can be used to describe
variability in microbial metagenomic data.  This package is an
interface to code originally made available by Holmes, Harris, and
Quince, 2012, PLoS ONE 7(2): 1-15, as discussed further in the man
page for this package, @code{?DirichletMultinomial}.")
    (license license:lgpl3)))

(define-public r-annotationfilter
  (package
    (name "r-annotationfilter")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "AnnotationFilter" version))
              (sha256
               (base32
                "0pxvswjzwibdfmrkdragxmzcl844z73pmkn82z92wahwa6gjfyi7"))))
    (properties
     `((upstream-name . "AnnotationFilter")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicranges" ,r-genomicranges)
       ("r-lazyeval" ,r-lazyeval)))
    (home-page "https://github.com/Bioconductor/AnnotationFilter")
    (synopsis "Facilities for filtering Bioconductor annotation resources")
    (description
     "This package provides classes and other infrastructure to
implement filters for manipulating Bioconductor annotation resources.
The filters are used by ensembldb, Organism.dplyr, and other
packages.")
    (license license:artistic2.0)))

(define-public r-ensembldb
  (package
    (name "r-ensembldb")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ensembldb" version))
       (sha256
        (base32
         "1np96nry1hba8lk4bg3grf8w3k6xz9lgd2jcl3vrj6wsl184c3fr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-curl" ,r-curl)
       ("r-dbi" ,r-dbi)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-protgenerics" ,r-protgenerics)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/jotsetung/ensembldb")
    (synopsis "Utilities to create and use Ensembl-based annotation databases")
    (description
     "The package provides functions to create and use transcript
centric annotation databases/packages.  The annotation for the
databases are directly fetched from Ensembl using their Perl API.  The
functionality and data is similar to that of the TxDb packages from
the GenomicFeatures package, but, in addition to retrieve all
gene/transcript models and annotations from the database, the
ensembldb package provides also a filter framework allowing to
retrieve annotations for specific entries like genes encoded on a
chromosome region or transcript models of lincRNA genes.")
    ;; No version specified
    (license license:lgpl3+)))

(define-public r-organismdbi
  (package
    (name "r-organismdbi")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "OrganismDbi" version))
       (sha256
        (base32
         "17jamgx9hqyi8ia48whqf7jj6ibdah2641zvx1xpv2lm8mhl3qzc"))))
    (properties `((upstream-name . "OrganismDbi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-graph" ,r-graph)
       ("r-iranges" ,r-iranges)
       ("r-rbgl" ,r-rbgl)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "http://bioconductor.org/packages/OrganismDbi")
    (synopsis "Software to enable the smooth interfacing of database packages")
    (description "The package enables a simple unified interface to
several annotation packages each of which has its own schema by taking
advantage of the fact that each of these packages implements a select
methods.")
    (license license:artistic2.0)))

(define-public r-biovizbase
  (package
    (name "r-biovizbase")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "biovizBase" version))
       (sha256
        (base32
         "1pfyhjwlxw9p2q5ip0irxpwndgakvn6z6ay5ahgz2gkkk8x8i29w"))))
    (properties `((upstream-name . "biovizBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-dichromat" ,r-dichromat)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "http://bioconductor.org/packages/biovizBase")
    (synopsis "Basic graphic utilities for visualization of genomic data")
    (description
     "The biovizBase package is designed to provide a set of
utilities, color schemes and conventions for genomic data.  It serves
as the base for various high-level packages for biological data
visualization.  This saves development effort and encourages
consistency.")
    (license license:artistic2.0)))

(define-public r-ggbio
  (package
    (name "r-ggbio")
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ggbio" version))
       (sha256
        (base32
         "1g8m03hyjr2jmqp6m6a29lwvfq2d0lsfg1wjkjhzkysr45c360z7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationfilter" ,r-annotationfilter)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-biovizbase" ,r-biovizbase)
       ("r-bsgenome" ,r-bsgenome)
       ("r-ensembldb" ,r-ensembldb)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggally" ,r-ggally)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-organismdbi" ,r-organismdbi)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "http://tengfei.github.com/ggbio/")
    (synopsis "Visualization tools for genomic data")
    (description
     "The ggbio package extends and specializes the grammar of
graphics for biological data.  The graphics are designed to answer
common scientific questions, in particular those often asked of high
throughput genomics data.  All core Bioconductor data structures are
supported, where appropriate.  The package supports detailed views of
particular genomic regions, as well as genome-wide overviews.
Supported overviews include ideograms and grand linear views.
High-level plots include sequence fragment length, edge-linked
interval to data view, mismatch pileup, and several splicing
summaries.")
    (license license:artistic2.0)))

(define-public r-gprofiler
  (package
    (name "r-gprofiler")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gProfileR" version))
       (sha256
        (base32
         "1qix15d0wa9nspdclcawml94mng4qmr2jciv7d24py315wfsvv8p"))))
    (properties `((upstream-name . "gProfileR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-plyr" ,r-plyr)
       ("r-rcurl" ,r-rcurl)))
    (home-page "http://cran.r-project.org/web/packages/gProfileR")
    (synopsis "Interface to the 'g:Profiler' Toolkit")
    (description
     "This package provides tools for functional enrichment analysis,
gene identifier conversion and mapping homologous genes across related
organisms via the g:Profiler
toolkit (http://biit.cs.ut.ee/gprofiler/).")
    (license license:gpl2+)))

(define-public r-gqtlbase
  (package
    (name "r-gqtlbase")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gQTLBase" version))
       (sha256
        (base32
         "1583w1nmm7rshrmz7m3clcvqxxfymj81x4lddywpr9pcp2dncm9c"))))
    (properties `((upstream-name . "gQTLBase")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-batchjobs" ,r-batchjobs)
       ("r-bbmisc" ,r-bbmisc)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-bit" ,r-bit)
       ("r-doparallel" ,r-doparallel)
       ("r-ff" ,r-ff)
       ("r-ffbase" ,r-ffbase)
       ("r-foreach" ,r-foreach)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page
     "http://bioconductor.org/packages/gQTLBase")
    (synopsis "Infrastructure for eQTL, mQTL and similar studies")
    (description
     "This package provides infrastructure for eQTL, mQTL and similar
studies.")
    (license license:artistic2.0)))

(define-public r-snpstats
  (package
    (name "r-snpstats")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "snpStats" version))
       (sha256
        (base32
         "1f8i8pj741h8539lqj508ji27p5ljghyvmdrh3qcfx5jwn9jq8bj"))))
    (properties `((upstream-name . "snpStats")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-matrix" ,r-matrix)
       ("r-survival" ,r-survival)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page "http://bioconductor.org/packages/snpStats")
    (synopsis "SnpMatrix and XSnpMatrix classes and methods")
    (description
     "This package provides classes and statistical methods for large
SNP association studies.  This extends the earlier snpMatrix package,
allowing for uncertainty in genotypes.")
    (license license:gpl3)))

(define-public r-org-hs-eg-db
  (package
    (name "r-org-hs-eg-db")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "org.Hs.eg.db_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "001bkj40kcgf3w9g2xnbsz8qny0iynyh14mdwl3p07asjbqv71qg"))))
    (properties
     `((upstream-name . "org.Hs.eg.db")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)))
    (home-page "http://bioconductor.org/packages/org.Hs.eg.db/")
    (synopsis "Genome wide annotation for Human")
    (description
     "This package contains genome wide annotations for Human,
primarily based on mapping using Entrez Gene identifiers.")
    (license license:artistic2.0)))

(define-public r-homo-sapiens
  (package
    (name "r-homo-sapiens")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "http://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "Homo.sapiens_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "151vj7h5p1c8yd5swrchk46z469p135wk50hvkl0nhgndvy0jj01"))))
    (properties
     `((upstream-name . "Homo.sapiens")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomicfeatures" ,r-genomicfeatures)
       ("r-go-db" ,r-go-db)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-txdb-hsapiens-ucsc-hg19-knowngene"
        ,r-txdb-hsapiens-ucsc-hg19-knowngene)
       ("r-organismdbi" ,r-organismdbi)
       ("r-annotationdbi" ,r-annotationdbi)))
    (home-page "http://bioconductor.org/packages/Homo.sapiens/")
    (synopsis "Annotation package for the Homo.sapiens object")
    (description
     "This package contains the Homo.sapiens object to access data
from several related annotation packages.")
    (license license:artistic2.0)))

(define-public r-ldblock
  (package
    (name "r-ldblock")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ldblock" version))
       (sha256
        (base32
         "1fr38vs0na6brfwzkppmq3z4n0gdrl9jxjih4w80gqp8672n2hrh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-erma" ,r-erma)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-go-db" ,r-go-db)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-matrix" ,r-matrix)
       ("r-rsamtools" ,r-rsamtools)
       ("r-snpstats" ,r-snpstats)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page
     "http://bioconductor.org/packages/ldblock")
    (synopsis "Data structures for linkage disequilibrium measures in populations")
    (description
     "This package defines data structures for linkage disequilibrium
measures in populations.")
    (license license:artistic2.0)))

(define-public r-erma
  (package
    (name "r-erma")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "erma" version))
       (sha256
        (base32
         "1wn9fvnwrk08s30nd5xy61brsx20wmzdmc0xq92rbnmy2c3c0kvd"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-foreach" ,r-foreach)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "http://bioconductor.org/packages/erma")
    (synopsis "Epigenomic road map adventures")
    (description
     "This package provides software and data to support epigenomic
road map adventures.")
    (license license:artistic2.0)))

(define-public r-mice
  (package
    (name "r-mice")
    (version "2.30")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mice" version))
       (sha256
        (base32
         "1r673x51vs3w7kz4bkp2rih4445hcmajw86gjwz7m2piajwvs817"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-rcpp" ,r-rcpp)
       ("r-rpart" ,r-rpart)
       ("r-survival" ,r-survival)))
    (home-page "http://www.stefvanbuuren.nl")
    (synopsis "Multivariate imputation by chained equations")
    (description
     "Multiple imputation using Fully Conditional Specification (FCS)
implemented by the MICE algorithm as described in Van Buuren and
Groothuis-Oudshoorn (2011) <doi:10.18637/jss.v045.i03>.  Each variable
has its own imputation model.  Built-in imputation models are provided
for continuous data (predictive mean matching, normal), binary
data (logistic regression), unordered categorical data (polytomous
logistic regression) and ordered categorical data (proportional odds).
MICE can also impute continuous two-level data (normal model, pan,
second-level variables).  Passive imputation can be used to maintain
consistency between variables.  Various diagnostic plots are available
to inspect the quality of the imputations.")
    ;; Any of these two versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-hardyweinberg
  (package
    (name "r-hardyweinberg")
    (version "1.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HardyWeinberg" version))
       (sha256
        (base32
         "0xbcchmzii0jv0ygr91n72r39j1axraxd2i607b56v4yd5d8sy4k"))))
    (properties `((upstream-name . "HardyWeinberg")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mice" ,r-mice)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/package=HardyWeinberg")
    (synopsis "Statistical tests and graphics for Hardy-Weinberg equilibrium")
    (description
     "This package contains tools for exploring Hardy-Weinberg
equilibrium for diallelic genetic marker data.  All classical
tests (chi-square, exact, likelihood-ratio and permutation tests) for
Hardy-Weinberg equilibrium are included in the package, as well as
functions for power computation and for the simulation of marker data
under equilibrium and disequilibrium.  Routines for dealing with
markers on the X-chromosome are included.  Functions for testing
equilibrium in the presence of missing data by using multiple
imputation are also provided.  Implements several graphics for
exploring the equilibrium status of a large set of diallelic markers:
ternary plots with acceptance regions, log-ratio plots and Q-Q
plots.")
    (license license:gpl2+)))

(define-public r-gqtlstats
  (package
    (name "r-gqtlstats")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "gQTLstats" version))
        (sha256
          (base32
            "14zdgysd6c4jil7k699m5j4bfj6fdnwz8j05x2wq8r6sbz2pqkwp"))))
    (properties `((upstream-name . "gQTLstats")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-annotationdbi" ,r-annotationdbi)
        ("r-batchjobs" ,r-batchjobs)
        ("r-bbmisc" ,r-bbmisc)
        ("r-beeswarm" ,r-beeswarm)
        ("r-biobase" ,r-biobase)
        ("r-biocgenerics" ,r-biocgenerics)
        ("r-doparallel" ,r-doparallel)
        ("r-dplyr" ,r-dplyr)
        ("r-erma" ,r-erma)
        ("r-ffbase" ,r-ffbase)
        ("r-foreach" ,r-foreach)
        ("r-genomeinfodb" ,r-genomeinfodb)
        ("r-genomicfeatures" ,r-genomicfeatures)
        ("r-genomicfiles" ,r-genomicfiles)
        ("r-genomicranges" ,r-genomicranges)
        ("r-ggplot2" ,r-ggplot2)
        ("r-gqtlbase" ,r-gqtlbase)
        ("r-hardyweinberg" ,r-hardyweinberg)
        ("r-iranges" ,r-iranges)
        ("r-ldblock" ,r-ldblock)
        ("r-limma" ,r-limma)
        ("r-mgcv" ,r-mgcv)
        ("r-plotly" ,r-plotly)
        ("r-reshape2" ,r-reshape2)
        ("r-s4vectors" ,r-s4vectors)
        ("r-shiny" ,r-shiny)
        ("r-snpstats" ,r-snpstats)
        ("r-summarizedexperiment" ,r-summarizedexperiment)
        ("r-variantannotation" ,r-variantannotation)))
    (home-page "http://bioconductor.org/packages/gQTLstats")
    (synopsis "Computationally efficient analysis for eQTL and allied studies")
    (description
      "This package provides tools for the computationally efficient
analysis of eQTL, mQTL, dsQTL, etc.")
    (license license:artistic2.0)))

(define-public r-gviz
  (package
    (name "r-gviz")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Gviz" version))
       (sha256
        (base32
         "161mf1lwqcgl8058xsypbcy48p8jhc93gbg9x375p721ccfdxrps"))))
    (properties `((upstream-name . "Gviz")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biomart" ,r-biomart)
       ("r-biostrings" ,r-biostrings)
       ("r-biovizbase" ,r-biovizbase)
       ("r-bsgenome" ,r-bsgenome)
       ("r-digest" ,r-digest)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-matrixstats" ,r-matrixstats)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "http://bioconductor.org/packages/Gviz")
    (synopsis "Plotting data and annotation information along genomic coordinates")
    (description
     "Genomic data analyses requires integrated visualization of known
genomic information and new experimental data.  Gviz uses the biomaRt
and the rtracklayer packages to perform live annotation queries to
Ensembl and UCSC and translates this to e.g.  gene/transcript
structures in viewports of the grid graphics package.  This results in
genomic information plotted together with your data.")
    (license license:artistic2.0)))

(define-public r-gwascat
  (package
    (name "r-gwascat")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "gwascat" version))
       (sha256
        (base32
         "0hcfjhbgw6n35qabkjd823mjq1jkxm2nz8f9xvc4vl4ypg0q4gab"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-annotationhub" ,r-annotationhub)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gqtlstats" ,r-gqtlstats)
       ("r-graph" ,r-graph)
       ("r-gviz" ,r-gviz)
       ("r-homo-sapiens" ,r-homo-sapiens)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-snpstats" ,r-snpstats)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "http://bioconductor.org/packages/gwascat")
    (synopsis "Tools for data in the EMBL-EBI GWAS catalog")
    (description
     "This package provides tools for representing and modeling data
in the EMBL-EBI GWAS catalog.")
    (license license:artistic2.0)))

(define-public r-prabclus
  (package
    (name "r-prabclus")
    (version "2.2-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "prabclus" version))
              (sha256
               (base32
                "0qjsxrx6yv338bxm4ki0w9h8hind1l98abdrz828588bwj02jya1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-mclust" ,r-mclust)))
    (home-page "http://www.homepages.ucl.ac.uk/~ucakche")
    (synopsis "Functions for clustering of presence-absence, abundance and multilocus genetic data")
    (description
     "This package provides distance-based parametric bootstrap tests
for clustering with spatial neighborhood information.  Some distance
measures, Clustering of presence-absence, abundance and multilocus
genetical data for species delimitation, nearest neighbor based noise
detection.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-sm
  (package
    (name "r-sm")
    (version "2.2-5.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sm" version))
       (sha256
        (base32
         "0hnq5s2fv94gaj0nyqc1vjdjd64vsp9z23nqa8hxvjcaf996rwj9"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://www.stats.gla.ac.uk/~adrian/sm")
    (synopsis "Smoothing methods for nonparametric regression and density estimation")
    (description
     "This is software linked to the book 'Applied Smoothing
Techniques for Data Analysis - The Kernel Approach with S-Plus
Illustrations' Oxford University Press.")
    (license license:gpl2+)))

(define-public r-vioplot
  (package
    (name "r-vioplot")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vioplot" version))
       (sha256
        (base32
         "16wkb26kv6qr34hv5zgqmgq6zzgysg9i78pvy2c097lr60v087v0"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-sm" ,r-sm)))
    (home-page "http://wsopuppenkiste.wiso.uni-goettingen.de/~dadler")
    (synopsis "Violin plot")
    (description
     "This package provides a violin plot is a combination of a box
plot and a kernel density plot.")
    (license license:bsd-3)))

(define-public r-sushi
  (package
    (name "r-sushi")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "Sushi" version))
              (sha256
               (base32
                "0cn8kwrx030vb2wqxp8f6cc6sqyvjdfkpgyadyhrar6154fpx58r"))))
    (properties `((upstream-name . "Sushi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biomart" ,r-biomart)
       ("r-zoo" ,r-zoo)))
    (home-page "http://bioconductor.org/packages/Sushi")
    (synopsis "Tools for visualizing genomics data")
    (description
     "This package provides flexible, quantitative, and integrative
genomic visualizations for publication-quality multi-panel figures.")
    (license license:gpl2+)))

(define-public r-fdrtool
  (package
    (name "r-fdrtool")
    (version "1.2.15")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "fdrtool" version))
              (sha256
               (base32
                "1h46frlk7d9f4qx0bg6p55nrm9wwwz2sv6d1nz7061wdfsm69yb5"))))
    (build-system r-build-system)
    (home-page "http://strimmerlab.org/software/fdrtool/")
    (synopsis "Estimation of false discovery rates and higher criticism")
    (description
     "This package provides tools to e stimate both @dfn{tail
area-based false discovery rates} (Fdr) as well as @dfn{local false
discovery rates} (fdr) for a variety of null models (p-values,
z-scores, correlation coefficients, t-scores).  The proportion of null
values and the parameters of the null distribution are adaptively
estimated from the data.  In addition, the package contains functions
for non-parametric density estimation (Grenander estimator), for
monotone regression (isotonic regression and antitonic regression with
weights), for computing the @dfn{greatest convex minorant} (GCM) and
the @dfn{least concave majorant} (LCM), for the half-normal and
correlation distributions, and for computing empirical @dfn{higher
criticism} (HC) scores and the corresponding decision threshold.")
    (license license:gpl3+)))

(define-public r-fithic
  (package
    (name "r-fithic")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "FitHiC" version))
              (sha256
               (base32
                "10jalj7gjqsldx92c349ihx608m6234y8s0pj7kgdfm9jwmgrwww"))))
    (properties `((upstream-name . "FitHiC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-fdrtool" ,r-fdrtool)
       ("r-rcpp" ,r-rcpp)))
    (home-page "http://bioconductor.org/packages/FitHiC")
    (synopsis "Confidence estimation for intra-chromosomal contact maps")
    (description
     "Fit-Hi-C is a tool for assigning statistical confidence
estimates to intra-chromosomal contact maps produced by genome-wide
genome architecture assays such as Hi-C.")
    (license license:gpl2+)))

(define-public r-hitc
  (package
    (name "r-hitc")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "HiTC" version))
              (sha256
               (base32
                "1gkg8774rr5qihm5fmx8mi1a0111iniz3i80ygg7jpn5imv60p4h"))))
    (properties `((upstream-name . "HiTC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rtracklayer" ,r-rtracklayer)))
    (home-page "http://bioconductor.org/packages/HiTC")
    (synopsis "High throughput chromosome conformation capture analysis")
    (description
     "The HiTC package was developed to explore high-throughput 'C'
data such as 5C or Hi-C.  Dedicated R classes as well as standard
methods for quality controls, normalization, visualization, and
further analysis are also provided.")
    (license license:artistic2.0)))

(define-public r-rsofia
  (package
    (name "r-rsofia")
    (version "1.1")
    (source (origin
              (method url-fetch)
              ;; This package has been removed from CRAN, so we can
              ;; only fetch it from the archives.
              (uri (string-append "https://cran.r-project.org/src/"
                                  "contrib/Archive/RSofia/RSofia_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0q931y9rcf6slb0s2lsxhgqrzy4yqwh8hb1124nxg0bjbxvjbihn"))))
    (properties `((upstream-name . "RSofia")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/src/contrib/Archive/RSofia")
    (synopsis "Port of sofia-ml to R")
    (description "This package is a port of sofia-ml to R.  Sofia-ml
is a suite of fast incremental algorithms for machine learning that
can be used for training models for classification or ranking.")
    (license license:asl2.0)))

(define-public r-bbmle
  (package
    (name "r-bbmle")
    (version "1.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bbmle" version))
       (sha256
        (base32
         "014h6mw16gv4acs2p78dy7lla7s428n633aybsb1mbi6250dg0p8"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-numderiv" ,r-numderiv)))
    (home-page "http://cran.r-project.org/web/packages/bbmle")
    (synopsis "Tools for General Maximum Likelihood Estimation")
    (description
     "Methods and functions for fitting maximum likelihood models in
R.  This package modifies and extends the 'mle' classes in the
'stats4' package.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public mageck
  (package
    (name "mageck")
    (version "0.5.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mageck/"
                                  (version-major+minor version)
                                  "/mageck-" version ".tar.gz"))
              (sha256
               (base32
                "1sc1pwjh6hsxhkiq251vijnx7sp1k0f4dmqijbz3avpd5bpbdrh7"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f
       #:modules ((guix build python-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'wrap 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (tests '(("demo1" "run.sh")
                            ("demo2" "runmageck.sh")
                            ;; FIXME: these two tests fail with
                            ;; ValueError: max() arg is an empty sequence
                            ;;("demo3" "run.sh")
                            ;;("demo4" "run.sh")
                            )))
               (setenv "PATH"
                       (string-append out "/bin:"
                                      (getenv "PATH")))
               (every (match-lambda
                        ((dir script)
                         (with-directory-excursion (string-append "demo/" dir)
                           (zero? (system* "bash" script)))))
                      tests)))))))
    (inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-statsmodels" ,python-statsmodels)
       ("python-pyqt" ,python-pyqt)
       ("r-minimal" ,r-minimal)
       ("r-xtable" ,r-xtable)
       ("r-gplots" ,r-gplots)))
    (home-page "http://mageck.sourceforge.net")
    (synopsis "Model-based analysis of genome-wide CRISPR-Cas9 Knockout")
    (description
     "Model-based Analysis of Genome-wide CRISPR-Cas9
Knockout (MAGeCK) is a computational tool to identify important genes
from the recent genome-scale CRISPR-Cas9 knockout screens
technology.  Its features include:

@enumerate
@item Simple, easy to use pipeline to screen genes in Genome-wide
   CRISPR-Cas9 Knockout experiments;
@item High sensitivity and low false discovery rate;
@item Fully utilize the screening data by performing both positive and
   negative screening in one dataset;
@item Provide statistical evaluation in genes, sgRNAs and pathways;
@item Require as few as 2 samples;
@item Identify cell-type specific targets;
@item A set of visualization features that generate publication
   standard figures.
@end enumerate\n")
    ;; It's unclear which BSD variant it is, because no copy of the
    ;; license text is included.
    (license license:bsd-3)))

(define-public python-louvain-igraph
  (package
    (name "python-louvain-igraph")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/vtraag/louvain-igraph/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hc4rjgcajm1fnq4r6xf5s4h5583c4r2qmd1ycz9i9nmxr8ymi0v"))))
    (build-system python-build-system)
    ;; There are no tests.
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-igraph" ,python-igraph)))
    (inputs
     `(("igraph" ,igraph)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/vtraag/louvain-igraph")
    (synopsis "Implementation of the Louvain algorithm")
    (description "This package implements the Louvain algorithm for
community detection in C++ and exposes it to Python.  Besides the
relative flexibility of the implementation, it also scales well, and
can be run on graphs of millions of nodes (as long as they can fit in
memory).  The core function is @code{find_partition} which finds the
optimal partition using the louvain algorithm for a number of
different methods.")
    (license license:gpl3+)))

(define-public r-savr
  (package
    (name "r-savr")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "savR" version))
       (sha256
        (base32
         "0x5lkz07rls4zsi0w0cav3bn65cbb0ayh6fclgmi0g4lb10wzqid"))))
    (properties `((upstream-name . "savR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-reshape2" ,r-reshape2)
       ("r-scales" ,r-scales)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/bcalder/savR")
    (synopsis "Parse and analyze Illumina SAV files")
    (description
     "This package provides tools to parse Illumina Sequence Analysis
Viewer (SAV) files, access data, and generate QC plots.")
    (license license:agpl3+)))

(define-public kallisto
  (package
    (name "kallisto")
    (version "0.43.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pachterlab/"
                                  "kallisto/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03j3iqhvq7ya3c91gidly3k3jvgm97vjq4scihrlxh315j696r11"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no "check" target
    (inputs
     `(("hdf5" ,hdf5)
       ("zlib" ,zlib)))
    (home-page "http://pachterlab.github.io/kallisto/")
    (synopsis "Near-optimal RNA-Seq quantification")
    (description
     "Kallisto is a program for quantifying abundances of transcripts
from RNA-Seq data, or more generally of target sequences using
high-throughput sequencing reads.  It is based on the novel idea of
pseudoalignment for rapidly determining the compatibility of reads
with targets, without the need for alignment.  Pseudoalignment of
reads preserves the key information needed for quantification, and
kallisto is therefore not only fast, but also as accurate as existing
quantification tools.")
    (license license:bsd-2)))

(define-public perl-cworld-dekker
  (package
    (name "perl-cworld-dekker")
    (version "1.01")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dekkerlab/"
                                  "cworld-dekker/archive/v" version
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h35y42yix2ivja2fi9xw7sbldy19xwyf4zaxbccismxbcqjszji"))))
    (build-system perl-build-system)
    (arguments
     `(#:modules ((guix build perl-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hardcode-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bedtools (assoc-ref inputs "bedtools"))
                   (r (assoc-ref inputs "r-minimal")))
               (substitute* '("scripts/python/getEigenVectors.py"
                              "scripts/python/matrix2EigenVectors.py")
                 (("bedtools intersect")
                  (string-append bedtools "/bin/bedtools intersect")))
               (substitute* "lib/cworld/dekker.pm"
                 (("bedtools --version")
                  (string-append bedtools "/bin/bedtools --version")))
               (substitute* '("scripts/perl/correlateMatrices.pl"
                              "scripts/perl/matrix2scaling.pl"
                              "scripts/perl/matrix2distance.pl"
                              "scripts/perl/coverageCorrect.pl"
                              "scripts/perl/matrix2anchorPlot.pl"
                              "scripts/python/matrix2EigenVectors.py"
                              "scripts/python/matrix2insulation-lite.py"
                              "scripts/perl/matrix2compartment.pl"
                              "scripts/perl/anchorPurge.pl"
                              "scripts/perl/applyCorrection.pl"
                              "scripts/perl/compareInsulation.pl"
                              "scripts/perl/fillMissingData.pl"
                              "scripts/perl/matrix2loess.pl"
                              "scripts/python/getEigenVectors.py"
                              "scripts/perl/aggregateBED.pl"
                              "scripts/perl/collapseMatrix.pl"
                              "scripts/perl/matrix2direction.pl"
                              "scripts/perl/singletonRemoval.pl"
                              "lib/cworld/dekker.pm"
                              "scripts/perl/matrix2insulation.pl")
                 (("(`|\")Rscript" _ pre)
                  (string-append pre r "/bin/Rscript")))
               #t)))
         (add-after 'install 'install-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/cworld-dekker")))
               (mkdir-p share)
               (copy-recursively "scripts" share)

               ;; Make all scripts executable and wrap them.
               (let ((r     (find-files share "\\.R$"))
                     (py    (find-files share "\\.py$"))
                     (pl    (find-files share "\\.pl$"))
                     (wrap  (lambda* (script var #:optional (extra ""))
                              (let ((path (string-append (getenv var)
                                                         extra)))
                                (wrap-program script
                                  `(,var ":" prefix (,path)))))))
                 (for-each (cut chmod <> #o555) (append r py pl))
                 (for-each (cut wrap <> "PERL5LIB"
                                (string-append ":" out
                                               "/lib/perl5/site_perl"))
                           pl)
                 (for-each (cut wrap <> "PYTHONPATH") py))
               #t))))))
    (inputs
     `(("libgd" ,gd)
       ("perl-gd" ,perl-gd)
       ("bedtools" ,bedtools)
       ("python" ,python-wrapper)
       ("python-scipy" ,python-scipy)
       ("python-numpy" ,python-numpy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-h5py" ,python-h5py)
       ("python-scikit-learn" ,python-scikit-learn)
       ("r-minimal" ,r-minimal)))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (home-page "https://github.com/dekkerlab/cworld-dekker")
    (synopsis "Utility and analysis scripts for 3C, 4C, 5C, and Hi-C data")
    (description "This package is a collection of Perl, Python, and R
scripts for manipulating 3C/4C/5C/Hi-C data.")
    (license license:asl2.0)))

(define-public r-phangorn
  (package
    (name "r-phangorn")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "phangorn" version))
       (sha256
        (base32
         "02r1b65d1v05bi62wmq0mr99dnbg7gfq4lca81blqa14yld1mj1j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-fastmatch" ,r-fastmatch)
       ("r-igraph" ,r-igraph)
       ("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-quadprog" ,r-quadprog)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/KlausVigo/phangorn")
    (synopsis "Phylogenetic analysis in R")
    (description
     "Phangorn is a package for phylogenetic analysis in R.  It
supports estimation of phylogenetic trees and networks using Maximum
Likelihood, Maximum Parsimony, distance methods and Hadamard
conjugation.")
    (license license:gpl2+)))

(define htslib-1.3
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

(define-public qtltools
  (package
    (name "qtltools")
    (version "1.1")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://qtltools.github.io/qtltools/"
                                  "binaries/QTLtools_" version
                                  "_source.tar.gz"))
              (sha256
               (base32
                "1vgr9kbah1cfgcln4mwafnr5x5akjzq3q0gx8vh3yzgn6lmln1d6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:make-flags
       (list (string-append "BOOST_INC="
                            (assoc-ref %build-inputs "boost") "/include")
             (string-append "BOOST_LIB="
                            (assoc-ref %build-inputs "boost") "/lib")
             (string-append "HTSLD_INC="
                            (assoc-ref %build-inputs "htslib") "/include")
             (string-append "HTSLD_LIB="
                            (assoc-ref %build-inputs "htslib") "/lib")
             (string-append "RMATH_INC="
                            (assoc-ref %build-inputs "rmath-standalone")
                            "/include")
             (string-append "RMATH_LIB="
                            (assoc-ref %build-inputs "rmath-standalone")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linkage
           (lambda _
             (substitute* "Makefile"
               (("libboost_iostreams.a")
                "libboost_iostreams.so")
               (("libboost_program_options.a")
                "libboost_program_options.so")
               (("-lblas") "-lopenblas"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "bin/QTLtools" bin)
               #t))))))
    (inputs
     `(("gsl" ,gsl)
       ("boost" ,boost)
       ("rmath-standalone" ,rmath-standalone)
       ("htslib" ,htslib-1.3)
       ("openblas" ,openblas)
       ("zlib" ,zlib)))
    (home-page "https://qtltools.github.io/qtltools/")
    (synopsis "Tool set for molecular QTL discovery and analysis")
    (description "QTLtools is a tool set for molecular QTL discovery
and analysis.  It allows to go from the raw genetic sequence data to
collection of molecular @dfn{Quantitative Trait Loci} (QTLs) in few
easy-to-perform steps.")
    (license license:gpl3+)))
