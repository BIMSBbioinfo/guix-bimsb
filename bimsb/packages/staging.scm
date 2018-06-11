;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2017 CM Massimo <carlomaria.massimo@mdc-berlin.de>
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
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
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
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
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
    (version "1.1.453")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rstudio/rstudio.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0caz8c0p7kgz0s524r37jycsv7clpry4k54xg02jbwzw37imag30"))
              (file-name (string-append name "-" version "-checkout"))))
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
       ("pandoc" ,ghc-pandoc-1)
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
       ("qtwebchannel" ,qtwebchannel)
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
       #:use-setuptools? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ;; TODO: make this robust!
               (("0.10.1") ,(package-version python2-flask))
               (("0.6.8") ,(package-version python2-cmd2))
               (("2.0.4") ,(package-version python2-intervaltree))
               (("2.5.1") ,(package-version python2-requests)))
             #t))
         (add-before 'build 'set-vars
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; This is needed to build the parcel library, which is
             ;; placed in $HOME/.parcel/lib.
             (setenv "HOME" "/tmp")
             ;; The installer aborts if the target directory is not on
             ;; PYTHONPATH.
             (let* ((out (assoc-ref outputs "out"))
                    (target (string-append out "/lib/python"
                                           ((@@ (guix build python-build-system)
                                                get-python-version)
                                            (assoc-ref inputs "python"))
                                           "/site-packages/")))
               (mkdir-p target)
               (setenv "PYTHONPATH"
                       (string-append target ":" (getenv "PYTHONPATH"))))
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
       #:use-setuptools? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("(lxml==).*," _ pre)
                (string-append pre ,(package-version python2-lxml) "',\n"))
               (("(PyYAML==).*," _ pre)
                (string-append pre ,(package-version python2-pyyaml) "',\n"))
               (("(jsonschema==).*," _ pre)
                (string-append pre ,(package-version python2-jsonschema) "',\n"))
               (("setuptools==") "setuptools>="))
             #t)))))
    (inputs
     `(("python2-parcel" ,python2-parcel-for-gdc-client)
       ("python2-jsonschema" ,python2-jsonschema)
       ("python2-pyyaml" ,python2-pyyaml)
       ("python2-lxml" ,python2-lxml)
       ("python2-functools32" ,python2-functools32)
       ("python2-setuptools" ,python2-setuptools)))
    (home-page "https://gdc.nci.nih.gov/access-data/gdc-data-transfer-tool")
    (synopsis "GDC data transfer tool")
    (description "The gdc-client provides several convenience
functions over the GDC API which provides general download/upload via
HTTPS.")
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
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreenleafLab/NucleoATAC.git")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "00mmyyiyagviksqs9rk12saa3949wxy25bllwhvia0sqj56v6shn"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           ;; The tests need a valid HOME directory
           (lambda _ (setenv "HOME" (getcwd)) #t))
         ;; The default check phase just tells us to run "python
         ;; tests.py", so that's what we're doing.
         (replace 'check
           (lambda _ (invoke "python" "tests.py"))))))
    (inputs
     `(("python-pandas" ,python2-pandas)
       ("python-numpy" ,python2-numpy)
       ("python-scipy" ,python2-scipy)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-pysam" ,python2-pysam)))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)
       ("python-nose" ,python2-nose)
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
             #t))
         ;; This is necessary for ABI compatibility when linking with
         ;; the Boost 1.55 which has been built with GCC 4.9.  This
         ;; wouldn't be necessary if we managed to build Boost with
         ;; GCC 5.  See https://stackoverflow.com/a/30668880/519736
         (add-after 'unpack 'use-old-abi
           (lambda _
             (substitute* "Makefile.in"
               (("^CPPFLAGS = " line)
                (string-append line "-D_GLIBCXX_USE_CXX11_ABI=0 ")))
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
       ;; FIXME: this is currently broken:
       ;;        dStorm/display/rapidstorm-store_image.o: In function `make_key_image':
       ;; /tmp/guix-build-rapidstorm-3.3.0.drv-0/rapidstorm-3.3.0/dStorm/display/store_image.cpp:143: undefined reference to `Magick::Image::annotate(std::string const&, Magick::Geometry const&, MagickLib::GravityType, double)'
       ;; /tmp/guix-build-rapidstorm-3.3.0.drv-0/rapidstorm-3.3.0/dStorm/display/store_image.cpp:153: undefined reference to `Magick::Image::annotate(std::string const&, Magick::Geometry const&, MagickLib::GravityType, double)'
       ;; dStorm/display/rapidstorm-store_image.o: In function `write_scale_bar':
       ;; /tmp/guix-build-rapidstorm-3.3.0.drv-0/rapidstorm-3.3.0/dStorm/display/store_image.cpp:202: undefined reference to `Magick::Image::annotate(std::string const&, Magick::Geometry const&, MagickLib::GravityType)'
       ;; collect2: error: ld returned 1 exit status
       ("graphicsmagick" ,graphicsmagick)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ;; For documentation
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ;;("doxygen" ,doxygen)
       ("libxslt" ,libxslt)
       ("texlive" ,texlive-tiny)
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

(define-public r-chipcomp
  (package
    (name "r-chipcomp")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ChIPComp" version))
       (sha256
        (base32
         "1nj3903nhii76v6jrm7bfw8kngbq525zikb3nwhvmp4905c77z40"))))
    (properties `((upstream-name . "ChIPComp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-bsgenome-hsapiens-ucsc-hg19" ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-bsgenome-mmusculus-ucsc-mm9" ,r-bsgenome-mmusculus-ucsc-mm9)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-limma" ,r-limma)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://bioconductor.org/packages/ChIPComp")
    (synopsis "Quantitative comparison of multiple ChIP-seq datasets")
    (description
     "ChIPComp detects differentially bound sharp binding sites across
multiple conditions considering matching control in ChIP-seq
datasets.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

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

(define-public python2-pyml
  (package
    (name "python2-pyml")
    (version "0.7.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pyml/PyML-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1p8y0b597x1jb6q9b4k8q6r8wv8wbja9lav3bwi4mwdaa3pcjql4"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))   ; python2 only
    (inputs
     `(("python2-numpy" ,python2-numpy)))
    (home-page "http://pyml.sourceforge.net")
    (synopsis "Interactive object oriented framework for machine learning")
    (description "PyML is an interactive object oriented framework for
machine learning written in Python.  PyML is focused on kernel-methods
for classification and regression, including Support Vector
Machines (SVM).  It provides tools for feature selection, model
selection, syntax for combining classifiers and methods for assessing
classifier performance.")
    (license license:gpl2+)))

(define-public python-multicore-tsne
  (let ((commit "e1a40182068d4815e7fbe523db266caab20773ff")
        (revision "1"))
    (package
      (name "python-multicore-tsne")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DmitryUlyanov/Multicore-TSNE.git")
               (commit commit)))
         (sha256
          (base32
           "1k5vvsak19ap75rvanll0k71j5fi4kpyj83rl1006v4h04hwv1x6"))))
      (build-system python-build-system)
      ;; Tests need python-cffi and pypi/psutils (the latter not in
      ;; guix)
      (arguments `(#:tests? #f))
      (native-inputs
       `(("cmake" ,cmake)))
      (home-page "https://github.com/DmitryUlyanov/Multicore-TSNE")
      (synopsis "Parallel t-SNE implementation with Python and Torch wrappers")
      (description
       "This package contains a multicore Barnes-Hut implementation of
the t-SNE algorithm.  The implementation is described here:
@url{http://lvdmaaten.github.io/publications/papers/JMLR_2014.pdf}.")
      (license license:bsd-3))))

(define-public umi-tools5
  (package
  (inherit umi-tools)
  (name "umi-tools")
  (version "0.5.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "umi_tools" version))
      (sha256
       (base32
        "04i9cxj3jf08pr0pafh7b0h4rn8cnkrsb2lzsai8zj9hgnka0dd5"))))
  (inputs
  `(("python-setuptools" ,python-setuptools)
    ("python-pandas" ,python-pandas)
    ("python-future" ,python-future)
    ("python-scipy" ,python-scipy)
    ("python-matplotlib" ,python-matplotlib)
    ("python-regex" ,python-regex)
    ("python-pysam" ,python-pysam)))))

(define-public footprint-pipeline
  (package
    (name "footprint-pipeline")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://bimsbstatic.mdc-berlin.de/"
                                  "ohler/asli/footprint-pipeline-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0aws3r67f04sfg0gsji4k29k7v2k9k4m0dyyw2lahkjwlvya5isx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'fix-broken-shebang
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "run_footprinting.R.in"
               (("@RSCRIPT@") (which "Rscript")))
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/find_footprints.sh")
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))))
             #t)))))
    (inputs
     `(("r-minimal" ,r-minimal)
       ("r-mixtools" ,r-mixtools)
       ("r-gtools" ,r-gtools)
       ("r-genomicranges" ,r-genomicranges)
       ("perl" ,perl)
       ("samtools" ,samtools-1.1)
       ("bedtools" ,bedtools-2.18)))
    (home-page "https://ohlerlab.mdc-berlin.de/software/Reproducible_footprinting_139/")
    (synopsis "Find transcription factor footprints in ATAC-seq or DNase-seq data")
    (description "This is a pipeline to find transcription factor
footprints in ATAC-seq or DNase-seq data.")
    ;; TODO: there is no license!
    (license license:gpl3+)))

(define-public cpat
  (package
    (name "cpat")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/rna-cpat/v"
                                  version "/CPAT-" version ".tar.gz"))
              (sha256
               (base32
                "0wqcj3p2r3b368504c10ggnmwd98ij849q3fnp04r0ax2x56448h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-download-setuptools
           (lambda _
             (substitute* "setup.py"
               (("use_setuptools\\(\\)")
                "import setuptools"))
             #t)))))
    ;; This package bundles a possibly modified copy of the samtools
    ;; and pysam sources.
    (inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-cython" ,python2-cython)
       ("r-minimal" ,r-minimal)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python2-nose" ,python2-nose)))
    (home-page "http://rna-cpat.sourceforge.net/")
    (synopsis "Alignment-free distinction between coding and noncoding RNA")
    (description "CPAT is a method to distinguish coding and noncoding
RNA by using a logistic regression model based on four pure
sequence-based, linguistic features: ORF size, ORF coverage, Ficket
TESTCODE, and Hexamer usage bias.  Linguistic features based method
does not require other genomes or protein databases to perform
alignment and is more robust. Because it is alignment-free, it runs
much faster and also easier to use.")
    ;; TODO: There is some license confusion.  The website says
    ;; GPLv2+, setup.py says "Artistic License, see COPYING",
    ;; "COPYING" contains the Expat license.
    (license license:gpl2+)))

(define-public samstat
  (package
    (name "samstat")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/samstat/"
                                  "samstat-" version ".tar.gz"))
              (sha256
               (base32
                "0hq5fsialpdv4dnhzws3nwiwzhhlhnf1438vg8h81yr63f84dw66"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-samtools-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/io.c"
               (("samtools")
                (string-append (assoc-ref inputs "samtools")
                               "/bin/samtools")))
             #t)))))
    (inputs
     `(("samtools" ,samtools)))
    (home-page "http://samstat.sourceforge.net/")
    (synopsis "Sequence statistics for next generation sequencing")
    (description "SAMStat displays various properties of
next-generation genomic sequencing reads stored in SAM/BAM format.")
    (license license:gpl3+)))

(define-public splicegrapher
  (package
    (name "splicegrapher")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/splicegrapher/"
                                  "SpliceGrapher-" version ".tgz"))
              (sha256
               (base32
                "1z5vwk7n1fa9g34qmz6dmc2s99qhk7s7zhaymyhzgc1mgpqlbq8q"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python2-pyml" ,python2-pyml)
       ("python2-matplotlib" ,python2-matplotlib)))
    (home-page "http://splicegrapher.sourceforge.net/")
    (synopsis "Predict alternative splicing patterns")
    (description "SpliceGrapher predicts alternative splicing patterns
and produces splice graphs that capture in a single structure the ways
a gene's exons may be assembled.  It enhances gene models using
evidence from next-generation sequencing and EST alignments.")
    (license license:lgpl2.1+)))

(define-public r-riboprofiling
  (package
    (name "r-riboprofiling")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RiboProfiling" version))
       (sha256
        (base32
         "1w969imn7lizz56nsxaydfx678p01z457rf3rpc222qbm54p0ynn"))))
    (properties `((upstream-name . "RiboProfiling")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-data-table" ,r-data-table)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iranges" ,r-iranges)
       ("r-plyr" ,r-plyr)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-sqldf" ,r-sqldf)))
    (home-page "https://bioconductor.org/packages/RiboProfiling/")
    (synopsis "Ribosome profiling data analysis")
    (description "Starting with a BAM file, this package provides the
necessary functions for quality assessment, read start position
recalibration, the counting of reads on CDS, 3'UTR, and 5'UTR, and
plotting of count data: pairs, log fold-change, codon frequency and
coverage assessment, principal component analysis on codon coverage.")
    (license license:gpl3)))

(define-public r-bayseq
  (package
    (name "r-bayseq")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "baySeq" version))
       (sha256
        (base32
         "1d918rmbzqncr7jw6byr3xyqybvcsgcyf96immqcl1c5xvgmqn5z"))))
    (properties `((upstream-name . "baySeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-edger" ,r-edger)
       ("r-genomicranges" ,r-genomicranges)))
    (home-page "https://bioconductor.org/packages/baySeq/")
    (synopsis "Empirical Bayesian analysis of patterns of differential expression in count data")
    (description
     "This package identifies differential expression in
high-throughput count data, such as that derived from next-generation
sequencing machines, calculating estimated posterior likelihoods of
differential expression (or more complex hypotheses) via empirical
Bayesian methods.")
    (license license:gpl3)))

(define-public r-riboseqr
  (package
    (name "r-riboseqr")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "riboSeqR" version))
       (sha256
        (base32
         "0fg2zn5az1djdjnsrli26rl53ljrrjzmspx02dmgwf1a7nm7qmfz"))))
    (properties `((upstream-name . "riboSeqR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-bayseq" ,r-bayseq)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-seqlogo" ,r-seqlogo)))
    (home-page "https://bioconductor.org/packages/riboSeqR/")
    (synopsis "Analysis of sequencing data from ribosome profiling experiments")
    (description
     "This package provides plotting functions, frameshift detection
and parsing of genetic sequencing data from ribosome profiling
experiments.")
    (license license:gpl3)))

(define-public r-wasabi
  (let ((commit "f31c73eed6bcb9d0be43b607c14211dd899e5a6c")
        (revision "1"))
    (package
      (name "r-wasabi")
      (version (string-append "0.2-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/COMBINE-lab/wasabi.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1phmn030yi92z17ijpwj99q7p681k44nj2vr8q6vxcrl6qgriram"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-data-table" ,r-data-table)
         ("r-rjson" ,r-rjson)
         ("r-rhdf5" ,r-rhdf5)))
      (home-page "https://github.com/COMBINE-lab/wasabi")
      (synopsis "Prepare Sailfish and Salmon output for downstream analysis")
      (description
       "Wasabi allows you to easily prepare the output of the RNA-seq
quantification tools Sailfish and Salmon output for downstream
analysis.  Currently, its main purpose it to prepare output for
downstream analysis with sleuth.")
      (license license:bsd-3))))

(define-public r-sleuth
  (package
    (name "r-sleuth")
    (version "0.29.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pachterlab/sleuth/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05qad0hc0gixrd41vd70xncb9a6hhq0zls746p0s1w7qm71m6b2d"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-tidyr" ,r-tidyr)
       ("r-reshape2" ,r-reshape2)
       ("r-rhdf5" ,r-rhdf5)
       ("r-lazyeval" ,r-lazyeval)
       ("r-matrixstats" ,r-matrixstats)
       ("r-shiny" ,r-shiny)
       ("r-mass" ,r-mass)
       ("r-testthat" ,r-testthat)
       ("r-knitr" ,r-knitr)))
    (home-page "http://pachterlab.github.io/sleuth")
    (synopsis "Differential analysis of RNA-Seq data")
    (description
     "Sleuth is a program for differential analysis of RNA-Seq data.
It makes use of quantification uncertainty estimates obtained via
Kallisto for accurate differential analysis of isoforms or genes,
allows testing in the context of experiments with complex designs, and
supports interactive exploratory data analysis via sleuth live.")
    (license license:gpl3)))

(define-public python2-nanoraw
  (package
    (name "python2-nanoraw")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nanoraw" version))
       (sha256
        (base32
         "1y974jjzb8w9q2y2vc0lijncp0qpzqqkrfwrsa18nglgq4n6dzj4"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-h5py" ,python2-h5py)))
    (native-inputs
     `(("python2-nose2" ,python2-nose2)))
    (home-page "https://github.com/marcus1487/nanoraw")
    (synopsis "Analysis of nanopore sequencing data")
    (description "This package provides tools for the analysis of raw
nanopore sequencing data, including correction of basecalls and
visualization.")
    (license license:bsd-3)))

(define-public libgff
  (package
    (name "libgff")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Kingsford-Group/"
                    "libgff/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vc4nxyhlm6g9vvmx5l4lfs5pnvixsv1hiiy4kddf2y3p6jna8ls"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no tests included
    (home-page "https://github.com/Kingsford-Group/libgff")
    (synopsis "Parser library for reading/writing GFF files")
    (description
     "This is a simple \"libraryfication\" of the GFF/GTF parsing code that is
used in the Cufflinks codebase.  The goal of this library is to provide this
functionality without the necessity of drawing in a heavy-weight dependency
like SeqAn.")
    (license (license:x11-style "http://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))

(define-public python-pyfasta
  (package
    (name "python-pyfasta")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyfasta" version))
       (sha256
        (base32
         "0n5j8l7dys3bqfyz6vkryhc2gjlwbmymc41xjf8vqlq2m5gxf25b"))))
    (build-system python-build-system)
    ;; The tests cannot be run even after setting PYTHONPATH.  That's
    ;; because of relative module imports.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/brentp/pyfasta/")
    (synopsis "Pythonic access to fasta sequence files")
    (description
     "This library provides fast, memory-efficient, pythonic (and
command-line) access to fasta sequence files.  It stores a flattened
version of a fasta sequence file without spaces or headers and uses
either a @code{mmap} in numpy binary format or
@code{fseek}/@code{fread} so the sequence data is never read into
memory.  It saves a pickle (@code{.gdx}) of the start and stop (for
@code{fseek}/@code{mmap}) locations of each header in the fasta file
for internal use.")
    (license license:expat)))

(define-public python2-pyfasta
  (package-with-python2 python-pyfasta))

(define-public python2-ont-tombo
  (package
    (name "python2-ont-tombo")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ont-tombo" version))
       (sha256
        (base32
         "07ixvxw2nwv8g2skg8anlacdkxk7vrl4w9999gjzaaj87adda3p7"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python-h5py" ,python2-h5py)
       ("python-numpy" ,python2-numpy)
       ("python-scipy" ,python2-scipy)
       ("python-scikit-learn" ,python2-scikit-learn)))
    (native-inputs
     `(("python-nose2" ,python2-nose2)
       ("python-cython" ,python2-cython)))
    (home-page "https://github.com/nanoporetech/tombo")
    (synopsis "Analysis of raw nanopore sequencing data")
    (description "Tombo is a suite of tools primarily for the
identification of modified nucleotides from nanopore sequencing
data. Tombo also provides tools for the analysis and visualization of
raw nanopore signal.")
    (license license:mpl2.0)))

(define old-r-dropbead
  (let ((commit "41f27229371ad0addeb9996f5ce5ca5d86c07549")
        (revision "1"))
    (package (inherit r-dropbead)
      (name "old-r-dropbead")
      (version (string-append "0-" revision "." (string-take commit 7)))
      (source
       (origin (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/rajewsky-lab/dropbead.git")
                     (commit commit)))
               (file-name (string-append "r-dropbead-" version "-checkout"))
               (sha256
                (base32
                 "1mw4nm8bq5ia4wia56dv48h8806s74bghgp8i0gh6f4q3j983adw")))))))

(define-public pigx-scrnaseq
  (package
    (name "pigx-scrnaseq")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/"
                                  "pigx_scrnaseq/releases/download/v"
                                  version "/pigx_scrnaseq-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1p3594b51v4nd9k3w9djrhf24r3234qcm6jk3h2hc5gjj7kc12d1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "PICARDJAR=" (assoc-ref %build-inputs "java-picard")
                            "/share/java/picard.jar")
             (string-append "DROPSEQJAR=" (assoc-ref %build-inputs "dropseq-tools")
                            "/share/java/dropseq-tools/dropseq.jar"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-CLASSPATH
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH" (string-append (assoc-ref inputs "java-picard")
                                                "/share/java/picard.jar:"
                                                (assoc-ref inputs "dropseq-tools")
                                                "/share/java/dropseq-tools/dropseq.jar"))
             #t))
         (add-after 'install 'wrap-executable
           ;; Make sure the executable finds all R modules.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/pigx-scrnaseq")
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))
                 `("PYTHONPATH"  ":" = (,(getenv "PYTHONPATH")))
                 `("CLASSPATH"   ":" = (,(getenv "CLASSPATH")))))
             #t)))))
    (native-inputs
     `(("javac" ,icedtea-8 "jdk")))
    (inputs
     `(("dropseq-tools" ,dropseq-tools)
       ("fastqc" ,fastqc)
       ("java-picard" ,java-picard)
       ("java" ,icedtea-8)
       ("python-wrapper" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pandas" ,python-pandas)
       ("python-numpy" ,python-numpy)
       ("python-loompy" ,python-loompy)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-pandoc-citeproc" ,ghc-pandoc-citeproc)
       ("snakemake" ,snakemake)
       ("star" ,star)
       ("r-minimal" ,r-minimal)
       ("r-argparser" ,r-argparser)
       ("r-cowplot" ,r-cowplot)
       ("r-data-table" ,r-data-table)
       ("r-delayedarray" ,r-delayedarray)
       ("r-delayedmatrixstats" ,r-delayedmatrixstats)
       ("r-dplyr" ,r-dplyr)
       ("r-dropbead" ,old-r-dropbead)
       ("r-dt" ,r-dt)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hdf5array" ,r-hdf5array)
       ("r-pheatmap" ,r-pheatmap)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rtsne" ,r-rtsne)
       ("r-scater" ,r-scater)
       ("r-scran" ,r-scran)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-stringr" ,r-stringr)
       ("r-yaml" ,r-yaml)))
    (home-page "https://github.com/BIMSBbioinfo/pigx_scrnaseq/")
    (synopsis "Analysis pipeline for single-cell RNA sequencing experiments")
    (description "PiGX scRNAseq is an analysis pipeline for
preprocessing and quality control for single cell RNA sequencing
experiments.  The inputs are read files from the sequencing
experiment, and a configuration file which describes the experiment.
It produces processed files for downstream analysis and interactive
quality reports.  The pipeline is designed to work with UMI based
methods.")
    (license license:gpl3+)))

(define-public r-tgutil
  (package
    (name "r-tgutil")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://tanaylab.bitbucket.io/repo/src/"
                           "contrib/tgutil_" version ".tar.gz"))
       (sha256
        (base32
         "1qgrywkdr4a2dzmki9vyaj0nbcqm0x80d5m7g385wh92xrpvvipy"))))
    (build-system r-build-system)
    (inputs
     `(("hdf5" ,hdf5)))
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-tibble" ,r-tibble)
       ("r-matrixstats" ,r-matrixstats)
       ("r-data-table" ,r-data-table)
       ("r-broom" ,r-broom)))
    (home-page "https://bitbucket.org/tanaylab/tgutil/")
    (synopsis "Utility functions for packages maintained by the Tanay lab")
    (description
     "This package provides simple utility functions that are shared
across several packages maintained by the Tanay lab.")
    (license license:gpl3)))

(define-public r-interactionset
  (package
    (name "r-interactionset")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "InteractionSet" version))
       (sha256
        (base32
         "1wmp4dqxj19dbd97r6zfzv81j06vh9j7bpypcxib8f2lyx26cwm9"))))
    (properties
     `((upstream-name . "InteractionSet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "https://bioconductor.org/packages/InteractionSet")
    (synopsis "Base classes for storing genomic interaction data")
    (description
     "This packages provides the @code{GInteractions},
@code{InteractionSet} and @code{ContactMatrix} objects and associated
methods for storing and manipulating genomic interaction data from
Hi-C and ChIA-PET experiments.")
    (license license:gpl3)))

(define-public r-genomicinteractions
  (package
    (name "r-genomicinteractions")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicInteractions" version))
       (sha256
        (base32
         "0f0ki2zsaxg6f4qr47xgyhxm6jvms0s1zab7f0vcnw8jd7vhmnzn"))))
    (properties
     `((upstream-name . "GenomicInteractions")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gviz" ,r-gviz)
       ("r-igraph" ,r-igraph)
       ("r-interactionset" ,r-interactionset)
       ("r-iranges" ,r-iranges)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-stringr" ,r-stringr)))
    (home-page "https://github.com/ComputationalRegulatoryGenomicsICL/GenomicInteractions/")
    (synopsis "R package for handling genomic interaction data")
    (description
     "This R package provides tools for handling Genomic interaction
data, such as ChIA-PET/Hi-C, annotating genomic features with
interaction information and producing various plots / statistics.")
    (license license:gpl3)))

(define-public r-h5
  (package
    (name "r-h5")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "h5" version))
       (sha256
        (base32
         "14p7i1sj24ky87kd7qr3n9fc9l64s0bp0rwbyl6i2x69xn75gpsx"))))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("hdf5" ,hdf5)))
    (native-inputs
     `(("which" ,(@ (gnu packages base) which))))
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/mannau/h5")
    (synopsis "Interface to the HDF5 Library")
    (description
     "This package provides an S4 interface to the HDF5 library
supporting fast storage and retrieval of R-objects like vectors,
matrices and arrays to binary files in a language independent format.
The HDF5 format can therefore be used as an alternative to R's
save/load mechanism.  Since h5 is able to access only subsets of
stored data it can also handle data sets which do not fit into
memory.")
    (license license:bsd-2)))

(define-public ensembl-vep
  (let* ((api-version "92")
         (api-module
          (lambda (name hash)
            (origin (method git-fetch)
                    (uri (git-reference
                          (url (string-append "https://github.com/Ensembl/"
                                              name ".git"))
                          (commit (string-append "release/" api-version))))
                    (file-name (string-append name "-" api-version "-checkout"))
                    (sha256 (base32 hash))))))
    (package
      (name "ensembl-vep")
      (version (string-append api-version ".1"))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ensembl/ensembl-vep.git")
               (commit (string-append "release/" version))))
         (sha256
          (base32
           "0qsqjvbvcapfkhadicr95npkg98fp0c60zm2jafw6z4v79174cs5"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 match))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           ;; Tests need to run after installation
           (delete 'check)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((modules '(("ensembl" "/")
                                 ("ensembl-variation" "/Variation")
                                 ("ensembl-funcgen"   "/Funcgen")
                                 ("ensembl-io"        "/IO")))
                      (scripts '(("convert_cache.pl" "vep_convert_cache.pl")
                                 ("INSTALL.pl"       "vep_install.pl")
                                 ("haplo"            #f)
                                 ("variant_recoder"  #f)
                                 ("filter_vep"       #f)
                                 ("vep"              #f)))
                      (out  (assoc-ref outputs "out"))
                      (bin  (string-append out "/bin"))
                      (perl (string-append out "/lib/perl5/site_perl")))
                 (for-each
                  (match-lambda
                    ((name path)
                     (let ((dir (string-append perl "/Bio/EnsEMBL" path)))
                       (mkdir-p dir)
                       (copy-recursively
                        (string-append (assoc-ref inputs (string-append "api-module-" name))
                                       "/modules/Bio/EnsEMBL" path)
                        dir))))
                  modules)
                 (copy-recursively "modules/" perl)
                 (mkdir-p bin)
                 (for-each
                  (match-lambda
                    ((script new-name)
                     (let ((location (string-append bin "/"
                                                    (or new-name (basename script)))))
                       (copy-file script location)
                       (chmod location #o555)
                       (wrap-program location
                         `("PERL5LIB" ":" = (,(getenv "PERL5LIB")
                                             ,perl))))))
                  scripts)

                 ;; Fix path to tools
                 (with-directory-excursion (string-append perl "/Bio/EnsEMBL")
                   (substitute* '("Funcgen/RunnableDB/ProbeMapping/PrePipelineChecks.pm"
                                  "VEP/BaseRunner.pm"
                                  "VEP/Utils.pm"
                                  "VEP/AnnotationSource/Cache/VariationTabix.pm"
                                  "VEP/AnnotationSource/Cache/BaseSerialized.pm"
                                  "Variation/Utils/BaseVepTabixPlugin.pm"
                                  "Variation/Utils/VEP.pm"
                                  "Variation/Pipeline/ReleaseDataDumps/PreRunChecks.pm")
                     (("`which")
                      (string-append "`"
                                     (assoc-ref inputs "which")
                                     "/bin/which"))))
                 #t)))
           ;; FIXME: there are test, but they are tricky to run.
           (add-after 'install 'check
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke (string-append (assoc-ref outputs "out")
                                      "/bin/vep")
                       "--help"))))))
      ;; TODO: haplo needs Set/IntervalTree.pm
      (inputs
       `(("bioperl-minimal" ,bioperl-minimal)
         ("perl-dbi" ,perl-dbi)
         ("perl-dbd-mysql" ,perl-dbd-mysql)
         ("perl-libwww" ,perl-libwww)
         ("perl-http-tiny" ,perl-http-tiny)
         ("perl-json" ,perl-json)
         ("which" ,(@ (gnu packages base) which))))
      (propagated-inputs
       `(("kentutils" ,kentutils)))
      (native-inputs
       `(("unzip" ,unzip)
         ("api-module-ensembl"
          ,(api-module "ensembl"
                       "106yz0kg38zqxxdsrppisbw6d4gx0r9bn7nh4xvh3h0pmlc21smj"))
         ("api-module-ensembl-variation"
          ,(api-module "ensembl-variation"
                       "0vxsigcgqqdg9jdzrsl48g1gd97ps25a7nas9bckfwj8qjvrg9ni"))
         ("api-module-ensembl-funcgen"
          ,(api-module "ensembl-funcgen"
                       "0hhzvam51gcywr0s7pamw23nniag2wby4vnpql83mzwlnlrsrf98"))
         ("api-module-ensembl-io"
          ,(api-module "ensembl-io"
                       "1gylvpik0npgarfppccxw0kmrz9lw30rgzz7vbdb2x4b2cxygbkz"))
         ("perl-test-harness" ,perl-test-harness)
         ("perl-test-exception" ,perl-test-exception)))
      (home-page "http://www.ensembl.org/vep")
      (synopsis "Predict functional effects of genomic variants")
      (description
       "This package provides a Variant Effect Predictor, which predicts
the functional effects of genomic variants.  It also provides
Haplosaurus, which uses phased genotype data to predict
whole-transcript haplotype sequences, and Variant Recoder, which
translates between different variant encodings.")
      (license license:asl2.0))))

(define-public r-ggsci
  (package
    (name "r-ggsci")
    (version "2.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggsci" version))
       (sha256
        (base32
         "0g73x6grbka7ahjh6z23m3wrcifp5rdfdiasbl8lq4sp6rplxwaa"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-scales" ,r-scales)))
    (home-page "https://nanx.me/ggsci/")
    (synopsis "Scientific journal and sci-fi themed color palettes for ggplot2")
    (description
     "This package provides a collection of ggplot2 color palettes
inspired by plots in scientific journals, data visualization
libraries, science fiction movies, and TV shows.")
    (license license:gpl3)))

(define-public r-ggsignif
  (package
    (name "r-ggsignif")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggsignif" version))
       (sha256
        (base32
         "1rn58d7pb3axk6chiihryykrzw76adaa2yiafq4d0j6qbhax78f7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)))
    (home-page "https://github.com/const-ae/ggsignif")
    (synopsis "Significance brackets for ggplot2")
    (description
     "Enrich your ggplots with group-wise comparisons.  This package
provides an easy way to indicate if two groups are significantly
different.  Commonly this is shown by a bracket on top connecting the
groups of interest which itself is annotated with the level of
significance.  The package provides a single layer that takes the
groups for comparison and the test as arguments and adds the
annotation to the plot.")
    (license license:gpl3)))

(define-public r-ggpubr
  (package
    (name "r-ggpubr")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggpubr" version))
       (sha256
        (base32
         "0mvw215bj887958p34f0dzlrb8mgyfcz9b5zvsschvbhamqinqna"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cowplot" ,r-cowplot)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-ggsci" ,r-ggsci)
       ("r-ggsignif" ,r-ggsignif)
       ("r-gridextra" ,r-gridextra)
       ("r-magrittr" ,r-magrittr)
       ("r-purrr" ,r-purrr)
       ("r-scales" ,r-scales)
       ("r-tidyr" ,r-tidyr)))
    (home-page "http://www.sthda.com/english/rpkgs/ggpubr")
    (synopsis "ggplot2-based publication-ready plots")
    (description
     "The ggplot2 package is an excellent and flexible package for
elegant data visualization in R.  However the default generated plots
require some formatting before we can send them for publication.  The
ggpubr package provides some easy-to-use functions for creating and
customizing ggplot2-based publication-ready plots.")
    (license license:gpl2)))

(define-public r-ellipse
  (package
    (name "r-ellipse")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ellipse" version))
       (sha256
        (base32
         "0g82vc51m3c1k0hnpp2zla6amxxgk2mmkl8ssnsc49jv3599r6hs"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ellipse/")
    (synopsis "Functions for drawing ellipses and ellipse-like confidence regions")
    (description
     "This package contains various routines for drawing ellipses and
ellipse-like confidence regions, implementing the plots described in
Murdoch and Chow (1996), A graphical display of large correlation
matrices, The American Statistician 50, 178-180.  There are also
routines implementing the profile plots described in Bates and
Watts (1988), Nonlinear Regression Analysis and its Applications.")
    (license license:gpl2+)))

(define-public r-flashclust
  (package
    (name "r-flashclust")
    (version "1.01-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flashClust" version))
       (sha256
        (base32
         "0l4lpz451ll7f7lfxmb7ds24ppzhfg1c3ypvydglcc35p2dq99s8"))))
    (properties `((upstream-name . "flashClust")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/flashClust/")
    (synopsis "Implementation of optimal hierarchical clustering")
    (description
     "This package provides a fast implementation of hierarchical
clustering.")
    (license license:gpl2+)))
