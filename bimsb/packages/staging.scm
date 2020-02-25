;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2017 CM Massimo <carlomaria.massimo@mdc-berlin.de>
;;; Copyright © 2018, 2019 Marcel Schilling <marcel.schilling@mdc-berlin.de>
;;; Copyright © 2019, 2020 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
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
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fabric-management)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages jemalloc)
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
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
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
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (bimsb packages variants)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

;; We can't upgrade to 1.2.x because that depends on qtwebengine,
;; which bundles Chromium.
(define-public rstudio-server
  (package
    (name "rstudio-server")
    (version "1.1.463")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rstudio/rstudio.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "014g984znsczzy1fyn9y1ly3rbsngryfs674lfgciz60mqnl8im6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DRSTUDIO_TARGET=Server")
       #:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-type-error
           (lambda _
             (substitute* "src/cpp/core/DateTime.cpp"
               (("return time_t_epoch \\+ seconds\\(sec\\);")
                "return time_t_epoch + seconds(static_cast<long>(sec));"))
             (substitute* "src/cpp/core/file_lock/FileLock.cpp"
               (("= boost::posix_time::seconds\\((timeoutInterval|refreshRate)\\)" _ val)
                (string-append "= boost::posix_time::seconds(static_cast<long>("
                               val "))"))
               (("FileLock::s_timeoutInterval\\(kDefaultTimeoutInterval\\)")
                "FileLock::s_timeoutInterval(static_cast<long>(kDefaultTimeoutInterval))")
               (("FileLock::s_refreshRate\\(kDefaultRefreshRate\\)")
                "FileLock::s_refreshRate(static_cast<long>(kDefaultRefreshRate))"))
             #t))
         (add-after 'unpack 'override-tmpdir
           (lambda _
             ;; The location of the server cookie is hardcoded to
             ;; /tmp/rstudio-server; this makes it impossible for
             ;; different users on the same machine to use R Studio.
             (substitute* "src/cpp/server/ServerSecureKeyFile.cpp"
               (("\"/tmp/rstudio-server\"")
                "getenv(\"HOME\")"))
             #t))
         (add-before 'build 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This is needed for Java, obviously.
             (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
             (match (string-split ,version #\.)
               ((major minor patch)
                (setenv "RSTUDIO_VERSION_MAJOR" major)
                (setenv "RSTUDIO_VERSION_MINOR" minor)
                (setenv "RSTUDIO_VERSION_PATCH" patch)))
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
               (invoke "unzip" "-qd" "dictionaries"
                       (assoc-ref inputs "dictionaries")))
             #t))
         (add-after 'unpack 'unpack-mathjax
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "dependencies/common"
               (mkdir "mathjax-26")
               (invoke "unzip" "-qd" "mathjax-26"
                       (assoc-ref inputs "mathjax")))
             #t))
         (add-after 'unpack 'unpack-gin
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/gwt"
               (install-file (assoc-ref inputs "junit") "lib")
               (mkdir-p "lib/gin/1.5")
               (invoke "unzip" "-qd" "lib/gin/1.5"
                       (assoc-ref inputs "gin")))
             #t))
         (add-after 'unpack 'unpack-gwt
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/gwt"
               (mkdir-p "lib/gwt")
	           (invoke "unzip" "-qd" "lib/gwt"
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
       ;; Boost 1.69 no longer offers Boost.Signals; Rstudio does not
       ;; yet support Boost.Signals2, so we need to use an older
       ;; version of Boost.
       ("boost" ,boost-1.68)
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WebLogo/weblogo.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14hpkpzrm5prgd392c047r15p674912d9mg79scvzs2rkf8ayp6n"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LabAdvComp/parcel.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1r99c075m4a3j1h0hsi7bxm37cp1fsr6x7lkwhwcl5c13hn0wsml"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NCI-GDC/gdc-client.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10822bd1n75b7iqcf74ddx05izl3lz0cc5ckr6s7a7yrnzjvqzcx"))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lammps/lammps.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16x8xz68y3fxnk0g6dnq17fjf48a1pkd7s6jbywnnz8qprg5pblp"))))
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
               (invoke "make" (string-append "HDF5_PATH="
                                             (assoc-ref inputs "hdf5"))))
             (invoke "make"
                     "yes-molecule"
                     "yes-granular"
                     "yes-user-h5md"
                     "yes-user-misc")
             #t))
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
  (let ((commit "f912ad5689220c32844fd8faa36d521a89271e60")
        (revision "0"))
    (package
      (name "rapidstorm")
      (version (git-version "3.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stevewolter/rapidSTORM.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kngg83g6f4kcj2b656mgg4kgh2frinwwbgh5gjxkfiw01573rsi"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "XSLT_FLAGS=--nonet --novalid"
                            "BIB2XML=touch")
         ;; There is one test and it fails because runtest cannot be
         ;; found.  It also says this:
         ;;     Test setup error: test unit with name 'gaussian_psf'
         ;;     registered multiple times in the test suite 'Master
         ;;     Test Suite'
         #:tests? #f
         #:configure-flags
         (list "--enable-documentation=no")
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'build-guilabels
             (lambda _
               (with-directory-excursion "doc"
                 (invoke "xsltproc"
                         "--nonet"
                         "--novalid"
                         "--path" ".:"
                         "--xinclude"
                         "-o" "guilabels.h" "rapidstorm_guilabels.xsl"
                         "./rapidstorm.xml"))))
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
           (add-after 'unpack 'fix-build-system
             (lambda _
               (substitute* "Makefile.am"
                 (("doxygen-doc") ""))
               (with-output-to-file "README" (const #t))
               #t)))))
      (inputs
       `(("boost" ,boost)
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
         ("graphicsmagick" ,graphicsmagick)
         ("zlib" ,zlib)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("autoconf-archive" ,autoconf-archive)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ;; For documentation
         ("docbook-xml" ,docbook-xml-4.2)
         ("docbook-xsl" ,docbook-xsl)
         ("doxygen" ,doxygen)
         ("libxslt" ,libxslt)
         ("texlive" ,texlive-tiny)
         ("zip" ,zip)))
      (home-page "https://stevewolter.github.io/rapidSTORM/")
      (synopsis "Process single-molecule localization microscopy data")
      (description "RapidSTORM provides fast and highly configurable
data processing for single-molecule localization microscopy such as
dSTORM.  It provides both two-dimensional and three-dimensional,
multi-color data analysis as well as a wide range of filtering and
image generation capabilities.")
      ;; Documentation is under fdl1.3+, most of rapidstorm is released
      ;; under GPL; parts are released under LGPL.
      (license (list license:gpl3+
                     license:lgpl3+
                     license:fdl1.3+)))))

(define-public circ-explorer
  (package
    (name "circ-explorer")
    (version "1.1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/YangLab/CIRCexplorer.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d98xmc5pmbzgrzpmb68q20p2qphaa2qfrsnhmqj4nsss25gh1aa"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-import-in-executable
           (lambda _
             (substitute* "circ/CIRCexplorer.py"
               (("from genomic_interval import Interval")
                "from circ.genomic_interval import Interval"))
             #t))
         (add-before 'check 'set-PYTHONPATH
           (lambda _
             (setenv "PYTHONPATH" (string-append (getcwd) "/test:"
                                                 (getenv "PYTHONPATH")))
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
               (invoke "make" "-f" "makefile.linux"))))
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
     `(("java-ecj" ,java-ecj)))
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
    (version "4.0.6")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference
             (url "https://bitbucket.org/tanaylab/misha-package")
             (changeset version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xir6msfs3snn300ms1ywnxy5bld21l4zn6caf8nd1r195fqilrw"))
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
             #t)))))
    (inputs
     `(("kentutils" ,kentutils)))
    (home-page "https://bitbucket.org/tanaylab/misha-package")
    (synopsis "Toolkit for analysis of genomic data")
    (description "This package is intended to help users to
efficiently analyze genomic data resulting from various experiments.")
    (license license:gpl2)))

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

(define-public r-shaman
  (let ((commit "52d5edefc1709aa03cd282e00cb703ffd98ea3eb")
        (release "2.0")
        (revision "1"))
    (package
      (name "r-shaman")
      (version (git-version release revision commit))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/tanaylab/shaman")
                      (changeset commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1g31f3lj9r87bx9mnr0jjcl9nixzkbk8djh6r7z6cpylv7ap8p8v"))
                (snippet
                 ;; This file will be generated.
                 '(begin (delete-file "inst/doc/shaman-package.R") #t))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-misha" ,r-misha)
         ("r-rcpp" ,r-rcpp)
         ("r-rann" ,r-rann)
         ("r-data-table" ,r-data-table)
         ("r-ggplot2" ,r-ggplot2)
         ("r-reshape2" ,r-reshape2)
         ("r-gviz" ,r-gviz)
         ("r-plyr" ,r-plyr)
         ("r-domc" ,r-domc)
         ;; For vignettes
         ("r-rmarkdown" ,r-rmarkdown)
         ("r-knitr" ,r-knitr)))
      (home-page "https://tanaylab.bitbucket.io/shaman/")
      (synopsis "Sampling HiC contact matrices for a-parametric normalization")
      (description "The Shaman package implements functions for
resampling Hi-C matrices in order to generate expected contact
distributions given constraints on marginal coverage and
contact-distance probability distributions.  The package also provides
support for visualizing normalized matrices and statistical analysis
of contact distributions around selected landmarks.")
      ;; Any version of the GPL
      (license license:gpl3+))))

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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkathail/magic.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ll9m7ivcfb56n6yhzww03x37kpmr3jd9a50nvjh342hjd4zhqpk"))))
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
    (version "0.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mageck/"
                                  (version-major+minor version)
                                  "/mageck-" version ".tar.gz"))
              (sha256
               (base32
                "1gcjm41q3s3fqs47fg16n5dn0x9niy3j8ls5bqdi7m6iqkzfjfx8"))))
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
                            ("demo3" "run.sh")
                            ("demo4" "run.sh"))))
               (setenv "PATH"
                       (string-append out "/bin:"
                                      (getenv "PATH")))
               (for-each (match-lambda
                           ((dir script)
                            (with-directory-excursion (string-append "demo/" dir)
                              (invoke "bash" script))))
                         tests)
               #t))))))
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
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vtraag/louvain-igraph.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0w31537sifkf65sck1iaip5i6d8g64pa3wdwad83d6p9jwkck57k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-igraph" ,python-igraph)))
    (inputs
     `(("igraph" ,igraph)))
    (native-inputs
     `(("python-ddt" ,python-ddt)
       ("pkg-config" ,pkg-config)))
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

(define-public perl-cworld-dekker
  (package
    (name "perl-cworld-dekker")
    (version "1.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dekkerlab/cworld-dekker.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dvh23fx52m59y6304xi2j2pl2hiqadlqg8jyv2pm14j1hy71ych"))))
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

(define-public perl-pdf-api2
  (package
    (name "perl-pdf-api2")
    (version "2.033")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/S/SS/SSIMMS/"
                           "PDF-API2-" version ".tar.gz"))
       (sha256
        (base32
         "1817knk32xrimks4nvv0rss06ncjwvdmqpyaz8xgflrh3bn6c24w"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-font-ttf" ,perl-font-ttf)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-memory-cycle" ,perl-test-memory-cycle)))
    (home-page "https://metacpan.org/release/PDF-API2")
    (synopsis "Facilitates the creation and modification of PDF files")
    (description "This module facilitates the creation and modification of
PDF files.")
    (license license:lgpl2.1+)))

(define-public squid
  (package
    (name "squid")
    (version "latest")
    (source
     (origin
       (method url-fetch)
       (uri "http://eddylab.org/software/squid/squid.tar.gz")
       (sha256
        (base32
         "19ywv1h581a84yyjnp64gwww99vhgbxi8v4rl37xp92ag7l44brh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-perl-search-path
           (lambda _
             ;; Work around "dotless @INC" build failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) "/Testsuite:"
                                    (getenv "PERL5LIB")))
             #t)))))
    (inputs
     `(("perl" ,perl)))
    (home-page "http://eddylab.org/software.html")
    (synopsis "C function library for sequence analysis")
    (description "SQUID is Sean Eddy's personal library of C functions and
utility programs for sequence analysis.")
    (license license:gpl2)))

(define-public randfold
  (package
    (name "randfold")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://bioinformatics.psb.ugent.be/"
                           "supplementary_data/erbon/nov2003/downloads/"
                           "randfold-" version ".tar.gz"))
       (sha256
        (base32
         "0gqixl4ncaibrxmn25d6lm2hrw4ml2fj13nrc9q1kilsxdfi91mj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; no tests provided
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let*  ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "randfold" bin)
               #t))))))
    (inputs
     `(("squid" ,squid)))
    (home-page (string-append "http://bioinformatics.psb.ugent.be/"
                              "supplementary_data/erbon/nov2003"))
    (synopsis "Minimum free energy of folding randomization test software")
    (description "randfold computes the probability that, for a given
sequence, the Minimum Free Energy (MFE) of the secondary structure is
different from MFE computed with random sequences.")
    (license license:gpl2)))

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

(define-public qtltools-old
  (package (inherit qtltools)
    (name "qtltools")
    (version "1.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://qtltools.github.io/qtltools/"
                                  "binaries/QTLtools_" version
                                  "_source.tar.gz"))
              (sha256
               (base32
                "1drckp02jgpl8lswa09w10xa6fyd7r8nlg08yhg6c5hls0zbm277"))))))

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
    (version "0.30.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pachterlab/sleuth.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1zk9y8llv8s751lc37smzlk8q8f980l67j3pmnxqmmw9f4v1lm51"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-aggregation" ,r-aggregation)
       ("r-ggplot2" ,r-ggplot2)
       ("r-pheatmap" ,r-pheatmap)
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
    (home-page "https://pachterlab.github.io/sleuth")
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

(define-public python-mappy
  (package
    (name "python-mappy")
    (version "2.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mappy" version))
       (sha256
        (base32
         "0wwm4x9kqi6d5igl99236hw4nb05hwgn5m189aqlg90cxy1m6r2l"))))
    (build-system python-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/lh3/minimap2")
    (synopsis "Minimap2 python binding")
    (description "This package provides Python bindings to Minimap2,
an aligner for genomic and spliced nucleotide sequences.")
    (license license:expat)))

(define-public python-ont-tombo
  (package
    (name "python-ont-tombo")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ont-tombo" version))
       (sha256
        (base32
         "1y9ms01gxh6agbh97x9gz1ax1hdy6qbd9ndxas5r2rn8znk9q0ih"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-h5py" ,python-h5py)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-scikit-learn" ,python-scikit-learn)
       ("python-tqdm" ,python-tqdm)
       ("python-future" ,python-future)
       ("python-mappy" ,python-mappy)))
    (native-inputs
     `(("python-nose2" ,python-nose2)
       ("python-cython" ,python-cython)))
    (home-page "https://github.com/nanoporetech/tombo")
    (synopsis "Analysis of raw nanopore sequencing data")
    (description "Tombo is a suite of tools primarily for the
identification of modified nucleotides from nanopore sequencing
data. Tombo also provides tools for the analysis and visualization of
raw nanopore signal.")
    (license license:mpl2.0)))

(define-public python2-ont-tombo
  (package-with-python2 python-ont-tombo))

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

(define-public kissplice
  (package
    (name "kissplice")
    (version "2.4.0-p1")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://pbil.univ-lyon1.fr/pub/logiciel/"
                                  "kissplice/download/kissplice-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xvn2rjb8c009i6xrsx904i0kbyb7wyqhgsw5ax0xcfh9i29nmpl"))))
    (build-system cmake-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("python" ,python-2)))
    (home-page "http://kissplice.prabi.fr/")
    (synopsis "De-novo calling alternative splicing events from RNA-seq data")
    (description "This package provides a program whose purpose is to
detect alternative splicing events from RNA-seq data.")
    (license license:cecill)))

(define-public trinityrnaseq
  (package
    (name "trinityrnaseq")
    (version "2.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/trinityrnaseq/trinityrnaseq.git")
                    (commit (string-append "Trinity-v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ikf1ndwfnd43g5g2bvh372gfhvndld7aaaqy0x95jrf5my10320"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-1))
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             #t))
         (add-after 'build 'build-plugins
           (lambda _
             ;; Run this in the subdirectory to avoid running the
             ;; tests right here.
             (with-directory-excursion "trinity-plugins"
               (invoke "make" "plugins")) #t))
         ;; The install script uses rsync, provides no overrides for
         ;; the default location at /usr/local/bin, and patching it
         ;; would change all lines that do something.
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/trinity/"))
                    (bin   (string-append out "/bin/")))
               (mkdir-p bin)
               (copy-recursively "." share)
               (delete-file (string-append share "/Chrysalis/build/CMakeFiles/CMakeOutput.log"))
               (delete-file (string-append share "/Inchworm/build/CMakeFiles/CMakeOutput.log"))

               (wrap-program (string-append share "Trinity")
                 `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))
                 `("PERL5LIB"    ":" = (,(getenv "PERL5LIB")))
                 `("PYTHONPATH"  ":" = (,(getenv "PYTHONPATH")))
                 `("PATH"        ":" =
                   ,(cons (string-append out "share/trinity/trinity-plugins/BIN")
                          (filter-map (match-lambda
                                        ((name . dir)
                                         (string-append dir "/bin")))
                                      inputs))))
               (symlink (string-append share "Trinity")
                        (string-append bin "Trinity")))
             #t))
         (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (map (lambda (file)
                    (make-file-writable file))
                  (find-files (assoc-ref outputs "out") ".*\\.gz$"))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("perl-uri-escape" ,(@ (gnu packages perl-web) perl-uri-escape))
       ("java" ,icedtea-8)
       ("python" ,python-wrapper)
       ("python-numpy" ,python-numpy)
       ("r-tidyverse" ,r-tidyverse)
       ("r-edger" ,r-edger)
       ("r-fastcluster" ,r-fastcluster)
       ("r-deseq2" ,r-deseq2)
       ("r-ape" ,r-ape)
       ("r-ctc" ,r-ctc)
       ("r-gplots" ,r-gplots)
       ("r-biobase" ,r-biobase)
       ("r-qvalue" ,r-qvalue)
       ("r-goseq" ,r-goseq)
       ("r-glimma" ,r-glimma)
       ("r-rots" ,r-rots)
       ("r-goplot" ,r-goplot)
       ("r-argparse" ,r-argparse)
       ("r-sm" ,r-sm)
       ("r-minimal" ,r-minimal)
       ("sra-tools" ,sra-tools)
       ("kallisto" ,kallisto)
       ("multiqc" ,multiqc)
       ("rsem" ,rsem)
       ("bowtie" ,bowtie)
       ("samtools" ,samtools)
       ("jellyfish" ,jellyfish)
       ("star" ,star)
       ("hisat" ,hisat)
       ("salmon" ,salmon)
       ("fastqc" ,fastqc)
       ("blast+" ,blast+)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("which" ,(@ (gnu packages base) which))
       ("coreutils" ,coreutils)
       ("gzip" ,gzip)))
    (native-inputs
     `(("cmake" ,cmake)))
    (home-page "https://github.com/trinityrnaseq/trinityrnaseq/wiki")
    (synopsis "Trinity RNA-Seq de novo transcriptome assembly")
    (description "Trinity assembles transcript sequences from Illumina
RNA-Seq data.  Trinity represents a novel method for the efficient and
robust de novo reconstruction of transcriptomes from RNA-seq data.
Trinity combines three independent software modules: Inchworm,
Chrysalis, and Butterfly, applied sequentially to process large
volumes of RNA-seq reads.  Trinity partitions the sequence data into
many individual de Bruijn graphs, each representing the
transcriptional complexity at a given gene or locus, and then
processes each graph independently to extract full-length splicing
isoforms and to tease apart transcripts derived from paralogous
genes.")
    (license license:bsd-3)))

(define-public idr-legacy
  (package
    (name "idr-legacy")
    (version "0")
    (source (origin
              (method url-fetch)
              (uri "https://docs.google.com/uc?export=download&id=0B_ssVVyXv8ZSX3luT0xhV3ZQNWc")
              (file-name (string-append "idr-legacy-" version ".tar.gz"))
              (sha256
               (base32
                "1d4zxh860in3sf6nkwwz427srh8p3hx35a7x1izbf73hlfbjk6a5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/idr-legacy")))
               (substitute* '("batch-consistency-plot.r"
                              "batch-consistency-plot-merged2.r"
                              "batch-consistency-analysis.r")
                 (("source\\(\"functions-all-clayton-12-13.r\"\\)")
                  (string-append "source(\"" share "/functions-all-clayton-12-13.r\")")))
               (mkdir-p share)
               (copy-recursively "." share)
               #t))))))
    (propagated-inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("r-minimal" ,r-minimal)))
    (home-page "https://sites.google.com/site/anshulkundaje/projects/idr/deprecated")
    (synopsis "Legacy pipeline for measuring the Irreproducible Discovery Rate")
    (description "The IDR (Irreproducible Discovery Rate) framework is
a unified approach to measure the reproducibility of findings
identified from replicate experiments and provide highly stable
thresholds based on reproducibility.  This package provides the
original implementation of a pipeline using this method.")
    (license license:gpl2+)))

(define-public psm/patched
  (package
    (name "psm")
    (version "3.3.20170428")
    (home-page "https://github.com/intel/psm")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit "604758e76dc31e68d1de736ccf5ddf16cb22355b")))
       (file-name (string-append "psm-" version ".tar.gz"))
       (sha256
        (base32 "0nsb325dmhn5ia3d2cnksqr0gdvrrx2hmvlylfgvmaqdpq76zm85"))
       (patches (search-patches
                 "psm-arch.patch"     ; uname -p returns "unknown" on Debian 9
                 "psm-ldflags.patch"  ; build shared lib with LDFLAGS
                 "psm-repro.patch"))))  ; reproducibility
    (build-system gnu-build-system)
    (inputs `(("libuuid" ,util-linux)))
    (arguments
     '(#:make-flags `("PSM_USE_SYS_UUID=1" "CC=gcc" "WERROR="
                      ,(string-append "INSTALL_PREFIX=" %output)
                      ,(string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-/usr/include
                    (lambda _
                      (substitute* "Makefile"
                        (("\\$\\{DESTDIR}/usr/include")
                         (string-append %output "/include")))
                      (substitute* "Makefile"
                        (("/lib64") "/lib"))
                      #t))
                  (add-after 'unpack 'patch-sysmacros
                    (lambda _
                      (substitute* "ipath/ipath_proto.c"
                        (("#include <sys/poll.h>" m)
                         (string-append m "\n"
                                        "#include <sys/sysmacros.h>")))
                      #t)))))
    (synopsis "Intel Performance Scaled Messaging (PSM) Libraries")
    (description
     "The PSM Messaging API, or PSM API, is Intel's low-level user-level
communications interface for the True Scale family of products.  PSM users are
enabled with mechanisms necessary to implement higher level communications
interfaces in parallel environments.")
    ;; Only Intel-compatable processors are supported.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license (list license:bsd-2 license:gpl2))))

(define-public openmpi-with-hwloc2
  (package (inherit openmpi)
    (name "openmpi-with-hwloc2")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags)
        `(cons* "--enable-orterun-prefix-by-default"
                "--with-orte"
                ,flags))))
    (inputs
     `(("hwloc" ,hwloc-2 "lib")
       ,@(package-inputs openmpi)))))

(define-public openmpi-psm-only
  (package (inherit openmpi)
    (name "openmpi-psm-only")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags)
        `(cons* "--enable-orterun-prefix-by-default"
                "--with-orte"
                "--with-psm"
                ,flags))))
    (inputs
     `(("hwloc" ,hwloc-2 "lib")
       ("psm" ,psm)
       ,@(alist-delete "psm2" (package-inputs openmpi))))))

(define-public openmpi-psm2-only
  (package (inherit openmpi)
    (name "openmpi-psm2-only")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags)
        `(cons* "--enable-orterun-prefix-by-default"
                "--with-orte"
                "--with-psm2"
                ,flags))))
    (inputs
     `(("hwloc" ,hwloc-2 "lib")
       ("psm2" ,psm2)
       ,@(alist-delete "psm" (package-inputs openmpi))))))

(define-public bustools
  (package
    (name "bustools")
    (version "0.39.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BUStools/bustools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "111crf85g4gsickylqs99r5pqd5x851sfdylksl29xzypfsqmh3h"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))  ; no tests
    (home-page "https://bustools.github.io")
    (synopsis "Tools for working with BUS files")
    (description "bustools is a program for manipulating BUS files for single
cell RNA-Seq datasets.  It can be used to error correct barcodes, collapse
UMIs, produce gene count or transcript compatibility count matrices, and is useful
for many other tasks.")
    (license license:bsd-2)))

(define-public r-snapatac
  (let ((commit "c3ab177558f0fe9c47cbd68969df7b06de5b07d9")
        (revision "1"))
    (package
      (name "r-snapatac")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/r3fang/SnapATAC")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "037jzlbl436fi7lkpq7d83i2vd1crnrik3vac2x6xj75dbikb2av"))))
      (properties `((upstream-name . "SnapATAC")))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-bigmemory" ,r-bigmemory)
         ("r-doparallel" ,r-doparallel)
         ("r-dosnow" ,r-dosnow)
         ("r-edger" ,r-edger)
         ("r-foreach" ,r-foreach)
         ("r-genomicranges" ,r-genomicranges)
         ("r-igraph" ,r-igraph)
         ("r-iranges" ,r-iranges)
         ("r-irlba" ,r-irlba)
         ("r-matrix" ,r-matrix)
         ("r-plot3d" ,r-plot3d)
         ("r-plyr" ,r-plyr)
         ("r-rann" ,r-rann)
         ("r-raster" ,r-raster)
         ("r-rcolorbrewer" ,r-rcolorbrewer)
         ("r-rhdf5" ,r-rhdf5)
         ("r-rtsne" ,r-rtsne)
         ("r-scales" ,r-scales)
         ("r-viridis" ,r-viridis)))
      (home-page "https://github.com/r3fang/SnapATAC")
      (synopsis "Single nucleus analysis package for ATAC-Seq")
      (description
       "This package provide a analysis toolkit for single cell data.  The
analysis is designed to be used for Assay for Transposase-Accessible Chromatin
using sequencing (ATAC-seq) tehnique.  Single cell ATAC-seq can resolve the
heterogeneity of a complex tissue and reveal cell-type specific regulatory
landscapes.  However, the exceeding data sparsity has posed unique challenges
for the data analysis.  SnapATAC is a end-to-end bioinformatics pipeline for
analyzing large-scale single cell ATAC-seq data which includes quality control,
normalization, clustering analysis, differential analysis, motif inference and
exploration of single cell ATAC-seq sequencing data.")
      (license license:gpl3))))

(define-public r-lsd
  (package
    (name "r-lsd")
    (version "4.0-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "LSD" version))
       (sha256
        (base32 "0fsp3pwrnnic9mzkd6yxa4bnxbvg68712lb20vd42wf6jb39r2h3"))))
    (properties `((upstream-name . "LSD")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/LSD/")
    (synopsis "Lots of superior depictions tool creates colorful plots")
    (description
     "This package creates lots of colorful plots in a multitude of variations.
Try a demo of the LSD by running demotour().")
    (license #f))) ;unlimited

(define-public r-fourcseq
  (package
    (name "r-fourcseq")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "FourCSeq" version))
       (sha256
        (base32 "0rnlgvxyi32g6nng8pkdsd13xy4x09lv6zg64m82m9s1zfaksclm"))))
    (properties `((upstream-name . "FourCSeq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq2" ,r-deseq2)
       ("r-fda" ,r-fda)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggbio" ,r-ggbio)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtools" ,r-gtools)
       ("r-lsd" ,r-lsd)
       ("r-matrix" ,r-matrix)
       ("r-reshape2" ,r-reshape2)
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page
     "https://bioconductor.org/packages/release/bioc/html/FourCSeq.html")
    (synopsis "Analysis of multiplexed 4C sequencing data")
    (description
     "This package is an R package dedicated to the analysis of (multiplexed) 4C
sequencing data.  @code{r-fourcseq} provides a pipeline to detect specific
interactions between DNA elements and identify differential interactions between
conditions.  The statistical analysis in R starts with individual bam files for
each sample as inputs.  To obtain these files, the package contains a python
script (extdata/python/demultiplex.py) to demultiplex libraries and trim off
primer sequences.  With a standard alignment software the required bam files can
be then be generated.")
    (license license:gpl3+)))

(define-public r-phylogram
  (package
    (name "r-phylogram")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "phylogram" version))
       (sha256
        (base32 "1p4h5pirc0m5pzc18q0jk3mcmb5n48gdf9abz03vml3a209xxl2v"))))
    (properties `((upstream-name . "phylogram")))
    (build-system r-build-system)
    (propagated-inputs `(("r-ape" ,r-ape)))
    (home-page "https://github.com/ropensci/phylogram/")
    (synopsis "Dendrograms for evolutionary analysis")
    (description
     "The @code{r-phylogram} package is a tool for for developing phylogenetic
trees as deeply-nested lists known as \"dendrogram\" objects.  It provides
functions for conversion between \"dendrogram\" and \"phylo\" class objects, as
well as several tools for command-line tree manipulation and import/export via
Newick parenthetic text.  This improves accessibility to the comprehensive range
of object-specific analytical and tree-visualization functions found across a
wide array of bioinformatic R packages.")
    (license license:gpl3)))

(define-public r-kmer
  (package
    (name "r-kmer")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kmer" version))
       (sha256
        (base32 "0jimn9r0abglwxdl1zqz0lxa99cmj6haydkxjzqfbpx9by80wnww"))))
    (properties `((upstream-name . "kmer")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-openssl" ,r-openssl)
       ("r-phylogram" ,r-phylogram)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/shaunpwilkinson/kmer/")
    (synopsis
     "Fast K-Mer counting and clustering for biological sequence analysis")
    (description
     "@code{r-kmer} is an R package for rapidly computing distance matrices and
clustering large sequence datasets using fast alignment-free k-mer counting and
recursive k-means partitioning.")
    (license license:gpl3)))

(define-public r-rblast
  (let ((commit
         "7e792877edd9c04defbcd3928d950870d0299082")
        (revision "1"))
    (package
      (name "r-rblast")
      (version (git-version "0.99.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mhahsler/rBLAST")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xk5bm1sg3020nkqwf12dxs3ji99q9gc7k1xzkr152ds4wx82s9a"))))
      (properties `((upstream-name . "rBLAST")))
      (build-system r-build-system)
      (inputs `(("blast+" ,blast+)))
      (propagated-inputs `(("r-biostrings" ,r-biostrings)))
      (native-inputs `(("gfortran" ,gfortran)))
      (home-page "https://github.com/mhahsler/rBLAST/")
      (synopsis "Interface to the Basic Local Alignment Search Tool (BLAST)")
      (description
       "This package provides an interface for the Basic Local Alignment Search
Tool (BLAST) to search genetic sequence data bases.  This includes interfaces to
@code{blastn}, @code{blastp}, @code{blastx}, and @code{makeblastdb}.")
      (license license:gpl3))))

(define-public r-bsgenome-hsapiens-ncbi-grch38
  (package
    (name "r-bsgenome-hsapiens-ncbi-grch38")
    (version "1.3.1000")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri
             "BSgenome.Hsapiens.NCBI.GRCh38"
             version
             'annotation))
       (sha256 (base32 "0y75qdq578fh6420vbvsbwmdw8jvr3g06qli2h3vj3pxmjykh9c1"))))
    (properties `((upstream-name . "BSgenome.Hsapiens.NCBI.GRCh38")))
    (build-system r-build-system)
    (propagated-inputs `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     (string-append "https://bioconductor.org/packages/release/data/annotation/"
                    "html/BSgenome.Hsapiens.NCBI.GRCh38.html"))
    (synopsis "Full genome sequences for Homo sapiens (GRCh38)")
    (description
     "This package provides full genome sequences for Homo sapiens (Human) as
provided by NCBI (GRCh38, 2013-12-17) and stored in Biostrings objects.")
    (license license:artistic2.0)))
