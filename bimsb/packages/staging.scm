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
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
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
  #:use-module (gnu packages zip)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

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

(define-public rstudio-server
  (package
    (name "rstudio-server")
    (version "1.0.115")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rstudio/rstudio/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "09vmfpcp7gv78qjpzdq339zg8srlk7khdd5s62hqr5f0nf8aw2ai"))
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
  (let ((commit "1.0.44---rsession-override")
        (revision "1"))
    (package
      (inherit rstudio-server)
      (name "rstudio-server-bimsb")
      (version "1.0.44")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BIMSBbioinfo/rstudio.git")
               (commit commit)))
         (sha256
          (base32
           "1149ambzsb5dzckymzy9y75wi2bhhkcqaa1ba45dvjfa1zvi7k47"))))
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

(define-public r-purrr
  (package
    (name "r-purrr")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "purrr" version))
       (sha256
        (base32
         "0lss8q733nv7s154wargm6vnxq55qygnxakib8xdj4jv0y86sxc3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-dplyr" ,r-dplyr)
       ("r-lazyeval" ,r-lazyeval)
       ("r-magrittr" ,r-magrittr)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/hadley/purrr")
    (synopsis "Functional programming tools")
    (description
     "This package completes R's functional programming tools with
missing features present in other programming languages.")
    (license license:gpl3+)))

(define-public r-rematch
  (package
    (name "r-rematch")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rematch" version))
       (sha256
        (base32
         "0y3mshvpvz9csmq8hk8jbabx4nxlv5sckvfzvm6920ndg34xw2d4"))))
    (build-system r-build-system)
    (home-page "https://github.com/MangoTheCat/rematch")
    (synopsis "Match regular expressions with a nicer API")
    (description
     "This package provides a small wrapper on @code{regexpr} to
extract the matches and captured groups from the match of a regular
expression to a character vector.")
    (license license:expat)))

(define-public r-cellranger
  (package
    (name "r-cellranger")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cellranger" version))
       (sha256
        (base32
         "16fgi3annn34c3cxi0pxf62mmmmxi21hp0zzlv7bkfsjqy4g4f2x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rematch" ,r-rematch)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/rsheets/cellranger")
    (synopsis "Translate spreadsheet cell ranges to rows and columns")
    (description
     "This package provides helper functions to work with spreadsheets
and the @code{A1:D10} style of cell range specification.")
    (license license:expat)))

(define-public r-googlesheets
  (package
    (name "r-googlesheets")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "googlesheets" version))
       (sha256
        (base32
         "0ps13h1cv7fj5dh8s4nvwi64wnnyqdsadcaa4iizq1c5s615cwk3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cellranger" ,r-cellranger)
       ("r-dplyr" ,r-dplyr)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-purrr" ,r-purrr)
       ("r-readr" ,r-readr)
       ("r-stringr" ,r-stringr)
       ("r-tidyr" ,r-tidyr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/jennybc/googlesheets")
    (synopsis "Manage Google spreadsheets from R")
    (description "This package provides tools to interact with Google
Sheets from within R.")
    (license license:expat)))

(define-public ritornello
  (package
    (name "ritornello")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/KlugerLab/"
                                  "Ritornello/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02nik86gq9ljjriv6pamwlmqnfky3ads1fpklx6mc3hx6k40pg38"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-samtools-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("src/SamStream.h"
                            "src/BufferedGenomeReader.h")
               (("<sam.h>") "<samtools/sam.h>"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (bin    (string-append out "/bin/")))
               (mkdir-p bin)
               (install-file "bin/Ritornello" bin)
               #t))))))
    (inputs
     `(("samtools" ,samtools-0.1)
       ("fftw" ,fftw)
       ("boost" ,boost)
       ("zlib" ,zlib)))
    (home-page "https://github.com/KlugerLab/Ritornello")
    (synopsis "Control-free peak caller for ChIP-seq data")
    (description "Ritornello is a ChIP-seq peak calling algorithm
based on signal processing that can accurately call binding events
without the need to do a pair total DNA input or IgG control sample.
It has been tested for use with narrow binding events such as
transcription factor ChIP-seq.")
    (license license:gpl3+)))

(define-public gess
  (package
    (name "gess")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              ;; There are no versioned tarballs
              (uri "http://compbio.uthscsa.edu/GESS_Web/files/gess.src.tar.gz")
              (sha256
               (base32
                "16qvzr51mlg152glvxyk4n88ny6428219a83cbf1hn1rv20ny1bb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((python (assoc-ref inputs "python"))
                   (out    (assoc-ref outputs "out"))
                   (bin    (string-append out "/bin/"))
                   (target (string-append
                            out "/lib/python2.7/site-packages/gess/")))
              (mkdir-p target)
              (copy-recursively "." target)
              ;; Make GESS.py executable
              (chmod (string-append target "GESS.py") #o555)
              ;; Add Python shebang to the top
              (substitute* (string-append target "GESS.py")
                (("\"\"\"Description:" line)
                 (string-append "#!" (which "python") "
import matplotlib
matplotlib.use('Agg')
" line)))
              ;; Make sure GESS has all modules in its path
              (wrap-program (string-append target "GESS.py")
                `("PYTHONPATH" ":" prefix (,target ,(getenv "PYTHONPATH"))))
              (mkdir-p bin)
              (symlink (string-append target "GESS.py")
                       (string-append bin "GESS.py"))
              #t))))))
    (inputs
     `(("python" ,python-2)
       ("python2-pysam" ,python2-pysam)
       ("python2-scipy" ,python2-scipy)
       ("python2-numpy" ,python2-numpy)
       ("python2-networkx" ,python2-networkx)
       ("python2-biopython" ,python2-biopython)))
    (synopsis "Detect exon-skipping events from raw RNA-seq data")
    (description
     "GESS is an implementation of a novel computational method to detect de
novo exon-skipping events directly from raw RNA-seq data without the prior
knowledge of gene annotation information.  GESS stands for the graph-based
exon-skipping scanner detection scheme.")
    (home-page "http://compbio.uthscsa.edu/GESS_Web/")
    (license license:bsd-3)))

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

(define-public phylip
  (package
    (name "phylip")
    (version "3.696")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://evolution.gs.washington.edu/phylip/"
                           "download/phylip-" version ".tar.gz"))
       (sha256
        (base32
         "01jar1rayhr2gba2pgbw49m56rc5z4p5wn3ds0m188hrlln4a2nd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags (list "-f" "Makefile.unx" "install")
       #:parallel-build? #f ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "src") #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/bin")))
               (mkdir-p target)
               (for-each (lambda (file)
                           (install-file file target))
                         (find-files "../exe" ".*")))
             #t)))))
    (home-page "http://evolution.genetics.washington.edu/phylip/")
    (synopsis "Tools for inferring phylogenies")
    (description "PHYLIP (the PHYLogeny Inference Package) is a
package of programs for inferring phylogenies (evolutionary trees).")
    (license license:bsd-2)))

(define-public imp
  (package
    (name "imp")
    (version "2.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://integrativemodeling.org/"
                           version "/download/imp-" version ".tar.gz"))
       (sha256
        (base32
         "0lxqx7vh79d771svr611dkilp6sn30qrbw8zvscbrm37v38d2j6h"))))
    (build-system cmake-build-system)
    ;; Some tests fail because they produce warnings, others fail
    ;; because the PYTHONPATH does not include the modeller's
    ;; directory.
    (arguments
     `(#:tests? #f
       #:configure-flags
       ;; Do not place libraries in an architecture-specific
       ;; directory.
       (list "-DCMAKE_INSTALL_LIBDIR=lib")))
    (inputs
     `(("boost" ,boost)
       ("gsl" ,gsl)
       ("swig" ,swig)
       ("hdf5" ,hdf5)
       ("fftw" ,fftw)
       ("python" ,python-2)))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-pandas" ,python2-pandas)
       ("python2-scikit-learn" ,python2-scikit-learn)
       ("python2-networkx" ,python2-networkx)))
    (home-page "https://integrativemodeling.org")
    (synopsis "Integrative modeling platform")
    (description "IMP's broad goal is to contribute to a comprehensive
structural characterization of biomolecules ranging in size and
complexity from small peptides to large macromolecular assemblies, by
integrating data from diverse biochemical and biophysical experiments.
IMP provides a C++ and Python toolbox for solving complex modeling
problems, and a number of applications for tackling some common
problems in a user-friendly way.")
    ;; IMP is largely available under the GNU Lesser GPL; see the file
    ;; COPYING.LGPL for the full text of this license. Some IMP
    ;; modules are available under the GNU GPL (see the file
    ;; COPYING.GPL).
    (license (list license:lgpl2.1+
                   license:gpl3+))))

(define-public tadbit
  ;; There have been no proper releases.
  (let ((commit "8e548124b5a7dbd2b754f8187b4ac64a6387b7a3")
        (revision "1"))
    (package
      (name "tadbit")
      (version (string-append "0-" revision (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3DGenomes/TADbit.git")
               (commit commit)))
         (sha256
          (base32
           "0ndypmhir0jnq03ajcv6ypj11cznxmll4ygiyk5dympvb8v5y3dp"))))
      (build-system python-build-system)
      (arguments
       `(;; Tests are included and must be run after installation, but
         ;; they are incomplete and thus cannot be run.
         #:tests? #f
         #:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-problems-with-setup.py
             (lambda* (#:key outputs #:allow-other-keys)
               ;; setup.py opens these files for writing
               (chmod "_pytadbit/_version.py" #o664)
               (chmod "README.rst" #o664)

               ;; Don't attempt to install the bash completions to
               ;; the home directory.
               (rename-file "extras/.bash_completion"
                            "extras/tadbit")
               (substitute* "setup.py"
                 (("\\(path.expanduser\\('~'\\)")
                  (string-append "(\""
                                 (assoc-ref outputs "out")
                                 "/etc/bash_completion.d\""))
                 (("extras/\\.bash_completion")
                  "extras/tadbit"))
               #t)))))
      (native-inputs
       `(("python2-setuptools" ,python2-setuptools)))
      (inputs
       ;; TODO: add Chimera for visualization
       `(("imp" ,imp)
         ("mcl" ,mcl)
         ("python2-scipy" ,python2-scipy)
         ("python2-numpy" ,python2-numpy)
         ("python2-matplotlib" ,python2-matplotlib)
         ("python2-pysam" ,python2-pysam)))
      (home-page "http://3dgenomes.github.io/TADbit/")
      (synopsis "Analyze, model, and explore 3C-based data")
      (description
       "TADbit is a complete Python library to deal with all steps to
analyze, model, and explore 3C-based data.  With TADbit the user can
map FASTQ files to obtain raw interaction binned matrices (Hi-C like
matrices), normalize and correct interaction matrices, identify adn
compare the so-called @dfn{Topologically Associating Domains} (TADs),
build 3D models from the interaction matrices, and finally, extract
structural properties from the models.  TADbit is complemented by
TADkit for visualizing 3D models.")
      (license license:gpl3+))))

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

(define-public r-spams
  (package
    (name "r-spams")
    (version "2.5-svn2014-07-04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gforge.inria.fr/frs/download.php/33815/"
                           "spams-R-v" version ".tar.gz"))
       (sha256
        (base32
         "1k459jg9a334slkw31w63l4d39xszjzsng7dv5j1mp78zifz7hvx"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "spams") #t))
         ;; Since R 3.3.0 including R headers inside of an extern "C"
         ;; block causes C headers to be included, which results in a
         ;; lot of duplicate definitions.  This can be avoided by
         ;; defining NO_C_HEADERS before including the R headers.
         (add-after 'chdir 'patch-use-of-R-headers
           (lambda _
             (substitute* "src/spams.cpp"
               (("#include <R.h>" line)
                (string-append "#define NO_C_HEADERS\n" line)))
             #t))
         ;; This looks like a syntax error.
         (add-after 'chdir 'patch-isnan
           (lambda _
             (substitute* '"src/spams/linalg/linalg.h"
               (("if isnan\\(lambda\\) \\{")
                "if (isnan(lambda)) {"))
             #t)))))
    (home-page "http://spams-devel.gforge.inria.fr")
    (synopsis "Toolbox for solving sparse estimation problems")
    (description "SPAMS (SPArse Modeling Software) is an optimization
toolbox for solving various sparse estimation problems.  It includes
tools for the following problems:

@enumerate
@item Dictionary learning and matrix factorization (NMF, sparse
 @dfn{principle component analysis} (PCA), ...)
@item Solving sparse decomposition problems with LARS, coordinate
 descent, OMP, SOMP, proximal methods
@item Solving structured sparse decomposition problems (l1/l2,
 l1/linf, sparse group lasso, tree-structured regularization,
 structured sparsity with overlapping groups,...).
@end enumerate\n")
    (license license:gpl3+)))

(define-public r-fastcluster
  (package
    (name "r-fastcluster")
    (version "1.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastcluster" version))
       (sha256
        (base32
         "0rlbxhh894znf10x0xgkv9dzpibgq9jw5aqpgviccdnxc2c5hwid"))))
    (build-system r-build-system)
    (home-page "http://danifold.net/fastcluster.html")
    (synopsis "Fast hierarchical clustering routines")
    (description
     "This is a two-in-one package which provides interfaces to both R
and Python.  It implements fast hierarchical, agglomerative clustering
routines.  Part of the functionality is designed as drop-in
replacement for existing routines: @code{linkage()} in the SciPy
package @code{scipy.cluster.hierarchy}, @code{hclust()} in R's
@code{stats} package, and the @code{flashClust} package.  It provides
the same functionality with the benefit of a much faster
implementation.  Moreover, there are memory-saving routines for
clustering of vector data, which go beyond what the existing packages
provide.")
    (license license:bsd-2)))

(define-public r-getopt
  (package
    (name "r-getopt")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "getopt" version))
       (sha256
        (base32
         "00f57vgnzmg7cz80rjmjz1556xqcmx8nhrlbbhaq4w7gl2ibl87r"))))
    (build-system r-build-system)
    (home-page "https://github.com/trevorld/getopt")
    (synopsis "C-like getopt behavior")
    (description
     "This package is designed to be used with Rscript to write
shebang scripts that accept short and long flags/options.  Many users
will prefer using instead the packages @code{optparse} or
@code{argparse} which add extra features like automatically generated
help option and usage, support for default values, positional argument
support, etc.")
    (license license:gpl2+)))

(define-public r-optparse
  (package
    (name "r-optparse")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "optparse" version))
       (sha256
        (base32 "1g8as89r91xxi5j5azsd6vrfrhg84mnfx2683j7pacdp8s33radw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-getopt" ,r-getopt)))
    (home-page "https://github.com/trevorld/optparse")
    (synopsis "Command line option parser")
    (description
     "This package provides a command line parser inspired by Python's
@code{optparse} library to be used with Rscript to write shebang
scripts that accept short and long flag/options.")
    (license license:gpl2+)))

(define-public r-r4rna
  (package
    (name "r-r4rna")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.e-rna.org/r-chie/files/R4RNA_"
                           version ".tar.gz"))
       (sha256
        (base32
         "1p0i78wh76jfgmn9jphbwwaz6yy6pipzfg08xs54cxavxg2j81p5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-optparse" ,r-optparse)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (synopsis "Analysis framework for RNA secondary structure")
    (description
     "The R4RNA package aims to be a general framework for the
analysis of RNA secondary structure and comparative analysis in R.")
    (home-page "http://www.e-rna.org/r-chie/index.cgi")
    (license license:gpl3+)))

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
     `(("bc" ,bc)))
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

(define-public r-dynamictreecut
  (package
    (name "r-dynamictreecut")
    (version "1.63-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dynamicTreeCut" version))
       (sha256
        (base32
         "1fadbql7g5r2vvlkr89nlrjxwp4yx4xrdqmv077qvmnx9vv0f4w3"))))
    (properties
     `((upstream-name . "dynamicTreeCut")))
    (build-system r-build-system)
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/BranchCutting/")
    (synopsis "Detect clusters in hierarchical clustering dendrograms")
    (description
     "This package contains methods for the detection of clusters in
hierarchical clustering dendrograms.")
    (license license:gpl2+)))

(define-public r-preprocesscore
  (package
    (name "r-preprocesscore")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "1n8y12q7145f385gm2k3c6y3vwvin7jlb47la4mnl7mar6pq9kmp"))))
    (properties
     `((upstream-name . "preprocessCore")))
    (build-system r-build-system)
    (home-page "https://github.com/bmbolstad/preprocessCore")
    (synopsis "Collection of pre-processing functions")
    (description
     "This package provides a library of core pre-processing and
normalization routines.")
    (license license:lgpl2.0+)))

(define-public r-wgcna
  (package
    (name "r-wgcna")
    (version "1.51")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "0hzvnhw76vwg8bl8x368f0c5szpwb8323bmrb3bir93i5bmfjsxx"))))
    (properties `((upstream-name . "WGCNA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-doparallel" ,r-doparallel)
       ("r-dynamictreecut" ,r-dynamictreecut)
       ("r-fastcluster" ,r-fastcluster)
       ("r-foreach" ,r-foreach)
       ("r-go-db" ,r-go-db)
       ("r-hmisc" ,r-hmisc)
       ("r-impute" ,r-impute)
       ("r-matrixstats" ,r-matrixstats)
       ("r-preprocesscore" ,r-preprocesscore)))
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
    (synopsis "Weighted correlation network analysis")
    (description
     "This package provides functions necessary to perform Weighted
Correlation Network Analysis on high-dimensional data.  It includes
functions for rudimentary data cleaning, construction of correlation
networks, module identification, summarization, and relating of
variables and modules to sample traits.  It also includes a number of
utility functions for data manipulation and visualization.")
    (license license:gpl2+)))

(define-public r-sfsmisc
  (package
    (name "r-sfsmisc")
    (version "1.1-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sfsmisc" version))
       (sha256
        (base32
         "0580piv4n1nispl3pa8nfjjfnb8iwaqky2dzdy0aqnxrxgrhqhvz"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/sfsmisc")
    (synopsis "Utilities from \"Seminar fuer Statistik\" ETH Zurich")
    (description
     "This package provides useful utilities from Seminar fuer
Statistik ETH Zurich, including many that are related to graphics.")
    (license license:gpl2+)))

(define-public r-kernlab
  (package
    (name "r-kernlab")
    (version "0.9-25")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kernlab" version))
       (sha256
        (base32
         "0qnaq9x3j2xc6jrmmd98wc6hkzch487s4p3a9lnc00xvahkhgpmr"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/kernlab")
    (synopsis "Kernel-based machine learning tools")
    (description
     "This package provides kernel-based machine learning methods for
classification, regression, clustering, novelty detection, quantile
regression and dimensionality reduction.  Among other methods
@code{kernlab} includes Support Vector Machines, Spectral Clustering,
Kernel PCA, Gaussian Processes and a QP solver.")
    (license license:gpl2)))

(define-public r-gtools
  (package
    (name "r-gtools")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gtools" version))
       (sha256
        (base32
         "1xknwk9xlsj027pg0nwiizigcrsc84hdrig0jn0cgcyxj8dabdl6"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/gtools")
    (synopsis "Various R programming tools")
    (description
     "This package contains a collection of various functions to
assist in R programming, such as tools to assist in developing, updating,
and maintaining R and R packages, calculating the logit and
inverse logit transformations, tests for whether a
value is missing, empty or contains only @code{NA} and @code{NULL}
values, and many more.")
    (license license:gpl2)))

(define-public r-chipkernels
  (let ((commit "c9cfcacb626b1221094fb3490ea7bac0fd625372")
        (revision "1"))
    (package
      (name "r-chipkernels")
      (version (string-append "1.1-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ManuSetty/ChIPKernels.git")
               (commit commit)))
         (sha256
          (base32
           "14bj5qhjm1hsm9ay561nfbqi9wxsa7y487df2idsaaf6z10nw4v0"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-iranges" ,r-iranges)
         ("r-xvector" ,r-xvector)
         ("r-biostrings" ,r-biostrings)
         ("r-bsgenome" ,r-bsgenome)
         ("r-gtools" ,r-gtools)
         ("r-genomicranges" ,r-genomicranges)
         ("r-sfsmisc" ,r-sfsmisc)
         ("r-kernlab" ,r-kernlab)
         ("r-s4vectors" ,r-s4vectors)
         ("r-biocgenerics" ,r-biocgenerics)))
      (home-page "https://github.com/ManuSetty/ChIPKernels")
      (synopsis "Build string kernels for DNA Sequence analysis")
      (description "ChIPKernels is an R package for building different
string kernels used for DNA Sequence analysis.  A dictionary of the
desired kernel must be built and this dictionary can be used for
determining kernels for DNA Sequences.")
    (license license:gpl2+))))

(define-public r-seqgl
  (package
    (name "r-seqgl")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ManuSetty/SeqGL/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pnk1p3sci5yipyc8xnb6jbmydpl80fld927xgnbcv104hy8h8yh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biostrings" ,r-biostrings)
       ("r-chipkernels" ,r-chipkernels)
       ("r-genomicranges" ,r-genomicranges)
       ("r-spams" ,r-spams)
       ("r-wgcna" ,r-wgcna)
       ("r-fastcluster" ,r-fastcluster)))
    (home-page "https://github.com/ManuSetty/SeqGL")
    (synopsis "Group lasso for Dnase/ChIP-seq data")
    (description "SeqGL is a group lasso based algorithm to extract
TF sequence signals from ChIP, DNase and ATAC-seq profiles.  This
package presents a method which uses group lasso to discriminate
between bound and non bound genomic regions to accurately identify
transcription factors bound at the specific regions.")
    (license license:gpl2+)))

(define-public hisat2
  (package
    (name "hisat2")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2"
                           "/downloads/hisat2-"
                           version "-source.zip"))
       (sha256
        (base32
         "0lywnr8kijwsc2aw10dwxic0n0yvip6fl3rjlvc8zzwahamy4x7g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags (list "CC=gcc" "CXX=g++" "allall")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-deterministic
           (lambda _
             (substitute* "Makefile"
               (("`date`") "0"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (doc (string-append out "/share/doc/hisat2/")))
               (for-each (cut install-file <> bin)
                         (find-files "."
                                     "hisat2(-(build|align|inspect)(-(s|l)(-debug)*)*)*$"))
               (mkdir-p doc)
               (install-file "doc/manual.inc.html" doc))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)
       ("perl" ,perl)
       ("pandoc" ,ghc-pandoc))) ; for documentation
    (home-page "http://ccb.jhu.edu/software/hisat2/index.shtml")
    (synopsis "Graph-based alignment of genomic sequencing reads")
    (description "HISAT2 is a fast and sensitive alignment program for
mapping next-generation sequencing reads (both DNA and RNA) to a
population of human genomes (as well as to a single reference genome).
In addition to using one global @dfn{graph FM} (GFM) index that
represents a population of human genomes, HISAT2 uses a large set of
small GFM indexes that collectively cover the whole genome.  These
small indexes, combined with several alignment strategies, enable
rapid and accurate alignment of sequencing reads.  This new indexing
scheme is called a @dfn{Hierarchical Graph FM index} (HGFM).")
    ;; HISAT2 contains files from Bowtie2, which is released under
    ;; GPLv2 or later.  The HISAT2 source files are released under
    ;; GPLv3 or later.
    (license license:gpl3+)))

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
