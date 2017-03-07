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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages guile)
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
  #:use-module (bimsb packages variants)
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

(define-public trim-galore
  (package
    (name "trim-galore")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bioinformatics.babraham.ac.uk/"
                           "projects/trim_galore/trim_galore_v"
                           version ".zip"))
       (sha256
        (base32
         "0b9qdxi4521gsrjvbhgky8g7kry9b5nx3byzaxkgxz7p4k8bn1mn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; The archive contains plain files.
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (zero? (system* "unzip" source))))
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'hardcode-tool-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "trim_galore"
               (("\\$path_to_cutadapt = 'cutadapt'")
                (string-append "$path_to_cutadapt = '"
                               (assoc-ref inputs "cutadapt")
                               "/bin/cutadapt'"))
               (("\\| gzip")
                (string-append "| "
                               (assoc-ref inputs "gzip")
                               "/bin/gzip"))
               (("\"gunzip")
                (string-append "\""
                               (assoc-ref inputs "gzip")
                               "/bin/gunzip")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (install-file "trim_galore" bin)
               #t))))))
    (inputs
     `(("gzip" ,gzip)
       ("perl" ,perl)
       ("cutadapt" ,cutadapt)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/trim_galore/")
    (synopsis "Wrapper around Cutadapt and FastQC")
    (description "Trim Galore! is a wrapper script to automate quality
and adapter trimming as well as quality control, with some added
functionality to remove biased methylation positions for RRBS sequence
files.")
    (license license:gpl3+)))

(define-public gess
  (package
    (name "gess")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://compbio.uthscsa.edu/"
                                  "GESS_Web/files/"
                                  "gess-" version ".src.tar.gz"))
              (sha256
               (base32
                "0hyk403kxscclzfs24pvdgiv0wm03kjcziqdrp5w46cb049gz0d7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
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
               ;; Add Python shebang to the top and make Matplotlib
               ;; usable.
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

(define-public r-mclust
  (package
    (name "r-mclust")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mclust" version))
       (sha256
        (base32
         "08swza5zg6dxlgwfl30nwh7170mxb5fhsrq5b42fffawsc86v8k5"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://www.stat.washington.edu/mclust/")
    (synopsis "Gaussian mixture modelling")
    (description
     "This package provides Gaussian finite mixture models fitted via
EM algorithm for model-based clustering, classification, and density
estimation, including Bayesian regularization, dimension reduction for
visualisation, and resampling-based inference.")
    (license license:gpl2+)))

(define-public python-fastcluster
  (package
    (inherit r-fastcluster)
    (name "python-fastcluster")
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "src/python") #t)))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))))

(define-public python2-fastcluster
  (package-with-python2 python-fastcluster))

(define-public bismark
  (package
    (name "bismark")
    (version "0.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/FelixKrueger/Bismark/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1204i0pa02ll2jn5pnxypkclnskvv7a2nwh5nxhagmhxk9wfv9sq"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin"))
                   (docdir  (string-append (assoc-ref outputs "out")
                                           "/share/doc/bismark"))
                   (docs    '("Bismark_User_Guide.pdf"
                              "RELEASE_NOTES.txt"))
                   (scripts '("bismark"
                              "bismark_genome_preparation"
                              "bismark_methylation_extractor"
                              "bismark2bedGraph"
                              "bismark2report"
                              "coverage2cytosine"
                              "deduplicate_bismark"
                              "bismark_sitrep.tpl"
                              "bam2nuc"
                              "bismark2summary")))
               (mkdir-p docdir)
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin))
                         scripts)
               (for-each (lambda (file) (install-file file docdir))
                         docs)
               #t))))))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/bismark/")
    (synopsis "Map bisulfite treated sequence reads and analyze methylation")
    (description "Bismark is a program to map bisulfite treated
sequencing reads to a genome of interest and perform methylation calls
in a single step.  The output can be easily imported into a genome
viewer, such as SeqMonk, and enables a researcher to analyse the
methylation levels of their samples straight away.  Its main features
are:

@itemize
@item Bisulfite mapping and methylation calling in one single step
@item Supports single-end and paired-end read alignments
@item Supports ungapped and gapped alignments
@item Alignment seed length, number of mismatches etc are adjustable
@item Output discriminates between cytosine methylation in CpG, CHG
  and CHH context
@end itemize\n")
    (license license:gpl3+)))

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

(define-public r-bsgenome-hsapiens-ucsc-mm9-masked
  (package
    (name "r-bsgenome-hsapiens-ucsc-mm9-masked")
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
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "GenomicFiles" version))
       (sha256
        (base32
         "03yqkl2yjdz999j1y7azcs16vg0vydrqs6sxcfkgn11fiwi6i3l2"))))
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

(define-public kentutils
  (package
    (name "kentutils")
    ;; 302.1.0 is out, but the only difference is the inclusion of
    ;; pre-built binaries.
    (version "302.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ENCODE-DCC/kentUtils/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "134aja3k1cj32kbk1nnw0q9gxjb2krr15q6sga8qldzvc0585rmm"))
       (modules '((guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw)))
       (snippet
        '(begin
           ;; Only the contents of the specified directories are free
           ;; for all uses, so we remove the rest.  "hg/autoSql" and
           ;; "hg/autoXml" are nominally free, but they depend on a
           ;; library that is built from the sources in "hg/lib",
           ;; which is nonfree.
           (let ((free (list "." ".."
                             "utils" "lib" "inc" "tagStorm"
                             "parasol" "htslib"))
                 (directory? (lambda (file)
                               (eq? 'directory (stat:type (stat file))))))
             (for-each (lambda (file)
                         (and (directory? file)
                              (delete-file-recursively file)))
                       (map (cut string-append "src/" <>)
                            (scandir "src"
                                     (lambda (file)
                                       (not (member file free)))))))
           ;; Only make the utils target, not the userApps target,
           ;; because that requires libraries we won't build.
           (substitute* "Makefile"
             ((" userApps") " utils"))
           ;; Only build libraries that are free.
           (substitute* "src/makefile"
             (("DIRS =.*") "DIRS =\n")
             (("cd jkOwnLib.*") "")
             ((" hgLib") "")
             (("cd hg.*") ""))
           (substitute* "src/utils/makefile"
             ;; These tools depend on "jkhgap.a", which is part of the
             ;; nonfree "src/hg/lib" directory.
             (("raSqlQuery") "")
             (("pslLiftSubrangeBlat") "")

             ;; Do not build UCSC tools, which may require nonfree
             ;; components.
             (("ALL_APPS =.*") "ALL_APPS = $(UTILS_APPLIST)\n"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(;; There is no global test target and the test target for
       ;; individual tools depends on input files that are not
       ;; included.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "Makefile"
               (("/bin/echo") (which "echo")))
             #t))
         (add-after 'unpack 'prepare-samtabix
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "samtabix")
                               "samtabix")
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (copy-recursively "bin" bin))
             #t)))))
    (native-inputs
     `(("samtabix"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "http://genome-source.cse.ucsc.edu/samtabix.git")
                 (commit "10fd107909c1ac4d679299908be4262a012965ba")))
           (sha256
            (base32
             "0c1nj64l42v395sa84n7az43xiap4i6f9n9dfz4058aqiwkhkmma"))))))
    (inputs
     `(("zlib" ,zlib)
       ("tcsh" ,tcsh)
       ("perl" ,perl)
       ("libpng" ,libpng)
       ("mysql" ,mysql)
       ("openssl" ,openssl)))
    (home-page "http://genome.cse.ucsc.edu/index.html")
    (synopsis "Assorted bioinformatics utilities")
    (description "This package provides the kentUtils, a selection of
bioinformatics utilities used in combination with the UCSC genome
browser.")
    ;; Only a subset of the sources are released under a non-copyleft
    ;; free software license.  All other sources are removed in a
    ;; snippet.  See this bug report for an explanation of how the
    ;; license statements apply:
    ;; https://github.com/ENCODE-DCC/kentUtils/issues/12
    (license (license:non-copyleft
              "http://genome.ucsc.edu/license/"
              "The contents of this package are free for all uses."))))

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

(define-public r-rook
  (package
    (name "r-rook")
    (version "1.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rook" version))
       (sha256
        (base32
         "00s9a0kr9rwxvlq433daxjk4ji8m0w60hjdprf502msw9kxfrx00"))))
    (properties `((upstream-name . "Rook")))
    (build-system r-build-system)
    (propagated-inputs `(("r-brew" ,r-brew)))
    (home-page "http://cran.r-project.org/web/packages/Rook")
    (synopsis "Web server interface for R")
    (description
     "This package contains the Rook specification and convenience
software for building and running Rook applications.")
    (license license:gpl2)))

(define-public r-rmtstat
  (package
    (name "r-rmtstat")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RMTstat" version))
       (sha256
        (base32
         "1nn25q4kmh9kj975sxkrpa97vh5irqrlqhwsfinbck6h6ia4rsw1"))))
    (properties `((upstream-name . "RMTstat")))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/RMTstat")
    (synopsis "Distributions, statistics and tests derived from random matrix theory")
    (description
     "This package provides functions for working with the Tracy-Widom
laws and other distributions related to the eigenvalues of large
Wishart matrices.")
    (license license:bsd-3)))

(define-public r-lmoments
  (package
    (name "r-lmoments")
    (version "1.2-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Lmoments" version))
       (sha256
        (base32
         "13p0r4w16jvjnyjmkhkp3dwdfr1gap2l0k4k5jy41m8nc5fvcx79"))))
    (properties `((upstream-name . "Lmoments")))
    (build-system r-build-system)
    (home-page "http://www.tilastotiede.fi/juha_karvanen.html")
    (synopsis "L-moments and quantile mixtures")
    (description
     "This package contains functions to estimate L-moments and
trimmed L-moments from the data.  It also contains functions to
estimate the parameters of the normal polynomial quantile mixture and
the Cauchy polynomial quantile mixture from L-moments and trimmed
L-moments.")
    (license license:gpl2)))

(define-public r-distillery
  (package
    (name "r-distillery")
    (version "1.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "distillery" version))
       (sha256
        (base32
         "12m4cacvc18fd3aayc8iih5q6bwsmvf29b55fwp7vs8wp1h8nd8c"))))
    (build-system r-build-system)
    (home-page "http://www.ral.ucar.edu/staff/ericg")
    (synopsis "Functions for confidence intervals and object information")
    (description
     "This package provides some very simple method functions for
confidence interval calculation and to distill pertinent information
from a potentially complex object; primarily used in common with the
packages extRemes and SpatialVx.")
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

(define-public r-rcppeigen
  (package
    (name "r-rcppeigen")
    (version "0.3.2.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "RcppEigen" version))
        (sha256
          (base32
            "0fy9kr03160f1ywzff3p380s8a59jz7d2d0rggb14g2y0slzpbr5"))))
    (properties `((upstream-name . "RcppEigen")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "http://eigen.tuxfamily.org")
    (synopsis "Rcpp integration for the Eigen linear algebra library")
    (description
     "This package provides an R interface for the linear algebra
library Eigen.  This is achieved using Rcpp. 'Eigen' is a C++ template
library for linear algebra: matrices, vectors, numerical solvers and
related algorithms.  It supports dense and sparse matrices on integer,
floating point and complex numbers, decompositions of such matrices,
and solutions of linear systems.")
    ;; This package bundles the header files of a version of Eigen,
    ;; which is licensed under MPLv2.
    (license license:gpl2+)))

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

(define-public r-lme4
  (package
    (name "r-lme4")
    (version "1.1-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lme4" version))
       (sha256
        (base32
         "0j60l5kgx1wvw2wm3jwfqwi63hammaq8gfcxzwa4h552likvaxi9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-minqa" ,r-minqa)
       ("r-nloptr" ,r-nloptr)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "http://cran.r-project.org/web/packages/lme4")
    (synopsis "Linear mixed-effects models")
    (description
     "This package provides linear and generalized linear
mixed-effects models.  The models and their components are represented
using S4 classes and methods.  The core computational algorithms are
implemented using the @code{Eigen} C++ library for numerical linear
algebra and @code{RcppEigen} \"glue\".")
    (license license:gpl2+)))

(define-public r-pbkrtest
  (package
    (name "r-pbkrtest")
    (version "0.4-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbkrtest" version))
       (sha256
        (base32
         "00cw18q7wvddzjrbxz917wkix6r7672vi2wmsp4gwgzady8vha4x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lme4" ,r-lme4)))
    (home-page "http://people.math.aau.dk/~sorenh/software/pbkrtest/")
    (synopsis "Parametric bootstrap and methods for mixed model comparison")
    (description
     "This package provides tools for mixed effects models.
Attention is on mixed effects models as implemented in the @code{lme4}
package.  This package implements a parametric bootstrap test and a
Kenward Roger modification of F-tests for linear mixed effects models
and a parametric bootstrap test for generalized linear mixed models.")
    (license license:gpl2+)))

(define-public r-matrixmodels
  (package
    (name "r-matrixmodels")
    (version "0.4-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MatrixModels" version))
       (sha256
        (base32
         "0cyfvhci2p1vr2x52ymkyqqs63x1qchn856dh2j94yb93r08x1zy"))))
    (properties `((upstream-name . "MatrixModels")))
    (build-system r-build-system)
    (home-page "http://Matrix.R-forge.R-project.org/")
    (synopsis "Modelling with sparse and dense matrices")
    (description
     "This package provides tools for modelling with sparse and dense
@code{Matrix} matrices, using modular prediction and response module
classes.")
    (license license:gpl2+)))

(define-public r-quantreg
  (package
    (name "r-quantreg")
    (version "5.29")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quantreg" version))
       (sha256
        (base32
         "098gy8xv9kcl5y0cm93b8chr5sm6crrdxi20bkx9lmwmybl3himv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrixmodels" ,r-matrixmodels)
       ("r-sparsem" ,r-sparsem)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://www.r-project.org")
    (synopsis "Methods for quantile regression")
    (description
     "This package provides estimation and inference methods for
models of conditional quantiles: linear and nonlinear parametric and
non-parametric (total variation penalized) models for conditional
quantiles of a univariate response and several methods for handling
censored survival data.  Portfolio selection methods based on expected
shortfall risk are also included.")
    (license license:gpl2+)))

(define-public r-car
  (package
    (name "r-car")
    (version "2.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "car" version))
       (sha256
        (base32
         "0a6v7rsd1xsdyapnfqy37m7c4kx9wslkzsizc9k0lmnba0bwyfgx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-pbkrtest" ,r-pbkrtest)
       ("r-quantreg" ,r-quantreg)))
    (home-page "https://r-forge.r-project.org/projects/car/")
    (synopsis "Supporting code for \"An R Companion to Applied Regression\"")
    (description
     "This package provides functions and datasets to accompany J. Fox and S. Weisberg,
\"An R Companion to Applied Regression\", Second Edition, Sage,
2011.")
    (license license:gpl2+)))
