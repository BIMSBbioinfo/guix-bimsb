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
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages zip)
  #:use-module (srfi srfi-1))

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

(define-public rstudio
  (package
    (name "rstudio")
    (version "0.99.1201")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rstudio/rstudio/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0b3nipdmxrfq5fhwyy0x9hhf54yqmvdkniyj6x3kmhcha0c33l8n"))
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
               (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rmarkdown\"\\)") "if (FALSE)"))
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
               (mkdir "mathjax-23")
               (zero? (system* "unzip" "-qd" "mathjax-23"
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
           (uri "https://s3.amazonaws.com/rstudio-buildtools/mathjax-23.zip")
           (sha256
            (base32 "16fnq4jsifbldjcvrri3g6d7zhbh22k3jas2jpigmmphnmgd6hjj"))))
       ("dictionaries"
        ,(origin
           (method url-fetch)
           (uri "https://s3.amazonaws.com/rstudio-dictionaries/core-dictionaries.zip")
           (sha256
            (base32 "153lg3ai97qzbqp6zjg10dh3sfvz80v42cjw45zwz7gv1risjha3"))))))
    (inputs
     `(("r" ,r)
       ("r-rmarkdown" ,r-rmarkdown) ; TODO: must be linked to another location
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

(define-public python-numexpr/latest
  (package (inherit python-numexpr)
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/05/cd/"
                           "8dbb09b835539234bafc6c5fa02452186da9869e44e7489037ef3994471e"
                           "/numexpr-" version ".tar.gz"))
       (sha256
        (base32
         "0i6iagl2hhbr8q4qzbbjd859v5806vqylq87fq7pi914ps6d6cag"))))))

(define-public python2-numexpr/latest
  (let ((numexpr (package-with-python2 python-numexpr/latest)))
    (package (inherit numexpr)
      ;; Make sure to use special packages for Python 2 instead
      ;; of those automatically rewritten by package-with-python2.
      (propagated-inputs
       `(("python2-numpy" ,python2-numpy)
         ,@(alist-delete "python-numpy"
                         (package-propagated-inputs numexpr)))))))

(define-public python-tables
  (package
    (name "python-tables")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tables" version))
       (sha256
        (base32
         "117s6w7s3yxafpmf3zz3svana7xfrsviw01va1xp7h8ylx8v6r1m"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--hdf5="
                            (assoc-ref %build-inputs "hdf5")))
       ;; FIXME: python-build-system does not pass configure-flags to "build"
       ;; or "check", so we must override the build and check phases.
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "build"
                             (string-append "--hdf5="
                                            (assoc-ref inputs "hdf5"))))))
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "check"
                             (string-append "--hdf5="
                                            (assoc-ref inputs "hdf5")))))))))
    (propagated-inputs
     `(("python-numexpr" ,python-numexpr/latest)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-cython" ,python-cython)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("hdf5" ,hdf5)
       ("bzip2" ,bzip2)
       ("zlib" ,zlib)))
    (home-page "http://www.pytables.org/")
    (synopsis "Hierarchical datasets for Python")
    (description "PyTables is a package for managing hierarchical datasets and
designed to efficently cope with extremely large amounts of data.")
    (license license:bsd-3)))

(define-public python2-tables
  (let ((tables (package-with-python2 python-tables)))
    (package (inherit tables)
      (propagated-inputs
       `(("python2-numexpr" ,python2-numexpr/latest)
         ,@(alist-delete "python-numexpr"
                         (package-propagated-inputs tables)))))))

(define-public seek
  ;; There are no release tarballs.  According to the installation
  ;; instructions at http://seek.princeton.edu/installation.jsp, the latest
  ;; stable release is identified by this changeset ID.
  (let ((changeset "2329130")
        (revision "1"))
    (package
      (name "seek")
      (version (string-append "0-" revision "." changeset))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/libsleipnir/sleipnir")
                      (changeset changeset)))
                (sha256
                 (base32
                  "0qrvilwh18dpbhkf92qvxbmay0j75ra3jg2wrhz67gf538zzphsx"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((srfi srfi-1)
                    (guix build gnu-build-system)
                    (guix build utils))
         #:phases
         (let ((dirs '("SeekMiner"
                       "SeekEvaluator"
                       "SeekPrep"
                       "Distancer"
                       "Data2DB"
                       "PCL2Bin")))
           (modify-phases %standard-phases
             (add-before 'configure 'bootstrap
               (lambda _
                 (zero? (system* "bash" "gen_auto"))))
             (add-after 'build 'build-additional-tools
               (lambda* (#:key make-flags #:allow-other-keys)
                 (fold (lambda (dir result)
                         (with-directory-excursion (string-append "tools/" dir)
                           (and result
                                (zero? (apply system*
                                              `("make" ,@make-flags))))))
                       #t dirs)))
             (add-after 'install 'install-additional-tools
               (lambda* (#:key make-flags #:allow-other-keys)
                 (fold (lambda (dir result)
                         (with-directory-excursion (string-append "tools/" dir)
                           (and result
                                (zero? (apply system*
                                              `("make" ,@make-flags "install"))))))
                       #t dirs)))))))
      (inputs
       `(("gsl" ,gsl)
         ("boost" ,boost)
         ("libsvm" ,libsvm)
         ("readline" ,readline)
         ("gengetopt" ,gengetopt)
         ("log4cpp" ,log4cpp)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("perl" ,perl)))
      (home-page "http://seek.princeton.edu")
      (synopsis "Gene co-expression search engine")
      (description
       "SEEK is a computational gene co-expression search engine.  SEEK provides
biologists with a way to navigate the massive human expression compendium that
now contains thousands of expression datasets.  SEEK returns a robust ranking
of co-expressed genes in the biological area of interest defined by the user's
query genes.  It also prioritizes thousands of expression datasets according
to the user's query of interest.")
      (license license:cc-by3.0))))

(define-public multiqc
  (package
    (name "multiqc")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/23/d9/95a5e62eea95f220007d7a6683d0da1ee1d9f99e6932bcff793b6601ef15/multiqc-"
             version ".tar.gz"))
       (sha256
        (base32
         "0avw11h63ldpxy5pizc3wl1wa01ha7q10wb240nggsjz3jaqvyiy"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)
       ("python-simplejson" ,python-simplejson)
       ("python-pyyaml" ,python-pyyaml)
       ("python-click" ,python-click)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://multiqc.info")
    (synopsis "Aggregate bioinformatics analysis reports")
    (description
     "MultiQC is a tool to aggregate bioinformatics results across many
samples into a single report.  It contains modules for a large number of
common bioinformatics tools.")
    (license license:gpl3)))
