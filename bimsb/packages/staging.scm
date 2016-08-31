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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
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

(define-public infernal-1.0
  (package
    (name "infernal")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              ;; There are no versioned tarballs
              (uri (string-append "http://eddylab.org/software/infernal/"
                                  "infernal-" version ".tar.gz"))
              (sha256
               (base32
                "1ba3av8xg4309dpy1ls73nxk2v7ri0yp0ix6ad5b1j35x319my64"))))
    (build-system gnu-build-system)
    (arguments
     ;; We need to disable tests because we don't seem to have
     ;; getopts.pl.
     `(#:tests? #f))
    (native-inputs
     `(("perl" ,perl))) ; for tests
    (synopsis "Inference of RNA alignments")
    (description "Infernal (\"INFERence of RNA ALignment\") is a tool
for searching DNA sequence databases for RNA structure and sequence
similarities.  It is an implementation of a special case of profile
stochastic context-free grammars called @dfn{covariance
models} (CMs). A CM is like a sequence profile, but it scores a
combination of sequence consensus and RNA secondary structure
consensus, so in many cases, it is more capable of identifying RNA
homologs that conserve their secondary structure more than their
primary sequence.")
    (home-page "http://eddylab.org/infernal/")
    (license license:bsd-3)))

(define-public r-hwriter
  (package
    (name "r-hwriter")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hwriter" version))
       (sha256
        (base32
         "0arjsz854rfkfqhgvpqbm9lfni97dcjs66isdsfvwfd2wz932dbb"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/hwriter")
    (synopsis "Output R objects in HTML format")
    (description
     "This package provides easy-to-use and versatile functions to
output R objects in HTML format.")
    (license license:lgpl2.1+)))

(define-public r-rjson
  (package
    (name "r-rjson")
    (version "0.2.15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rjson" version))
       (sha256
        (base32
         "1vzjyvf57k1fjizlk28rby65y5lsww5qnfvgnhln74qwda7hvl3p"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/rjson")
    (synopsis "JSON library for R")
    (description
     "This package provides functions to convert R objects into JSON
objects and vice-versa.")
    (license license:gpl2+)))

(define-public r-rpart
  (package
    (name "r-rpart")
    (version "4.1-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rpart" version))
       (sha256
        (base32
         "119dvh2cpab4vq9blvbkil5hgq6w018amiwlda3ii0fki39axpf5"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/rpart")
    (synopsis "Recursive partitioning and regression trees")
    (description
     "This package provides recursive partitioning functions for
classification, regression and survival trees.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-geneplotter
  (package
    (name "r-geneplotter")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "geneplotter" version))
       (sha256
        (base32
         "0lvrywl0251g4y0h0qlgkbg4l83ja5544c85z1wj30qxiy77iqc2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-lattice" ,r-lattice)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "http://bioconductor.org/packages/geneplotter")
    (synopsis "Graphics functions for genomic data")
    (description
     "This package provides functions for plotting genomic data.")
    (license license:artistic2.0)))

(define-public r-hmisc
  (package
    (name "r-hmisc")
    (version "3.17-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Hmisc" version))
       (sha256
        (base32
         "1hr2kycpm0h3li9gnlbx9pl6h13das7g2wqfk6cip1kx6lv00ypw"))))
    (properties `((upstream-name . "Hmisc")))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-acepack" ,r-acepack)
       ("r-cluster" ,r-cluster)
       ("r-data-table" ,r-data-table)
       ("r-foreign" ,r-foreign)
       ("r-formula" ,r-formula)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-nnet" ,r-nnet)
       ("r-rpart" ,r-rpart)))
    (home-page "http://biostat.mc.vanderbilt.edu/Hmisc")
    (synopsis "Miscellaneous data analysis and graphics functions")
    (description
     "This package contains many functions useful for data analysis,
high-level graphics, utility operations, functions for computing
sample size and power, importing and annotating datasets, imputing
missing values, advanced table making, variable clustering, character
string manipulation, conversion of R objects to LaTeX code, and
recoding variables.")
    (license license:gpl2+)))

(define-public r-annotate
  (package
    (name "r-annotate")
    (version "1.50.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "annotate" version))
       (sha256
        (base32
         "00wnhbjp5i6a5vyvlq4f5hs8qngjxz7fm869kla1spmd0dp2ynsy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-xml" ,r-xml)
       ("r-xtable" ,r-xtable)))
    (home-page
     "http://bioconductor.org/packages/annotate")
    (synopsis "Annotation for microarrays")
    (description "This package provides R enviroments for the
annotation of microarrays.")
    (license license:artistic2.0)))

(define-public r-genefilter
  (package
    (name "r-genefilter")
    (version "1.54.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "genefilter" version))
       (sha256
        (base32
         "1hmz6as0njvrsrdbgmk72jyclnnqvfdvp6kqv456h43ldq2ajfv5"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "http://bioconductor.org/packages/genefilter")
    (synopsis "Filter genes from high-throughput experiments")
    (description
     "This package provides basic functions for filtering genes from
high-throughput experiments.")
    (license license:artistic2.0)))

(define-public r-deseq2
  (package
    (name "r-deseq2")
    (version "1.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "DESeq2" version))
       (sha256
        (base32
         "12h77f0dpi5xaj7aqf50kkyn6lq9j7bcsly1r0ffmyfcszrp1sfx"))))
    (properties `((upstream-name . "DESeq2")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-against-armadillo
           (lambda _
             (substitute* "src/Makevars"
               (("PKG_LIBS =" prefix)
                (string-append prefix "-larmadillo"))))))))
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-genefilter" ,r-genefilter)
       ("r-geneplotter" ,r-geneplotter)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hmisc" ,r-hmisc)
       ("r-iranges" ,r-iranges)
       ("r-locfit" ,r-locfit)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-s4vectors" ,r-s4vectors)
       ("r-summarizedexperiment" ,r-summarizedexperiment)))
    (home-page "http://bioconductor.org/packages/DESeq2")
    (synopsis "Differential gene expression analysis")
    (description
     "This package provides functions to estimate variance-mean
dependence in count data from high-throughput nucleotide sequencing
assays and test for differential expression based on a model using the
negative binomial distribution.")
    (license license:lgpl3+)))

(define-public r-matrix
  (package
    (name "r-matrix")
    (version "1.2-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Matrix" version))
       (sha256
        (base32
         "18x3mdq5cdhbk1lw5cj7vbr41lk8w9p4i5kzh8wslgq6p3d9ac3c"))))
    (properties `((upstream-name . "Matrix")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "http://Matrix.R-forge.R-project.org/")
    (synopsis "Sparse and dense matrix classes and methods")
    (description
     "This package provides classes and methods for dense and sparse
matrices and operations on them using LAPACK and SuiteSparse.")
    (license license:gpl2+)))

(define-public r-pheatmap
  (package
    (name "r-pheatmap")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pheatmap" version))
       (sha256
        (base32
         "1ik0k69kb4n7xl3bkx4p09kw08ri93855zcsxq1c668171jqfiji"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gtable" ,r-gtable)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-scales" ,r-scales)))
    (home-page
     "http://cran.r-project.org/web/packages/pheatmap")
    (synopsis "Pretty heatmaps")
    (description
     "This package provides an implementation of heatmaps that offers
more control over dimensions and appearance.")
    (license license:gpl2+)))

(define-public r-sendmailr
  (package
    (name "r-sendmailr")
    (version "1.2-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sendmailR" version))
       (sha256
        (base32
         "0z7ipywnzgkhfvl4zb2fjwl1xq7b5wib296vn9c9qgbndj6b1zh4"))))
    (properties `((upstream-name . "sendmailR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)))
    (home-page
     "http://cran.r-project.org/web/packages/sendmailR")
    (synopsis "send email using R")
    (description
     "This package contains a simple SMTP client which provides a
portable solution for sending email, including attachment, from within
R.")
    (license license:gpl2+)))

(define-public r-backports
  (package
    (name "r-backports")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "backports" version))
       (sha256
        (base32
         "0s04mbb7imqc00jl37i081y4yf7qdimk687dyrkvb20nixvjvjyh"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/backports")
    (synopsis "Reimplementations of functions introduced since R 3.0.0")
    (description
     "Provides implementations of functions which have been introduced
in R since version 3.0.0.  The backports are conditionally exported
which results in R resolving the function names to the version shipped
with R (if available) and uses the implemented backports as fallback.
This way package developers can make use of the new functions without
worrying about the minimum required R version.")
    (license license:gpl2+)))

(define-public r-checkmate
  (package
    (name "r-checkmate")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "checkmate" version))
       (sha256
        (base32
         "1nqyi58jl33af82y8kw8iy9xbna2080y1khhy90kf6lim6q74024"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)))
    (home-page "https://github.com/mllg/checkmate")
    (synopsis "Fast and versatile argument checks")
    (description
     "This package provides tests and assertions to perform frequent
argument checks.  A substantial part of the package was written in C
to minimize any worries about execution time overhead.")
    (license license:bsd-3)))

(define-public r-bbmisc
  (package
    (name "r-bbmisc")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BBmisc" version))
       (sha256
        (base32
         "0cw2mhw7qhdrx91zzd5iwyh7ch9fy4wxga8j63731q6sxr1airjl"))))
    (properties `((upstream-name . "BBmisc")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)))
    (home-page "https://github.com/berndbischl/BBmisc")
    (synopsis "Miscellaneous functions for R package development")
    (description
     "This package provides miscellaneous helper functions for package
development.")
    (license license:bsd-3)))

(define-public r-fail
  (package
    (name "r-fail")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fail" version))
       (sha256
        (base32
         "0vfm6kmpmgsamda5p0sl771kbnsscan31l2chzssyw93kwmams7d"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bbmisc" ,r-bbmisc)
       ("r-checkmate" ,r-checkmate)))
    (home-page "https://github.com/mllg/fail")
    (synopsis "File abstraction interface layer (FAIL)")
    (description
     "This package provides a more comfortable interface to work with
R data or source files in a key-value fashion.")
    (license license:bsd-3)))

(define-public r-batchjobs
  (package
    (name "r-batchjobs")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "BatchJobs" version))
        (sha256
          (base32
            "1kb99024jih5bycc226bl4jyvbbl1sg72q3m2wnlshl7s8p6vva0"))))
    (properties `((upstream-name . "BatchJobs")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-bbmisc" ,r-bbmisc)
        ("r-brew" ,r-brew)
        ("r-checkmate" ,r-checkmate)
        ("r-dbi" ,r-dbi)
        ("r-digest" ,r-digest)
        ("r-fail" ,r-fail)
        ("r-rsqlite" ,r-rsqlite)
        ("r-sendmailr" ,r-sendmailr)
        ("r-stringr" ,r-stringr)))
    (home-page "https://github.com/tudo-r/BatchJobs")
    (synopsis "Batch computing with R")
    (description
      "This package provides Map, Reduce and Filter variants to
generate jobs on batch computing systems like PBS/Torque, LSF, SLURM
and Sun Grid Engine.  Multicore and SSH systems are also supported.")
    (license license:bsd-2)))

(define-public r-annotationforge
  (package
    (name "r-annotationforge")
    (version "1.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "AnnotationForge" version))
       (sha256
        (base32
         "1vkdd1qdv5g680ipw4vwjvn52xn66xpg6ngmwyknz77ckxnnpf4q"))))
    (properties
     `((upstream-name . "AnnotationForge")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-dbi" ,r-dbi)
       ("r-rsqlite" ,r-rsqlite)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xml" ,r-xml)))
    (home-page
     "http://bioconductor.org/packages/AnnotationForge")
    (synopsis
     "Code for Building Annotation Database Packages")
    (description
     "This package provides code for generating Annotation packages and their
databases.  Packages produced are intended to be used with
AnnotationDbi.")
    (license license:artistic2.0)))

(define-public r-rbgl
  (package
    (name "r-rbgl")
    (version "1.48.1")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "RBGL" version))
        (sha256
          (base32
            "1k82zcbyfx3p9hc8r0hwq73krbhakjan8fgbfr6w8z2crfkv3zmz"))))
    (properties `((upstream-name . "RBGL")))
    (build-system r-build-system)
    (propagated-inputs `(("r-graph" ,r-graph)))
    (home-page "http://www.bioconductor.org")
    (synopsis
      "An interface to the BOOST graph library")
    (description
     "This package provides a fairly extensive and comprehensive
interface to the graph algorithms contained in the BOOST library.")
    (license license:artistic2.0)))

(define-public r-gseabase
  (package
    (name "r-gseabase")
    (version "1.34.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "GSEABase" version))
        (sha256
          (base32
            "0l3y7hq3vabildr6djsn50x0kfgihpbqxwvh7rw6k18pv5k4j72c"))))
    (properties `((upstream-name . "GSEABase")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-annotate" ,r-annotate)
        ("r-annotationdbi" ,r-annotationdbi)
        ("r-biobase" ,r-biobase)
        ("r-biocgenerics" ,r-biocgenerics)
        ("r-graph" ,r-graph)
        ("r-xml" ,r-xml)))
    (home-page "http://bioconductor.org/packages/GSEABase")
    (synopsis "Gene set enrichment data structures and methods")
    (description
     "This package provides classes and methods to support Gene Set
Enrichment Analysis (GSEA).")
    (license license:artistic2.0)))

(define-public r-category
  (package
    (name "r-category")
    (version "2.38.0")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "Category" version))
        (sha256
          (base32
            "0c8px9ar589f3iqkbk9vfhwj30dpnxj81h8sfq20cl1cbmcx2a04"))))
    (properties `((upstream-name . "Category")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-annotate" ,r-annotate)
        ("r-annotationdbi" ,r-annotationdbi)
        ("r-biobase" ,r-biobase)
        ("r-biocgenerics" ,r-biocgenerics)
        ("r-genefilter" ,r-genefilter)
        ("r-graph" ,r-graph)
        ("r-gseabase" ,r-gseabase)
        ("r-matrix" ,r-matrix)
        ("r-rbgl" ,r-rbgl)
        ("r-rsqlite" ,r-rsqlite)))
    (home-page "http://bioconductor.org/packages/Category")
    (synopsis "Category analysis")
    (description
      "This package provides a collection of tools for performing
category analysis.")
    (license license:artistic2.0)))

(define-public r-gostats
  (package
    (name "r-gostats")
    (version "2.38.1")
    (source
      (origin
        (method url-fetch)
        (uri (bioconductor-uri "GOstats" version))
        (sha256
          (base32
            "1hhw6vqr8f3g4jzq0v8f2za0r1h117j5s6av87zxs41cv7dq1wb3"))))
    (properties `((upstream-name . "GOstats")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-annotate" ,r-annotate)
        ("r-annotationdbi" ,r-annotationdbi)
        ("r-annotationforge" ,r-annotationforge)
        ("r-biobase" ,r-biobase)
        ("r-category" ,r-category)
        ("r-go-db" ,r-go-db)
        ("r-graph" ,r-graph)
        ("r-rbgl" ,r-rbgl)))
    (home-page "http://bioconductor.org/packages/GOstats")
    (synopsis "Tools for manipulating GO and microarrays")
    (description
      "This package provides a set of tools for interacting with GO
and microarray data.  A variety of basic manipulation tools for
graphs, hypothesis testing and other simple calculations.")
    (license license:artistic2.0)))

(define-public r-shortread
  (package
    (name "r-shortread")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "ShortRead" version))
       (sha256
        (base32
         "0qlxns4bhwfpafx3km2lnivgl2qyp7n4g1ardm6vrinpq8paxbjg"))))
    (properties `((upstream-name . "ShortRead")))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-hwriter" ,r-hwriter)
       ("r-iranges" ,r-iranges)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-rsamtools" ,r-rsamtools)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)
       ("r-zlibbioc" ,r-zlibbioc)))
    (home-page
     "http://bioconductor.org/packages/ShortRead")
    (synopsis "FASTQ input and manipulation")
    (description
     "This package implements sampling, iteration, and input of FASTQ
files.  The package includes functions for filtering and trimming
reads, and for generating a quality assessment report.  Data are
represented as DNAStringSet-derived objects, and easily manipulated
for a diversity of purposes.  The package also contains legacy support
for early single-end, ungapped alignment formats.")
    (license license:artistic2.0)))

(define-public r-systempiper
  (package
    (name "r-systempiper")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "systemPipeR" version))
       (sha256
        (base32
         "0s2g46a5d5bvx45i3cgmib48wf8hrniyladhm0f7kgcbfx57248m"))))
    (properties `((upstream-name . "systemPipeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-batchjobs" ,r-batchjobs)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-deseq2" ,r-deseq2)
       ("r-edger" ,r-edger)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-go-db" ,r-go-db)
       ("r-gostats" ,r-gostats)
       ("r-limma" ,r-limma)
       ("r-pheatmap" ,r-pheatmap)
       ("r-rjson" ,r-rjson)
       ("r-rsamtools" ,r-rsamtools)
       ("r-shortread" ,r-shortread)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-variantannotation" ,r-variantannotation)))
    (home-page "https://github.com/tgirke/systemPipeR")
    (synopsis "Next generation sequencing workflow and reporting environment")
    (description
     "This R package provides tools for building and running automated
end-to-end analysis workflows for a wide range of @dfn{next generation
sequence} (NGS) applications such as RNA-Seq, ChIP-Seq, VAR-Seq and
Ribo-Seq.  Important features include a uniform workflow interface
across different NGS applications, automated report generation, and
support for running both R and command-line software, such as NGS
aligners or peak/variant callers, on local computers or compute
clusters.  Efficient handling of complex sample sets and experimental
designs is facilitated by a consistently implemented sample annotation
infrastructure.")
    (license license:artistic2.0)))

(define-public r-grohmm
  (package
    (name "r-grohmm")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "groHMM" version))
       (sha256
        (base32
         "1l9mcyzyc548114ysb9r0q7hgzw3yy7gpiahrzkzj6hblc4f1jyp"))))
    (properties `((upstream-name . "groHMM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)))
    (home-page "https://github.com/Kraus-Lab/groHMM")
    (synopsis "GRO-seq analysis pipeline")
    (description
     "This package provides a pipeline for the analysis of GRO-seq
data.")
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

(define-public r-rhtslib
  (package
    (name "r-rhtslib")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rhtslib" version))
       (sha256
        (base32
         "1wgpn9x8abjj7fc087pdavqc3fz0pl5xdh231mgjila18irwlhb3"))))
    (properties `((upstream-name . "Rhtslib")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/nhayden/Rhtslib")
    (synopsis "High-throughput sequencing library as an R package")
    (description
     "This package provides the HTSlib C library for high-throughput
sequence analysis.  The package is primarily useful to developers of
other R packages who wish to make use of HTSlib.")
    (license license:lgpl2.0+)))

(define-public r-bamsignals
  (package
    (name "r-bamsignals")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "bamsignals" version))
       (sha256
        (base32
         "1xqiqvg52p6fcvhr4146djbz79r3j1kmh75mq7rndwglmiybpwmy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rcpp" ,r-rcpp)
       ("r-rhtslib" ,r-rhtslib)
       ("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://bioconductor.org/packages/bamsignals")
    (synopsis "Extract read count signals from bam files")
    (description "This package allows to efficiently obtain count
vectors from indexed bam files.  It counts the number of nucleotide
sequence reads in given genomic ranges and it computes reads profiles
and coverage profiles.  It also handles paired-end data.")
    (license license:gpl2+)))

(define-public clipper
  (package
    (name "clipper")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/YeoLab/clipper/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pflmsvhbf8izbgwhbhj1i7349sw1f55qpqj8ljmapp16hb0p0qi"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; remove unnecessary setup dependency
                  (substitute* "setup.py"
                    (("setup_requires = .*") ""))
                  (for-each delete-file
                            '("clipper/src/peaks.so"
                              "clipper/src/readsToWiggle.so"))
                  (delete-file-recursively "dist/")
                  #t))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2)) ; only Python 2 is supported
    (inputs
     `(("htseq" ,htseq)
       ("python-pybedtools" ,python2-pybedtools)
       ("python-cython" ,python2-cython)
       ("python-scikit-learn" ,python2-scikit-learn)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-pandas" ,python2-pandas)
       ("python-pysam" ,python2-pysam)
       ("python-numpy" ,python2-numpy)
       ("python-scipy" ,python2-scipy)))
    (native-inputs
     `(("python-mock" ,python2-mock) ; for tests
       ("python-pytz" ,python2-pytz) ; for tests
       ("python-setuptools" ,python2-setuptools)))
    (home-page "https://github.com/YeoLab/clipper")
    (synopsis "CLIP peak enrichment recognition")
    (description
     "CLIPper is a tool to define peaks in CLIP-seq datasets.")
    (license license:gpl2)))
