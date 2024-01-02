;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2017 CM Massimo <carlomaria.massimo@mdc-berlin.de>
;;; Copyright © 2018, 2019, 2021 Marcel Schilling <marcel.schilling@uni-luebeck.de>
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
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
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
  #:use-module (gnu packages curl)
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
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
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
  #:use-module (guix-science packages rstudio)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public rstudio-server
  (@ (guix-science packages rstudio) rstudio-server))
(define-public rstudio-server-bimsb
  (deprecated-package "rstudio-server-bimsb"
                      (@ (guix-science packages rstudio) rstudio-server-multi-version)))
(define-public rstudio
  (@ (guix-science packages rstudio) rstudio))

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
         (add-after 'unpack 'patch-imports
           (lambda _
             (substitute* "circ/CIRCexplorer.py"
               (("from genomic_interval import Interval")
                "from circ.genomic_interval import Interval"))
             (substitute* (find-files "test" "\\.py")
               (("from utils import")
                "from .utils import")))))))
    (inputs
     (list python-pysam python-docopt))
    (native-inputs
     (list python-setuptools))
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

(define-public isolator
  (let ((commit "24bafc0a102dce213bfc2b5b9744136ceadaba03")
        (revision "1"))
    (package
      (name "isolator")
      (version (git-version "0.0.2" revision commit))
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

(define-public pacbio-htslib
  (let ((commit "6b6c81388e699c0c0cf2d1f7fe59c5da60fb7b9a")
        (revision "1"))
    (package (inherit htslib-1.1)
      (name "pacbio-htslib")
      (version (git-version "1.1" revision commit))
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
      (version (git-version "0" revision commit))
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
     (list python-numpy python-pandas))
    (native-inputs
     (list python-setuptools))
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
    (version "0.5.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mageck/"
                                  (version-major+minor version)
                                  "/mageck-" version ".tar.gz"))
              (sha256
               (base32
                "1rvbvwr1jsnijy6pplqgzcz0k05ksbparvl6zkdfxfxfg80142ql"))
			  (snippet
			   '(begin
				  (delete-file "bin/RRA")
				  (delete-file "bin/mageckGSEA")))))
    (build-system python-build-system)
    (arguments
     (list
      #:use-setuptools? #f
      #:modules '((guix build python-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
		  (add-after 'unpack 'use-python3
		    (lambda _
			  (substitute* "bin/mageck"
			    (("python2") "python"))))
		  (add-before 'build 'build-rra-and-gsea
		    (lambda _
			  (with-directory-excursion "rra"
			    (invoke "make"))
			  (with-directory-excursion "gsea"
			    (invoke "make"))))
          (delete 'check)
          (add-after 'wrap 'check
            (lambda _
              (let ((tests '(("demo1" "run.sh")
                             ("demo2" "runmageck.sh")
                             ("demo3" "run.sh")
                             ("demo4" "run.sh"))))
                (setenv "PATH"
                        (string-append #$output "/bin:"
                                       (getenv "PATH")))
                (for-each (match-lambda
                            ((dir script)
                             (with-directory-excursion (string-append "demo/" dir)
                               (invoke "bash" script))))
                          tests)))))))
    (inputs
     (list python-numpy
           python-scipy
           python-matplotlib
           python-statsmodels
           python-pyqt
           r-minimal
           r-xtable
           r-gplots))
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
                "1drckp02jgpl8lswa09w10xa6fyd7r8nlg08yhg6c5hls0zbm277"))))
    (arguments
     `(#:tests? #f                      ; no tests included
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
               (("-Wl,-Bstatic ") "")
               (("-Wl,-Bdynamic ") "")
               (("libboost_iostreams.a")
                "libboost_iostreams.so")
               (("libboost_program_options.a")
                "libboost_program_options.so")
               (("-lblas") "-lopenblas"))
             (substitute* "Makefile"
               (("LIB_FLAGS=-lz")
                "LIB_FLAGS=-lz -lcrypto -lssl -lcurl")
               (("LIB_FILES=\\$\\(RMATH_LIB\\)/libRmath.a \
\\$\\(HTSLD_LIB\\)/libhts.a \
\\$\\(BOOST_LIB\\)/libboost_iostreams.a \
\\$\\(BOOST_LIB\\)/libboost_program_options.a")
                "LIB_FILES=$(RMATH_LIB)/libRmath.so \
$(HTSLD_LIB)/libhts.so \
$(BOOST_LIB)/libboost_iostreams.so \
$(BOOST_LIB)/libboost_program_options.so"))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "bin/QTLtools" bin)))))))
    (inputs
     `(("gsl" ,gsl)
       ("boost" ,boost)
       ("curl" ,curl)
       ("rmath-standalone" ,rmath-standalone)
       ("htslib" ,htslib-1.3)
       ("openblas" ,openblas)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))))

(define-public python-multicore-tsne
  (let ((commit "e1a40182068d4815e7fbe523db266caab20773ff")
        (revision "1"))
    (package
      (name "python-multicore-tsne")
      (version (git-version "0" revision commit))
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
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "CPAT" version))
              (sha256
               (base32
                "0dfrwwbhv1n4nh2a903d1qfb30fgxgya89sa70aci3wzf8h2z0vd"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-pysam))
    (native-inputs
     (list python-nose))
    (home-page "https://wlcb.oit.uci.edu/cpat")
    (synopsis "Alignment-free distinction between coding and noncoding RNA")
    (description
     "CPAT is a method to distinguish coding and noncoding RNA by using a
logistic regression model based on four pure sequence-based,
linguistic features: ORF size, ORF coverage, Ficket TESTCODE, and
Hexamer usage bias.  Linguistic features based method does not require
other genomes or protein databases to perform alignment and is more
robust.  Because it is alignment-free, it runs much faster and also
easier to use.")
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

(define-public r-wasabi
  (let ((commit "f31c73eed6bcb9d0be43b607c14211dd899e5a6c")
        (revision "1"))
    (package
      (name "r-wasabi")
      (version (git-version "0.2" revision commit))
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; rhd5f stopped exporting h5write.default which is not used, but
         ;; imported anyways by sleuth breaking the build.
         ;; While waiting for an upstream fix (see
         ;; https://github.com/pachterlab/sleuth/issues/259 and
         ;; https://github.com/pachterlab/sleuth/pull/260), this can be worked
         ;; around by commenting out the superfluous import statement.
         (add-after 'unpack 'patch-NAMESPACE
           (lambda _
             (substitute* "NAMESPACE"
               (("^importFrom\\(rhdf5,h5write\\.default\\)" line)
                (string-append "#" line))))))))
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
    (arguments
     `(#:tests? #false  ; there are two errors
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'python3.9-compat
           (lambda _
             (substitute* "pyfasta/__init__.py"
               (("from fasta import")
                "from pyfasta.fasta import")
               (("from records import")
                "from pyfasta.records import")
               (("from split_fasta import")
                "from pyfasta.split_fasta import"))
             (substitute* "pyfasta/fasta.py"
               (("from records import")
                "from pyfasta.records import"))
             (substitute* "pyfasta/records.py"
               (("cPickle") "pickle")
               (("\\(int, long\\)")
                "(int, int)"))
             (substitute* "pyfasta/split_fasta.py"
               (("from cStringIO import")
                "from io import"))
             (substitute* "tests/test_all.py"
               (("for k in f.iterkeys\\(\\)") "for k in iter(f.keys())")
               (("tests/data/" m)
                (string-append (getcwd) "/" m))))))))
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
    (version "2.13.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/trinityrnaseq/trinityrnaseq.git")
                    (commit (string-append "Trinity-v" version))
                    (recursive? #true)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qszrxqbx4q5pavpgm4rkrh1z1v1mf7qx83vv3fnlqdmncnsf1gv"))))
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
             ;; Do not require version.h, which triggers a local build
             ;; of a vendored htslib.
             (substitute* "trinity-plugins/bamsifter/Makefile"
               (("sift_bam_max_cov.cpp htslib/version.h")
                "sift_bam_max_cov.cpp"))))
         (add-after 'build 'build-plugins
           (lambda _
             ;; Run this in the subdirectory to avoid running the
             ;; tests right here.
             (with-directory-excursion "trinity-plugins"
               (invoke "make" "plugins"))))
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
                 `("PYTHONPATH"  ":" = (,(getenv "GUIX_PYTHONPATH")))
                 `("PATH"        ":" =
                   ,(cons (string-append out "share/trinity/trinity-plugins/BIN")
                          (filter-map (match-lambda
                                        ((name . dir)
                                         (string-append dir "/bin")))
                                      inputs))))
               (symlink (string-append share "Trinity")
                        (string-append bin "Trinity"))))))))
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
       ("htslib" ,htslib)
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

(define-public python-louvain
  (package
    (name "python-louvain")
    (version "0.14")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-louvain" version))
        (sha256
          (base32 "0l89vxibnjw3dfp90vx4v9gfaql84sc6479arl859d473rx0r9g0"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-networkx" ,python-networkx)
        ("python-numpy" ,python-numpy)))
    (home-page "https://github.com/taynaud/python-louvain")
    (synopsis "Louvain algorithm for community detection in large networks.")
    (description "@code{Louvain} algorithm, based on modularity optimization,
can extract the community structure of large networks.")
    (license license:bsd-3)))

(define-public python-snaptools
  (package
    (name "python-snaptools")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "snaptools" version))
       (sha256
        (base32
         "1s5373g5jjbshh3q39zy7dlxr7nda6ksxq9d1gw46h82c4fsmfbn"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-future" ,python-future)
       ("python-h5py" ,python-h5py)
       ("python-louvain@0.14" ,python-louvain)
       ("python-numpy" ,python-numpy)
       ("python-pybedtools" ,python-pybedtools)
       ("python-pysam" ,python-pysam)))
    (home-page "https://github.com/r3fang/SnapTools")
    (synopsis "Tools for processing snap files" )
    (description
     "@code{SnapTools} can operate on snap files the following types of opperations:
@itemize
@item index the reference genome before alingment;
@item align reads to the corresponding reference genome;
@item pre-process by convert pair-end reads into fragments, checking the mapping
quality score, alingment and filtration;
@item create the cell-by-bin matrix. @end itemize")
    (license license:asl2.0)))

;; This package contains a lot of minified JavaScript.
(define-public r-shinywidgets
  (package
    (name "r-shinywidgets")
    (version "0.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyWidgets" version))
       (sha256
        (base32
         "1468yw3kc058c8js4s8wf76jv5njgy52291drgbxwfmpmn6mqf7b"))))
    (properties `((upstream-name . "shinyWidgets")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-shiny" ,r-shiny)))
    (home-page "https://github.com/dreamRs/shinyWidgets")
    (synopsis "Custom inputs widgets for Shiny")
    (description
     "This package is a collection of custom input controls and user interface
components for R Shiny applications.")
    (license license:gpl3)))

;; This depends on r-shinywidgets
(define-public r-isee
  (package
    (name "r-isee")
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "iSEE" version))
       (sha256
        (base32
         "04q13y3xbm0lz83apwfz2c54r91lfx69p722yw07p4hc9a1l6crh"))))
    (properties `((upstream-name . "iSEE")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-circlize" ,r-circlize)
       ("r-colourpicker" ,r-colourpicker)
       ("r-complexheatmap" ,r-complexheatmap)
       ("r-dt" ,r-dt)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-igraph" ,r-igraph)
       ("r-mgcv" ,r-mgcv)
       ("r-rintrojs" ,r-rintrojs)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shiny" ,r-shiny)
       ("r-shinyace" ,r-shinyace)
       ("r-shinydashboard" ,r-shinydashboard)
       ("r-shinyjs" ,r-shinyjs)
       ("r-shinywidgets" ,r-shinywidgets)
       ("r-singlecellexperiment" ,r-singlecellexperiment)
       ("r-summarizedexperiment" ,r-summarizedexperiment)
       ("r-vipor" ,r-vipor)
       ("r-viridislite" ,r-viridislite)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/iSEE/iSEE")
    (synopsis "Interactive SummarizedExperiment explorer")
    (description
     "This package lets you create an interactive Shiny-based
graphical user interface for exploring data stored in
SummarizedExperiment objects, including row- and column-level
metadata.  The interface supports transmission of selections between
plots and tables, code tracking, interactive tours, interactive or
programmatic initialization, preservation of app state, and
extensibility to new panel types via S4 classes.  Special attention is
given to single-cell data in a SingleCellExperiment object with
visualization of dimensionality reduction results.")
    (license license:expat)))

;; This package contains a lot of minified JavaScript and CSS
(define-public r-billboarder
  (package
    (name "r-billboarder")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "billboarder" version))
       (sha256
        (base32
         "1n51xildr8h3fqm3yhalgvq6pwlpzbcv230jg7skp8147wmwcmi7"))))
    (properties `((upstream-name . "billboarder")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-conflicting-vignette
           (lambda _
             (for-each delete-file
                       ;; These files generate some of the same
                       ;; outputs as billboarder.Rmd
                       (append (find-files "inst/doc" "billboarder-.*.Rmd$")
                               ;; Delete generated files.
                               (find-files "inst/doc" "\\.(R|html)$")))
             #t)))))
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-magrittr" ,r-magrittr)
       ("r-scales" ,r-scales)
       ("r-shiny" ,r-shiny)))
    (native-inputs
     `(("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown))) ; this is not mentioned in DESCRIPTION
    (home-page "https://github.com/dreamRs/billboarder")
    (synopsis "Create interactive charts with the JavaScript Billboard library")
    (description
     "This package provides an htmlwidgets interface to billboard.js,
a re-usable easy interface JavaScript chart library, based on D3
version 4 or later.  Chart types include line charts, scatterplots,
bar/lollipop charts, histogram/density plots, pie/donut charts and
gauge charts.  All charts are interactive, and a proxy method is
implemented to smoothly update a chart without rendering it again in
Shiny apps.")
    (license license:expat)))

;; This depends on billboarder
(define-public r-shinymanager
  (package
    (name "r-shinymanager")
    (version "1.0.300")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinymanager" version))
       (sha256
        (base32
         "1zv8rrplp3cknyk51m5cfmkr992k9cbjh89pmrbb8c24izqilq2a"))))
    (properties `((upstream-name . "shinymanager")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-billboarder" ,r-billboarder)
       ("r-dbi" ,r-dbi)
       ("r-dt" ,r-dt)
       ("r-htmltools" ,r-htmltools)
       ("r-openssl" ,r-openssl)
       ("r-r-utils" ,r-r-utils)
       ("r-r6" ,r-r6)
       ("r-rsqlite" ,r-rsqlite)
       ("r-scrypt" ,r-scrypt)
       ("r-shiny" ,r-shiny)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/datastorm-open/shinymanager")
    (synopsis "Authentication management for Shiny applications")
    (description
     "This package provides a simple and secure authentification
mechanism for single Shiny applications.  Credentials are stored in an
encrypted SQLite database.")
    (license license:gpl3)))

;; Lots of JavaScript that may be difficult to build from source.
(define-public r-reactlog
  (package
    (name "r-reactlog")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reactlog" version))
       (sha256
        (base32
         "0vngy7fixiighhaa0db8xfr6962jmrfn3675qrn3hj7rfh7siagz"))))
    (properties `((upstream-name . "reactlog")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-jsonlite" ,r-jsonlite)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://rstudio.github.io/reactlog/")
    (synopsis "Reactivity visualizer for Shiny")
    (description
     "Building interactive web applications with R is incredibly easy with
Shiny.  Behind the scenes, Shiny builds a reactive graph that can quickly
become intertwined and difficult to debug.  The reactlog package provides a
visual insight into that black box of Shiny reactivity by constructing a
directed dependency graph of the application's reactive state at any time
point in a reactive recording.")
    (license license:gpl3)))

;; This depends on r-visnetwork.
;; It also contains a number of minified JavaScript libraries.
(define-public r-diagrammer
  (package
    (name "r-diagrammer")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DiagrammeR" version))
       (sha256
        (base32
         "1gqaqk7jdh37zzadv0aymr9yb8lpqgj3l8n1n3cds38i4zz2d934"))))
    (properties `((upstream-name . "DiagrammeR")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-downloader
            r-dplyr
            r-glue
            r-htmltools
            r-htmlwidgets
            r-igraph
            r-influencer
            r-magrittr
            r-purrr
            r-rcolorbrewer
            r-readr
            r-rlang
            r-rstudioapi
            r-scales
            r-stringr
            r-tibble
            r-tidyr
            r-viridis
            r-visnetwork))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/rich-iannone/DiagrammeR")
    (synopsis "Graph/network visualization")
    (description
     "With the @code{DiagrammeR} package you can create, modify, analyze, and
visualize network graph diagrams.  The output can be incorporated into @code{R Markdown}
documents, integrated with Shiny web apps, converted to other graph
formats, or exported as image files.")
    (license license:expat)))

(define-public r-diagrammersvg
  (package
    (name "r-diagrammersvg")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "DiagrammeRsvg" version))
        (sha256
          (base32 "0j2cm1mx3zrb2k3pcrb96z2z3kws61gyyjsjjv5rqcb5lzdgi65k"))))
    (properties `((upstream-name . "DiagrammeRsvg")))
    (build-system r-build-system)
    (propagated-inputs (list r-v8))
    (home-page "https://github.com/rich-iannone/DiagrammeRsvg")
    (synopsis "Export DiagrammeR graphviz graphs as svg")
    (description
     "This package allows exporting a @code{DiagrammeR Graphviz} objects to
@code{SVG}.")
    (license license:expat)))

(define-public r-shinydashboardplus
  (package
    (name "r-shinydashboardplus")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinydashboardPlus" version))
       (sha256
        (base32
         "0g0cw07z7dlpc7q2abis0cdgwk6vlya0im5lxai82v21r60i5biy"))))
    (properties
     `((upstream-name . "shinydashboardPlus")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-shiny" ,r-shiny)
       ("r-shinydashboard" ,r-shinydashboard)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RinteRface/shinydashboardPlus")
    (synopsis "AdminLTE2 components for shinydashboard")
    (description
     "This package extends shinydashboard with AdminLTE2 components.
AdminLTE2 is a Bootstrap 3 dashboard template.  You can customize
boxes, add timelines and a lot more.")
    ;; The shinydashboardPlus package as a whole is distributed under
    ;; Version 2 of the GPL or any later version.  It includes code
    ;; under other compatible licenses, such as the Expat license.
    (license license:gpl2+)))

;; This package contains minified JavaScript
(define-public r-excelr
  (package
    (name "r-excelr")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "excelR" version))
       (sha256
        (base32
         "1pb4sy54zjv5vrh7gjjv7qlpab74km6mfsmfyl0yhmr0jx01hrw0"))))
    (properties `((upstream-name . "excelR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)))
    (home-page "https://github.com/Swechhya/excelR")
    (synopsis "Wrapper of the JavaScript library jExcel")
    (description
     "This package provides an R interface to the jExcel JavaScript
library to create web-based interactive tables and spreadsheets
compatible with Excel or any other spreadsheet software.")
    (license license:expat)))

;; This package contains a number of minified JavaScript files, some
;; without original source code.
(define-public r-argonr
  (package
    (name "r-argonr")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "argonR" version))
       (sha256
        (base32
         "15hlvansqnky9bnq4r7xza3hb1hzylmhz8117wxz9lxa1wiky2is"))))
    (properties `((upstream-name . "argonR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-rstudioapi" ,r-rstudioapi)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/RinteRface/argonR")
    (synopsis "R interface to Argon HTML design")
    (description
     "This package provides an R wrapper around the argon library.")
    (license license:gpl2)))

;; This package contains minified JavaScript
(define-public r-argondash
  (package
    (name "r-argondash")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "argonDash" version))
       (sha256
        (base32
         "1wykr7y5375g1nb18ynybccxmd948xrr0gdwxxqsfjf782vlgd2d"))))
    (properties `((upstream-name . "argonDash")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-argonr" ,r-argonr)
       ("r-htmltools" ,r-htmltools)
       ("r-shiny" ,r-shiny)))
    (home-page "https://github.com/RinteRface/argonDash")
    (synopsis "Argon Shiny dashboard template")
    (description
     "Create Bootstrap 4 dashboards powered by Argon.")
    (license license:gpl2)))

(define-public r-music
  (let ((commit "7c5834830223957f5d8134c86d6acf653bfff4e7")
        (revision "2"))
    (package
      (name "r-music")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xuranw/MuSiC.git")
                      (commit commit)))
                (sha256
                 (base32
                  "098bv7v4phg8iv5h5c27ql4pzc7mcala82mcd4i1y91cf68d0grz"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-ggplot2" ,r-ggplot2)
         ("r-plyr" ,r-plyr)
         ("r-mcmcpack" ,r-mcmcpack)
         ("r-nnls" ,r-nnls)
         ("r-xbioc" ,r-xbioc)))
      (home-page "https://github.com/xuranw/MuSiC")
      (synopsis "Multi-subject Single Cell deconvolution")
      (description
       "MuSiC is a deconvolution method that utilizes cross-subject scRNA-seq
to estimate cell type proportions in bulk RNA-seq data.")
      (license license:gpl3))))

(define-public r-dsa
  (let ((commit "f181f549fd86feb58955f11e53ee38da67ac4f5e")
        (revision "1"))
    (package
      (name "r-dsa")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/zhandong/DSA/raw/" commit "/"
                      "Package/version_" version "/"
                      "DSA_" version ".tar.gz"))
                (sha256
                 (base32
                  "1m2g760lwyxzqm7vxp5j1haw2qldgysbfj86w27h15adj604129z"))))
      (build-system r-build-system)
      (home-page "https://github.com/zhandong/DSA")
      (synopsis "Digital sorting algorithm")
      (description
       "This package provides functions to implement the @dfn{Digital sorting
algorithm} (DSA) for extracting cell-type specific gene expression profiles
from mixed tissue samples.")
      (license license:gpl2))))
