;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2024 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (guix build-system pyproject)
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
  #:use-module (past packages bioinformatics)
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

(define-public python-biomagic
  (deprecated-package "python-biomagic" python-magic-impute))

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

(define-public squid
  (deprecated-package "squid" eddylab-squid))

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

(define-public python-pyfasta
  ;; The release on pypi does not contain the test data files.
  (let ((commit "c2f0611c5311f1b1466f2d56560447898b4a8b03")
        (revision "1"))
    (package
      (name "python-pyfasta")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/brentp/pyfasta")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0a189id3fbv88gssyk6adbmz2ll1mqpmyw8vxmx3fi955gvaq9j7"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'python3.10-compat
             (lambda _
               (substitute* "pyfasta/__init__.py"
                 (("from fasta import")
                  "from pyfasta.fasta import")
                 (("from records import")
                  "from pyfasta.records import")
                 (("from split_fasta import")
                  "from pyfasta.split_fasta import")
                 (("in f.iteritems")
                  "in f.items"))
               (substitute* "pyfasta/fasta.py"
                 (("from collections import Mapping")
                  "from collections.abc import Mapping")
                 (("from records import")
                  "from pyfasta.records import"))
               (substitute* "pyfasta/records.py"
                 (("cPickle") "pickle")
                 (("\\(int, long\\)")
                  "(int, int)")
                 ;; XXX: it's not clear if this is really correct.
                 (("buffer\\(self\\)")
                  "memoryview(bytes(str(self), encoding='utf-8'))")
                 (("sys.maxint") "sys.maxsize"))
               (substitute* "pyfasta/split_fasta.py"
                 (("from cStringIO import")
                  "from io import")
                 (("in lens.iteritems") "in lens.items"))
               (substitute* "tests/test_all.py"
                 (("f.keys\\(\\)\\) == \\['a-extra'")
                  "list(f.keys())) == ['a-extra'")
                 (("f.iterkeys\\(\\)") "iter(f.keys())")
                 (("tests/data/" m)
                  (string-append (getcwd) "/" m))))))))
      (propagated-inputs (list python-numpy))
      (native-inputs (list python-nose))
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
      (license license:expat))))

(define-public python-ont-tombo
  (deprecated-package "python-ont-tombo"
                      (@ (gnu packages bioinformatics) tombo)))

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
