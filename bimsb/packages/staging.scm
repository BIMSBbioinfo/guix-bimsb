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

(define-public python-biomagic
  (deprecated-package "python-biomagic" python-magic-impute))

(define-public squid
  (deprecated-package "squid" eddylab-squid))

(define-public footprint-pipeline
  (package
    (name "footprint-pipeline")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aslihankarabacak/FootprintPipeline/")
                    (commit "6ff3646e71203e1147f0c9e99dab930905fb6763")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bzl5v09jz1lxlp6nhc3cnfg9kl74gs90vrdyj51l02jzcxahx4f"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build-system
            (lambda _
              ;; The checks for R packages fail.
              (substitute* "configure.ac"
                (("AX_R_PACKAGE.*") "echo skip\n"))
              (delete-file "configure")
              (with-output-to-file "VERSION"
                (lambda () (display #$version)))))
          (add-after 'configure 'fix-broken-shebang
            (lambda _
              (substitute* "run_footprinting.R.in"
                (("@RSCRIPT@") (which "Rscript")))))
          (add-after 'install 'wrap-executable
            (lambda _
              (wrap-program (string-append #$output "/bin/find_footprints.sh")
                `("R_LIBS_SITE" ":" = (,(getenv "R_LIBS_SITE")))))))))
    (inputs
     (list r-minimal
           r-mixtools
           r-gtools
           r-genomicranges
           perl
           samtools-1.1
           bedtools-2.18))
    (native-inputs (list autoconf automake))
    (home-page "https://ohlerlab.mdc-berlin.de/software/Reproducible_footprinting_139/")
    (synopsis "Find transcription factor footprints in ATAC-seq or DNase-seq data")
    (description "This is a pipeline to find transcription factor
footprints in ATAC-seq or DNase-seq data.")
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
