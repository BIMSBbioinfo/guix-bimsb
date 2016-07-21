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

(define-module (bimsb packages bioinformatics-nonfree)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix licenses-nonfree) #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip))

(define-public bcl2fastq
  (package
    (name "bcl2fastq")
    (version "2.17.1.14")
    (source (origin
              (method url-fetch)
              ;; Download manually from here:
              ;; ftp://webdata2:webdata2@ussd-ftp.illumina.com/downloads/Software/bcl2fastq/bcl2fastq2-v2.17.1.14.tar.zip
              (uri (string-append "file:///gnu/remote/bcl2fastq2-v"
                                  version ".tar.zip"))
              (sha256
               (base32
                "09qcz1l5yw46n5crbxsgsj0m9p404s012m81cx1rwqh2pzw6dx9w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DBCL2FASTQ_VERSION:STRING=" ,version)
             "-DBCL2FASTQ_NAME_SHORT:STRING=bcl2fastq"
             "-DBCL2FASTQ_NAME_LONG:STRING=BCL to FASTQ file converter"
             "-DBCL2FASTQ_COPYRIGHT:STRING=Copyright (c) 2007-2015 Illumina, Inc."
             (string-append "-DBCL2FASTQ_SOURCE_DIR:STRING=" (getcwd) "/bcl2fastq/src"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and (zero? (system* "unzip" source))
                 (zero? (system* "tar" "-xvf"
                                 (string-append "bcl2fastq2-v"
                                                ,version ".tar.gz"))))))
         (add-after 'unpack 'enter-dir (lambda _ (chdir "bcl2fastq/src") #t))
         (add-after 'enter-dir 'patch-stuff
                    (lambda _
                      ;; Update for boost 1.54 -> 1.56
                      (substitute* "cxx/lib/io/Xml.cpp"
                        (("xml_writer_make_settings\\(")
                         "xml_writer_make_settings<ptree::key_type>("))
                      ;; Do not use bundled libraries
                      (substitute* "cmake/cxxConfigure.cmake"
                        (("\"\\$\\{LIBEXSLT_LIBRARIES\\}\"")
                         (string-append (assoc-ref %build-inputs "libxslt")
                                        "/lib/libexslt.so"))
                        (("find_library_redist\\(LIBXSLT .*")
                         "bcl2fastq_find_library(LIBXSLT libxslt/xsltconfig.h xslt)\n")
                        (("find_library_redist\\(LIBXML2 .*")
                         "bcl2fastq_find_library(LIBXML2 libxml/xpath.h xml2)\n")
                        (("find_library_redist\\(LIBEXSLT .*")
                         "bcl2fastq_find_library(LIBEXSLT libexslt/exslt.h exslt)\n")
                        (("redist_package") "#")
                        (("^  +\"--prefix=.*") ""))
                      ;; Work around broken version checking
                      (substitute* "CMakeLists.txt"
                        (("BCL2FASTQ_LIBXML2_VERSION 2.7.8")
                         "BCL2FASTQ_LIBXML2_VERSION 2.9.2")
                        (("BCL2FASTQ_LIBXSLT_VERSION 1.1.26")
                         "BCL2FASTQ_LIBXSLT_VERSION 1.1.28"))
                      #t)))))
    (inputs
     `(("boost" ,boost)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("zlib" ,zlib)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://support.illumina.com/downloads/bcl2fastq_conversion_software.html")
    (synopsis "Convert files in BCL format to FASTQ")
    (description
     "bcl2fastq is conversion software, which can be used to both
demultiplex data and convert BCL files to FASTQ files.")
    (license (nonfree:non-free
              (string-append "http://support.illumina.com/content/dam"
                             "/illumina-support/documents/documentation"
                             "/software_documentation/bcl2fastq/"
                             "bcl2fastq2-v2-16-EULA.pdf")
              "This is an extremely restrictive license and it would
be better to avoid using this proprietary program.  I encourage people
to write a free software alternative rather than using this tool."))))

(define-public bcl2fastq1
  (package (inherit bcl2fastq)
    (name "bcl2fastq1")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              ;; Download manually from here:
              ;; ftp://webdata:webdata@ussd-ftp.illumina.com/Downloads/Software/bcl2fastq/bcl2fastq-1.8.4.tar.bz2
              (uri (string-append "file:///gnu/remote/bcl2fastq-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "14s15h8kk9vqqwy0hykdzffz6zlkbqpvg5wnnfiwd2x7cwxizikm"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir (lambda _ (chdir "src") #t))
         (add-after
          'enter-dir 'patch-stuff
          (lambda _
            ;; Update to boost 1.56
            (substitute* "c++/lib/demultiplex/BclDemultiplexer.cpp"
              (("boost::bind\\(\\&fs::path::string, _1\\)")
               (string-append "boost::bind("
                              "static_cast< std::string const & "
                              "(boost::filesystem::path::*)() const >"
                              "(&boost::filesystem::path::string), _1)")))
            (substitute* "c++/lib/io/PtreeXml.cpp"
              (("xml_writer_make_settings\\(")
               "xml_writer_make_settings<ptree::key_type>("))
            #t))
         (add-after
          'install 'wrap-perl-scripts
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make sure perl scripts finds all perl inputs at runtime.
            (let ((out (assoc-ref outputs "out")))
              (for-each (lambda (prog)
                          (wrap-program (string-append out "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(getenv "PERL5LIB")))))
                        '("configureBclToFastq.pl"
                          "configureQseqToFastq.pl"
                          "configureValidation.pl"))
              #t))))))
    (inputs
     `(("perl-xml-simple" ,perl-xml-simple)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl" ,perl)
       ,@(package-inputs bcl2fastq)))))

(define-public bowtie1
  (package
    (inherit bowtie)
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bowtie-bio/bowtie/"
                                  version "/bowtie-" version "-src.zip"))
              (sha256
               (base32
                "1bipnvm94nlzbawix09bkfdvig41qr235qyrjccgszi04p4crsdi"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                  (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                  (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\"")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no "check" target
       #:make-flags
       (list "all"
             "WITH_TBB=1"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("tbb" ,tbb)))
    (home-page "http://bowtie-bio.sourceforge.net/index.shtml")
    (synopsis "Fast aligner for short nucleotide sequence reads")
    (description
     "Bowtie is a fast, memory-efficient short read aligner.  It aligns short
DNA sequences (reads) to the human genome at a rate of over 25 million 35-bp
reads per hour.  Bowtie indexes the genome with a Burrows-Wheeler index to
keep its memory footprint small: typically about 2.2 GB for the human
genome (2.9 GB for paired-end).")
    (supported-systems '("x86_64-linux"))
    (license nonfree:artistic1.0)))

(define-public dinup
  (package
    (name "dinup")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tongji.edu.cn/~zhanglab/DiNuP/dinup_"
                    version ".tar.gz"))
              (sha256
               (base32
                "14s15h8kk9vqqwy0hykdzffz6zlkbqpvg5wnnfiwd2x7cwxizikm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no "test" target
       #:python ,python-2
       #:phases (alist-replace
                 'unpack
                 ;; The release tarball contains loose files.
                 (lambda* (#:key source #:allow-other-keys)
                   (and (mkdir "dinup")
                        (zero? (system* "tar" "-C" "dinup" "-xvf" source))
                        (chdir "dinup")))
                 %standard-phases)))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)))
    (home-page "http://www.tongji.edu.cn/~zhanglab/DiNuP/")
    (synopsis "Identify regions of differential nucleosome positioning")
    (description
     "DiNuP compares the nucleosome profiles generated by high-throughput
sequencing between different conditions.  DiNuP provides a statistical p-value
for each identified region of differential nucleosome positioning (RDNP) based
on the difference of read distributions.  It also empirically estimates the
false discovery rate as a cutoff when two samples have different sequencing
depths and differentiate reliable RDNPs from the background noise.")
    (license nonfree:artistic1.0)))

(define-public kallisto
  (package
    (name "kallisto")
    (version "0.42.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pachterlab/kallisto/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yx126g2yrk5gaw42bcd1cpil1c9xllgbkq14dds9y62zvxibygn"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no "check" target
    (inputs
     `(("hdf5" ,hdf5)
       ("zlib" ,zlib)))
    (home-page "http://pachterlab.github.io/kallisto/")
    (synopsis "Near-optimal RNA-Seq quantification")
    (description
     "kallisto is a program for quantifying abundances of transcripts
from RNA-Seq data, or more generally of target sequences using
high-throughput sequencing reads.  It is based on the novel idea of
pseudoalignment for rapidly determining the compatibility of reads
with targets, without the need for alignment.  On benchmarks with
standard RNA-Seq data, kallisto can quantify 30 million human reads in
less than 3 minutes on a Mac desktop computer using only the read
sequences and a transcriptome index that itself takes less than 10
minutes to build.  Pseudoalignment of reads preserves the key
information needed for quantification, and kallisto is therefore not
only fast, but also as accurate than existing quantification tools.
In fact, because the pseudoalignment procedure is robust to errors in
the reads, in many benchmarks kallisto significantly outperforms
existing tools.")
    (license (nonfree:non-free
              "http://pachterlab.github.io/kallisto/license.html"
              "This software may only be used for educational and
research not-for-profit purposes."))))

(define-public macs-1
  (package (inherit macs)
    (name "macs")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/M/MACS/MACS-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17lbf76gkisrxhnjwf8iw4pvinny2376dp9dyrgald2l0ww6s4d9"))
              (patches (list (search-patch "macs-1.4-fix-parser.patch")))))))

(define-public python2-mirnylib
  (let ((commit "ccec2e72dfa33eb04fe8b2ebd9bc2d88a1776d63")
        (revision "2"))
    (package
      (name "python2-mirnylib")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/mirnylab/mirnylib")
                      (changeset commit)))
                (sha256
                 (base32
                  "08ac4jg6bz4528x7sbnybkvjhk1w7jigmbl30qh4qlp6jfhf03bk"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2 ; python2 only
         #:tests? #f ; tests expect additional test data
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-distutils
            (lambda _
              (substitute* "setup.py"
                (("from setuptools import setup")
                 "from distutils.core import setup"))
              #t)))))
      (inputs
       `(("gcc" ,gcc "lib")    ; libgomp
         ("hdf5" ,hdf5)))      ; FIXME: probably should be propagated by h5py
      (propagated-inputs
       `(("python-biopython" ,python2-biopython)
         ("python-joblib" ,python2-joblib)
         ("python-bx-python" ,python2-bx-python)
         ("python-numpy" ,python2-numpy)
         ("python-scipy" ,python2-scipy)
         ("python-pysam" ,python2-pysam)
         ("python-matplotlib" ,python2-matplotlib)
         ("python-h5py" ,python2-h5py)))
      (native-inputs
       `(("python-cython" ,python2-cython)
         ("python-setuptools" ,python2-setuptools)))
      (home-page "https://bitbucket.org/mirnylab/mirnylib")
      (synopsis "Libraries shared between different mirnylab projects")
      (description
       "This package provides assorted libraries used by different mirnylab
projects.")
      (license nonfree:undeclared))))

;; https://bitbucket.org/mirnylab/hiclib/issues/36/no-license-declaration
(define-public python2-hiclib
  (let ((commit "1193891")
        (revision "2"))
    (package
      (name "python2-hiclib")
      (version (string-append "0-" revision "." commit))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/mirnylab/hiclib")
                      (changeset commit)))
                (sha256
                 (base32
                  "1aa58chqwr2j6pmqjr5wgx79znb96612kbhziaa6szrcd1dm574c"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; tests depend on unavailable test data
         #:python ,python-2 ; python2 only
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-distutils
            (lambda _
              ;; HOME needs to be set to unpack the Egg archive
              (setenv "HOME" "/tmp")
              (substitute* "setup.py"
                (("from setuptools import setup")
                 "from distutils.core import setup"))
              #t))
           (add-after 'unpack 'set-matplotlib-backend-to-agg
            (lambda _
              ;; Set the matplotlib backend to Agg to avoid problems using the
              ;; GTK backend without a display.
              (substitute* (find-files "tests" "\\.py")
                (("import matplotlib\\.pyplot as plt" line)
                 (string-append "import matplotlib;matplotlib.use('Agg');"
                                line)))
              #t))
           (add-before 'build 'build-binarySearch
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((python-version (string-take (string-take-right
                                                   (assoc-ref inputs "python") 6) 3))
                     (path (string-append "lib/python" python-version
                                          "/site-packages")))
                (substitute* "setup.py"
                  (("binarySearch/fastBinSearch.pyx")
                   "binarySearch/fastBinSearch.cpp")
                  (("packages=\\['hiclib'\\],")
                   (string-append "packages=['hiclib'], "
                                  "data_files=[('" path "/hiclib', "
                                  "['binarySearch/fastBinSearch.so'])],")))
                (with-directory-excursion "binarySearch"
                  (setenv "CPATH"
                          (string-append (assoc-ref inputs "python-numpy")
                                         "/" path "/numpy/core/include/:"
                                         (or (getenv "CPATH") "")))
                  (zero? (system* "make")))))))))
      (propagated-inputs
       `(("hdf5" ,hdf5) ; FIXME: probably should be propagated by h5py
         ("python-biopython" ,python2-biopython)
         ("python-numpy" ,python2-numpy)
         ("python-scipy" ,python2-scipy)
         ("python-matplotlib" ,python2-matplotlib)
         ("python-pysam" ,python2-pysam)
         ("python-mirnylib" ,python2-mirnylib)))
      (native-inputs
       `(("python-cython" ,python2-cython)
         ("python-setuptools" ,python2-setuptools)))
      (home-page "https://bitbucket.org/mirnylab/hiclib")
      (synopsis "Collection of tools to map, filter and analyze Hi-C data")
      (description
       "Hi-C lib is a collection of tools to map, filter and analyze Hi-C
data.")
      (license nonfree:undeclared))))

(define-public meme
  (package
    (name "meme")
    (version "4.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://meme-suite.org/meme-software/"
                                  version "/meme_" version ".tar.gz"))
              (sha256
               (base32
                "0zr3gvz4k30ggx0wqrbxdrr446vcc1v8q25xnjyjbvqn90gq9i2x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths-to-tools
           (lambda _
             (substitute* "src/utils.c"
               (("\"hostname")
                (string-append "\"" (which "hostname"))))
             #t))
         (add-after 'unpack 'remove-unused-tests
           (lambda _
             ;; We don't build the web server stuff, so we don't need
             ;; to run the tests for that either.
             (substitute* "tests/scripts/Makefile.in"
               (("tomtom.test") ""))))
         (add-before 'configure 'check-perl-dependencies
           (lambda _
             (zero? (system* "perl" "./scripts/dependencies.pl"))))
         (add-after 'install 'wrap-perl-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure perl scripts finds all perl inputs at runtime.
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program (string-append out "/bin/" prog)
                             `("PERL5LIB" ":" prefix
                               (,(getenv "PERL5LIB")))))
                         '("ama-qvalues"
                           "beeml2meme"
                           "chen2meme"
                           "dreme_xml_to_html"
                           "dreme_xml_to_txt"
                           "elm2meme"
                           "fasta-center"
                           "fasta-fetch"
                           "fasta-grep"
                           "fasta-make-index"
                           "fasta-most"
                           "fasta-subsample"
                           "fasta-unique-names"
                           "hart2meme-bkg"
                           "hartemink2psp"
                           "iupac2meme"
                           "jaspar2meme"
                           "mast_xml_to_html"
                           "mast_xml_to_txt"
                           "matrix2meme"
                           "meme-chip"
                           "meme-rename"
                           "meme_xml_to_html"
                           "nmica2meme"
                           "priority2meme"
                           "psp-gen"
                           "rna2meme"
                           "rsat-retrieve-seq"
                           "rsat-supported-organisms"
                           "scpd2meme"
                           "sites2meme"
                           "taipale2meme"
                           "tamo2meme"
                           "tomtom_xml_to_html"
                           "transfac2meme"
                           "uniprobe2meme"))
              #t))))))
    (inputs
     `(("perl" ,perl)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-html-template" ,perl-html-template)
       ("perl-xml-simple" ,perl-xml-simple)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-wsdl11" ,perl-xml-compile-wsdl11)
       ("perl-xml-parser" ,perl-xml-parser)
       ("python" ,python-2) ;only works with Python 2
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("openmpi" ,openmpi)
       ("ghostscript" ,ghostscript)
       ("inetutils" ,inetutils) ;for "hostname"
       ("zlib" ,zlib)))
    (propagated-inputs
     ;; "which" must be propagated because of the weird way it is used
     ;; in "src/exec_parallel.c".  The buffer "cmd_len" is arranged to
     ;; be 6 characters longer than the argument, just enough for the
     ;; string "which ".  I don't want to mess with pointers and
     ;; buffer lengths just to hardcode a path to the "which"
     ;; executable.
     `(("which" ,which)))
    (home-page "http://www.tbi.univie.ac.at/RNA/index.html")
    (synopsis "Motif-based sequence analysis tools")
    (description
     "The MEME Suite allows the biologist to discover novel motifs in
collections of unaligned nucleotide or protein sequences, and to
perform a wide variety of other motif-based analyses.

The MEME Suite supports motif-based analysis of DNA, RNA and protein
sequences.  It provides motif discovery algorithms using both
probabilistic and discrete models, which have complementary strengths.
It also allows discovery of motifs with arbitrary insertions and
deletions (GLAM2).  In addition to motif discovery, the MEME Suite
provides tools for scanning sequences for matches to motifs (FIMO,
MAST and GLAM2Scan), scanning for clusters of motifs (MCAST),
comparing motifs to known motifs (Tomtom), finding preferred spacings
between motifs (SpaMo), predicting the biological roles of
motifs (GOMo), measuring the positional enrichment of sequences for
known motifs (CentriMo), and analyzing ChIP-seq and other large
datasets (MEME-ChIP).")
    (license (nonfree:non-free "http://meme-suite.org/doc/copyright.html"
                               "license forbids commercial usage"))))

(define-public structure
  (package
    (name "structure")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pritchardlab.stanford.edu/"
                           "structure_software/release_versions/v" version
                           "/structure_kernel_source.tar.gz"))
       (sha256
        (base32
         "0dxvq34lyzicjwgsyrw49b1pmjms7nmc3g8vj8zga555i68jpdzj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "structure" bin)))))))
    (home-page "http://pritchardlab.stanford.edu/structure.html")
    (synopsis "Tool for investigating population structure")
    (description "Structure is a package for using multi-locus genotype data
to investigate population structure.  Its uses include inferring the presence
of distinct populations, assigning individuals to populations, studying hybrid
zones, identifying migrants and admixed individuals, and estimating population
allele frequencies in situations where many individuals are migrants or
admixed.  It can be applied to most of the commonly-used genetic markers,
including SNPS, microsatellites, RFLPs and AFLPs.")
    ;; I have asked upstream for information about the license:
    ;; https://groups.google.com/forum/#!topic/structure-software/1g7bDoN9140
    (license nonfree:undeclared)))

(define-public viennarna
  (package
    (name "viennarna")
    (version "2.1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tbi.univie.ac.at/RNA/packages/source/ViennaRNA-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1swjnfir5gx424srsnggw4sf8x0p8kiqfzgzp5m34zdzvn4nlzrn"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (home-page "http://www.tbi.univie.ac.at/RNA/index.html")
    (synopsis "Prediction and comparison of RNA secondary structures")
    (description
     "RNA secondary structure prediction through energy minimization is the
most used function in the package.  Three kinds of dynamic programming
algorithms for structure prediction are provided: the minimum free energy
algorithm of Zuker & Stiegler (1981) which yields a single optimal structure,
the partition function algorithm of McCaskill (1990) which calculates base
pair probabilities in the thermodynamic ensemble, and the suboptimal folding
algorithm of Wuchty et.al (1999) which generates all suboptimal structures
within a given energy range of the optimal energy.  For secondary structure
comparison, the package contains several measures of
distance (dissimilarities) using either string alignment or
tree-editing (Shapiro & Zhang 1990).  Finally, an algorithm to design
sequences with a predefined structure (inverse folding) is provided.")
    (license (nonfree:non-free "TODO" "license forbids commercial usage"))))
