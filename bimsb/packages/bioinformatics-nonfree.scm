;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public bcl2fastq
  (package
    (name "bcl2fastq")
    (version "2.17.1.14")
    (source (origin
              (method url-fetch)
              ;; Download manually from here:
              ;; ftp://webdata2:webdata2@ussd-ftp.illumina.com/downloads/Software/bcl2fastq/bcl2fastq2-v2.17.1.14.tar.gz
              (uri (string-append "file:///srv/bcl2fastq2-v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14s15h8kk9vqqwy0hykdzffz6zlkbqpvg5wnnfiwd2x7cwxizikm"))))
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
         (add-after 'unpack 'enter-dir (lambda _ (chdir "src") #t))
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
              (uri (string-append "file:///srv/bcl2fastq-"
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
            (let* ((out (assoc-ref outputs "out"))
                   (xml (assoc-ref inputs "perl-xml-simple")))
              (for-each (lambda (prog)
                          (wrap-program (string-append out "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(string-append xml "/lib/perl5/site_perl")))))
                        '("configureBclToFastq.pl"
                          "configureQseqToFastq.pl"
                          "configureValidation.pl"))
              #t))))))
    (inputs
     `(("perl-xml-simple" ,perl-xml-simple)
       ("perl" ,perl)
       ,@(package-inputs bcl2fastq)))))

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

(define-public tophat
  (package
    (name "tophat")
    (version "2.0.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ccb.jhu.edu/software/tophat/downloads/tophat-"
                    version ".tar.gz"))
              (sha256
               (base32
                "04p5a7pnqk4f93bh19gnbpf8yic3kxy13pv6nza5640k8wd8zgmc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f))
    (inputs
     `(("boost" ,boost)
       ("bowtie" ,bowtie)
       ("ncurses" ,ncurses)
       ("python" ,python-2)
       ("perl" ,perl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gcc" ,gcc-4.8)))
    (home-page "http://ccb.jhu.edu/software/tophat/index.shtml")
    (synopsis "Spliced read mapper for RNA-Seq")
    (description
     "TopHat is a fast splice junction mapper for RNA-Seq reads.  It aligns
RNA-Seq reads to mammalian-sized genomes using the ultra high-throughput short
read aligner Bowtie, and then analyzes the mapping results to identify splice
junctions between exons.")
    (license nonfree:artistic1.0)))

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
