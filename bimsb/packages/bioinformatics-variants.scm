;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (bimsb packages bioinformatics-variants)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (bimsb packages staging)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages serialization))

;; A different version of MACS2 for Rebecca
(define-public macs/rebecca
  (package (inherit macs)
    (version "2.1.0.20150420.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MACS2" version))
       (sha256
        (base32
         "1zpxn9nxmfwj8zr8b4gjrx8j81mijgjmagl4prpkghphfbziwraw"))))))

(define-public bedtools-2.23.0
  (package
    (inherit bedtools)
    (version "2.23.0")
    (name "bedtools")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/arq5x/bedtools2/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mk0lg3bl6k7kbn675hinwby3jrb17mml7nms4srikhi3mbamb4x"))))))

;; Patched version of bedtools 2.25.0, which segfaults on some
;; RCAS/dorina test datasets in Bed6+ format.
(define-public bedtools/patched
  (package
    (inherit bedtools)
    (name "bedtools")
    (version "2.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/arq5x/bedtools2/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ywcy3yfwzhl905b51l0ffjia55h75vv3mw5xkvib04pp6pj548m"))
       (patches (list (search-patch "bedtools-fix-null-segfault.patch")))))))

(define-public samtools-1.1
  (package
    (inherit samtools-0.1)
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "1y5p2hs4gif891b4ik20275a8xf3qrr1zh9wpysp4g8m0g1jckf2"))))))

(define-public htslib-1.3
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

(define-public htslib-1.4
  (package
    (inherit htslib)
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "1crkk79kgxcmrkmh5f58c4k93w4rz6lp97sfsq3s6556zxcxvll5"))))))

(define-public samtools-1.3
  (package
    (inherit samtools)
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "0znnnxc467jbf1as2dpskrjhfh8mbll760j6w6rdkwlwbqsp8gbc"))))
    (inputs
     `(("htslib" ,htslib-1.3)
       ,@(package-inputs samtools)))))

(define-public samtools-1.4
  (package (inherit samtools)
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/samtools/samtools/"
                           version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "0vzxjm5vkgvzynl7cssm1l560rqs2amdaib1x8sp2ch9b7bxx9xx"))))
    (inputs
     `(("htslib" ,htslib-1.4)
       ,@(package-inputs samtools)))))

(define-public samtools-0
  (package (inherit samtools)
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/samtools/samtools/"
                       version "/samtools-" version ".tar.bz2"))
       (sha256
        (base32
         "16js559vg13zz7rxsj4kz2l96gkly8kdk8wgj9jhrqwgdh7jq9iv"))))
    (arguments
     `(#:tests? #f ;no "check" target
       ,@(substitute-keyword-arguments `(#:modules ((guix build gnu-build-system)
                                                    (guix build utils)
                                                    (srfi srfi-1)
                                                    (srfi srfi-26))
                                         ,@(package-arguments samtools))
           ((#:make-flags flags)
            `(cons "LIBCURSES=-lncurses" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'patch-tests)
               (delete 'configure)
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append
                               (assoc-ref outputs "out") "/bin")))
                     (mkdir-p bin)
                     (copy-file "samtools" (string-append bin "/samtools-" ,version)))
                   #t)))))))))

;; Fixed version of ParDRe for Harm.
(define-public pardre/fixed
  (package (inherit pardre)
    (name "pardre-fixed")
    (source (origin (inherit (package-source pardre))
                    (patches (search-patches "pardre-fix-utils.patch"))))))

;; This is needed for MEDICC
(define-public python2-biopython-1.62
  (package
    (inherit python2-biopython)
    (version "1.62")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "biopython" version))
              (sha256
               (base32
                "1gbq54l0nlqyjxxhq6nrqjfdx98x039akcxxrr5y3j7ccjw47wm2"))))))

(define-public infernal-1.0
  (package (inherit infernal)
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://eddylab.org/software/infernal/"
                                  "infernal-" version ".tar.gz"))
              (sha256
               (base32
                "1ba3av8xg4309dpy1ls73nxk2v7ri0yp0ix6ad5b1j35x319my64"))))
    (arguments
     ;; We need to disable tests because we don't seem to have
     ;; getopts.pl.
     `(#:tests? #f))))

(define-public bamtools-2.0
  (package (inherit bamtools)
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pezmaster31/bamtools/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1s95gjhnlfd91mr692xp14gmirfwq55dsj1jir9fa06m1lk4iw1n"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "src/third_party/")
                  (substitute* "src/CMakeLists.txt"
                    (("add_subdirectory \\(third_party\\)") ""))
                  (substitute* "src/toolkit/bamtools_filter.cpp"
                    (("jsoncpp/json.h") "json/json.h"))
                  #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments bamtools)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'add-install-target-for-utils-library
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/utils/CMakeLists.txt"
                 (("target_link_libraries.*" line)
                  (string-append line "\ninstall(TARGETS BamTools-utils \
LIBRARY DESTINATION \"lib/bamtools\")")))
               #t))))))
    (inputs
     `(("jsoncpp" ,jsoncpp)
       ,@(package-inputs bamtools)))))

(define-public htslib-latest
  (package (inherit htslib)
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/htslib/releases/download/"
                    version "/htslib-" version ".tar.bz2"))
              (sha256
               (base32
                "0bcjmnbwp2bib1z1bkrp95w9v2syzdwdfqww10mkb1hxlmg52ax0"))))))

(define-public bcftools-latest
  (package (inherit bcftools)
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/samtools/bcftools/releases/download/"
                    version "/bcftools-" version ".tar.bz2"))
              (sha256
               (base32
                "0093hkkvxmbwfaa7905s6185jymynvg42kq6sxv7fili11l5mxwz"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled htslib.
               '(delete-file-recursively "htslib-1.5"))))
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (list "--enable-libgsl"
             (string-append "--with-htslib="
                            (assoc-ref %build-inputs "htslib")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-tests
           (lambda _
             (substitute* "test/test.pl"
               (("/bin/bash") (which "bash")))
             #t)))))
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("htslib" ,htslib-latest)
       ("gsl" ,gsl)
       ("zlib" ,zlib)))))

(define-public r-3.3.1
  (package (inherit r-minimal)
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qm9znh8akfy9fkzzi6f1vz2w1dd0chsr6qn7kw80lqzhgjrmi9x"))))))

(define-public r-3.3.2
  (package (inherit r-minimal)
    (version "3.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0k2i9qdd83g09fcpls2198q4ykxkii5skczb514gnx7mx4hsv56j"))))))

(define-public r-3.3.3
  (package (inherit r-minimal)
    (version "3.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0v7wpj89b0i3ad3fi1wak5c93hywmbxv8sdnixhq8l17782nidss"))))))

(define-public r-devel
  (package (inherit r-minimal)
    (name "r-devel")
    (version "2017-04-27_r72634")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base-prerelease/"
                                  "R-devel_" version ".tar.gz"))
              (sha256
               (base32
                "02x5dqps8v31djr38s4akq8q3hk6xf8x5knk5454awyvjipkry2j"))))))

(define-public r-methylkit-devel
  (let ((commit "46c8556f34eea9f068f2225e36adf11ba7ea6d07")
        (revision "1"))
    (package (inherit r-methylkit)
      (name "r-methylkit-devel")
      (version (string-append "1.3.3-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/al2na/methylKit.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "061yx5lgm5c37v9asnvbl4wxay04791cbxs52ar16x0a0gd13p53")))))))

(define-public starlong
  (package (inherit star)
    (name "starlong")
    (arguments
     (substitute-keyword-arguments (package-arguments star)
       ((#:make-flags flags)
        `(list "STARlong"))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
                 (install-file "STARlong" bin))
               #t))))))))

(define-public jupyter-with-python2
  (package (inherit jupyter)
    (name "jupyter-python2")
    (build-system python-build-system)
    ;; FIXME: it's not clear how to run the tests.
    (arguments `(#:tests? #f
                 #:python ,python-2))
    (propagated-inputs
     `(("python-ipykernel" ,python2-ipykernel)
       ("python-ipywidgets" ,python2-ipywidgets)
       ("python-jupyter-console" ,python2-jupyter-console)
       ("python-nbconvert" ,python2-nbconvert)
       ("python-notebook" ,python2-notebook)
       ;; TODO: this should be propagated by tornado
       ("python-certifi" ,python2-certifi)))))
