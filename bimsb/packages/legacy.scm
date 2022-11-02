;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (bimsb packages legacy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz))

(define-public python-mirnylib
  (let ((commit "2d27793111b63ac7d5f82ba3369f927c6c9745da")
        (revision "2"))
    (package
      (name "python-mirnylib")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mirnylab/mirnylib-legacy")
                      (commit commit)))
                (sha256
                 (base32
                  "15s3755f17lnayx1vrpr9pd5wra9kc083f1d8s2qivww5gdxim6q"))))
      (build-system python-build-system)
      (arguments '(#:tests? #f)) ; tests expect additional test data
      (inputs
       `(("hdf5" ,hdf5)))      ; FIXME: probably should be propagated by h5py
      (propagated-inputs
       `(("python-biopython" ,python-biopython)
         ("python-joblib" ,python-joblib)
         ("python-bx-python" ,python-bx-python)
         ("python-numpy" ,python-numpy)
         ("python-scipy" ,python-scipy)
         ("python-pysam" ,python-pysam)
         ("python-matplotlib" ,python-matplotlib)
         ("python-h5py" ,python-h5py)))
      (native-inputs
       `(("python-cython" ,python-cython)
         ("python-setuptools" ,python-setuptools)))
      (home-page "https://github.com/mirnylab/mirnylib-legacy")
      (synopsis "Libraries shared between different mirnylab projects")
      (description
       "This package provides assorted libraries used by different mirnylab
projects.")
      (license license:expat))))

(define-public python2-mirnylib
  (deprecated-package "python2-mirnylib" python-mirnylib))

(define-public python-hiclib
  (let ((commit "518546e41987dca8a40f45ddc63601a5aaf46bfa")
        (revision "4"))
    (package
      (name "python-hiclib")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mirnylab/hiclib-legacy")
                      (commit commit)))
                (sha256
                 (base32
                  "04zd0hwgb3sw7y9ylp409nnkskpnyij55db2lkvlw7fszc5zdf9k"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; tests depend on unavailable test data
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'set-matplotlib-backend-to-agg
            (lambda _
              ;; Set the matplotlib backend to Agg to avoid problems using the
              ;; GTK backend without a display.
              (substitute* (find-files "tests" "\\.py")
                (("import matplotlib\\.pyplot as plt" line)
                 (string-append "import matplotlib;matplotlib.use('Agg');"
                                line)))
              #t)))))
      (propagated-inputs
       `(("hdf5" ,hdf5) ; FIXME: probably should be propagated by h5py
         ("python-biopython" ,python-biopython)
         ("python-numpy" ,python-numpy)
         ("python-scipy" ,python-scipy)
         ("python-matplotlib" ,python-matplotlib)
         ("python-pysam" ,python-pysam)
         ("python-mirnylib" ,python-mirnylib)))
      (native-inputs
       `(("python-cython" ,python-cython)
         ("python-setuptools" ,python-setuptools)))
      (home-page "https://bitbucket.org/mirnylab/hiclib")
      (synopsis "Collection of tools to map, filter and analyze Hi-C data")
      (description
       "Hi-C lib is a collection of tools to map, filter and analyze Hi-C
data.")
      (license license:expat))))

(define-public python2-hiclib
  (deprecated-package "python2-hiclib" python-hiclib))
