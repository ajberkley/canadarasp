;;; -*- mode: lisp; -*-

(in-package :cl-user)

(defpackage :cl-gdal-asd
  (:use :cl :asdf))

(in-package :cl-gdal-asd)

(defsystem :cl-gdal
    :version "2013.8.25"      ; YYYY.MM.DD -- digits to suit the ASDF
    :licence "BSD"
    :description "CL-GDAL is a Common Lisp wrapper for the GDAL library."
    :author "Victor Anyakin <anyakinvictor@yahoo.com>"
    :long-description
    "A Common Lisp wrapper for GDAL library to perform IO operations.

GDAL is a translator library for raster geospatial data formats that
is released under an X/MIT style Open Source license by the Open
Source Geospatial Foundation. As a library, it presents a single
abstract data model to the calling application for all supported
formats. It also comes with a variety of useful commandline utilities
for data translation and processing. The NEWS page describes the
August 2013 GDAL/OGR 1.10.1 release.

The related OGR library (which lives within the GDAL source tree)
provides a similar capability for simple features vector data.

Master: http://www.gdal.org"
    :serial t
    :components
    ((:module "src"
	      :serial t
	      :components ((:file "gdal-package")
			   (:file "gdal-core")
			   (:file "gdal-dataset")
			   (:file "gdal-rasterband")
			   (:file "gdal-common"))))
    :depends-on (:cffi
		 :trivial-garbage))

;; EOF
