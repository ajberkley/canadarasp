;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

;; OGRSpatialReferenceH <ogr_srs_api.h>

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToWkt" osr-export-to-wkt) ogr-err
  (ref ogr-spatial-reference-h)
  (buf (:pointer :string)))
(export 'osr-export-to-wkt)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToPrettyWkt" osr-export-to-pretty-wkt) ogr-err
  (ref ogr-spatial-reference-h)
  (buf (:pointer :string)))
(export 'osr-export-to-pretty-wkt)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToProj4" osr-export-to-proj4) ogr-err
  (ref ogr-spatial-reference-h)
  (buf :pointer)) ; char**
(export 'osr-export-to-proj4)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToPCI" osr-export-to-pci) ogr-err
  (ref ogr-spatial-reference-h)
  (buf1 (:pointer :string))
  (buf2 (:pointer :string))
  (buf3 (:pointer :double)))
(export 'osr-export-to-pci)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToUSGS" osr-export-to-usgs) ogr-err
  (ref ogr-spatial-reference-h)
  (buf1 (:pointer :long))
  (buf2 (:pointer :long))
  (buf3 (:pointer (:pointer :double)))
  (buf4 (:pointer :long)))
(export 'osr-export-to-usgs)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToXML" osr-export-to-xml) ogr-err
  (ref ogr-spatial-reference-h)
  (buf1 (:pointer :string))
  (buf2 :string))
(export 'osr-export-to-xml)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToPanorama" osr-export-to-panorama) ogr-err
  (ref ogr-spatial-reference-h)
  (buf1 (:pointer :long))
  (buf2 (:pointer :long))
  (buf3 (:pointer :long))
  (buf4 (:pointer :long))
  (buf5 (:pointer :double)))
(export 'osr-export-to-panorama)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToMICoordSys" osr-export-to-mi-coord-sys) ogr-err
  (ref ogr-spatial-reference-h)
  (buf (:pointer :string)))
(export 'osr-export-to-mi-coord-sys)

;; --------------------------------------------------------

(cffi:defcfun ("OSRExportToERM" osr-export-to-erm) ogr-err
  (ref ogr-spatial-reference-h)
  (buf1 :string)
  (buf2 :string)
  (buf3 :string))
(export 'osr-export-to-erm)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

(defgeneric get-proj4 (s)
  (:documentation "Proj.4 representation of the Spatial Reference
System.")
  (:method ((s-ref <spatial-ref>))
    (cffi:with-foreign-object (*buf :pointer)
      (osr-export-to-proj4 (pointer s-ref) *buf)
      (let ((str (cffi:mem-ref *buf :pointer)))
	(unwind-protect (cffi:foreign-string-to-lisp str)
	  #+ignore
	  (cpl-free str))))))
(export 'get-proj4)

;; EOF
