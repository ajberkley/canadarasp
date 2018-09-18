;;; Generate API documentation using the ATDOC package developed by
;;; David Lichteblau
;;;
;;; Just cd to the doc directory and run:
;;;
;;; sbcl --load ../src/mkapi.lisp
;;;
;;; This will produce documentation files in the current directory.


(ql:quickload "atdoc")
;; (ql:quickload "cl-gdal")
(ql:quickload "cl-ogr")
(ql:quickload "cl-markdown")

#|(atdoc:generate-html-documentation
 '(:cl-gdal)
 "./"
 :index-title "Common Lisp GDAL API reference"
 :heading "GDAL wrapper for Lisp"
 :single-page-p t
 :include-internal-symbols-p nil)|#

(atdoc:generate-html-documentation
 '(:cl-ogr)
 "./"
 :index-title "Common Lisp OGR API reference"
 :heading "OGR wrapper for Lisp"
 :single-page-p t
 :include-internal-symbols-p nil)

(quit)

;; EOF
