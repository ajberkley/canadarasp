(in-package :cl-gdal)
;; Local changes from ajb

(defmacro with-gdal-file ((filevar filename &optional (access (or :ga_readonly :ga_update))) &body body)
  `(let (,filevar)
     (unwind-protect
	  (progn
	    (setf ,filevar (gdal-open ,filename ,access))
	    (assert (not (cffi::null-pointer-p ,filevar)) nil "Failed to open file")
	    ,@body)
       (when (not (cffi::null-pointer-p ,filevar)) (gdal-close ,filevar)))))

(cffi:defcfun ("GDALGetRasterYSize" gdal-get-raster-y-size) :int
  "Fetch YSize of raster."
  (hBand gdal-raster-band-h))

(cffi:defcfun ("GDALGetRasterXSize" gdal-get-raster-x-size) :int
  "Fetch XSize of raster."
  (hBand gdal-raster-band-h))

(defparameter *initialized* nil)

(defun get-geo-transform (dshandle)
  (let ((blarg (make-array 6 :element-type 'double-float)))
    (sb-ext::with-pinned-objects (blarg)
      (gdalgetgeotransform dshandle (sb-kernel::vector-sap blarg)))
    blarg))

(defun make-geo-transform (hband)
  (let ((blarg (get-geo-transform hband)))
    (values 
     (lambda (x-idx y-idx) ;; pixel indices, from 0
       (let ((center-x (+ x-idx 0.5))
	     (center-y (+ y-idx 0.5)))
	 (list (+ (aref blarg 0) (* (aref blarg 1) center-x) (* (aref blarg 2) center-y))
	       (+ (aref blarg 3) (* (aref blarg 4) center-x) (* (aref blarg 5) center-y)))))
     blarg)))

(defun gdal-get-raster-statistics* (raster-band &optional (b-approx-ok 0) (bforce 1))
  (cffi:with-foreign-object (pdfMin :double)
    (cffi:with-foreign-object (pdfMax :double)
      (cffi:with-foreign-object (pdfMean :double)
	(cffi:with-foreign-object (pdfStdDev :double)
	  (gdal-get-raster-statistics raster-band b-approx-ok bforce pdfmin pdfmax pdfmean pdfstddev)
	  (list :min (cffi:mem-ref pdfMin :double)
		:max (cffi:mem-ref pdfMax :double)
		:mean (cffi:mem-ref pdfMean :double)
		:stddev (cffi:mem-ref pdfStdDev :double)))))))




(defun gdal-get-block-size* (hband)
  (cffi:with-foreign-object (x-blk-size :int)
    (cffi:with-foreign-object (y-blk-size :int)
      (gdal-get-block-size hband x-blk-size y-blk-size)
      (list (cffi:mem-ref x-blk-size :int) (cffi:mem-ref y-blk-size :int)))))

(defun gdal-get-actual-block-size* (hband x-blk-idx y-blk-idx)
  (cffi:with-foreign-object (x-blk-size :int)
    (cffi:with-foreign-object (y-blk-size :int)
      (gdal-get-actual-block-size hband x-blk-idx y-blk-idx x-blk-size y-blk-size)
      (list (cffi:mem-ref x-blk-size :int) (cffi:mem-ref y-blk-size :int)))))
  
(defun gdal-read-all-data (hband)
  (assert (eq (gdal-get-raster-data-type hband) :gdt_float64))
  (let* ((x-size (gdal-get-raster-band-x-size hband))
	 (y-size (gdal-get-raster-band-y-size hband))
	 (array (make-array (list y-size x-size) :element-type 'double-float)))
    (destructuring-bind (x-blk-size y-blk-size)
	(gdal-get-block-size* hband)
      (assert (= x-blk-size x-size))
      (assert (= y-blk-size 1))
      ;;(sb-ext::with-pinned-objects (array)
      (cffi:with-pointer-to-vector-data (ptr (sb-kernel:%array-data-vector array))
	(loop :for y-offset :from 0 :below y-size
	   :do
	   (gdal-read-block hband 0 y-offset
			    (cffi:inc-pointer ptr (* 8 x-size y-offset))))))
    array))

(declaim (inline array-map))
(defun array-map (output-type function &rest arrays)
  "maps the function over the arrays.
   Assumes that all arrays are of the same dimensions.
   Returns a new result array of the same dimension."
  (declare (optimize (speed 3)))
  (declare (type function function))
  (labels ((make-displaced-array (array)
	     (make-array (reduce #'* (array-dimensions array))
			 :displaced-to array :element-type (array-element-type array))))
    (let* ((displaced-arrays (mapcar #'make-displaced-array arrays)))
      (if output-type
	  (let* ((result-array (make-array (array-dimensions (first arrays)) :element-type output-type))
		 (displaced-result-array (make-displaced-array result-array)))
	    (apply #'map-into displaced-result-array function displaced-arrays)
	    result-array)
	  (map nil function displaced-arrays)))))

(defun gdal-get-statistics* (hband)
  ;; Agrees with (gdal-get-raster-statistics* uband))
  ;; stupid slow, but just a lazy check
  (let ((data (gdal-read-all-data hband))
	(res))
    (array-map nil (lambda (x) (push x res) (values)) data)
    (iterate:iter (iterate:for x in res)
		  (iterate:minimizing x into min)
		  (iterate:maximizing x into max)
		  (iterate:summing x into sum)
		  (iterate:finally (return (list :min min :max max :mean (/ sum (length res))))))))

(defun maybe-initialize-gdal-ogr ()
  (when (not *initialized*)
    (gdal-all-register)
    (cl-ogr:ogr-register-all)
    (setf *initialized* t)))

(defun set-geo-transform (hdriver array)
  (sb-int::with-pinned-objects (array)
    (gdalsetgeotransform hdriver (sb-int::vector-sap array))))

;; (defun create-ogr-layer (OGRDataSourceH name OGRSpatialReferenceH eType &optional (options (cffi:null-pointer)))
;;   (ogr-ds-create-layer OGRDataSourceH name OGRSpatialReferenceH etype options))

(cffi:defcfun ("OSRNewSpatialReference" osr-new-spatial-reference) cl-ogr::ogr-spatial-reference-h
  (wkt :string))

(cffi:defctype ogr-coordinate-transformation :pointer "ogr-coordinate-transformation")

(cffi:defcfun ("OCTNewCoordinateTransformation" make-coordinate-transformation) ogr-coordinate-transformation
  (source cl-ogr::ogr-spatial-reference-h)
  (dest cl-ogr::ogr-spatial-reference-h))

(cffi:defcfun ("OCTTransform" octt-transform-array) :int
  (htransform ogr-coordinate-transformation)
  (count :int)
  (x :pointer)
  (y :pointer)
  (z :pointer))

(defparameter *xa* (make-array 1 :element-type 'double-float))
(defparameter *ya* (make-array 1 :element-type 'double-float))
(defparameter *za* (make-array 1 :element-type 'double-float))

(declaim (type (simple-array double-float (1)) *xa* *ya* *za*))


(declaim (inline transform-point-unthreadsafe))
(defun transform-point-unthreadsafe (coordinate-transformation x y)
  "This is a faster version of transform-point*, but not thread safe"
  (declare (optimize (speed 3)))
  (setf (aref *xa* 0) x)
  (setf (aref *ya* 0) y)
  (sb-int::with-pinned-objects (*xa* *ya* *za*)
    (octt-transform-array coordinate-transformation
			  1
			  (sb-kernel::vector-sap *xa*)
			  (sb-kernel::vector-sap *ya*)
			  (sb-kernel::vector-sap *za*))
    (cons (aref *xa* 0) (aref *ya* 0))))

(defun transform-point* (coordinate-transformation x y &optional (z 0d0 z-p))
  (let ((xa (make-array 1 :element-type 'double-float :initial-element x))
	(ya (make-array 1 :element-type 'double-float :initial-element y))
	(za (make-array 1 :element-type 'double-float :initial-element z)))
    (sb-int::with-pinned-objects (xa ya za)
      (octt-transform-array coordinate-transformation
			    1
			    (sb-kernel::vector-sap xa)
			    (sb-kernel::vector-sap ya)
			    (sb-kernel::vector-sap za))
      (if z-p
	  (list (aref xa 0) (aref ya 0) (aref za 0))
	  (list (aref xa 0) (aref ya 0))))))

(cffi:defcfun ("OGR_DS_CreateLayer" ogr-ds-create-layer) cl-ogr::ogr-layer-h
  "This function attempts to create a new layer on the data source with
 the indicated name, coordinate system, geometry type.

 The papszOptions argument can be used to control driver specific
 creation options. These options are normally documented in the format
 specific documentation.

 This function is the same as the C++ method
 OGRDataSource::CreateLayer().

Parameters:	hDS 	The dataset handle.
	pszName 	the name for the new layer. This should ideally not match any existing layer on the datasource.
	hSpatialRef 	handle to the coordinate system to use for the new layer, or NULL if no coordinate system is available.
	eType 	the geometry type for the layer. Use wkbUnknown if there are no constraints on the types geometry to be written.
	papszOptions 	a StringList of name=value options. Options are driver specific, and driver information can be found at the following url: http://www.gdal.org/ogr/ogr_formats.html

 @return{NULL is returned on failure, or a new OGRLayer handle on success.}

Example:
...
@begin{pre}
        OGRLayerH *hLayer;
        char     **papszOptions;

        if( OGR_DS_TestCapability( hDS, ODsCCreateLayer ) )
        (
            ...
        )

        papszOptions = CSLSetNameValue( papszOptions, \"DIM\", \"2\" );
        hLayer = OGR_DS_CreateLayer( hDS, \"NewLayer\", NULL, wkbUnknown,
                                     papszOptions );
        CSLDestroy( papszOptions );

        if( hLayer == NULL )
        (
            ...
        )
@end{pre}"
  (hDS :pointer)			; OGRDataSourceH
  (pszName :string)			; const char *
  (hSpatialRef :pointer)		; OGRSpatialReferenceH
  (eType cl-ogr::ogr-wkb-geometry-type)				; OGR-wkb-Geometry-Type ;; typo here
  (papszOptions (:pointer :string)))

(cffi:defcfun ("CPLGetLastErrorMsg" cpl-get-last-error-msg) :string)

;; (defun test-ogr (fname)
;;   (maybe-initialize-gdal-ogr)
;;   (with-gdal-file (handle "/home/ajb/canadarasp/continental-test/output.grib2")
;;     (let ((projectionref (gdalGetProjectionRef handle)))
;;       (print (list 'pref projectionref))
;;       (let* ((outdriver (cl-ogr:ogr-get-driver-by-name "GEOJson"))
;; 	     (ds (cl-ogr::ogr_dr_createdatasource outdriver fname (cffi:null-pointer))))
;; 	(print (list 'outdriver outdriver))
;; 	(print (list 'ds ds))
;; 	(when (cffi:null-pointer-p ds)
;; 	  (print (cpl-get-last-error-msg)))
;; 	(let* ((spatial-reference (osr-new-spatial-reference projectionref))) ;; ok
;; 	  (print (list 'sr spatial-reference)) ;; success
;; 	  (let ((layer (create-ogr-layer ds "myarrow" spatial-reference :wkb-line-string)))
;; 	    ;; not sure if I should do wkb_multi_line_string
;; 	    (print (list 'layer layer))
;; 	    (let ((feature (ogr-f-create (ogr-l-get-layer-defn layer))))
;; 	      (print (list 'feature feature))
;; 	      (ogr-f-set-field-string feature (ogr-f-get-field-index feature "Name") "MyName")
;; 	      (let ((geometry (ogr-g-create-geometry :wkb-line-string)))
;; 		(print (list 'geometry geometry))
;; 		(ogr-g-set-point-2d geometry 0 0d0 0d0)
;; 		(ogr-g-set-point-2d geometry 1 1d0 1d0)
;; 		(ogr-f-set-geometry feature geometry)
;; 		;;(ogr-g-destroy-geometry geometry)
;; 		(ogr-l-create-feature layer feature)
;; 		(ogr-f-destroy feature)
;; 		(ogr-ds-destroy ds)))))))))

(defconstant +google-map-wkt+
  "GEOGCS[\"WGS 84\",
    DATUM[\"WGS_1984\",
        SPHEROID[\"WGS 84\",6378137,298.257223563,
            AUTHORITY[\"EPSG\",\"7030\"]],
        AUTHORITY[\"EPSG\",\"6326\"]],
    PRIMEM[\"Greenwich\",0,
        AUTHORITY[\"EPSG\",\"8901\"]],
    UNIT[\"degree\",0.01745329251994328,
        AUTHORITY[\"EPSG\",\"9122\"]],
    AUTHORITY[\"EPSG\",\"4326\"]]")

(defun generate-lat-lon-to-pixel-transform (handle)
  (declare (optimize (speed 3)))
  "Remember pixel 0 0 is the upper left of the image... so the bottom right of a 100x100 pixel image
   is actually at pixel 100x100 (the bottom right of the last pixel at (99,99))"
  (let* ((x-size (gdal-get-raster-band-x-size (gdal-get-raster-band handle 1)))
	 (y-size (gdal-get-raster-band-y-size (gdal-get-raster-band handle 1)))
	 (my-projection (osr-new-spatial-reference (gdalGetProjectionRef handle)))
	 (google-projection (osr-new-spatial-reference +google-map-wkt+))
	 (itransform (make-coordinate-transformation google-projection my-projection))
	 (ftransform (make-coordinate-transformation my-projection google-projection))
	 (source-geo-transform (get-geo-transform handle))
	 (ulx (aref source-geo-transform 0))
	 (lry (+ (aref source-geo-transform 3) (* x-size (aref source-geo-transform 4)) (* y-size (aref source-geo-transform 5))))
	 (lrx (+ (aref source-geo-transform 0) (* x-size (aref source-geo-transform 1)) (* y-size (aref source-geo-transform 2))))
	 (uly (aref source-geo-transform 3)))
    (declare (type fixnum x-size y-size)
	     (type double-float ulx lry lrx uly)
	     (type (simple-array double-float (6)) source-geo-transform))
    (labels ((from-source-pixel-to-source-geo-ref (pixelx pixely)
	       (list (+ (aref source-geo-transform 3) (* pixelx (aref source-geo-transform 4)) (* pixely (aref source-geo-transform 5)))
		     (+ (aref source-geo-transform 0) (* pixelx (aref source-geo-transform 1)) (* pixely (aref source-geo-transform 2)))))
	     (from-source-geo-ref-to-source-pixel (ref)
	       (let ((xpixel (* x-size (/ (- (the double-float (car ref)) ulx) (- lrx ulx))))
		     (ypixel (* y-size (/ (- (the double-float (cdr ref)) uly) (- lry uly)))))
		 (list (round xpixel) (round ypixel))))
	     (from-source-geo-ref-to-lon-lat (georefx georefy)
	       (transform-point* ftransform georefy georefx))
	     (from-source-pixel-to-lon-lat (pixelx pixely)
	       (apply #'from-source-geo-ref-to-lon-lat (from-source-pixel-to-source-geo-ref pixelx pixely)))
	     (from-google-geo-ref-to-source-geo-ref (a b)
	       (from-source-geo-ref-to-source-pixel
		(transform-point-unthreadsafe itransform (coerce a 'double-float) (coerce b 'double-float)))))
      (destructuring-bind (ullon ullat)
	  (transform-point* ftransform ulx uly)
	(destructuring-bind (lrlon lrlat)
	    (transform-point* ftransform lrx lry)
	  (format t "Corner Coordinates of source file:~%")
	  (format t "Upper Left (~,4f, ~,4f) (LON ~,6f, LAT ~,6f) ~%" ulx uly ullon ullat)
	  (format t "Lower Right (~,4f, ~,4f) (LON ~,6f, LAT ~,6f)~%" lrx lry lrlon lrlat)
	  (format t "Upper Left is ~A~%" (from-source-geo-ref-to-source-pixel (cons ulx uly)))
	  (format t "Lower Right is ~A~%" (from-source-geo-ref-to-source-pixel (cons lrx lry)))
	  (destructuring-bind (x y) (from-source-geo-ref-to-source-pixel (transform-point-unthreadsafe itransform -123.524985d0 49.132d0))
	    (format t "At -123.524985, 49.132 we are at pixel ~A ~A~%" x y)
	    (let ((data (gdal-read-all-data (gdal-get-raster-band handle 1))))
	      (format t "At which point the value of the field is ~A~%" (aref data (round y) (round x)))))
	  (destructuring-bind (tile-x-min . tile-y-min)
	      (transform-point-unthreadsafe itransform -122.0d0 48.0d0)
	    (destructuring-bind (tile-x-max . tile-y-max)
		(transform-point-unthreadsafe itransform -120.0d0 50.0d0)
	      (format t "TILE -122:48:-120:50 is ~A -> ~A ~%" (list tile-x-min tile-y-min) (list tile-x-max tile-y-max))
	      ;; now invert the affine transformation
	      (destructuring-bind (ulpixelx ulpixely)
		  (from-source-geo-ref-to-source-pixel (cons tile-x-min tile-y-min))
		(format t "TILE -122:48:-120:50 is ~A -> ~A~%"
			(list ulpixelx ulpixely)
			(from-source-geo-ref-to-source-pixel (cons tile-x-max tile-y-max)))
		;; Now lets try figuring out what those pixels actually are
		(let ((geo-ref (from-source-pixel-to-source-geo-ref ulpixelx ulpixely)))
		  (format t "Pixel (~A,~A) is really ~A which is ~A~%" ulpixelx ulpixely geo-ref
			  (apply #'from-source-geo-ref-to-lon-lat geo-ref)))
		)))))
      (values #'from-google-geo-ref-to-source-geo-ref #'from-source-pixel-to-lon-lat))))
