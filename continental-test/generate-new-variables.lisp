;; 5 August 2018 version 0.2 ajb
;;  Write driver routine to process all the hours
;;  Done.  TODO add the parallel call before generate-tiles...
;;  Now going to work on the HRDPS side

;; 4 August 2018 version 0.1 ajb
;;  10 hours of work
;;  Can calculate HCRIT and write a new grib file!
;;  Why does pspear use a different shape for thermal?   I switched back to -1.1 from -0.8.
;;  this will generate less optimistic lift though!  Should probably switch to -0.8 or so.

;; July 2018 version 0.0 ajb
;;  Started eccodes library, can read gribs.  Played with
;;  cl-plplot a bit for contour plotting, not useful.
;;  played with proj projection library, may be useful but
;;  wgrib2 seems to do all I need for now (still rotated grid,
;;  but mercator projection so good enough if I just want to
;;  serve the data to a javascript frontend).

;; Plan for near-term is generating HCRIT, HCRIT-AGL, WSTAR, LCL
;; Let's start with HCRIT-AGL

;; Longer term would like to add
;;  TERRAIN_ALTITUDE (raster plotted to give people a sense of reality)
;;  SFC_PRES and PRES at 500 mb.

;; (grib-print-data "tiles/-122:-120:49:51/CMC_hrdps_west_TMP_ISBL_1000_ps2.5km_2018070318_P001-00.grib2")
;; +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs
;; proj 4 settings for google maps
;; grib_dump
;; wgrib2 CMC_hrdps_west_TMP_TGL_80_ps2.5km_2018070318_P046-00.grib2 -new_grid_winds earth -new_grid mercator:60 -122:85:1000:-120 49:103:1000:51  CMC_hrdps_west_TMP_TGL_80_ps2.5km_2018070318_P046-00.mercator.grib2
;; above seems to work... not totally sure though.  Need to ncl both and make sure they are the same.  The contour plots generated from the gribs look properly rotated, etc.  It takes 13 ms to do this transformation.  We have 6340 files per tile, and 10000 tiles, so 230 hours.  Hrmph.
;; time wgrib2 hrdps_west_2018-07-03-run18_P048.grib2 -new_grid_winds earth -new_grid mercator:60 -122:85:1000:-120 49:103:1000:51  hrdps_west_2018-07-03-run18_P048.mercator.grib2
;; is 244 ms, and only 48 per tile, 32 hours, divided by 16 cores is still 2 hours.
;; Takes 85 ms to do one of the per hour files, of which we have 6800, so only 6 minutes, ok, so we'll do that.

(load "~/quicklisp/setup.lisp")

(defun array-map-double-float (function &rest arrays)
  "maps the function over the arrays.
   Assumes that all arrays are of the same dimensions.
   Returns a new result array of the same dimension."
  (labels ((make-displaced-array (array)
           (make-array (reduce #'* (array-dimensions array))
                       :displaced-to array :element-type 'double-float)))
    (let* ((displaced-arrays (mapcar #'make-displaced-array arrays))
           (result-array (make-array (array-dimensions (first arrays)) :element-type 'double-float))
           (displaced-result-array (make-displaced-array result-array)))
      (apply #'map-into displaced-result-array function displaced-arrays)
      result-array)))

(defun assoc-value (alist akey &rest rest &key test test-not key)
  (declare (ignorable test test-not key))
  (cdr (apply #'assoc akey alist rest)))

(ql:quickload "cffi" :silent t)
;;(ql:quickload "cl-plplot")
;;(ql:quickload "cl-proj")
(ql:quickload "iterate" :silent t)
;; sudo apt-get install libproj-dev proj-data proj-bin
(ql:quickload "cl-ppcre" :silent t)
(use-package :cl-ppcre)
(use-package :cffi)
;; (use-package :cl-plplot)
;; (use-package :cl-proj)
(use-package :iterate)

(define-foreign-library libeccodes
  (:linux "libeccodes.so"))

(cffi::load-foreign-library "libeccodes.so")

;; The index structures are internal, so we can't grok them, but we can
;; pass them around

(defctype codes_index :pointer)
(defctype codes_context :pointer)
(defctype codes_handle :pointer)
(defctype grib_handle :pointer)
(defctype grib_context :pointer)

(defcfun codes_context_get_default codes_context)

(defcfun codes_index_new codes_index
  (codes_context :pointer)
  (keys :string)
  (err (:pointer :int)))

(defctype file :pointer)

(defcfun fopen file
  (pathname :string)
  (mode :string))

(defun fopen* (pathname &optional (mode "rb"))
  (fopen pathname mode))

(defcfun fclose file
  (file file))

(defmacro with-open-c-file ((var filename &optional (mode "rb")) &body body)
  `(let ((,var))
     (unwind-protect
	  (progn
	    (setf ,var (fopen* ,filename ,mode))
	    (assert (not (cffi:null-pointer-p ,var)) nil 'sb-int:simple-file-error :format-control "Unable to open file ~A" :format-arguments (list ,filename))
	    ,@body)
       (when (not (cffi:null-pointer-p ,var))
	 (fclose ,var)))))

(defcenum ProductKind
  :product_any
  :product_grib
  :product_bufr
  :product_metar
  :product_gts
  :product_taf)

(defcfun codes_handle_new_from_file grib_handle
  (grib_context grib_context)
  (file file)
  (product productkind)
  (error (:pointer :int)))

(defun codes_handle_new_from_file* (grib_context file product)
  (with-foreign-object (error :int)
    (let ((result (codes_handle_new_from_file grib_context file product error)))
      ;;(format t "error is ~A~%" error)
      (assert (not (null-pointer-p result)) nil "Invalid handle??")
      result)))

(defcfun codes_handle_clone grib_handle
  (grib_in grib_handle))

(defcfun codes_write_message :int
  (codes_handle codes_handle)
  (file :string)
  (mode :string))

(defcfun grib_check :void
  (call :string)
  (file :string)
  (line :int)
  (e :int)
  (msg :string))

(defcfun grib_get_error_message :string
  (code :int))

(defmacro codes_check (function)
  (let ((function-code (format nil "~A" function))
	(result (gensym)))
    `(let ((,result ,function))
       (when (not (= ,result 0))
	 (warn (format nil "~A: ~A" ,function-code (grib_get_error_message ,result)))))))

(defctype size_t :unsigned-int)

(defcfun codes_get_size :int
  (codes_handle codes_handle)
  (key :string)
  (size (:pointer size_t)))

(defun codes_get_size* (codes-handle key)
  (cffi:with-foreign-object (length :unsigned-int) ;; FIXME SHOULD BE SIZE_T
    (codes_check (codes_get_size codes-handle key length))
    (mem-ref length :unsigned-int)))

(defcfun codes_handle_delete :int
  (codes_handle codes_handle))

(defmacro with-code-handle ((handle generator) &body body)
  `(let ((,handle (cffi:null-pointer)))
     (unwind-protect
	  (progn
	    (setf ,handle ,generator)
	    ,@body)
       (when (not (cffi:null-pointer-p ,handle))
	 (let ((result (codes_handle_delete ,handle)))
	   (assert (= result 0) nil "Problem deleting handle"))))))

(defcfun codes_get_double_array :int
  (codes_handle codes_handle)
  (key :string)
  (codes_array :pointer)
  (length (:pointer size_t)))

(defcfun codes_get_bytes :int
  (codes_handle codes_handle)
  (key :string)
  (bytes :pointer)
  (length (:pointer size_t)))


(defcfun codes_get_double :int
  (codes_handle codes_handle)
  (key :string)
  (double :pointer))

(defun codes_get_double* (codes_handle key)
  (with-foreign-object (len :double)
    (codes_get_double codes_handle key len)
    (mem-ref len :double)))

(defcfun codes_get_long :int
  (codes_handle codes_handle)
  (key :string)
  (long :pointer))

(defun codes_get_long* (codes_handle key)
  (with-foreign-object (len :long)
    (codes_get_long codes_handle key len)
    (mem-ref len :long)))

(defcfun codes_set_long :int
  (codes_handle codes_handle)
  (key :string)
  (long :unsigned-int))

(defun codes-get-double-array (codes_handle key)
  (with-foreign-object (len :unsigned-int)
    (let ((nj (codes_get_long* codes_handle "Nj"))
	  (ni (codes_get_long* codes_handle "Ni"))
	  (size (codes_get_size* codes_handle "values")))
      (assert (= (* nj ni) size))
      (setf (mem-ref len :unsigned-int) size)
      (let ((result (make-array (list nj ni) :element-type 'double-float)))
	(sb-ext::with-pinned-objects (result)
	  (codes_check (codes_get_double_array
			codes_handle key
			(sb-kernel::vector-sap (sb-kernel:%array-data result))
			len)))
	(assert (= (mem-ref len :unsigned-int) size))
	result))))

(defun codes-get-bytes (codes_handle key)
  (with-foreign-object (len :unsigned-int)
    (let ((nj (codes_get_long* codes_handle "Nj"))
	  (ni (codes_get_long* codes_handle "Ni"))
	  (size (codes_get_size* codes_handle "values")))
      (assert (= (* nj ni) size))
      (setf (mem-ref len :unsigned-int) size)
      (let ((result (make-array (list nj ni) :element-type '(unsigned-byte 64))))
	(sb-ext::with-pinned-objects (result)
	  (codes_check (codes_get_double_array
			codes_handle key
			(sb-kernel::vector-sap (sb-kernel:%array-data result))
			len)))
	(assert (= (mem-ref len :unsigned-int) size))
	result))))

(defcfun codes_set_double_array :int
  (codes_handle codes_handle)
  (key :string)
  (codes_array :pointer)
  (length :unsigned-int))

(defcfun codes_set_long_array :int
  (codes_handle codes_handle)
  (key :string)
  (codes_array :pointer)
  (length :unsigned-int))

(defcfun codes_set_bytes :int
  (codes_handle codes_handle)
  (key :string)
  (bytes :pointer)
  (length (:pointer size_t)))

(defun to-unsigned-bytes (new-data)
  (let ((new-array (make-array (array-dimensions new-data) :element-type '(unsigned-byte 64))))
    (loop :for m :below (car (array-dimensions new-data)) :do
       (loop :for n :below (cadr (array-dimensions new-data))
	  :do (setf (aref new-array m n) (coerce (round (aref new-data m n)) '(unsigned-byte 64)))))
    new-array))

(defun codes-set-bytes (codes_handle key new-data)
  "new-data must be pinned!"
  (let ((nj (codes_get_long* codes_handle "Nj"))
	(ni (codes_get_long* codes_handle "Ni"))
	(size (codes_get_size* codes_handle "values")))
    (assert (= (* nj ni) size))
    (assert (equalp (array-dimensions new-data) (list nj ni)))
    (assert (typep new-data '(simple-array (unsigned-byte 64) (* *))))
    (with-foreign-object (len :unsigned-int)
      (setf (mem-ref len :unsigned-int) size)
      (let ((result
	     (codes_set_bytes
	      codes_handle key
	      (sb-kernel::vector-sap (sb-kernel:%array-data new-data))
	      len)))
	(format t "Initial length was ~A packed length is ~A~%" size (mem-ref len :unsigned-int))
	(assert (= result 0))))))

(defun codes-set-long-array (codes_handle key new-data)
  (let ((nj (codes_get_long* codes_handle "Nj"))
	(ni (codes_get_long* codes_handle "Ni"))
	(size (codes_get_size* codes_handle "values")))
    (assert (= (* nj ni) size))
    (assert (equalp (array-dimensions new-data) (list nj ni)))
    (let ((new-data
	   (if (typep new-data '(simple-array (unsigned-byte 64) (* *)))
	       new-data
	       (let ((new-array (make-array (array-dimensions new-data) :element-type '(unsigned-byte 64))))
		 (loop :for m :below (car (array-dimensions new-data)) :do
		    (loop :for n :below (cadr (array-dimensions new-data))
		       :do (setf (aref new-array m n) (coerce (round (aref new-data m n)) '(unsigned-byte 64)))))
		 new-array))))
      (sb-ext::with-pinned-objects (new-data)
	(with-foreign-object (len :unsigned-int)
	  (setf (mem-ref len :unsigned-int) size)
	  (let ((result
		 (codes_set_long_array
		  codes_handle key
		  (sb-kernel::vector-sap (sb-kernel:%array-data new-data))
		  len)))
	    (assert (= result 0))))))))

(defun codes-set-double-array (codes_handle key new-data)
  (let ((nj (codes_get_long* codes_handle "Nj"))
	(ni (codes_get_long* codes_handle "Ni"))
	(size (codes_get_size* codes_handle "values")))
    (assert (= (* nj ni) size))
    (assert (equalp (array-dimensions new-data) (list nj ni)))
    (assert (typep new-data '(simple-array double-float (* *))))
    (sb-ext::with-pinned-objects (new-data)
      (codes_check (codes_set_double_array
		    codes_handle key
		    (sb-kernel::vector-sap (sb-kernel:%array-data new-data))
		    size)))))


(defun codes-get-statistics (handle)
  (list (codes_get_double* handle "min")
	(codes_get_double* handle "max")
	(codes_get_double* handle "average")))

;; (defun codes_get_double_element* (codes_handle key codes
;; 				  ;; (format t "max is ~A~%" (codes_get_double* handle "max"))
;; 				  ;; (format t "min is ~A~%" (codes_get_double* handle "min"))
;; 				  ;; (format t "average is ~A~%" (codes_get_double* handle "average"))
;; 				  ))

(defun linspace (x y n)
  (let ((step (/ (- y x) (1- n))))
    (append (cons x
		  (loop :repeat (- n 2) :for b :from (+ x step) :by (/ (- y x) (1- n)) :collect b))
	    (list y))))

;; (defun contour-plot (data &key min max n (filename "/tmp/test.png"))
;;   (let ((w (basic-window))
;; 	(c (new-contour-plot data :fill-type :smooth :contour-levels
;; 			     (when (and min max n)
;; 			       (make-array n :initial-contents (linspace min max n))))))
;;     (add-plot-to-window w c)
;;     (render w "png" :filename filename :size-x 1000 :size-y 1000)
;;     ;; (run-program "/usr/bin/eog" '("/tmp/test.png") :wait nil)
;;     ))

(defun grib-load-data (filename &key (print-statistics nil))
  (with-open-c-file (file filename "rb")
    (with-code-handle (handle (codes_handle_new_from_file* (cffi:null-pointer) file :product_grib))
      (when print-statistics
	(destructuring-bind (min max average) (codes-get-statistics handle)
	  (declare (ignorable average))
	  (format t "min/max/average is ~A ~A ~A~%" min max average)))
      (codes-get-double-array handle "values"))))

(defun grib-clone (filename)
  (with-open-c-file (file filename "rb")
    (with-code-handle (handle (codes_handle_new_from_file* (cffi:null-pointer) file :product_grib))
      (codes_handle_clone handle))))

(defun grib-print-data (filename)
  (with-open-c-file (file filename "rb")
    (with-code-handle (handle (codes_handle_new_from_file* (cffi:null-pointer) file :product_grib))
      (destructuring-bind (min max average) (codes-get-statistics handle)
	(declare (ignorable average))
	(format t "min/max/average is ~A ~A ~A~%" min max average)
	(let* ((result (codes-get-double-array handle "values")))
	  ;;(contour-plot result :filename (format nil "~A.png" filename))
	  ;; (array-dimensions result)
	  result
	  )))))

(load "model-parameters.lisp")

(defparameter *forecast-hour* 1)

(defun input-file (variable &key level isbl-level (forecast-hour *forecast-hour*)
					    (date *date*) (forecast-zero-hour *forecast-zero-hour*))
  (assert (numberp forecast-zero-hour))
  (assert (numberp forecast-hour))
  (let* ((header (format nil "~A/~A" *directory* *fileheader*)))
    (cond
      (level (format nil "~A_~A_~A~A~A~2,'0D_P~3,'0D~A"
		     header variable level *resolution* date forecast-zero-hour forecast-hour *tail*))
      (isbl-level
       (if (member *model* '("gdps" "rdps") :test #'string=)
	   (format nil "~A_~A_~3,'0D~A~A~2,'0D_P~3,'0D~A"
		   header variable isbl-level *resolution* date forecast-zero-hour forecast-hour *tail*)
	   (format nil "~A_~A_~4,'0D~A~A~2,'0D_P~3,'0D~A"
		   header variable isbl-level *resolution* date forecast-zero-hour forecast-hour *tail*)))
      (t (format nil "~A_~A~A~A~2,'0D_P~3,'0D~A"
		 header variable date *resolution* forecast-zero-hour forecast-hour *tail*)))))

    
(defparameter *memoization-hashtable* (make-hash-table :test 'equal))

(defun clear-memoization-hashtable ()
 (setf *memoization-hashtable* (make-hash-table :test 'equal)))

(defun get-other-cache-keys ()
  (list *date* *forecast-zero-hour* *forecast-hour*))

(defmacro defcached (function-name (&rest params) &body body)
  ;; once-only function-name, params, etc...
  (assert (null params))
  (let ((key (gensym)))
  `(defun ,function-name (,@params)
     (let ((,key (cons ',function-name (get-other-cache-keys))))
       (or (apply #'values (gethash ,key *memoization-hashtable*))
	   (apply #'values (setf (gethash ,key *memoization-hashtable*) (multiple-value-list (progn ,@body)))))))))

(defcached get-terrain-altitude ()
  (let ((data (handler-case
		  (grib-load-data (input-file "HGT_SFC" :level 0 :forecast-hour *forecast-hour*))
		(sb-int:simple-file-error ()
		  (grib-load-data (input-file "HGT_SFC" :level 0 :forecast-hour 0))))))
    data))

(defcached get-altitudes ()
  (mapcar (lambda (isbl-level)
	     (cons isbl-level
		   (grib-load-data (input-file "HGT_ISBL" :isbl-level isbl-level :forecast-hour *forecast-hour*))))
	  *isbl-levels*))

(defcached get-temperatures ()
  (mapcar (lambda (isbl-level)
	     (cons isbl-level
		   (grib-load-data (input-file "TMP_ISBL" :isbl-level isbl-level :forecast-hour *forecast-hour*))))
	  *isbl-levels*))

(defcached get-surface-temperature ()
  (let ((data (grib-load-data (input-file "TMP_TGL" :level 2 :forecast-hour *forecast-hour*))))
    data))

(defconstant +dry-adiabatic-lapse-rate+ 0.0098d0 "K / meter")

(defcached get-boundary-layer-depth ()
  (let* ((surface-temperature (get-surface-temperature))
	 (terrain-altitude (get-terrain-altitude))
	 (altitudes (get-altitudes))
	 (temperatures (get-temperatures))
	 (dimensions (array-dimensions surface-temperature))
	 (bldepth (make-array dimensions :element-type 'double-float :initial-element 0d0)))
    (iter (for m below (car dimensions))
	  (iter (for n below (cadr dimensions))
		;; (format t "At m,n ~A,~A, sfc-altitude ~,1f, sfc-temp ~,1f~%"
		;; 	m n (aref terrain-altitude m n) (aref surface-temperature m n))
		;; (format t "~20A ~20A ~20A~%" "Altitude (m)" "Temperature (K)" "Parcel Temperature (K)")
		(iter (for isbl-level in *isbl-levels*)
		      (with sfc-temperature = (aref surface-temperature m n))
		      (with sfc-altitude = (aref terrain-altitude m n))
		      (for altitude = (aref (assoc-value altitudes isbl-level) m n))
		      (for previous-altitude previous altitude initially sfc-altitude)
		      (for temperature = (aref (assoc-value temperatures isbl-level) m n))
		      (for previous-temperature previous temperature initially sfc-temperature)
		      ;; if lifted packet is colder than surrounding air it will stop rising
		      (for parcel-temperature = (- sfc-temperature (* +dry-adiabatic-lapse-rate+ (- altitude sfc-altitude))))
		      ;; (format t "~20,1f ~20,3f ~20,3f~%" altitude temperature parcel-temperature)
		      ;; (sleep 0.01)
		      (for count from 0)
		      (until (< parcel-temperature temperature))
		      (finally
		       (when (> count 0)
			 (if (= temperature previous-temperature)
			     (setf (aref bldepth m n) previous-altitude)
			     ;; stable!
			     (let* ((lapse-rate (/ (- temperature previous-temperature)
						   (- altitude previous-altitude)))
				    (interp-bldepth
				     (/ (+ sfc-temperature (- previous-temperature) (* +dry-adiabatic-lapse-rate+ sfc-altitude) (* lapse-rate previous-altitude))
					(+ lapse-rate +dry-adiabatic-lapse-rate+))))
			       ;; (format t "interpolated bldepth is ~,1f m~%" interp-bldepth)
			       (if (> interp-bldepth 0d0)
				   (setf (aref bldepth m n) interp-bldepth)))))))))
    bldepth))

;; now need surface sensible heat flux, vhf, etc...

(defcached get-surface-sensible-heat-flux ()
  (grib-load-data (input-file "SHTFL_SFC" :level 0 :forecast-hour *forecast-hour*)))

(defcached get-latent-heat-flux ()
  (grib-load-data (input-file "LHTFL_SFC" :level 0 :forecast-hour *forecast-hour*)))

(defconstant +heat-capacity-of-air-rt+ 1006d0 "J/kg K at STP for dry air")
(defconstant +heat-of-vaporization-of-water+ 2.502d6 "J/kg")

(defcached get-virtual-temperature-heat-flux ()
  (array-map-double-float (lambda (shf lhf sfctmp)
			    (+ shf (* (* 0.61 +heat-capacity-of-air-rt+
					 (/ +heat-of-vaporization-of-water+))
				      sfctmp lhf)))
			  (get-surface-sensible-heat-flux)
			  (get-latent-heat-flux)
			  (get-surface-temperature)))

(defcached get-surface-pressure ()
  (grib-load-data (input-file "PRES_SFC" :level 0 :forecast-hour *forecast-hour*)))

(defcached get-potential-temperature ()
  "Potential temperature is the temperature a parcel of air would have if
   it would raised or lowered to 1000mb.  The Poisson equation says:
   T0/T = (p0/p)^0.286"
  (array-map-double-float
   (lambda (tsfc psfc)
     (* tsfc
	(expt (/ 100000d0 psfc) 0.28482d0)))
   (get-surface-temperature) (get-surface-pressure)))

(defcached get-wstar ()
  (array-map-double-float
     (lambda (vhf bldepth pot-temp)
       (if (or (< vhf 0d0) (< bldepth 0d0))
	   0d0
	   (expt (* 0.0075516d0 (/ pot-temp) vhf bldepth) 0.33333)))
     (get-virtual-temperature-heat-flux)
     (get-boundary-layer-depth)
     (get-potential-temperature)))

(defconstant +sink-rate+ 1.0)

;; will def-cache the grib reading funcs so that I don't have to worry about multiple calls

(defun close-to? (x y &optional (tolerance 1d-5))
  (< (abs (- x y)) tolerance))

(defun root? (f x &optional (tolerance 1d-5))
  (close-to? (funcall f x) 0 tolerance))

(defun includes-root? (f a b)
  (< (* (funcall f a) (funcall f b)) 0))

(defmacro named-let (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (,tag ,@(mapcar #'cadr var-vals))))

(define-condition root-not-bracketed (simple-error)())
(define-condition max-iters (simple-error)())

(defun secant (f a b &optional (tolerance 1d-5) (max-iters 50))
  (named-let next ((x1 a) (y1 (funcall f a)) (x2 b) (y2 (funcall f b)) (n max-iters))
    (let ((x3 (/ (- (* x1 y2) (* x2 y1)) (- y2 y1))))
      (cond
	((close-to? x3 x2 tolerance) x3)
	((zerop n) (error 'root-not-bracketed))
	(t (next x2 y2 x3 (funcall f x3) (- n 1)))))))

(defun bisection (f x1 x2 &optional (tolerance 5.0) (max-iters 50))
  (named-let divide ((a x1) (b x2) (n max-iters))
    (when (zerop n) (error 'max-iters))
    (if (<= (* (funcall f a) (funcall f b)) 0)
	(let ((c (* 0.5 (+ a b))))
	  (if (close-to? a b tolerance)
	      c
	      (or (divide a c (- 1 n)) (divide c b (- 1 n)))))
	nil)))

(defcached get-cloud-base ()
  ;; Need to get boundary layer depth to make sure we don't predict clouds
  ;; that would never form
  (let ((surface-dewpoint-depression
	 (grib-load-data (input-file "DEPR_TGL" :level 2 :forecast-hour *forecast-hour*))))
    (array-map-double-float (lambda (x h) (+ (* x 121d0) h))
			    surface-dewpoint-depression (get-terrain-altitude))))

(defun get-hcrit ()
  "(values altitude agl)"
  (declare (optimize (speed 3)))
  (let* ((terrain-altitude (get-terrain-altitude))
	 (lcl (get-cloud-base))
	 (bldepth (get-boundary-layer-depth))
	 (wstar (get-wstar))
	 (hcrit
	  (array-map-double-float
	   (lambda (lcl wstar bldepth terrain-altitude)
	     (declare (type double-float lcl wstar bldepth terrain-altitude))
	     (let ((bldepth-agl (max 0d0 (- bldepth terrain-altitude))))
	       (if (> bldepth-agl 100d0)
		   (labels ((updraft (agl)
			      (declare (type (double-float 0d0) agl)) ;; positive value
			      (let* ((updraft-velocity (* wstar 4d0 (expt (/ agl bldepth-agl) 0.3333d0)
							  (- 1d0 (* 1.1d0 (/ agl bldepth-agl))))))
				;; (format t "updraft velocity at ~,1f m agl is ~,2f m/s~%"
				;;  	  agl updraft-velocity)
				(- updraft-velocity +sink-rate+))))
		     ;; (format t "Boundary layer depth is ~,1f M AGL, wstar is ~,2f~%" bldepth-agl wstar)
		     (let ((max-height (+ terrain-altitude
					  (or
					   (the (or double-float null) (bisection #'updraft (* 0.25d0 bldepth-agl) bldepth-agl 10d0)) ;; 10m tolerance
					   0d0))))
		       ;; (format t "Found max-height ~,1f m (absolute) ground level ~,1f m~%" max-height terrain-altitude)
		       (max terrain-altitude (min max-height lcl))))
		   terrain-altitude))) lcl wstar bldepth terrain-altitude)))
    (values hcrit (array-map-double-float (lambda (hcrit terrain-altitude)
					    (declare (type double-float terrain-altitude hcrit))
					    (- hcrit terrain-altitude)) hcrit terrain-altitude))))
;; (defun calculate-surface-pressure ()
;;   "Works well, errors are less than 40 pascals for cases that are not
;;    strictly 0.  When we hit height 0.0 at the ocean, though, this can
;;    be off by 700 pascals.  Not a real issue for anything."
;;   ;; we have surface pressure directly from the data, but this calculates it
;;   ;; as a check
;;   ;; pressure = c*exp(a - (height / b))
;;   ;; log(pressure) = log(c)+a - height/b
;;   ;; so that if I have two known heights, the log(pressure) linearly iterpolates
;;   (let (;; (sfc-pressure (get-surface-pressure))
;; 	(sfc-altitude (get-terrain-altitude))
;; 	(height-vs-pressure (get-altitudes)))
;;     (labels ((pressure-at-height (height m n)
;; 	       (iter (for (pressure . altitude) in height-vs-pressure)
;; 		     (for alt = (aref altitude m n))
;; 		     (for prev-alt previous alt initially 0d0)
;; 		     (for prev-pressure previous pressure initially 1015.0)
;; 		     (for count from 0)
;; 		     ;; (format t "Pressure ~,1f mbar Height ~,1f m~%" pressure alt)
;; 		     ;; (sleep 1.0)
;; 		     (until (> alt height))
;; 		     (finally
;; 		      ;; interpolate between previous altitude / previous-pressure
;; 		      ;; and current altitude / current pressure
;; 		      (if (> count 0)
;; 			  (return
;; 			    (exp
;; 			     (- (log prev-pressure) (* (- height prev-alt)
;; 						       (/ (- (log prev-pressure) (log pressure))
;; 							  (- alt prev-alt))))))
;; 			  (return 1015d0))))))
;;       (let ((sfc-pressure (make-array (array-dimensions sfc-altitude) :element-type 'double-float)))
;; 	(iter (for m below (car (array-dimensions sfc-altitude)))
;; 	      (iter (for n below (cadr (array-dimensions sfc-altitude)))
;; 		    (setf (aref sfc-pressure m n) (* 100d0 (pressure-at-height (aref sfc-altitude m n) m n)))))
;; 	sfc-pressure
;;       ;; checker
;;       ;; (iter outer (for m below (car (array-dimensions sfc-altitude)))
;;       ;; 	  (iter (for n below (cadr (array-dimensions sfc-altitude)))
;;       ;; 		(for p-at-height = (pressure-at-height (aref sfc-altitude m n) m n))
;;       ;; 		(when p-at-height
;;       ;; 		  (for calculated-pressure = (* 100.0 p-at-height))
;;       ;; 		  (for sfc-pres = (aref sfc-pressure m n))
;;       ;; 		  ;;(format t "~A ~A: ~,1f ~,1f~%" m n calculated-pressure sfc-pres)
;;       ;; 		  (in outer (finding (list m n calculated-pressure sfc-pres)
;;       ;; 				     maximizing (- calculated-pressure sfc-pres))))))
;;     ))))
      
;; TODO check that get-altitudes and get-surface-pressure together predict get-terrain-altitude.
;;      DONE
;;      check that temperatures and surface pressure together predict get-surface-temperature

(defun write-double-array (array variable-name parameter-num)
  (sb-ext::with-pinned-objects (array)
    (with-code-handle (handle (handler-case (grib-clone (input-file "HGT_SFC" :level 0 :forecast-hour *forecast-hour*))
				(sb-int:simple-file-error ()
				  (grib-clone (input-file "HGT_SFC" :level 0 :forecast-hour 0)))))
      (codes_set_long handle "parameterCategory" 190)
      (codes_set_long handle "parameterNumber" parameter-num)
      (codes_set_long handle "local_table" 1)
      (codes-set-double-array handle "values" array)
      (let ((filename (cl-ppcre:regex-replace "HGT_SFC" (input-file "HGT_SFC" :level 0 :forecast-hour *forecast-hour*) variable-name)))
	(format t "Writing file ~A~%" filename)
	(codes_write_message handle filename "w")))))

(defun write-hcrit ()
  (multiple-value-bind (hcrit hcrit-agl) (get-hcrit)
    (write-double-array hcrit-agl "HCRIT_SFC" 190)
    (write-double-array hcrit "HCRIT_ABS" 191)))

(defun write-wstar ()
  (let ((wstar (get-wstar)))
    (write-double-array wstar "WSTAR" 192)))

(defun write-cloud-base ()
  (let ((lcl (get-cloud-base)))
    (write-double-array lcl "LCL" 193)))

(defun write-bldepth ()
  (let ((bldepth (get-boundary-layer-depth)))
    (write-double-array bldepth "BLDEPTH" 194)))

(defun read-back-hcrit-agl ()
  (grib-load-data (input-file "HCRIT_SFC" :level 0 :forecast-hour *forecast-hour*) :print-statistics t)
  nil)

(defun do-all-variables (hours)
  (format t "Generating files for ~A~A for ~A hour values~%" *date* *forecast-zero-hour* (length hours))
  (map nil (lambda (hour)
	     (let ((*forecast-hour* hour))
	       (format t "Forecast hour ~A~%" hour)
	       (handler-case
		   (progn
		     (write-hcrit)
		     (write-wstar)
		     (write-bldepth)
		     (format t "Done with forecast hour ~A~%" hour)
		     (clear-memoization-hashtable)
		     (sb-ext:gc :full t))
		 (error (e)
		   (format t "Failed at hour ~A with error ~A~%" hour e)))))
       hours))


;;ignore errors
(do-all-variables (or (mapcar #'parse-integer (cdr *posix-argv*)) (iter (for h from *timestart* to *timestop* by *timestep*) (collect h))))
