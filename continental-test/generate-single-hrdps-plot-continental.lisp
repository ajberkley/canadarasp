#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(setf asdf:*central-registry* '(#P"/home/ubuntu/quicklisp/quicklisp/"                                                                                                    
                                #P"/home/ubuntu/continental-test/cl-gdal/"                                                                                               
                                #P"/home/ubuntu/continental-test/"))

(ql:quickload "iterate")
(ql:quickload "trivial-garbage")
(use-package :iterate)

(asdf:load-system "vector-objects")
(use-package :vector-objects)

(require 'cl-gdal)
(require 'cl-ogr)
(load "cl-gdal/src/gdal-core.lisp")
(load "cl-gdal/src/ogr-core.lisp")
(load "cl-gdal-local.lisp")
(use-package :cl-gdal)
(use-package :cl-ogr)

(ql:quickload "png") ;; WARNING, I HAVE LOCAL CHANGES TO CL-PNG! in files image.lisp and libpng.lisp
(require 'png)

(quicklisp::quickload "local-time")

(ql:quickload "cl-ppcre")
(require 'cl-ppcre)
(sb-posix:setenv "GDFONTPATH" "/usr/share/fonts/truetype/msttcorefonts/" 1)
(ql:quickload "cl-gd") ;; don't forget, you have to compile the .so file!
;; sudo apt-get install libgd2-dev

(load "utils.lisp")
(load "model-parameters.lisp") ;; our config file 

(defun base-directory ()
  (format nil "/mnt/html/tiles/~A/" *model*))

(defun directory-from-tile-info (lat-lon-info)
  (format nil "~A:~A:~A:~A/" 
	  (first lat-lon-info) (third lat-lon-info) (second lat-lon-info) (fourth lat-lon-info)))

(defun directory-from-date (yyyy mm dd)
  (format nil "~A-~2,'0d-~2,'0d/" yyyy mm dd))

(defparameter *wind-color-scale*  
  '((0f0 (255 255 255))
    (5f0 (204 204 204))
    (10f0 (121 235 151))
    (15f0 (0 255 94))
    (20f0 (177 255 63))
    (25f0 (252 253 56))
    (30f0 (255 107 0))
    (35f0 (243 0 0))
    (40f0 (169 0 0))
    (45f0 (135 0 120))
    (50f0 (0 0 255))))

(defparameter *normalized-color-scale*
  `((0.0f0 (0 0 255))
    (0.10f0 (0 153 255))
    (0.20f0 (0 238 204 ))
    (0.30f0 (0 204 51 ))
    (0.40f0 (102 221 0))
    (0.50f0 (255 255 0))
    (0.60f0 (255 204 0))
    (0.70f0 (255 136 0))
    (0.80f0 (255 17 0))
    (0.90f0 (187 0 51))
    (1.00f0 (90 0 90))))

(defun find-bounding-colors (color-scale target-value)
  (let (low high)
    (iter (for (a c) in color-scale)
	  (when (or (not low) (<= a target-value))
	    (setf low (list a c)))
	  (when (and (not (first-time-p)) (not high) (>= a target-value)) (setf high (list a c))))
    (when (not high)
      (setf low (car (last color-scale 2)))
      (setf high (car (last color-scale 1))))
    (list low high)))

(declaim (inline lookup-in-color-scale))
(defun lookup-in-color-scale (value color-scale)
  (declare (optimize (speed 3)))
  (declare (type single-float value))
  (or (iter (for c in color-scale)
	    (for pc previous c initially (car c))
	    (until (>= (the single-float (car c)) value))
	    (finally (return (cadr pc)))) (cadar color-scale)))

(defun make-basic-color-scale (min max)
  (iter (for value from min by (/ (- max min) (1- (length *normalized-color-scale*))))
	(for c in *normalized-color-scale*)
	(collect (list value (cadr c)))))
	
(defparameter *cloud-color-scale*  (make-basic-color-scale 0f0 100f0))
(defparameter *vvel-color-scale*  (make-basic-color-scale -1.5f0 1.5f0))
(defparameter *thermal-color-scale*  (make-basic-color-scale -0.5f0 5.5f0))
(defparameter *cape-color-scale*  (make-basic-color-scale 0f0 5000f0))
(defparameter *hcrit-color-scale*  (make-basic-color-scale 0f0 4000f0))
(defparameter *surface-heating-color-scale*  (make-basic-color-scale 0f0 1000f0))
(defparameter *surface-temperature-color-scale* (make-basic-color-scale -4f0 36f0))
(defparameter *surface-dewpoint-depression-color-scale* (make-basic-color-scale -4f0 4f0))
(defparameter *surface-pressure-color-scale* (make-basic-color-scale 980f0 1050f0))
(defparameter *terrain-color-scale* (make-basic-color-scale 0f0 3000f0))
(defparameter *rain-color-scale* (make-basic-color-scale 0f0 20f0))

(defun write-text (x y text &key (point-size 10d0) (align (or :left :centerx :centery :top :right)) (font-name "arial") line-spacing)
  ;; if text is a list then it is multi lines
  (labels ((do-it (x y do-not-draw)
	     (cl-gd:draw-freetype-string x y text :font-name font-name :point-size point-size
					 :do-not-draw do-not-draw :line-spacing line-spacing)))
    ;;(format t "Target x y and alignment is ~A ~A ~A~%" x y :align)
    (let* ((bounding-box (do-it x y t))
	   (width (- (aref bounding-box 2) (aref bounding-box 0)))
	   (height (- (aref bounding-box 3) (aref bounding-box 5))))
      ;;(format t "Bounding box is ~A, width is ~A height is ~A~%" bounding-box width height)
      (map nil (lambda (a)
		 (ecase a
		   (:centerx (decf x (round width 2)))
		   (:centery (incf y (round height 2)))
		   (:bottom (decf y (- (aref bounding-box 1) y)))
		   (:top (decf y (- (aref bounding-box 5) y)))
		   (:right (decf x width))
		   (:left t))) (if (listp align) align (list align)))
      (do-it x y nil))))
		   

(defun draw-color-scale (color-scale filename &key (width 1000) (height 48) (format "~d")
				       (units "[%]") (process #'round) (border (/ width 10)) (view nil))
  (cl-gd:with-image* (width height)
    (cl-gd:allocate-color 255 255 255) ;; background color, white
    (let ((black (cl-gd:allocate-color 0 0 0))
	  (colors (iter (for c in color-scale)
			(collect (list (first c)
				       (apply #'cl-gd:allocate-color
					      (second c)))))))
      (cl-gd:with-default-color (black)
	(iter
	  (with step = (/ (- width (* 2 border)) (1- (length color-scale))))
	  (for x1 from border by step)
	  (for (label1 color) in (butlast colors))
	  (cl-gd:draw-rectangle* (round x1) 2 (round (+ x1 step)) (- height 18)
				 :filled t :color color)
	  (cl-gd:draw-line  (round x1) 2 (round x1) (- height 18))
	  (write-text (if (first-time-p) border (round x1))
		      (- height 18) (format nil format (funcall process label1))
		      :align '(:top :centerx)))
	(cl-gd:draw-line  (- width border) 2 (- width border) (- height 18))
	(cl-gd:draw-line  border (- height 18) (- width border) (- height 18))
	(cl-gd:draw-line border 2 (- width border) 2)
	(write-text  (- width border) (- height 18)
		     (format nil format (funcall process (caar (last colors))))
		     :align '(:top :centerx))
	(write-text 0 (+ (round height 2) 5) units :point-size 18d0 :align :left)
	(write-text width (+ (round height 2) 5) units :point-size 18d0 :align :right)))
      (cl-gd:write-image-to-file filename :compression-level 6 :if-exists :supersede))
    (when view (print/run-program "/usr/bin/eog" (list filename))))

(defun write-title (text1 text2 filename &key (width 500) (height 48) (view nil))
  ;; multi-line text needs :line-spacing 1.05 and #\newline in the strings...
  (cl-gd:with-image* (width height)
    (cl-gd:allocate-color 255 255 255) ;; background color, white
    (cl-gd:with-default-color ((cl-gd:allocate-color 0 0 0))
      (write-text (round width 2) 0 text1
		  :font-name "trebucbd"
		  :point-size 18d0 :align '(:top :centerx))
      (write-text (round width 2) 48 text2
		  :font-name "trebuc"
		  :point-size 12d0 :align '(:bottom :centerx))
      (cl-gd:write-image-to-file filename :compression-level 6 :if-exists :supersede)))
  (when view (print/run-program "/usr/bin/eog" (list filename))))

(defun write-default-title (parameter forecast-init-year forecast-init-month forecast-init-day forecast-init-hour filename &key (view nil))
  (let ((second-line (format nil "~A initialized ~A-~2,'0d-~A (~2,'0d:00) UTC"
			     (string-upcase *model*)
			     forecast-init-year forecast-init-month forecast-init-day
			     forecast-init-hour)))
    (write-title parameter second-line filename :view view)))

(defun wind-color (value)
  (lookup-in-color-scale (coerce value 'single-float) *wind-color-scale*))

(defun cloud-color (value)
  (declare (type double-float value))
  (lookup-in-color-scale (coerce value 'single-float) *cloud-color-scale*))

(defun angle-color (value)
  ;; degrees
  (let ((norm-value (/ value 360.0)))
    (if (> value 180.0)
	(list 0 0 (round (* 255 norm-value)))
	(list (round (* 255 norm-value)) 0 0))))



(defconstant +dst-no-data-single+ 99999f0)
(defconstant +dst-no-data-double+ 99999d0)

(defun warp-to-google-map-tif (filename-unwarped filename-warped)
  ;; Data is stored at doubles, so I should be able to recognize this
   (print/run-program "/usr/local/bin/gdalwarp" ;; 100 ms
		      (list "-overwrite" "-dstnodata" (format nil "~A" +dst-no-data-double+)
			    "-t_srs" "EPSG:3857" "-of" "GTiff"
			    filename-unwarped filename-warped)))

(defparameter *tmp-fileindex* 0)

(defparameter *unique-identifier* "blarg")

(defun tmp-filename (&optional file (extension ""))
  (if file
    (format nil "/mnt/~A~A-~A-~A.~A" *unique-identifier* (incf *tmp-fileindex*) file (sxhash (list file *tmp-fileindex* extension)) extension)
    (format nil "/mnt/~A~A.tmp" *unique-identifier* (incf *tmp-fileindex*))))

(defun rm-file (filename)
  (print/run-program "/bin/rm" (list filename)))

(defparameter *tmp-files* nil)

(defun translate-from-hrdps-names-to-current-model (name)
  "The GDPS and RDPS use UGRD_ISBL_950 instead of UGRD_ISBL_0950"
  (if (and (find *model* '("rdps" "gdps") :test #'string=) (> (length name) 9) (string= (subseq name 5 9) "ISBL"))
      (destructuring-bind (grd isbl num) (cl-ppcre:split #\_ name)
	(format nil "~A_~A_~A" grd isbl (parse-integer num)))
      name))

(defparameter *params-and-names*
  `(("sfcwind" "Wind at 10m" ("UGRD_TGL_10" "VGRD_TGL_10") :wind ,*wind-color-scale* "[km/hr]")
    ("sfcwind1" "Wind at 40m" ("UGRD_TGL_40" "VGRD_TGL_40") :wind ,*wind-color-scale* "[km/hr]")
    ("sfcwind2" "Wind at 80m" ("UGRD_TGL_80" "VGRD_TGL_80") :wind ,*wind-color-scale* "[km/hr]")
    ("sfcwind3" "Wind at 120m" ("UGRD_TGL_120" "VGRD_TGL_120") :wind ,*wind-color-scale* "[km/hr]")
    ("wind500"  "Wind at 500m" ("UGRD_ISBL_0950" "VGRD_ISBL_0950") :wind ,*wind-color-scale* "[km/hr]")
    ("wind1000" "Wind at 1000m" ("UGRD_ISBL_0900" "VGRD_ISBL_0900") :wind ,*wind-color-scale* "[km/hr]")
    ("wind1500" "Wind at 1500m" ("UGRD_ISBL_0850" "VGRD_ISBL_0850") :wind ,*wind-color-scale* "[km/hr]")
    ("wind2000" "Wind at 2000m" ("UGRD_ISBL_0800" "VGRD_ISBL_0800") :wind ,*wind-color-scale* "[km/hr]")
    ("wind2500" "Wind at 2500m" ("UGRD_ISBL_0750" "VGRD_ISBL_0750") :wind ,*wind-color-scale* "[km/hr]")
    ("wind3000" "Wind at 3000m" ("UGRD_ISBL_0700" "VGRD_ISBL_0700") :wind ,*wind-color-scale* "[km/hr]")
    ("wind3600" "Wind at 3600m" ("UGRD_ISBL_0650" "VGRD_ISBL_0650") :wind ,*wind-color-scale* "[km/hr]")
    ("wind4200" "Wind at 4200m" ("UGRD_ISBL_0600" "VGRD_ISBL_0600") :wind ,*wind-color-scale* "[km/hr]")
    ,@(if (string= *model* "hrdps") `(("vwind0" "Vertical component of wind at 230m" "VVEL_ISBL_1000" :mag ,*vvel-color-scale* "[m/s]" ,(lambda (x) (* x -0.0865)))))
    ("vwind1" "Vertical component of wind at 1600m" "VVEL_ISBL_0850" :mag ,*vvel-color-scale* "[m/s]" ,(lambda (x) (* -0.0865 x)))
    ("vwind2" "Vertical component of wind at 3100m" "VVEL_ISBL_0700" :mag ,*vvel-color-scale* "[m/s]" ,(lambda (x) (* -0.0865 x)))
    ("cloud" "Total Cloud Cover" "TCDC_SFC_0" :mag ,*cloud-color-scale* "[%]")
    ,@(if (string= *model* "hrdps") `(("CAPE" "Convective Available Potential Energy (CAPE)" "CAPE_ETAL_10000" :mag ,*cape-color-scale* "[J/kg]")))
    ("hwcritagl" "Top of Lift above ground level" "HCRIT_SFC_0" :mag ,*hcrit-color-scale* "[m AGL]")
    ("wstar" "Thermal Updraft Velocity Average" "WSTAR_0" :mag ,*thermal-color-scale* "[m/s]")
    ("bldepth" "Boundary layer height above ground level" "BLDEPTH_0" :mag ,*hcrit-color-scale* "[m AGL]")
    ("sfcshf" "Upward Sensible Surface Heat Flux" "SHTFL_SFC_0" :mag ,*surface-heating-color-scale* "[W/m^2]")
    ("sfctemp" "Surface temperature (2m AGL)" "TMP_TGL_2" :mag ,*surface-temperature-color-scale* "[C]" ;; ,(lambda (x) (print x) (- x 273.15) (break))
	       )
    ("sfcdewpt" "Surface Dew Point Depression (2m AGL)" "DEPR_TGL_2" :mag ,*surface-dewpoint-depression-color-scale* "[C]" ;; ,(lambda (x) (- x 273.15))
		)
    ("terrain" "Terrain Height" "HGT_SFC_0" :mag ,*terrain-color-scale* "m")
    ("sfcpres" "Pressure MSL (mbar)" "PRMSL_MSL_0" :mag ,*surface-pressure-color-scale* "mbar" ,(lambda (x) (/ x 100.0)))
    ("rain" "Rain (mm/hr)" "PRATE_SFC_0" :mag ,*rain-color-scale* "mm/hr" ,(lambda (x) (* x 3600.0))) ;; kg m^2/s, water density is 1 g/cm^3
    ))

(load "model-parameters.lisp")

(defun make-tile-directories (yyyy mm dd)
  (let ((tile-iterator (tile-iterator)))
    (iter (for tile = (funcall tile-iterator))
	  (while tile)
	  (for directory = (format nil "~A/~A/~A/" (base-directory) (directory-from-tile-info tile) (directory-from-date yyyy mm dd)))
	  (ensure-directories-exist directory))))

(defmacro with-drawable-image ((draw-func image y-size x-size &key debug (channels 4) (bits 8)) &body body)
  (let ((debug-gensym (when debug (gensym)))
	(displaced (gensym))
	(size (gensym))
	(offset (gensym)))
    `(let* ((,image (png::make-image ,y-size ,x-size ,channels ,bits))
	    (,displaced (array-displacement ,image))
	    (,size (* ,channels ,x-size))
	    ,@(when debug (list (list debug-gensym 0)))) ;; our output image
       (declare (type (array (unsigned-byte ,bits) (* * ,channels)) ,image)
		(type (simple-array (unsigned-byte ,bits) (*)) ,displaced))
       (labels (,(if (= channels 4)
		     `(,draw-func (x y red green blue) ;; 2.7 seconds
				  (declare (type (integer -1000000 100000000) x y))
				  (declare (type (unsigned-byte ,bits) red green blue))
				  (let ((,offset (+ (* ,channels x) (* ,size y))))
				    (declare (type (integer 0 100000000000) ,offset))
				    (setf (aref ,displaced (+ 0 ,offset)) red)
				    (setf (aref ,displaced (+ 1 ,offset)) green)
				    (setf (aref ,displaced (+ 2 ,offset)) blue)
				    (setf (aref ,displaced (+ 3 ,offset)) 255)
				    ,@(when debug
					    `((assert (= (aref ,image y x 0) red))
					      (assert (= (aref ,image y x 1) green))
					      (assert (= (aref ,image y x 2) blue))
					      (assert (= (aref ,image y x 3) 255))
					      (incf ,debug-gensym)
					      (when (= (mod ,debug-gensym 100) 0)
						(format t "~A: ~A,~A -> ~A, ~A, ~A~%" ,debug-gensym y x red green blue)
						(sleep 0.01))))
				    (values)))
		     `(,draw-func (x y gray)
				  (declare (type (integer -1000000 100000000) x y))
				  (declare (type (unsigned-byte ,bits) gray))
				  (setf (aref ,displaced (+ x (* ,size y))) gray)
				  (values))))
	 (declare (inline ,draw-func))
	 ,@body))))

(deftype image-size () '(integer 0 10000000000))

(defun chunk-image (image chunk-iterator &optional (filename-generator (lambda (lat-lon-info)
									 (format nil "test-stuff-~A.png" lat-lon-info))))
  (declare (optimize (speed 3)))
  (declare (type png::rgba-image image)
	   (type function chunk-iterator filename-generator))
  (assert (typep image 'png::rgba-image))
  (let* ((image-height (png::image-height image))
	 (image-width (png::image-width image))
	 (displaced-image (array-displacement image)))
    (declare (type image-size image-height image-width)
	     (type (simple-array (unsigned-byte 8) (*)) displaced-image))
    (macrolet ((image! (y x band)
		 `(aref displaced-image (the image-size (+ (the (integer 0 4) ,band)
							   (* 4 (the image-size ,x))
							   (* 4 image-width (the image-size ,y)))))))
      (labels ((do-chunk (x-pixel-min x-pixel-max y-pixel-min y-pixel-max)
		 (declare (type fixnum x-pixel-max x-pixel-min y-pixel-max y-pixel-min))
		 (with-drawable-image (draw output-image (the image-size (- y-pixel-max y-pixel-min)) (the image-size (- x-pixel-max x-pixel-min)))
		   (loop
		      for x-target fixnum from 0
		      for x-source fixnum from x-pixel-min below x-pixel-max
		      do
			(when (and (>= x-source 0) (< x-source image-width)) ;; why do i have to check?
			  (loop
			     for y-target fixnum from 0
			     for y-source fixnum from y-pixel-min below y-pixel-max
			     do
			       (when (and (>= y-source 0) (< y-source image-height)) ;; why do i have to check?
				 (when (= (image! y-source x-source 3) 255)
				   ;; (assert (= (image! y-source x-source 0) (aref image y-source x-source 0)))
				   ;; (assert (= (image! y-source x-source 1) (aref image y-source x-source 1)))
				   ;; (assert (= (image! y-source x-source 2) (aref image y-source x-source 2)))
				   ;; (loop for band below 4 do
				   ;; 	(setf (aref output-image y-target x-target band)
				   ;; 	      (image! y-source x-source band)
				   ;; 	      ))
				   (draw x-target y-target (image! y-source x-source 0) (image! y-source x-source 1) (image! y-source x-source 2))
				   )))))
		   output-image)))
	(iter (for (values chunk chunk-lat-lon) = (funcall chunk-iterator))
	      (while chunk)
	      (destructuring-bind (x-pixel-min y-pixel-max x-pixel-max y-pixel-min) (mapcar #'round chunk) ;; ugh, will get this straight one day
		(for filename = (funcall filename-generator chunk-lat-lon))
		(for image = (do-chunk x-pixel-min x-pixel-max y-pixel-min y-pixel-max))
		(png::encode-file image filename)))))))

(defun tile-bounds-to-pixel-bounds (tile inverter)
  (when tile
    (destructuring-bind (a b c d) tile
      (let ((res (append (funcall inverter a b)
			 (funcall inverter c d))))
	res))))

;; do this once...
(defun calculate-real-tile-locations (input-file output-stream)
  (declare (optimize (speed 3)))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let ((filename-warped (tmp-filename "blarg" "grib2")))
    (warp-to-google-map-tif input-file filename-warped)
    (cl-gdal::with-gdal-file (handle filename-warped)
      (let ((tile-iterator (tile-iterator))) ;; gives me lat lon bounds
	(multiple-value-bind (lon-lat-to-pixel pixel-to-lon-lat)
	    (cl-gdal::generate-lat-lon-to-pixel-transform handle)
	  (declare (type function tile-iterator lon-lat-to-pixel pixel-to-lon-lat))
	  (let ((outputlats (make-hash-table))
		(outputlons (make-hash-table)))
	    (labels ((set-check (hash key new-value)
		       (let ((value (gethash key hash)))
			 (if value
			     (assert (= value new-value))
			     (setf (gethash key hash) new-value))))
		     (set-lat (key new-value)
		       (set-check outputlats key new-value))
		     (set-lon (key new-value)
		       (set-check outputlons key new-value)))
	      (loop :for tile = (funcall tile-iterator)
		 :while tile
		 :do
		 (destructuring-bind (a b) (apply pixel-to-lon-lat (funcall lon-lat-to-pixel (first tile) (second tile)))
		   (destructuring-bind (c d) (apply pixel-to-lon-lat (funcall lon-lat-to-pixel (third tile) (fourth tile)))
		     (format t "~A:~A ~A:~A -> ~A:~A ~A:~A~%" (first tile) (second tile) (third tile) (fourth tile)  a b c d)
		     (set-lon (first tile) a)
		     (set-lat (second tile) b)
		     (set-lon (third tile) c)
		     (set-lat (fourth tile) d))))
	      (iter (for hash in (list outputlats outputlons))
		    (for var in '("realtilelats" "realtilelons"))
		    (format output-stream "~A = {~%" var)
		    (iter (for (k v) in-hashtable hash)
			  (when (not (first-time-p)) (format output-stream ",~%"))
			  (format output-stream "\"~,d\" : ~,8f" k v))
		    (format output-stream "};~%")))))))))

    ;; Our chunk-image function goes from x-pixel-min to below x-pixel-max, same for y, so the lat/lon bounds we
;; derive above are truly the upper left and lower right.

(defun draw-magnitude (file color-scale filename &optional (scale #'identity))
  (declare (optimize (speed 3)))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let ((filename-warped (tmp-filename "blarg" "grib2")))
    (warp-to-google-map-tif file filename-warped)
    (cl-gdal::with-gdal-file (handle filename-warped) ;; macroize this section of reading data... with-data-from-file
      (let* ((data (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band handle 1)))
	     (y-size (car (array-dimensions data)))
	     (x-size (cadr (array-dimensions data))))
	(declare (type (simple-array double-float (* *)) data)
		 (type (integer 0 10000000) x-size y-size))
	(with-drawable-image (draw image y-size x-size)
	  (loop for x fixnum below x-size do
	       (loop for y fixnum below y-size do
		    (let ((d (aref data y x)))
		      (when (not (= d +dst-no-data-double+))
			(destructuring-bind (red green blue)
			    (lookup-in-color-scale (funcall scale (coerce d 'single-float)) color-scale)
			  (draw x y red green blue))))))
	  (let ((tile-iterator (tile-iterator)) ;; gives me lat lon bounds
		(lon-lat-to-pixel-transformer (cl-gdal::generate-lat-lon-to-pixel-transform handle)))
	    (declare (type function tile-iterator lon-lat-to-pixel-transformer))
	    (chunk-image
	     image
	     (lambda () (let ((tile (funcall tile-iterator)))
			  (declare (type list tile))
			  (values (tile-bounds-to-pixel-bounds tile lon-lat-to-pixel-transformer)
				  tile)))
	     (lambda (lat-lon-info)
	       (format nil "~A~A~A" (base-directory) (directory-from-tile-info lat-lon-info) filename)))))))
    (rm-file filename-warped)
    t))

;; Simplest way to break it up is to write the whole thing after generating the scaled up image
;; it won't be the fastest, but whatever.  Let's do it.

(defun tile-id (lon lat)
  (let* ((lon (read-from-string lon))
	 (lat (read-from-string lat))
	 (latq (* *xstep* (floor (/ lat *xstep*))))
	 (lonq (* *ystep* (floor (/ lon *ystep*)))))
    (assert (< lonq lon (+ *ystep* lonq)))
    (assert (< latq lat (+ *xstep* latq)))
    (format nil "~A:~A:~A:~A" latq (+ *xstep* latq) lonq (+ *ystep* lonq))))

;; echo Using domain continental
;;     XVALS=($(seq -150 2 -50))
;;     YVALS=($(seq 37 2 70))



 ;;A chunk-generator gives me null or a list of ulx uly lrx lry bounds

(defun combine-winds-warp-and-return-new-files (ugrib vgrib)
  (let ((both-grib-rotated (tmp-filename "both-rotated" "grib2"))
	(ugrib-rotated (tmp-filename "ugrd-rotated" "grib2"))
	(vgrib-rotated (tmp-filename "vgrd-rotated" "grib2"))
	(utif-rotated (tmp-filename "ugrd-rotated" "tif"))
	(vtif-rotated (tmp-filename "vgrd-rotated" "tif")))
    (print/run-program "./combine-and-rotate-winds.sh" (list ugrib vgrib both-grib-rotated))
    (print/run-program "/usr/bin/wgrib2" (list both-grib-rotated "-match" "^(1):" "-grib" ugrib-rotated))
    (print/run-program "/usr/bin/wgrib2" (list both-grib-rotated "-match" "^(2):" "-grib" vgrib-rotated))
    (print/run-program "/usr/local/bin/gdalwarp" (list "-overwrite" "-r" "average" "-t_srs" "EPSG:3857"  ;; bilinear
						       "-dstnodata" (format nil "~A" +dst-no-data-double+) "-of" "GTiff" "-wo" "SAMPLE_GRID=YES"
						       "-wo" "SOURCE_EXTRA=1000" "-wo" "SAMPLE_STEP=100" ugrib-rotated utif-rotated))
    (print/run-program "/usr/local/bin/gdalwarp" (list "-overwrite" "-r" "average" "-t_srs" "EPSG:3857" "-of" "GTiff" "-wo" "SAMPLE_GRID=YES"
						       "-dstnodata" (format nil "~A" +dst-no-data-double+) ;; bilinear
						       "-wo" "SOURCE_EXTRA=1000" "-wo" "SAMPLE_STEP=100" vgrib-rotated vtif-rotated))
    ;; not sure bilinear is worth it, another second... but better than nothing.
    (rm-file both-grib-rotated)
    (rm-file ugrib-rotated)
    (rm-file vgrib-rotated)
    (list utif-rotated vtif-rotated)))

(defun prepare-files-for-draw-winds (ufile vfile)
  (combine-winds-warp-and-return-new-files ufile vfile))

;; Say this takes 100 seconds.  We want to add three wind levels (up to 4500m), so 13 wind levels
;; 48 hours = (* 13 48) -> 624 files * 100 seconds.  (* 624 100) 62400 seconds.  Now assuming
;; full 16x parallelization and 2x faster CPU -> (/ 62400 32) -> 1950 seconds or 32 minutes.
;; Cutting down to 24 hours (only daylight hours) gives us 16 minutes.

(let ((wind-levels 13)
      (hours-for-two-days 48)
      (time-per-file 80)
      (parallelization 16)
      (cpu-factor 2))
  (* wind-levels hours-for-two-days time-per-file (/ parallelization) (/ cpu-factor) (/ 3600) 60.0))
;; 26 minutes for winds

;; There are 10 other levels, each takes 4 seconds or so, so... another couple minutes.

(defun draw-winds** (ufile-input vfile-input filename vector-output-filename &key (color-scale *wind-color-scale*))
  (declare (optimize (speed 3)))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (destructuring-bind (ufile vfile)
      (prepare-files-for-draw-winds ufile-input vfile-input)
    (let (uband vband (y-size 0) (x-size 0) lon-lat-to-pixel-transformer)
      (declare (type image-size x-size y-size))
      (cl-gdal::with-gdal-file (uhandle ufile)
	(cl-gdal::with-gdal-file (vhandle vfile)
	  (setf uband (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band uhandle 1))) ;; u is an eastward wind
	  (setf vband (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band vhandle 1))) ;; v is a northward wind
	  (setf y-size (car (array-dimensions uband)))
	  (setf x-size (cadr (array-dimensions uband)))
	  (setf lon-lat-to-pixel-transformer (cl-gdal::generate-lat-lon-to-pixel-transform uhandle))))
      (labels ((calculate-magnitude (u v) ;; data is in m/s
		 (declare (type double-float u v)
			  (inline sb-kernel::float-nan-p))
		 (if (or (sb-kernel::float-nan-p u) (= u +dst-no-data-double+))
		     +dst-no-data-single+
		     (* 3.6f0 (coerce (sqrt (+ (* u u) (* v v))) 'single-float)))))
	(declare (type (simple-array double-float (* *)) uband vband))
	;;(format t "~Ax~A~%" x-size y-size)
	(let ((tile-iterator (tile-iterator)))
	  (declare (type function tile-iterator))
	  (loop for tile  = (funcall tile-iterator)
	     while tile
	     do
	       (destructuring-bind (x-pixel-min y-pixel-max x-pixel-max y-pixel-min)
		   (tile-bounds-to-pixel-bounds tile lon-lat-to-pixel-transformer)
		 (declare (type fixnum x-pixel-max y-pixel-min x-pixel-min y-pixel-max))
		 ;;(format t "~A: ~A->~A, ~A->~A~%" tile x-pixel-min x-pixel-max y-pixel-min y-pixel-max)
		 (setf x-pixel-min (max x-pixel-min 0))
		 (setf x-pixel-max (min x-pixel-max (1- x-size)))
		 (setf y-pixel-max (min y-pixel-max (1- y-size)))
		 (setf y-pixel-min (max y-pixel-min 0))
		 ;;(format t "~A: ~A->~A, ~A->~A~%" tile x-pixel-min x-pixel-max y-pixel-min y-pixel-max)
		 (let ((new-y-size (the image-size (- y-pixel-max y-pixel-min)))
		       (new-x-size (the image-size (- x-pixel-max x-pixel-min))))
		   (declare (type image-size new-x-size new-y-size x-pixel-min x-pixel-max y-pixel-min y-pixel-max))
		   (with-drawable-image (draw image new-y-size new-x-size)
		     (with-drawable-image (draw-vector image-vector new-y-size new-x-size :channels 1 :bits 8)
		       ;;(format t "New image: ~Ax~A~%" new-x-size new-y-size)
		       (loop for y fixnum from y-pixel-min below y-pixel-max
			    for y-real fixnum from 0
			  do
			    (loop for x fixnum from x-pixel-min below x-pixel-max
				 for x-real fixnum from 0
			       do
				 (let* ((u (aref uband y x))
					(v (aref vband y x))
					(mag (calculate-magnitude u v)))
				   (declare (type double-float u v))
				   (when (not (= mag +dst-no-data-single+))
				     (destructuring-bind (red green blue)
					 (lookup-in-color-scale mag color-scale)
				       (draw x-real y-real red green blue)
				       (let ((angle (- (float (atan v u) 0f0)))) ;; negative because y goes down the image
					 ;; (when (= x-real 0)
					 ;;   (format t "angle is ~A -> ~A~%" angle (round (+ 3145 (* angle 1000f0))))
					 ;;   (sleep 0.1))
					 (draw-vector x-real y-real (1+ (round (+ (* pi 37f0) (* angle 37f0))))))))))) ;; -pi to pi, (* 2 pi 37f0)
		     (png::encode-file image (format nil "~A/~A/~A" (base-directory) (directory-from-tile-info tile) filename))
		     (png::encode-file image-vector (format nil "~A/~A/~A" (base-directory) (directory-from-tile-info tile) vector-output-filename))
		     ))))))
    (rm-file ufile)
    (rm-file vfile)))))

(defun test-alpha-png ()
  (let ((image (png::make-image  128 128 4 8)))
    (iter (for x below 128)
	  (iter (for y below 128)
		(setf (aref image x y 0) x)
		(setf (aref image x y 1) y)
		(setf (aref image x y 3) x)))
    (png::encode-file image "testfile.png")))

	     
;; Looks like the geometry I want is an OGR Line String

;; OK, let's just write a 

;; use epsg 3857 for what I want.
;; SUPER IMPORTANT BELOW!!!
  ;; need to rotate the wind to N/S?  Or do i..... see below
  ;; cat CMC_hrdps_west_UGRD_TGL_80_ps2.5km_2018081018_P041-00.grib2 CMC_hrdps_west_VGRD_TGL_80_ps2.5km_2018081018_P041-00.grib2  > input.grib2
  ;; wgrib2 input.grib2 -set_grib_type s -new_grid_winds earth -new_grid_interpolation neighbor -new_grid nps:247.000000:60.000000 230.093688:685:2500.000000 44.689624:485:2500.000000  output.grib2 ;; 200 ms on my pc, no problem... only wind files
  ;; (with-output-from-program (grid-defn "/home/ajb/canadarasp/continental-test/grid_defn.pl" '("/home/ajb/canadarasp/continental-test/input.grib"))
  ;;   (with-output-from-program (var "/usr/bin/wgrib2" (append (list "/home/ajb/canadarasp/continental-test/input.grib2" "-set_grib_type" "s" "-new_grid_winds" "earth" "-new_grid_interpolation" "neighbor" "-new_grid") (cl-ppcre:split " " grid-defn) (list "output.grib2")))
  ;;     var))
  ;; gdalwarp -t_srs 'EPSG:4326' input.grib2 -of GTiff test4.tif
  ;; gdal_translate -of png -ot Byte -scale -10 10 1 255 -a_srs EPSG:4326 test4.tif test.png    ## could do -b 1 for n/s?, -b 2 e/w?
;; could draw the arrows without rotating wind into a png and then call
;; gdalwarp with the right input projection and output projection and bang!
;; it would rotate the arrows too...  sweeeet.

;; So, here's the plan.  We open the GRIB2 file, write out a GeoTIFF with the
;; new U/V data but 10x the resolution?

;; <defs>
;;     <marker id='head' orient='auto' markerWidth='2' markerHeight='4'
;;             refX='0.1' refY='2'>
;;       <path d='M0,0 V4 L2,2 Z' fill='red' />
;;     </marker>
;;   </defs>    
;;   <path
;;     marker-end='url(#head)'
;;     stroke-width='5' fill='none' stroke='black'  
;;     d='M0,0 C45,45 45,-45 90,0'
;;     />    




;; At this point, since I have the geotiff generation in hand, why not work on the javascript visualization instead?
;; I can do the background colors on the server, that's easy, but streamlines and the like seem hard?  Or not...
;; let me just do it and then I can convert to using d3.  Or, I can write it using javascript on the server and then
;; it would be easier to move it to the client, but I hate developing in javascript.  It's more fun to use common
;; lisp.

;; OK, let's just do it.  Let's see how to get wind arrows into the system so I can move to using the continental
;; HRDPS.  Then if I want I can expand to other weather systems, but really it's about presenting data in a useful
;; manner for soaring pilots.  Not eye candy.

;; so svg, or raster for the wind arrows... gdal makes it easy I think to handle vector data, so if I project it, then it should work and will be easy. Let's start by just making u arrows and seeing what happens.

;; The following works (with or without concatenating the two files
;; gdalwarp -overwrite -t_srs "EPSG:3857" -of GTiff CMC_hrdps_west_UGRD_TGL_120_ps2.5km_2018081118_P001-00.grib2 blarg.tif
;; gdal_translate -of png -ot Byte -scale -10 10 1 255 blarg.tif blarg.png

;; Also works if I go to grid_winds.


;; Now, below is a start on how I am going to automatically clip out the tiles.

;; from osgeo import osr, gdal

;; # get the existing coordinate system
;; ds = gdal.Open('path/to/file')
;; old_cs= osr.SpatialReference()
;; old_cs.ImportFromWkt(ds.GetProjectionRef())

;; # create the new coordinate system
;; wgs84_wkt = """
;; GEOGCS["WGS 84",
;;     DATUM["WGS_1984",
;;         SPHEROID["WGS 84",6378137,298.257223563,
;;             AUTHORITY["EPSG","7030"]],
;;         AUTHORITY["EPSG","6326"]],
;;     PRIMEM["Greenwich",0,
;;         AUTHORITY["EPSG","8901"]],
;;     UNIT["degree",0.01745329251994328,
;;         AUTHORITY["EPSG","9122"]],
;;     AUTHORITY["EPSG","4326"]]"""
;; new_cs = osr.SpatialReference()
;; new_cs .ImportFromWkt(wgs84_wkt) ;; this gives me lat/lon .. i will need to convert from new to old

;; # create a transform object to convert between coordinate systems
;; transform = osr.CoordinateTransformation(old_cs,new_cs) 

;; #get the point to transform, pixel (0,0) in this case
;; width = ds.RasterXSize
;; height = ds.RasterYSize
;; gt = ds.GetGeoTransform()
;; minx = gt[0]
;; miny = gt[3] + width*gt[4] + height*gt[5] 

;; #get the coordinates in lat long
;; latlong = transform.TransformPoint(x,y) 
    
#|
Trying some things:

TEST1: This cases gdalwarp to output a truncated file, though the corners look ok.

cat CMC_hrdps_continental_UGRD_TGL_120_ps2.5km_2018081206_P001-00.grib2 CMC_hrdps_continental_VGRD_TGL_120_ps2.5km_2018081206_P001-00.grib2 > input.grib2
wgrib2 input.grib2 -set_grib_type s -new_grid_winds earth -new_grid_interpolation neighbor -new_grid nps:252.000000:60.000000 231.9186:2576:2500.000000 35.6073:1456:2500.000000 output.grib2 # here I updated these based on the HRDPS continental page, otherwise small floating point errors 231.9186 = 360 - 128.0813 W

/usr/local/bin/gdalwarp -overwrite -t_srs "EPSG:3857" -of "GTiff" output.grib2 test.tiff
gdal_translate -of png -ot Byte -scale -10 10 1 255 test.tiff test.png
eog test.png

TEST2:

cat CMC_hrdps_continental_UGRD_TGL_120_ps2.5km_2018081206_P001-00.grib2 CMC_hrdps_continental_VGRD_TGL_120_ps2.5km_2018081206_P001-00.grib2 > input.grib2
wgrib2 input.grib2 -set_grib_type jpeg -new_grid_winds earth -new_grid_interpolation neighbor -new_grid nps:252.000000:60.000000 231.9186:2576:2500.000000 35.6073:1456:2500.000000 output.grib2 # here I updated these based on the HRDPS continental page, otherwise small floating point errors 231.9186 = 360 - 128.0813 W

/usr/local/bin/gdalwarp -overwrite -t_srs "EPSG:3857" -of "GTiff" output.grib2 test.tiff
gdal_translate -of png -ot Byte -scale -10 10 1 255 test.tiff test.png
eog test.png

Doesn't work, tried a bunch of stuff.  UGH.  So, let's split the files before warping?

|#


(defun display-file (file &optional (min "-10") (max "10"))
  (print/run-program "/usr/local/bin/gdal_translate" (list "-of" "png" "-ot" "Byte" "-scale" min max "1" "255" "-b" "1" file "/tmp/test.png"))
  (print/run-program "/usr/bin/eog" (list "/tmp/test.png")))

(defparameter *forecast-init-year* (parse-integer (or (sb-posix:getenv "YEAR") "2018")))
(defparameter *forecast-init-month* (parse-integer (or (sb-posix:getenv "MONTH") "9")))
(defparameter *forecast-init-day* (parse-integer (or (sb-posix:getenv "DAY") "17")))
(defparameter *forecast-init-hour* (parse-integer (or (sb-posix:getenv "HOUR") "6")))


(defun do-it-if-necessary (forecast-hour &key (forecast-init-year *forecast-init-year*) (forecast-init-month *forecast-init-month*) (forecast-init-day *forecast-init-day*) (forecast-init-hour *forecast-init-hour*)
					   (only-generate-header-footer nil) (params-and-names *params-and-names*))
  (local-time::reread-timezone-repository)
  (let* ((t0 (local-time:encode-timestamp 0 0 0 forecast-init-hour forecast-init-day forecast-init-month forecast-init-year :timezone local-time:+gmt-zone+))
	 (forecast-valid-time (local-time:timestamp+ t0 forecast-hour :hour))
	 (timezone (local-time:find-timezone-by-location-name "America/Vancouver"))
	 (hourpt (local-time:timestamp-hour forecast-valid-time :timezone timezone))
	 (daypt (local-time:timestamp-day forecast-valid-time :timezone timezone))
	 (monthpt (local-time:timestamp-month forecast-valid-time :timezone timezone))
	 (yearpt (local-time:timestamp-year forecast-valid-time :timezone timezone)))
    (make-tile-directories yearpt monthpt daypt)
    (labels ((gen-input-filename/s (filelabel/s)
	       (labels ((d (filelabel)
			  (format nil "~A/~A_~A~A~A~2,'0d~2,'0d~2,'0d_P~3,'0d~A"
				  *directory* *fileheader* (translate-from-hrdps-names-to-current-model filelabel) *resolution* forecast-init-year forecast-init-month forecast-init-day forecast-init-hour 
                                  (if (string= filelabel "HGT_SFC_0") 0 forecast-hour)
                                  *tail*)))
		 (if (listp filelabel/s) (mapcar #'d filelabel/s) (d filelabel/s))))
	     (output-file-name (param &optional (tag "body"))
	       (format nil "~A/~A_~A-~2,'0d-~2,'0d_~2,'0d00.~A.png" (directory-from-date yearpt monthpt daypt)
		       param yearpt monthpt daypt hourpt tag))
	     (vector-output-file-name (param)
	       (format nil "~A/~A_~A-~2,'0d-~2,'0d_~2,'0d00.vector.png" (directory-from-date yearpt monthpt daypt)
		       param yearpt monthpt daypt hourpt))
	     (header/footer-file-name (param tag)
	       (format nil "~A/~A_~2,'0d-~2,'0d-~2,'0d.~A.png" (base-directory) param yearpt monthpt daypt tag))
	     (handle (param full-name  filelabel/s type color-scale units scale)
	       (cond
		 (only-generate-header-footer
		  (draw-color-scale color-scale (print (header/footer-file-name param "foot"))  :units units :process (lambda (x) (format nil "~,1f" x)))
		  (write-default-title full-name forecast-init-year forecast-init-month forecast-init-day forecast-init-hour
				       (print (header/footer-file-name param "head"))))
		 (t
		  (let ((input-filename/s (gen-input-filename/s filelabel/s))
			(output-filename (output-file-name param)))
		    (ecase type
		      (:wind
		       (let ((output-vector-filename (vector-output-file-name param)))
			 (draw-winds** (car input-filename/s) (cadr input-filename/s) output-filename output-vector-filename :color-scale color-scale)))
		      (:mag
		       (draw-magnitude input-filename/s color-scale output-filename scale))))))))
	  (when (or (not (string= *model* "hrdps")) (<= 4 hourpt 22)) ;; 4 instead of 7 because of the 3 hour change to eastern canada!
	    (let ((*unique-identifier* forecast-hour))
	      (map nil (lambda (x)
			 (destructuring-bind (param full-name filelabel type color-scale units &optional (scale #'identity)) x
			   (ignore-errors (handle param full-name filelabel type color-scale units scale)))) params-and-names))))))
  
(if (string= (cadr *posix-argv*) "--only-generate-header-footer")
    (map nil (lambda (hour) (do-it-if-necessary hour :only-generate-header-footer t)) (iter (for hour from *timestart* to *timestop* by 8) (collect hour)))
    (map nil (lambda (hour) (do-it-if-necessary (parse-integer hour))) (cdr *posix-argv*)))
