;#!/usr/local/bin/sbcl --script

;; TODO: clean up float-nan-p and +dst-no-data-single+ +dst-no-data-double+
;; TODO: regenerate real tile boundaries
;; TODO: clean up unused functions
;; TODO: clean up start-up / require, move the actual work functions to a package instead of in this script

(load "~/quicklisp/setup.lisp")

(pushnew (make-pathname :directory "home/ubuntu/canadarasp/continental-test/") asdf:*central-registry*)

(defun quicklisp-setup ()
  (ql:quickload "iterate")
  (ql:quickload "alexandria")
  (ql:quickload "trivial-garbage")
  (ql:quickload "cl-gd")
  (ql:quickload "local-time")
  (ql:quickload "cl-ppcre")
  "Quicklisp setup done")

(require "iterate")
(use-package :iterate)
(require 'cl-gdal-local)
(use-package :cl-gdal)
(use-package :cl-ogr)
(require 'png)
(require 'local-time)
(require 'cl-ppcre)
(require 'cl-gd)

(sb-posix:setenv "GDFONTPATH" "/usr/share/fonts/truetype/msttcorefonts/" 1)
;; sudo apt-get install libgd2-dev before you (ql:quickload "cl-gd")
;; and you might have to instal the msttcorefonts too
(load "/home/ubuntu/canadarasp/continental-test/utils.lisp")
(load "/home/ubuntu/canadarasp/continental-test/model-parameters.lisp") ;; our config file

(deftype image-size () '(integer 0 100000))

;; Add these to model-parameters.lisp...
(declaim (type image-size *output-xsize* *output-ysize*))
(defvar *output-xsize* (if (string= *model* "gdps") 1650 3348))
(defvar *output-ysize* (if (string= *model* "gdps") 1100 2162))

(defun base-directory ()
  (format nil "~A/" *pngdir*))

(defun directory-from-tile-info (lat-lon-info)
  (format nil "~A:~A:~A:~A/" 
	  (first lat-lon-info) (third lat-lon-info) (second lat-lon-info) (fourth lat-lon-info)))

(defun directory-from-date (yyyy mm dd &optional (hh nil))
  (if hh
    (format nil "~A-~2,'0d-~2,'0d_~2,'0d/" yyyy mm dd hh)
    (format nil "~A-~2,'0d-~2,'0d/" yyyy mm dd)))

;; Color scale stuff.  Pull me out to a small library

(deftype rgb-color-type ()
  '(simple-array (unsigned-byte 8) (3)))

(defstruct (color-scale-entry)
  (value 0f0 :type single-float)
  (color #(0 0 0) :type rgb-color-type))

(defun make-color-scale (color-scale-info)
  (map 'vector (lambda (info)
                 (destructuring-bind (value color) info
                   (make-color-scale-entry :value (coerce value 'single-float)
                                           :color (make-array 3 :element-type '(unsigned-byte 8)
                                                              :initial-contents color))))
       color-scale-info))

(defparameter *wind-color-scale*
  (make-color-scale
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
     (50f0 (0 0 255)))))

(defparameter *normalized-color-scale*
  (make-color-scale
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
     (1.00f0 (90 0 90)))))

(defun double-color-scale (color-scale)
  (merge 'simple-vector
         color-scale
         (iter
           (for cse in-sequence color-scale)
           (for pcse previous cse)
           (when pcse
             (collect (make-color-scale-entry :value (/ (+ (color-scale-entry-value cse) (color-scale-entry-value pcse)) 2.0) :color (map 'rgb-color-type (lambda (a b) (round (/ (+ a b) 2.0))) (color-scale-entry-color cse) (color-scale-entry-color pcse))))))
         (lambda (x y) (< (color-scale-entry-value x) (color-scale-entry-value y)))))
      
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

;; (declaim (inline lookup-in-color-scale))
;; (defun lookup-in-color-scale (value color-scale)
;;   (declare (optimize (speed 3)))
;;   (declare (type single-float value))
;;   (or (iter (for c in color-scale)
;; 	    (for pc previous c initially (car c))
;; 	    (until (>= (the single-float (car c)) value))
;; 	    (finally (return (cadr pc))))
;;       (cadar color-scale)))

(declaim (inline lookup-in-color-scale))
(defun lookup-in-color-scale (value color-scale)
  (declare (optimize (speed 3)))
  (declare (type single-float value)
           (type simple-vector color-scale))
  ;; simple binsearch is probably faster
  (let* ((previous (aref color-scale 0)))
    (declare (type color-scale-entry previous))
    (loop
       for c of-type color-scale-entry across color-scale
       while (> value (the single-float (color-scale-entry-value c)))
       do (setf previous c)
       finally (return (color-scale-entry-color previous)))))

(defun make-basic-color-scale (min max)
  (make-color-scale
   (iter (for value from min by (/ (- max min) (1- (length *normalized-color-scale*))))
         (for c in-vector *normalized-color-scale*)
         (collect (list value (color-scale-entry-color c))))))
	
(defparameter *cloud-color-scale*  (make-basic-color-scale 0f0 100f0))
(defparameter *vvel-color-scale*  (make-basic-color-scale -1.0f0 1.0f0))
(defparameter *thermal-color-scale*  (make-basic-color-scale -0.5f0 5.5f0))
(defparameter *cape-color-scale*  (make-basic-color-scale 0f0 5000f0))
(defparameter *hcrit-color-scale*  (make-basic-color-scale 0f0 4000f0))
(defparameter *surface-heating-color-scale*  (make-basic-color-scale 0f0 1000f0))
(defparameter *surface-temperature-color-scale* (make-basic-color-scale -4f0 36f0))
(defparameter *surface-dewpoint-depression-color-scale* (make-basic-color-scale -4f0 4f0))
(defparameter *surface-pressure-color-scale* (make-basic-color-scale 980f0 1050f0))
(defparameter *terrain-color-scale* (make-basic-color-scale 0f0 3000f0))
(defparameter *rain-color-scale* (make-basic-color-scale 0f0 20f0))

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

;; cl-gd helper functions

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
	  (colors (iter (for c in-vector color-scale)
			(collect (list (color-scale-entry-value c)
                                       (let ((color (color-scale-entry-color c)))
				       (cl-gd:allocate-color (aref color 0) (aref color 1) (aref color 2))))))))
      (cl-gd:with-default-color (black)
	(iter
	  (with step = (/ (- width (* 2 border)) (1- (length color-scale))))
	  (for x1 from border by step)
	  (for (label1 color) in (butlast colors))
	  (cl-gd:draw-rectangle* (round x1) 2 (round (+ x1 step)) (- height 18)
				 :filled t :color color)
	  (cl-gd:draw-line  (round x1) 2 (round x1) (- height 18))
	  (write-text (if (first-time-p) border (round x1))
		      (- height 16) (format nil format (funcall process label1))
		      :align '(:top :centerx)))
	(cl-gd:draw-line  (- width border) 2 (- width border) (- height 18))
	(cl-gd:draw-line  border (- height 18) (- width border) (- height 18))
	(cl-gd:draw-line border 2 (- width border) 2)
	(write-text  (- width border) (- height 16)
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

(defconstant +dst-no-data-single+ 9999f0)
(defconstant +dst-no-data-double+ 9999d0)

(defparameter *tmp-fileindex* 0)

(defparameter *unique-identifier* "blarg")

(defun tmp-filename (&optional file (extension ""))
  (if file
    (format nil "/tmp/~A~A-~A-~A.~A" *unique-identifier* (incf *tmp-fileindex*) file (sxhash (list file *tmp-fileindex* extension)) extension)
    (format nil "/tmp/~A~A.tmp" *unique-identifier* (incf *tmp-fileindex*))))

(defun rm-file (filename)
  (print/run-program "/bin/rm" (list filename)))

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
    ("maxgust" "Max surface gust" "GUST_MAX_TGL_10" :mag ,*wind-color-scale* "[km/hr]" ,(lambda (x) (* 3.6 x)))
    ))

(defun make-tile-directories (yyyy mm dd initialized-yyyy initialized-mm initialized-dd initialized-hh)
  (let ((tile-iterator (tile-iterator)))
    (iter (for tile = (funcall tile-iterator))
	  (while tile)
	  (for directory = (format nil "~A/~A/~A/~A/" (base-directory) (directory-from-date initialized-yyyy initialized-mm initialized-dd initialized-hh)
				   (directory-from-date yyyy mm dd) (directory-from-tile-info tile)))
	  (ensure-directories-exist directory))))

(defmacro with-drawable-image ((draw-func image y-size x-size &key (channels 4) (bits 8)) &body body)
  ;; This belongs in cl-png probably
  `(let* ((,image (png::make-image ,y-size ,x-size ,channels ,bits))) ;; our output image
     (declare (type (simple-array (unsigned-byte ,bits) (* * ,channels)) ,image))
     (labels (,(if (= channels 4)
                   `(,draw-func (y x red green blue)
                                (declare (type (unsigned-byte ,bits) red green blue))
                                (setf (aref ,image y x 0) red)
                                (setf (aref ,image y x 1) green)
                                (setf (aref ,image y x 2) blue)
                                (setf (aref ,image y x 3) 255)
                                (values))
                   `(,draw-func (y x gray)
                                (declare (type (unsigned-byte ,bits) gray))
                                (setf (aref ,image y x 0) gray)
                                (values))))
       (declare (inline ,draw-func))
       ,@body)))

(defun tile-bounds-to-pixel-bounds (tile inverter)
  (when tile
    (destructuring-bind (a b c d) tile
      (let ((res (append (funcall inverter a b)
			 (funcall inverter c d))))
	res))))

;; Our chunk-image function goes from x-pixel-min to below x-pixel-max, same for y, so the lat/lon bounds we
;; derive above are truly the upper left and lower right.

(defmacro destructuring-vector-bind (bindings array &body body)
  (let ((arr (gensym)))
    `(let* ((,arr ,array)
           ,@(iter (for x in bindings)
                   (for y from 0)
                   (collect (list x `(aref ,arr ,y)))))
       ,@body)))

(defun warp-to-google-map-tif (filename-unwarped filename-warped)
  ;; Data is stored at doubles, so I should be able to recognize this
  (print/run-program "/usr/local/bin/gdalwarp" ;; 100 ms
                     (list "-overwrite" "-dstnodata" (format nil "~A" +dst-no-data-double+)
                           "-t_srs" "EPSG:3857" "-of" "GTiff"
                           filename-unwarped filename-warped)))

(defun calculate-real-tile-locations (input-file output-stream)
  (declare (optimize (speed 3)))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let ((filename-warped (tmp-filename "blarg" "grib2")))
    (warp-to-google-map-tif input-file filename-warped)
    (cl-gdal::with-gdal-file (handle filename-warped)
      (let ((tile-iterator (tile-iterator))) ;; gives me lat lon bounds
       (multiple-value-bind (lon-lat-to-pixel pixel-to-lon-lat)
           (cl-gdal::generate-lon-lat-to-pixel-transform handle)
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

(defun draw-magnitude-chunked-nn (file color-scale output-directory filename &optional (scale #'identity))
  (declare (optimize (speed 3) (safety 0)))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let ((scale (coerce scale 'function)))
    (declare (type function scale))
    (cl-gdal::with-gdal-file (handle file)
      (let* ((data (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band handle 1)))
             (x-size *output-xsize*);;3348);; input data is 2576 ;; dlon is 0.02 degrees which is 1.4 km at the south of Canada and 2.2 km at the equator
             (y-size *output-ysize*);; 2162);; input data is 1456 ;; dlat is 0.03 degrees, which is 3.3 km km at the northern most part of Canada, and 2.2 km at the south of Canada.
             (ulx (float *ulx* 0d0))
             (uly (float *uly* 0d0))
             (dlon (float (abs (/ (- *lrx* ulx) x-size)) 0d0))
             (dlat (float (abs (/ (- *lry* uly) y-size)) 0d0))
             (data-ysize (car (array-dimensions data)))
             (data-xsize (cadr (array-dimensions data)))
             (source-spatial-reference (cl-gdal::gdalGetProjectionRef handle))
             (source-geo-transform (get-geo-transform handle))
             (tile-iterator (tile-iterator)))
	(declare (type (simple-array double-float (* *)) data) (type function tile-iterator) (type image-size x-size y-size))
        ;; TODO choose dlon dlat to be close to exact for the chunking...  that is a rational number (say 0.02 for dlon and 0.025 for dlat), then derive x-size and y-size from that.
        (cached-transforms:with-cached-transform (xpixelref ypixelref nil ulx uly dlon dlat x-size y-size data-xsize data-ysize source-spatial-reference source-geo-transform)
          (labels ((lon-to-x (lon)
                     (declare (type (integer -360 360) lon))
                     (/ (- lon ulx) dlon)) 
                   (lat-to-y (lat)
                     (declare (type (integer -360 360) lat))
                     (/ (- uly lat) dlat)))
            (loop
              for tile = (funcall tile-iterator)
              while tile
              do
                 (destructuring-bind (lon-min lat-min lon-max lat-max)
                     tile
                   (let* ((y-min (round (lat-to-y lat-max))) ;; upper left hand corner source y pixel 
                          (x-min (round (lon-to-x lon-min))) ;; upper left hand corner source x pixel
                          (chunk-y-size (round (the (double-float 0d0 360d0) (- (lat-to-y lat-min) y-min))))
                          (chunk-x-size (round (the (double-float 0d0 360d0) (- (lon-to-x lon-max) x-min)))))
                     (declare (type image-size chunk-x-size chunk-y-size y-min x-min))
                     (with-drawable-image (draw image chunk-y-size chunk-x-size)
                       (loop for chunk-y-pixel fixnum from 0 below chunk-y-size
                             for source-y-pixel fixnum from y-min
                             do
                                (loop for chunk-x-pixel fixnum from 0 below chunk-x-size
                                      for source-x-pixel fixnum from x-min
                                      do
                                         (let* ((idx (+ (the fixnum (* x-size source-y-pixel)) source-x-pixel))
                                                (xpixel (xpixelref idx))
                                                (ypixel (ypixelref idx)))
                                           (declare (type (unsigned-byte 16) xpixel ypixel data-xsize data-ysize)
                                                    (type fixnum idx))
                                           (unless (or (= cl-gdal::+out-of-source-range+ xpixel) (= cl-gdal::+out-of-source-range+ ypixel))
                                             ;;(format t "LON ~A LAT ~A -> x ~A y ~A~%" (+ *ulx* (* x dlon)) (- *uly* (* y dlat)) xpixel ypixel)
                                             (let ((d (aref data ypixel xpixel)))
                                               (unless (= d +dst-no-data-double+)
                                                 (let ((color (lookup-in-color-scale (funcall scale (coerce d 'single-float)) color-scale)))
                                                   (declare (type (simple-array (unsigned-byte 8) (3)) color))
                                                   (destructuring-vector-bind (red green blue)
                                                       color
                                                     (draw chunk-y-pixel chunk-x-pixel red green blue)))))))))
                       (png::encode-file image
                                         (format nil "~A/~A/~A" output-directory (directory-from-tile-info tile) filename))))))))))))

(defun draw-magnitude-nn (file color-scale &optional (scale #'identity))
  "Draw one big output file given the GRIB2 input file FILE with the tile boundaries drawn on it"
  (declare (optimize (debug 3);; (speed 3) (safety 0)
                     ))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let ((scale (coerce scale 'function)))
    (declare (type function scale))
    (cl-gdal::with-gdal-file (handle file)
      (let* ((data (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band handle 1)))
             (x-size *output-xsize*);;3348);; input data is 2576 ;; dlon is 0.02 degrees which is 1.4 km at the south of Canada and 2.2 km at the equator
             (y-size *output-ysize*);; 2162);; input data is 1456 ;; dlat is 0.03 degrees, which is 3.3 km km at the northern most part of Canada, and 2.2 km at the south of Canada.
             (ulx (float *ulx* 0d0))
             (uly (float *uly* 0d0))
             (dlon (float (abs (/ (- *lrx* ulx) x-size)) 0d0))
             (dlat (float (abs (/ (- *lry* uly) y-size)) 0d0))
             (data-ysize (car (array-dimensions data)))
             (data-xsize (cadr (array-dimensions data)))
             (source-spatial-reference (cl-gdal::gdalGetProjectionRef handle))
             (source-geo-transform (get-geo-transform handle)))
	(declare (type (simple-array double-float (* *)) data))
        (cached-transforms:with-cached-transform (xpixelref ypixelref anglepixelref ulx uly dlon dlat x-size y-size data-xsize data-ysize source-spatial-reference source-geo-transform)
          (with-drawable-image (draw image y-size x-size)
            (format t "Converting lon lat pairs to pixels~%")
              ;; can make these loops a bit easier by not doing 2d loops...
            (format t "Drawing big image~%")
            (time
             (loop
               for x fixnum below x-size
               do (loop
                    for y fixnum below y-size
                    do
                       (let* ((idx (+ (* x-size y) x))
                              (xpixel (xpixelref idx))
                              (ypixel (ypixelref idx)))
                         (declare (type (unsigned-byte 16) xpixel ypixel data-xsize data-ysize))
                         (unless (or (= cl-gdal::+out-of-source-range+ xpixel) (= cl-gdal::+out-of-source-range+ ypixel))
                           (let ((d (aref data ypixel xpixel)))
                             (unless (= d 9999d0))
                               (let ((color (lookup-in-color-scale (funcall scale (coerce d 'single-float)) color-scale)))
                                 (declare (type (simple-array (unsigned-byte 8) (3)) color))
                                 (destructuring-vector-bind (red green blue)
                                     color
                                   (draw y x red green blue)))))))))
            (png::encode-file image "/tmp/new.png")
            (cl-gd:with-image-from-file* ("/tmp/new.png" :png)
              (cl-gd:with-default-color ((cl-gd:allocate-color 100 0 0))
                ;; (lon - ulx) / dlon  , (- uly lat) / dlat
                (let ((tile-iterator (alexandria:ensure-function (tile-iterator))))
                  (labels ((lon-lat-to-target-pixel (lon lat)
                             (declare (type fixnum lon lat))
                             (let ((lon (coerce lon 'double-float))
                                   (lat (coerce lat 'double-float))
                                   (res (list (round (the (double-float -1d9 1d9) (/ (- lon ulx) dlon))) (round (the (double-float -1d9 1d9) (/ (- uly lat) dlat))))))
                               (declare (type double-float lon lat))
                               (format t "~,3f ~,3f -> ~A~%" lon lat res)
                               res))
                           (mean (a b)
                             (/ (+ a b) 2)))
                  (declare (type function tile-iterator))
                  (iter 
                    (for tile = (funcall tile-iterator))
                    (while tile)
                    (destructuring-bind (x-pixel-min y-pixel-min x-pixel-max y-pixel-max)
                        (mapcar #'round
                                (tile-bounds-to-pixel-bounds tile #'lon-lat-to-target-pixel))
                      (cl-gd:draw-line x-pixel-min y-pixel-min x-pixel-max y-pixel-min)
                      (cl-gd:draw-line x-pixel-min y-pixel-min x-pixel-min y-pixel-max)
                      (cl-gd:draw-line x-pixel-max y-pixel-max x-pixel-max y-pixel-min)
                      (cl-gd:draw-line x-pixel-max y-pixel-max x-pixel-min y-pixel-max)
                      (let* ((mean-x-target (round (mean x-pixel-min x-pixel-max)))
                             (mean-y-target (round (mean y-pixel-min y-pixel-max)))
                             (height-target (abs (- y-pixel-max y-pixel-min)))
                             (width-target (abs (- x-pixel-max x-pixel-min)))
                             (angle-idx (+ mean-x-target (* mean-y-target x-size))))
                        (when (and (>= angle-idx 0) (< angle-idx (* x-size y-size)))
                             (let ((angle (anglepixelref angle-idx)))
                               (format t "box (~A ~A ~A ~A) is pixel (~A ~A): angle at idx ~A is ~A~%" x-pixel-min x-pixel-max y-pixel-min y-pixel-max mean-x-target mean-y-target angle-idx angle)
                               (cl-gd:draw-line mean-x-target mean-y-target (round (+ mean-x-target (* 0.5 width-target (cos angle)))) (round (+ mean-y-target (* 0.5 height-target (sin angle)))))))))))))
              (cl-gd:write-image-to-file "/tmp/new-annotated.png" :compression-level 6 :if-exists :supersede))))))
    t))

(declaim (inline encode-wind-angle-for-png))
(defun encode-wind-angle-for-png (angle)
  (declare (type single-float angle))
  (1+ (the (signed-byte 32) (round (the (float -300.0 300.0) (+ (* pi 37f0) (* angle 37f0)))))))

(declaim (inline encode-wind-angle-for-png-from-u-v-rot))

(defun encode-wind-angle-for-png-from-u-v-rot (u v rotation)
  (declare (type single-float rotation) (type double-float u v)
	   (optimize (speed 3)))
  (let ((angle (+ rotation (- (float (atan v u) 0f0))))) ;; negative because y goes down the image
    (declare (type single-float angle))
    (when (> angle pi) (decf angle (float (* 2 pi) 0f0))) ;; mod one period only (rotation is < 2 pi)
    (when (< angle (- pi)) (incf angle (float (* 2 pi) 0f0)))
    (encode-wind-angle-for-png angle)))

(declaim (inline encode-wind-magnitude-for-png))
(defun encode-wind-magnitude-for-png (u v)
  "u and v is in m/sec, so we scale by 3.6 to get to km/hr.  Returns a single float"
  (declare (type double-float u v)
	   (inline sb-kernel::float-nan-p))
  (if (or (sb-kernel::float-nan-p u) (= u +dst-no-data-double+))
      +dst-no-data-single+
      (* 3.6f0 (coerce (sqrt (+ (* u u) (* v v))) 'single-float))))

(defun draw-winds-nn (ufile vfile output-directory filename vector-output-filename &key (color-scale *wind-color-scale*) (scale #'identity))
  (declare (optimize (speed 3)) (type function scale))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let (uband vband source-spatial-reference source-geo-transform
              (data-ysize 0)
              (data-xsize 0) 
              (x-size *output-xsize*)
              (y-size *output-ysize*))
    (declare (type image-size data-xsize data-ysize x-size y-size))
    (cl-gdal::with-gdal-file (uhandle ufile)
      (cl-gdal::with-gdal-file (vhandle vfile)
        (setf uband (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band uhandle 1))) ;; u is an eastward wind
        (setf vband (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band vhandle 1))) ;; v is a northward wind
        (setf data-ysize (car (array-dimensions uband)))
        (setf data-xsize (cadr (array-dimensions uband)))
        (setf source-spatial-reference (cl-gdal::gdalGetProjectionRef uhandle))
        (setf source-geo-transform (get-geo-transform uhandle)))
      (let* ((ulx (float *ulx* 0d0))
             (uly (float *uly* 0d0))
             (good-data-points 0)
             (dlon (float (abs (/ (- *lrx* ulx) x-size)) 0d0))
             (dlat (float (abs (/ (- *lry* uly) y-size)) 0d0)))
	(declare (type (simple-array double-float (* *)) uband vband)
		 (type fixnum good-data-points))
	(cached-transforms:with-cached-transform (xpixelref ypixelref anglepixelref ulx uly dlon dlat x-size y-size data-xsize data-ysize source-spatial-reference source-geo-transform)
	  (with-drawable-image (draw image y-size x-size)
            (with-drawable-image (draw-vector image-vector y-size x-size :channels 1 :bits 8)
              (loop for x of-type image-size below x-size do
                (loop for y of-type image-size below y-size do
                  (let* ((idx (+ (* x-size y) x))
                         (source-y (ypixelref idx))
                         (source-x (xpixelref idx)))
                    (declare (type fixnum idx))
                    ;;(format t "At output ~A,~A maps to input ~A,~A~%" x y source-x source-y)
                    (unless (or (= source-y +out-of-source-range+) (= source-x +out-of-source-range+))
                      (let* ((rotation (anglepixelref idx))
                             (u (aref uband source-y source-x))
                             (v (aref vband source-y source-x))
                             (d (encode-wind-magnitude-for-png u v)))
                        (declare (type double-float u v) (type single-float d))
                        (when (not (= d +dst-no-data-single+))
                          (incf good-data-points)
                          (let ((color (lookup-in-color-scale (funcall scale d) color-scale)))
                            (declare (type (simple-array (unsigned-byte 8) (3)) color))
 
                            (destructuring-vector-bind (red green blue)
                                color
                              (draw y x red green blue)))
                          (draw-vector y x (encode-wind-angle-for-png-from-u-v-rot u v rotation))))))))
              (format t "Out of ~A total data points, ~A had data~%" (* x-size y-size) good-data-points)
              (png::encode-file image "/tmp/new-wind.png")
              (png::encode-file image-vector "/tmp/new-wind-vector.png"))))))))


(defun draw-winds-chunked-nn (ufile vfile output-directory filename vector-output-filename &key (color-scale *wind-color-scale*) (scale #'identity))
  (declare (optimize (speed 3)) (type function scale))
  (cl-gdal::maybe-initialize-gdal-ogr)
  (let (uband vband source-spatial-reference source-geo-transform
              (data-ysize 0)
              (data-xsize 0) 
              (x-size *output-xsize*)
              (y-size *output-ysize*)
              (tile-iterator (tile-iterator)))
    (declare (type image-size data-xsize data-ysize x-size y-size) (type function tile-iterator))
    (cl-gdal::with-gdal-file (uhandle ufile)
      (cl-gdal::with-gdal-file (vhandle vfile)
        (setf uband (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band uhandle 1))) ;; u is an eastward wind
        (setf vband (cl-gdal::gdal-read-all-data (cl-gdal::gdal-get-raster-band vhandle 1))) ;; v is a northward wind
        (setf data-ysize (car (array-dimensions uband)))
        (setf data-xsize (cadr (array-dimensions uband)))
        (setf source-spatial-reference (cl-gdal::gdalGetProjectionRef uhandle))
        (setf source-geo-transform (get-geo-transform uhandle)))
      (let* ((ulx (float *ulx* 0d0))
             (uly (float *uly* 0d0))
             (dlon (float (abs (/ (- *lrx* ulx) x-size)) 0d0))
             (dlat (float (abs (/ (- *lry* uly) y-size)) 0d0)))
        (declare (type (simple-array double-float (* *)) uband vband) (type double-float dlat dlon))
        (cached-transforms:with-cached-transform (xpixelref ypixelref anglepixelref ulx uly dlon dlat x-size y-size data-xsize data-ysize source-spatial-reference source-geo-transform)
          (labels ((lon-to-x (lon)
                     (declare (type (integer -360 360) lon))
                     (/ (- lon ulx) dlon)) 
                   (lat-to-y (lat)
                     (declare (type (integer -360 360) lat))
                     (/ (- uly lat) dlat)))
            (loop
              for tile = (funcall tile-iterator)
              while tile
              do
                 (destructuring-bind (lon-min lat-min lon-max lat-max)
                     tile
                   (let* ((y-min (round (the (double-float -10000d0 10000d0) (lat-to-y lat-max)))) ;; upper left hand corner source y pixel 
                          (x-min (round (the (double-float -10000d0 10000d0) (lon-to-x lon-min)))) ;; upper left hand corner source x pixel
                          (chunk-y-size (round (the (double-float 0d0 10000d0) (- (lat-to-y lat-min) y-min))))
                          (chunk-x-size (round (the (double-float 0d0 10000d0) (- (lon-to-x lon-max) x-min)))))
                     (declare (type image-size chunk-x-size chunk-y-size y-min x-min))
                     (with-drawable-image (draw image chunk-y-size chunk-x-size)
                       (with-drawable-image (draw-vector image-vector chunk-y-size chunk-x-size :channels 1 :bits 8)
                         (loop for chunk-y-pixel of-type image-size from 0 below chunk-y-size
                               for source-y-pixel of-type image-size from y-min
                               do
                                  (loop for chunk-x-pixel of-type image-size from 0 below chunk-x-size
                                        for source-x-pixel of-type image-size from x-min
                                        do
                                           (let* ((idx (+ (the fixnum (* x-size source-y-pixel)) source-x-pixel))
                                                  (source-x (xpixelref idx))
                                                  (source-y (ypixelref idx)))
                                             (declare (type fixnum idx))
                                             ;;(format t "At output ~A,~A maps to input ~A,~A~%" x y source-x source-y)
                                             (unless (or (= source-y +out-of-source-range+) (= source-x +out-of-source-range+))
                                               (let* ((rotation (anglepixelref idx))
                                                      (u (aref uband source-y source-x))
                                                      (v (aref vband source-y source-x))
                                                      (d (encode-wind-magnitude-for-png u v)))
                                                 (when (not (= d +dst-no-data-single+))
                                                   (let ((color (lookup-in-color-scale (funcall scale d) color-scale)))
                                                     (declare (type (simple-array (unsigned-byte 8) (3)) color))
                                                     (destructuring-vector-bind (red green blue)
                                                         color
                                                       (draw chunk-y-pixel chunk-x-pixel red green blue)))
                                                   (draw-vector chunk-y-pixel chunk-x-pixel (encode-wind-angle-for-png-from-u-v-rot u v rotation))))))))
                         (png::encode-file image (format nil "~A/~A/~A" output-directory (directory-from-tile-info tile) filename))
                         (png::encode-file image-vector (format nil "~A/~A/~A" output-directory (directory-from-tile-info tile) vector-output-filename)))))))))))))


(defun display-file (file &optional (min "-10") (max "10"))
  (print/run-program "/usr/local/bin/gdal_translate" (list "-of" "png" "-ot" "Byte" "-scale" min max "1" "255" "-b" "1" file "/tmp/test.png"))
  (print/run-program "/usr/bin/eog" (list "/tmp/test.png")))

(defparameter *forecast-init-year* (parse-integer (or (sb-posix:getenv "YEAR") "2020")))
(defparameter *forecast-init-month* (parse-integer (or (sb-posix:getenv "MONTH") "8")))
(defparameter *forecast-init-day* (parse-integer (or (sb-posix:getenv "DAY") "31")))
(defparameter *forecast-init-hour* (parse-integer (or (sb-posix:getenv "HOUR") "00")))

(defun do-it-if-necessary (forecast-hour &key (forecast-init-year *forecast-init-year*) (forecast-init-month *forecast-init-month*) (forecast-init-day *forecast-init-day*)
					   (forecast-init-hour *forecast-init-hour*) (only-generate-header-footer nil) (params-and-names *params-and-names*))
  (local-time::reread-timezone-repository)
  (let* ((t0 (local-time:encode-timestamp 0 0 0 forecast-init-hour forecast-init-day forecast-init-month forecast-init-year :timezone local-time:+gmt-zone+))
	 (forecast-valid-time (local-time:timestamp+ t0 forecast-hour :hour))
	 (timezone local-time:+utc-zone+) ;; (local-time:find-timezone-by-location-name "America/Vancouver")
	 (hourutc (local-time:timestamp-hour forecast-valid-time :timezone timezone))
	 (dayutc (local-time:timestamp-day forecast-valid-time :timezone timezone))
	 (monthutc (local-time:timestamp-month forecast-valid-time :timezone timezone))
	 (yearutc (local-time:timestamp-year forecast-valid-time :timezone timezone)))
    (make-tile-directories yearutc monthutc dayutc forecast-init-year forecast-init-month forecast-init-day forecast-init-hour)
    (labels ((gen-input-filename/s (filelabel/s)
	       (labels ((d (filelabel)
			  (format nil "~A/~A_~A~A~A~2,'0d~2,'0d~2,'0d_P~3,'0d~A"
				  *directory* *fileheader* (translate-from-hrdps-names-to-current-model filelabel) *resolution* forecast-init-year forecast-init-month forecast-init-day forecast-init-hour 
                                  (if (and (string= *model* "gdps") (string= filelabel "HGT_SFC_0")) 0 forecast-hour)
                                  *tail*)))
		 (if (listp filelabel/s) (mapcar #'d filelabel/s) (d filelabel/s))))
	     (output-directory ()
	       (format nil "~A/~A/~A/" (base-directory) (directory-from-date forecast-init-year forecast-init-month forecast-init-day forecast-init-hour) (directory-from-date yearutc monthutc dayutc)))
	     (output-file-name (param &optional (tag "body"))
	       (print (format nil "~A_~A-~2,'0d-~2,'0d_~2,'0d00.~A.png" param yearutc monthutc dayutc hourutc tag)))
	     (vector-output-file-name (param)
	       (output-file-name param "vector"))
	     (header/footer-directory ()
	       (format nil "~A/~A" (base-directory) (directory-from-date forecast-init-year forecast-init-month forecast-init-day forecast-init-hour)))
	     (header/footer-file-name (param tag)
	       (format nil "~A/~A_~2,'0d-~2,'0d-~2,'0d.~A.png" (header/footer-directory) param yearutc monthutc dayutc tag))
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
			 (draw-winds-chunked-nn (car input-filename/s) (cadr input-filename/s) (output-directory) output-filename output-vector-filename :color-scale color-scale)))
		      (:mag
		       (draw-magnitude-chunked-nn input-filename/s color-scale (output-directory) output-filename scale))))))))
	    (let ((*unique-identifier* forecast-hour))
	      (map nil (lambda (x)
			 (destructuring-bind (param full-name filelabel type color-scale units &optional (scale #'identity)) x
			   (ignore-errors (handle param full-name filelabel type color-scale units scale)))) params-and-names)))))
  
(if (string= (cadr *posix-argv*) "--only-generate-header-footer")
    (map nil (lambda (hour) (do-it-if-necessary hour :only-generate-header-footer t)) (remove-duplicates (append (list *timestop*) (iter (for hour from *timestart* to *timestop* by 8) (collect hour)))))
    (map nil (lambda (hour) (do-it-if-necessary (parse-integer hour))) (cdr *posix-argv*)))
