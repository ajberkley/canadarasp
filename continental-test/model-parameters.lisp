(defparameter *isbl-levels* '(1015 1000 985 970 950 925 900 875 850 800 750 700 650 600 550)) ;; same GDPS, RDPS, HRDPS
(defparameter *date* (concatenate 'string (sb-posix:getenv "YEAR") (sb-posix:getenv "MONTH") (sb-posix:getenv "DAY")))
(defparameter *model* (or (sb-posix:getenv "MODEL") "hrdps"))
(defparameter *forecast-zero-hour* (parse-integer (or (sb-posix:getenv "HOUR") "0")))
(defparameter *fileheader* (or (sb-posix:getenv "FILEHEADER") "CMC_hrdps_continental"))
(defparameter *directory* (or (sb-posix:getenv "OUTPUTDIR") "/mnt/input/hrdps"))
(defparameter *timestart* (parse-integer (or (sb-posix:getenv "TIMESTART") "0")))
(defparameter *timestep* (parse-integer (or (sb-posix:getenv "TIMESTEP") "1")))
(defparameter *timestop* (parse-integer (or (sb-posix:getenv "TIMESTOP") "48")))
(defparameter *tail* (or (sb-posix:getenv "TAIL") "-00.grib2"))
(defparameter *resolution* (or (sb-posix:getenv "RESOLUTION") "_ps2.5km_"))
(defparameter *tiledir* (or (sb-posix:getenv "TILEDIR") "/mnt/windgram-tiles/hrdps"))
(defparameter *pngdir* (or (sb-posix:getenv "PNGDIR") "/mnt/map-pngs/hrdps"))

(defparameter *ulx* (parse-integer (or (sb-posix:getenv "XMIN") "-152")))
(defparameter *lrx* (parse-integer (or (sb-posix:getenv "XMAX") "-42")))
(defparameter *uly* (parse-integer (or (sb-posix:getenv "YMAX") "71")))
(defparameter *lry* (parse-integer (or (sb-posix:getenv "YMIN") "27")))

(defparameter *xstep* (parse-integer (or (sb-posix:getenv "XSTEP") "2")))
(defparameter *ystep* (parse-integer (or (sb-posix:getenv "YSTEP") "2")))

(defun file-glob-for (year month day hour h)
  (if (member *model* '("hrdps" "gdps" "rdps") :test #'string=)
      (format nil "*~A~A~A~A_P0~A*.grib2" year month day hour h)
      (format nil "*~A~A~AT~AZ_P0~A*.grib2" year month day hour h)))

(defun time-string (init-year init-month init-day init-hour hour)
  (format nil "~A~2,'0d~2,'0d~A~2,'0d~A_P~3,'0d"
          init-year init-month init-day (if (string= *model* "hrdps_west")
                                            "T" "")
          init-hour (if (string= *model* "hrdps_west")
                        "Z" "")
          hour))

(defun time-string* (date forecast-zero-hour forecast-hour)
  (if (string= *model* "hrdps_west")
      (format nil "~AT~2,'0dZ_P~3,'0d" date forecast-zero-hour forecast-hour)
      (format nil "~A~2,'0d_P~3,'0d" date forecast-zero-hour forecast-hour)))

(defun translate-from-hrdps-names-to-current-model (name)
  "The GDPS and RDPS use UGRD_ISBL_950 instead of UGRD_ISBL_0950"
  (if (and (find *model* '("rdps" "gdps") :test #'string=) (> (length name) 9) (string= (subseq name 5 9) "ISBL"))
      (destructuring-bind (grd isbl num) (cl-ppcre:split #\_ name)
	(format nil "~A_~A_~A" grd isbl (parse-integer num)))
      name))

(defun filename (directory fileheader filelabel resolution init-year init-month init-day init-hour hour tail)
  (format nil "~A/~A_~A~A~A~A"
          directory fileheader (translate-from-hrdps-names-to-current-model filelabel) resolution
          (time-string init-year init-month init-day init-hour hour)
          tail))

(defun input-file (variable &key level isbl-level forecast-hour
					    (date *date*) (forecast-zero-hour *forecast-zero-hour*))
  (assert (numberp forecast-zero-hour))
  (assert (numberp forecast-hour))
  (let* ((header (format nil "~A/~A" *directory* *fileheader*)))
    (cond
      (level (format nil "~A_~A_~A~A~A~A"
		     header variable level *resolution* (time-string* date forecast-zero-hour forecast-hour) *tail*))
      (isbl-level
       (if (member *model* '("gdps" "rdps") :test #'string=)
	   (format nil "~A_~A_~3,'0D~A~A~A"
		   header variable isbl-level *resolution* (time-string* date forecast-zero-hour forecast-hour) *tail*)
	   (format nil "~A_~A_~4,'0D~A~A~A"
		   header variable isbl-level *resolution* (time-string* date forecast-zero-hour forecast-hour) *tail*)))
      (t (error "unknown format type")))))

(defun tile-iterator ()
  (let* ((ulx *ulx*)
	 (uly *uly*)
	 (lrx *lrx*)
	 (lry *lry*)
	 (minx ulx)
	 (maxx lrx)
	 (miny lry)
	 (maxy uly)
	 (x ulx)
	 (y (- uly *ystep*)))
    (lambda ()
      (if (and (<= minx x (- maxx *xstep*)) (<= miny y (- maxy *ystep*)))
	  (prog1
	      (list x y (+ x *xstep*) (+ y *ystep*))
	    (incf x *xstep*)
	    (when (> x (- maxx *xstep*))
	      (decf y *ystep*)
	      (setf x minx)))
	  nil))))

(defun tile-id (lon lat)
  (let* ((lon (read-from-string lon))
         (lat (read-from-string lat))
         (latq (* *xstep* (floor (/ lat *xstep*))))
         (lonq (* *ystep* (floor (/ lon *ystep*)))))
    (assert (<= lonq lon (+ *ystep* lonq)))
    (assert (<= latq lat (+ *xstep* latq)))
    (values (format nil "~A:~A:~A:~A" latq (+ *xstep* latq) lonq (+ *ystep* lonq))
	(list latq lonq (+ *xstep* latq) (+ *ystep* lonq)))))

;; Geographic corners (lon . lat) of the rotated 1km-west grid, in order, from
;; un-rotating the GRIB2 grid (confirmed against wgrib2 -grid / gdalwarp). The data
;; footprint is a tilted quad, NOT the axis-aligned XMIN..XMAX/YMIN..YMAX box: its
;; corners stick out past the data. A tile box landing entirely outside this quad has
;; zero grid points, and wgrib2 -small_grib then returns the FULL grid (~200MB), which
;; is what filled the disk.
(defparameter *hrdps-west-footprint*
  '((-126.254d0 . 45.933d0)    ; SW
    (-109.533d0 . 50.067d0)    ; SE
    (-114.450d0 . 60.295d0)    ; NE
    (-134.630d0 . 55.117d0)))  ; NW

(defun point-in-convex-polygon-p (x y polygon)
  "T if (x,y) is inside the convex POLYGON (ordered list of (x . y) vertices)."
  (let ((n (length polygon)) (sign 0))
    (dotimes (i n t)
      (destructuring-bind (x1 . y1) (nth i polygon)
	(destructuring-bind (x2 . y2) (nth (mod (1+ i) n) polygon)
	  (let ((cross (- (* (- x2 x1) (- y y1)) (* (- y2 y1) (- x x1)))))
	    (cond ((> cross 0) (if (minusp sign) (return nil) (setf sign 1)))
		  ((< cross 0) (if (plusp sign)  (return nil) (setf sign -1))))))))))

(defun site-in-domain-p (latitude longitude)
  "Is a site inside the model's actual data coverage? hrdps_west uses the real tilted
   1km footprint (so we never tile a box with no grid points); other models use the
   axis-aligned tiling box, which already covers all their sites."
  (if (string= *model* "hrdps_west")
      (point-in-convex-polygon-p longitude latitude *hrdps-west-footprint*)
      (and (<= *lry* latitude *uly*) (<= *ulx* longitude *lrx*))))
