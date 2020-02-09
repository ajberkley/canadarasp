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
(defparameter *tiledir* (or (sb-posix:getenv "TILEDIR") "/mnt/tiles/hrdps"))
(defparameter *pngdir* (or (sb-posix:getenv "PNGDIR") "/mnt/tiles/hrdps"))

(defparameter *ulx* (parse-integer (or (sb-posix:getenv "XMIN") "-152")))
(defparameter *lrx* (parse-integer (or (sb-posix:getenv "XMAX") "-42")))
(defparameter *uly* (parse-integer (or (sb-posix:getenv "YMAX") "71")))
(defparameter *lry* (parse-integer (or (sb-posix:getenv "YMIN") "27")))

(defparameter *xstep* (parse-integer (or (sb-posix:getenv "XSTEP") "2")))
(defparameter *ystep* (parse-integer (or (sb-posix:getenv "YSTEP") "2")))

(defun file-glob-for (year month day hour h)
  (if (member *model* '("hrdps" "gdps" "rdps"))
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
      (format nil "~AT~2,'0dZ_P~3,'0d" date forecast-zero-hour forecast-hour))
      (format nil "~A~2,'0d_P~3,'0d" date forecast-zero-hour forecast-hour))
  
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

