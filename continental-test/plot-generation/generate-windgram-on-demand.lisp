#!/usr/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(require :hunchentoot)
(require :cl-ppcre)
(require :cl-fad)
(require :iterate)
(require :parse-number)
(load "../model-parameters.lisp")
(require :local-time)
(use-package :parse-number)
(use-package :cl-ppcre)
(use-package :iterate)
(use-package :hunchentoot)
(use-package :cl-fad)
;;(use-package :local-time)
(load "utils.lisp")

(defparameter *counter* 0)

(defun read-binary-file (file)
    (with-open-file (str file :element-type '(unsigned-byte 8) :direction :input)
      (let ((buffer (make-array (file-length str)
				:element-type '(unsigned-byte 8))))
	(read-sequence buffer str)
	buffer)))

(defparameter *ncarg-root* (or (sb-posix:getenv "NCARG_ROOT") "/home/ubuntu/NCARG/"))

(defun find-latest-date (directory)
  (let ((files (cl-fad:list-directory directory)))
    (iter (for file in files)
          (destructuring-bind (date run)
	      (coerce (nth-value 1 (cl-ppcre:scan-to-strings "hrdps_(.*)-run([0-9]*)_P" (format nil "~A" file))) 'list)
	    (finding (list date run) maximizing (local-time:timestamp-to-universal (local-time:parse-timestring (format nil "~AT~A:00:00Z" date run))))))))

(defun generate-windgram (lon lat &key one-day-date just-image)
  ;; filenames are like: hrdps_continental_2018-09-02-run06_P017.grib2
  (sb-posix:setenv "TZ" "America/Vancouver" 1)
  (sb-posix:setenv "NCARG_ROOT" *ncarg-root* 1)
  ;; Need to choose the latest files... hrmph.  Scan through files, choose the latest date and the latest hour
  ;; then generate the input_files glob for that
  (let* ((tile-id (tile-id lat lon)))
    (destructuring-bind (date run)
	(find-latest-date (format nil "/mnt/windgram-tiles/hrdps/~A" tile-id))
      (format t "Initialized at ~A run hour ~A~%" date run)
      (let* ((output-filename (format nil "windgram-~A-~A-~A-~A.png" date run lon lat))
	     (real-output-file (if one-day-date
				   (format nil "/mnt/windgrams-data/oneDay/~A/~A" one-day-date output-filename)
				   (format nil "/mnt/windgrams-data/twoDay/~A" output-filename)))
             (output-href (if one-day-date
				   (format nil "/windgrams-data/oneDay/~A/~A" one-day-date output-filename)
				   (format nil "/windgrams-data/twoDay/~A" output-filename))))
	(ensure-directories-exist real-output-file)
	(if (cl-fad:file-exists-p real-output-file)
	    (format t "File exists!~%")
            (print/run-program (format nil "~A/bin/ncl" *ncarg-root*)
                               (list "-n" "windgram-continental.ncl"
                                     (format nil "input_files=\"/mnt/windgram-tiles/hrdps/~A/hrdps_~A-run~A_*.grib2\"" tile-id date run)
                                     "output_dir=\"/mnt/windgrams-data/\""
                                     "model=\"hrdps\""
                                     (format nil "output_files=\"~A\"" output-filename)
                                     "numdays=-1"
                                     (format nil "labels_lats_lons=\";~,4f ~,4f,~A,~A\"" (parse-real-number lat) (parse-real-number lon) lat lon))))
        (labels ((handle-failure (&optional two-day)
                   (if (and (not two-day) 
                            (let ((files (directory (format nil "/mnt/windgram-tiles/hrdps/~A/*.grib2" tile-id))))
                              (and files (not (zerop (nth-value 8 (sb-unix:unix-lstat (native-namestring (car files)))))))))
                       (format nil "<p style=\"color:white\">No data for ~A, try another day (change date/time selector or click above links) or try a two day windgram.</p>" one-day-date)
                       "<p style=\"color:white\">Invalid location (or data missing): please send an email to ajberkley@gmail.com to request this general area be added.  Generally anything within about 100 km of an existing windgram will work</p>")))
        (if (or (not one-day-date) just-image)
            (cond
              ((file-exists-p real-output-file)
               (setf (hunchentoot:content-type*) "image/png")
               (read-binary-file real-output-file))
              (t 
               (setf (hunchentoot:content-type*) "text/html")
               (format nil "<body style=\"background-color:black\">~A</body>" (handle-failure t))))
            (let* ((fmt '((:YEAR 4) "-" (:MONTH 2) "-" (:DAY 2)))
                   (daystamp (local-time:parse-timestring (format nil "~AT00:00:00Z" one-day-date)))
                   (day (local-time:format-timestring nil daystamp :format fmt))
                   (previousday (local-time:format-timestring nil (local-time:timestamp- daystamp 1 :day) :format fmt))
                   (nextday (local-time:format-timestring nil (local-time:timestamp+ daystamp 1 :day) :format fmt)))
              (setf (hunchentoot:content-type*) "text/html")
              (format nil
                      "<body style=\"background-color:black\">~
                  <center>~
                   <span style=\"align:left;float:left;width:10%\"><a href=\"/windgram?lon=~A&lat=~A&date=~A&interactive=t\">~A</a></span>~
                   <span style=\"align:center;width:80%\"><a href=\"/windgram?lon=~A&lat=~A&date=~A&interactive=t\">~A</a></span>~
                   <span style=\"align:right;float:right;width:10%\"><a href=\"/windgram?lon=~A&lat=~A&date=~A&interactive=t\">~A</a></span>~
                  </center><br>~
                  <center>~
                   ~A
                  </center>~
                 </body>"
                      lon lat previousday previousday lon lat day day lon lat nextday nextday
                      (if (file-exists-p real-output-file)
                          (format nil "<img src=\"~A\" height=95%></img>" output-href)
                          (handle-failure))))))))))

(hunchentoot:define-easy-handler (windgram-handler :uri "/windgram") (lon lat date interactive)
  (or (ignore-errors
	(assert (and (<= (length lon) 100) (<= (length lat) 100)))
	(parse-real-number lon) ;; this will error if it fails
	(parse-real-number lat) ;; this will error if it fails
	(assert (< (length date) 11))
	(assert (every (lambda (x) (member x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-))) date))
	(generate-windgram lon lat :one-day-date date :just-image (not interactive)))
      (progn
	(setf (hunchentoot:content-type*) "text/plain")
	"Invalid input or internal error: please send an email to ajberkley@gmail.com to report this failure.")))

(hunchentoot:define-easy-handler (default :uri (lambda (x) (not (string= "/windgram" (script-name* x))))) ()
   (setf (hunchentoot:content-type*) "text/plain")
   (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
   (format nil "Access denied"))

(defun start-webserver ()
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port 8090
		  :taskmaster (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
					     :max-accept-count 64 :max-thread-count 2))))

(start-webserver)
(iter (sleep 3600))
