#!/usr/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(load "../model-parameters.lisp")
(require :hunchentoot)
(require :cl-ppcre)
(require :cl-fad)
(require :iterate)
(require :local-time)
(require :parse-number)

(use-package :parse-number)
(use-package :cl-ppcre)
(use-package :iterate)
(use-package :hunchentoot)
(use-package :cl-fad)
(use-package :local-time)
(load "utils.lisp")

(defparameter *counter* 0)

(defun read-binary-file (file)
    (with-open-file (str file :element-type '(unsigned-byte 8) :direction :input)
      (let ((buffer (make-array (file-length str)
				:element-type '(unsigned-byte 8))))
	(read-sequence buffer str)
	buffer)))

(hunchentoot:define-easy-handler (windgram-handler :uri "/windgram") (lon lat date)
  (or (ignore-errors
	(setf (hunchentoot:content-type*) "image/png")
	(assert (and (<= (length lon) 100) (<= (length lat) 100)))
	(parse-real-number lon) ;; this will error if it fails
	(parse-real-number lat) ;; this will error if it fails
	(assert (< (length date) 11))
	(assert (every (lambda (x) (member x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-))) date))
	(generate-windgram lon lat :one-day-date date))
      (progn
	(setf (hunchentoot:content-type*) "text/plain")
	"Invalid location or input: please send an email to ajberkley@gmail.com to request this general area be added.  Generally anything within about 100 km of an existing windgram will work for now.  If you are generating a one day windgram (the default now), you want to choose a date and time in the date time picker so that the date is the desired date for the one day windgram in UTC time.  So, if you are on the west coast you want to choose a date / time that is at least 7 hours before the date of the target day.  So for example, date / times between 2019-10-01 17:00 and 2019-10-02 17:00 will generate a one day windgram for 2019-10-02.  Sorry for the confusion, I will fix this interface later.  If you want two day windgrams, select the checkbox at the bottom of the left hamburger menu. --ajb Oct 2019")))
  

(hunchentoot:define-easy-handler (default :uri (lambda (x) (not (string= "/windgram" (script-name* x))))) ()
   (setf (hunchentoot:content-type*) "text/plain")
   (format nil "Access denied"))

(defun start-webserver ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080)))

(defparameter *ncarg-root* (or (sb-posix:getenv "NCARG_ROOT") "/home/ubuntu/NCARG/"))

(defun find-latest-date (directory)
  (let ((files (cl-fad:list-directory directory)))
    (iter (for file in files)
          (destructuring-bind (date run)
	      (coerce (nth-value 1 (cl-ppcre:scan-to-strings "hrdps_(.*)-run([0-9]*)_P" (format nil "~A"file))) 'list)
	    (finding (list date run) maximizing (local-time:timestamp-to-universal (local-time:parse-timestring (format nil "~AT~A:00:00Z" date run))))))))

(defun generate-windgram (lon lat &key one-day-date)
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
				   (format nil "/mnt/windgrams-data/twoDay/~A" output-filename))))
	(ensure-directories-exist real-output-file)
	(if (cl-fad:file-exists-p real-output-file)
	    (progn (format t "File exists!~%")
		   (read-binary-file real-output-file))
	    (progn 
	      (print/run-program (format nil "~A/bin/ncl" *ncarg-root*)
				 (list "-n" "windgram-continental.ncl"
				       (format nil "input_files=\"/mnt/windgram-tiles/hrdps/~A/hrdps_~A-run~A_*.grib2\"" tile-id date run)
				       "output_dir=\"/mnt/windgrams-data/\""
				       "model=\"hrdps\""
				       (format nil "output_files=\"~A\"" output-filename)
                                       "numdays=-1"
				       (format nil "labels_lats_lons=\";~,4f ~,4f,~A,~A\"" (parse-real-number lat) (parse-real-number lon) lat lon)))
	      (read-binary-file real-output-file)))))))

(start-webserver)
(iter (sleep 3600))
