;; (load "~/quicklisp/setup.lisp")
(ql:quickload "hunchentoot")
(ql:quickload "cl-ppcre")
(ql:quickload "parse-float")
(ql:quickload "local-time")

(defvar *timezone-lookup-process* nil)

(defun initialize-timezone-process ()
  (let* ((process (sb-ext:run-program "/usr/bin/java" '("-cp" "/home/ubuntu/canadarasp/web-server/timezone/timezone-lookup/src/main/java/:/home/ubuntu/canadarasp/web-server/timezone" "timezone") :wait nil :input :stream :output :stream)))
    (when process
      (let ((in (sb-ext:process-output process))
	    (out (sb-ext:process-input process)))
	(read-line in)
	(read-line in)
	(format out "45.0 -123.0~%")
	(finish-output out)
	(read-line in)
	(format t "Done initializing~%")
	(setf *timezone-lookup-process* process)))))

(defun kill-timezone-process ()
  (when *timezone-lookup-process*
    (sb-ext:process-kill *timezone-lookup-process* 9)
    (sb-ext:process-wait *timezone-lookup-process* t)
    (sb-ext:process-exit-code *timezone-lookup-process*)
    (sb-ext:process-close *timezone-lookup-process*)
    (setf *timezone-lookup-process* nil)))

(defun lookup (lat lon)
  (let ((in (sb-ext:process-output *timezone-lookup-process*))
	(out (sb-ext:process-input *timezone-lookup-process*)))
    (format out "~f~%~f~%" lat lon)
    (finish-output out)
    (read-line in)))

(defun lookup-string (string)
  (let ((in (sb-ext:process-output *timezone-lookup-process*))
	(out (sb-ext:process-input *timezone-lookup-process*)))
    (write-line string)
    (finish-output out)
    (read-line in)))

;;For now we use hunchentoot.  Probably want to upgrade to woo / clack

(hunchentoot:define-easy-handler (timezone :uri "/timezone2") (location timestamp)
  (setf (hunchentoot:content-type*) "application/json")
  (handler-case
      (progn
	(cond
	  ((> (length location) 100) "{\"status\":\"INVALID_REQUEST\", \"errorMessage\":\"Invalid location\"}")
	  ((> (length timestamp) 100) "{\"status\":\"INVALID_REQUEST\", \"errorMessage\":\"Invalid timestring\"}")
	  (t
	   ;; (hunchentoot:log-message* :info (format nil "~A" location))
	   ;; (hunchentoot:log-message* :info (format nil "~A" timestamp))
	   (sb-ext:with-timeout 2.0
	     (let* ((timestamp (if timestamp (local-time:universal-to-timestamp (parse-integer timestamp)) (local-time:now)))
		    (timezone (apply #'lookup (mapcar #'parse-float:parse-float (cl-ppcre:split "," location))))
		    (timezone-parsed (local-time:find-timezone-by-location-name timezone)))
	       (multiple-value-bind
		     (offset dst)
		   (local-time:timestamp-subtimezone timestamp timezone-parsed)
		 (when dst
		   (incf offset 3600))
		 (if (string= timezone "null")
		     (format nil "{\"status\":\"ZERO_RESULTS\"}")
		     (format nil "{\"status\":\"OK\",\"rawOffset\":~d,\"dstOffset\":~d,\"timeZoneId\":\"~A\" }"
			     offset (if dst -3600 0) timezone))))))))
    (timeout ()
      (hunchentoot:log-message* :error "Timeout~%")
      (kill-timezone-process)
      (initialize-timezone-process)
      (format nil "{\"status\":\"INVALID_REQUEST\"}"))
    (error (e)
      (prog1
	  (format nil "{\"status\":\"INVALID_REQUEST\"}")
	(hunchentoot:log-message* :error (format nil "~A" e))
	(ignore-errors
	  (when (not (string= (lookup 49.0 -123.0) "America/Vancouver"))
	    (kill-timezone-process)
	    (initialize-timezone-process)))))))

(defvar *acceptor* nil)

(defun start-web-server ()
  (initialize-timezone-process)
  (local-time:reread-timezone-repository)
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8082 :access-log-destination nil))))

(start-web-server)
(loop :while (string= (lookup 49.0 -123.0) "America/Vancouver")
   :do (sleep 5))

