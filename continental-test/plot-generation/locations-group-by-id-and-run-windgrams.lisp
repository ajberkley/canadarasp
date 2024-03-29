#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(require :cl-ppcre)
(require :iterate)
(use-package :cl-ppcre)
(use-package :iterate)
(load "../model-parameters.lisp")


(defun handle-locations (&optional (filename "/home/ubuntu/continental-test/plot-generation/locations.txt") (outputfilename "/home/ubuntu/continental-test/plot-generation/run-my-windgrams.sh"))
  (let ((result-hash (make-hash-table :test 'equalp))
	(*print-pretty* nil))
    (with-open-file (str filename :direction :input)
      (read-line str)
      (loop :for line = (read-line str nil nil)
	    :for index :from 0
	    :while line
	    :do
	       (destructuring-bind (region location lon lat &optional model)
		   (mapcar (lambda (x) (string-trim '(#\Space) x)) (cl-ppcre:split "," line))
		 (declare (ignorable region))
		 (when (or (not model) (string= model *model*))
		   (push (list :label-lat-lon (format nil "~A,~A,~A" location lon lat) :index index) (gethash (tile-id lon lat) result-hash))))))
    (with-open-file (out outputfilename :direction :output :if-exists :supersede)
      (iter (for (tile-id list-of-sites) in-hashtable result-hash)
	       (for labels_lats_lons = "")
	       (for outputfiles = "")
	       (iter (for site in list-of-sites)
		     (setf labels_lats_lons (concatenate 'string labels_lats_lons ";" (getf site :label-lat-lon)))
		     (setf outputfiles (concatenate 'string outputfiles (format nil "~A~Awindgram~A.png" (if (first-time-p) "" ";") *model* (getf site :index)))))
	       (when (not (string= labels_lats_lons ""))
		 (format out "$NCARG_ROOT/bin/ncl -n windgram-continental.ncl 'input_files=\"/mnt/windgram-tiles/~A/~A/~A_*.grib2\"' 'output_dir=\"/mnt/windgrams-data/\"' 'output_files=\"~A\"' 'labels_lats_lons=\"~A\"' 'model=\"~A\"'~%"
			 *model* tile-id *model* outputfiles labels_lats_lons *model*))))))

(handle-locations)

