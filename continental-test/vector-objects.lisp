(defpackage :vector-objects
  (:use :cl :iterate)
  (:documentation
   "I used this when I was doing server side drawing of wind arrows on canadarasp.com.  It turned
    out better to just do it in javascript client side (it wasn't as fast, but it cost less in AWS
    fees and I could do easier dynamic scaling, etc).
    For drawing on a PNG an arrow withx and y components u and v:
   (with-drawable-image (draw image y-size x-size)
     (draw-lines (manipulate-object* *prototype-arrow*
                                     :rotate (- (float (atan v u) 0f0))
                                     :scale (* scale (/ (- scale 1f0) scale))
                                     :translate (point (float (* scale (- x x-pixel-min -0.5)) 0f0)
                                                       (float (* scale (- y y-pixel-min -0.5)) 0f0)))
               (lambda (x y)
                 (declare (type (integer -100000 10000000) x y))
                 (when (and (>= x 0) (>= y 0)
                            (< x x-size) (< y y-size))
                   (draw x y 255 0 255))))")
  (:export
   #:manipulate-object*
   #:rotate-object
   #:translate-object
   #:scale-object
   #:*prototype-arrow*
   #:draw-lines
   #:point
   #:line))

(in-package :vector-objects)

;; just handling line drawing right now... could do more later, but will move
;; to vecto and cl-vectors if doing anything more complicated.

(deftype point () '(simple-array single-float (2)))

(declaim (inline x y line line-start line-end point))

(defun point (&rest rest)
  (make-array 2 :element-type 'single-float :initial-contents rest))

(deftype line () '(simple-array point (2)))

(defun line (&rest rest)
  (make-array 2 :element-type 'point :initial-contents rest))

(defun line-start (line)
  (declare (type line line))
  (aref line 0))

(defun line-end (line)
  (declare (type line line))
  (aref line 1))

(defun x (point)
  (declare (type point point))
  (aref point 0))

(defun y (point)
  (declare (type point point))
  (aref point 1))

(defparameter *prototype-arrow*
  (list (line (point -0.5 0.0) (point 0.5 0.0))
	(line (point 0.5 0.0) (point 0.35 0.2))
	(line (point 0.5 0.0) (point 0.35 -0.2))))

(deftype lines () 'list)

(declaim (inline point line sin cos rotate-object translate-object scale-object))

(defun rotate-object (lines angle)
  (declare (optimize (speed 3)))
  (declare (single-float angle)
	   (type lines lines))
  (labels ((rotate (point)
	     (declare (type point point))
	     (let* ((x (x point))
		    (y (y point)))
	       (point (- (* (cos angle) x) (* (sin angle) y))
		      (+ (* (sin angle) x) (* (cos angle) y))))))
    (declare (ftype (function (point) point) rotate)
	     (inline rotate))
    (mapcar (lambda (line)
	      (declare (type line line))
	      (line (rotate (line-start line)) (rotate (line-end line))))
	    lines)))

(defun translate-object (lines trans)
  (declare (optimize (speed 3))
	   (type lines lines)
	   (type point trans))
  (let ((x-shift (x trans))
	(y-shift (y trans)))
    (labels ((translate (point)
	       (declare (type point point))
	       (point (+ (x point) x-shift)
		      (+ (y point) y-shift))))
      (declare (ftype (function (point) point) translate)
	       (inline translate))
      (mapcar (lambda (line)
		(declare (type line line))
		(line (translate (line-start line)) (translate (line-end line))))
	      lines))))

(defun scale-object (lines scale)
  (declare (optimize (speed 3))
	   (type lines lines)
	   (single-float scale))
    (labels ((scale (point)
	       (declare (type point point))
	       (point (* (x point) scale)
		      (* (y point) scale))))
      (declare (ftype (function (point) point) scale)
	       (inline scale))
      (mapcar (lambda (line)
		(declare (type line line))
		(line (scale (line-start line)) (scale (line-end line))))
	      lines)))

(defmacro manipulate-object* (lines &rest commands)
  (let ((obj (gensym)))
    `(let ((,obj ,lines))
       ,@(iter (for command = (car commands))
	       (for argument = (cadr commands))
	       (setf commands (cddr commands))
	       (collect
		   (ecase command
		     (:translate
		      `(setf ,obj (translate-object ,obj ,argument)))
		     (:scale
		      `(setf ,obj (scale-object ,obj ,argument)))
		     (:rotate 
		      `(setf ,obj (rotate-object ,obj ,argument)))))
	       (while commands))
       ,obj)))
  
(defun manipulate-object (lines &rest commands)
  "(manipulate-object lines :rotate 3.1415 :translate (point 0.0 0.1) :rotate -1.2)"
  (declare (optimize (speed 3))
	   (type lines lines))
  (iter (for command = (car commands))
	(for argument = (cadr commands))
	(setf commands (cddr commands))
	(ecase command
	  (:translate
	   (setf lines (translate-object lines argument)))
	  (:scale
	   (setf lines (scale-object lines argument)))
	  (:rotate 
	   (setf lines (rotate-object lines argument))))
	(while commands))
  lines)

;; fast enough

(defun draw-line (line plot-func)
  "line should be in approximate pixel units"
  (declare (optimize (speed 3))
	   (type function plot-func)
	   (type (function (fixnum fixnum) (values)) plot-func))
  (labels ((plot (x y)
	     (funcall plot-func x y)
	     (values))
	   (plot-line-high (x0 y0 x1 y1)
	     (declare (type (single-float -9f9 9f9) x0 y0 x1 y1))
	     (let ((dx (- x1 x0))
		   (dy (- y1 y0))
		   (xi 1)
		   (x (round x0))
		   (D 0.0))
	       (declare (type (integer -99999999 999999999) xi x)
			(type (single-float -9f9 9f9) dx dy D x0))
	       (when (< dx 0.0)
		 (setf xi -1)
		 (setf dx (- dx)))
	       (setf D (- (* 2.0 dx) dy))
	       (loop :for y fixnum :from (round y0) :to (round y1)
		  :do (progn
			(plot x y)
			(when (> D 0.0)
			  (incf x xi)
			  (decf D (* 2.0 dy)))
			(incf D (* 2 dx))))))
	   (plot-line-low (x0 y0 x1 y1)
	     (declare (type (single-float -9f9 9f9) x0 y0 x1 y1))
	     (let ((dx (- x1 x0))
		   (dy (- y1 y0))
		   (yi 1)
		   (y (round y0))
		   (D 0.0))
	       (declare (type (integer -9999999 99999999) yi y)
			(type (single-float -9f9 9f9) dx dy D y0))
	       (when (< dy 0.0)
		 (setf yi -1)
		 (setf dy (- dy)))
	       (setf D (- (* 2.0 dy) dx))
	       (loop :for x fixnum :from (round x0) :to (round x1)
		  :do
		  (progn
		    (plot x y)
		    (when (> D 0.0)
		      (incf y yi)
		      (decf D (* 2 dx)))
		    (incf D (* 2 dy)))))))
    (let* ((x0 (x (line-start line)))
	   (x1 (x (line-end line)))
	   (y0 (y (line-start line)))
	   (y1 (y (line-end line))))
      ;;(declare (type single-float x0 x1 y0 y1))
      (if (< (abs (- y1 y0)) (abs (- x1 x0)))
	  (if (> x0 x1)
	      (plot-line-low x1 y1 x0 y0)
	      (plot-line-low x0 y0 x1 y1))
	  (if (> y0 y1)
	      (plot-line-high x1 y1 x0 y0)
	      (plot-line-high x0 y0 x1 y1))))))
      
      
(defun draw-lines (lines plot-func)
  (declare (type lines lines))
  (map nil (lambda (line)
	     (declare (type line line))
	     (draw-line line plot-func)) lines))
