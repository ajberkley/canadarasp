(defmacro with-gensym ((&rest gensyms) &body body)
  `(let ,(iter (for g in gensyms)
	       (collect `(,g (gensym))))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	      ,@body)))))

(defmacro with-output-from-program ((var program args) &body body)
  (with-gensym (p)
    `(let* (,p ,var)
       (unwind-protect
	    (progn
	      (setf ,p (sb-ext:run-program ,program ,args :output :stream))
	      (setf ,var (iter (for r = (read-line (sb-ext::process-output ,p) nil))
			       (while r)
			       (collecting r into output)
			       (collecting (format nil "~A" #\newline) into output)
			       (finally (return (apply #'concatenate 'string output)))))
	      ,@body)
	 (sb-ext:process-close ,p)))))

(declaim (inline array-map-double-float))
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

(declaim (inline clip-to-bounds))
(defun clip-to-bounds (data low high)
  (if (< data low) low
      (if (> data high) high
	  data)))

(declaim (inline scale-to-byte))
(defun scale-to-byte (data low high)
  (let ((clipped (clip-to-bounds data low high)))
    (the (unsigned-byte 8) (round (* 255 (/ (- clipped low) (- high low)))))))

(defgeneric add (a b))

(defmethod add ((a sequence) (b number))
  (mapcar (lambda (x) (+ b x)) a))

(defmethod add ((b number) (a sequence))
  (mapcar (lambda (x) (+ b x)) a))

(defmethod add ((a sequence) (b sequence))
  (mapcar #'+ a b))

(defgeneric mul (a b))

(defmethod mul ((a sequence) (b number))
  (mapcar (lambda (x) (* b x)) a))

(defmethod mul ((b number) (a sequence))
  (mapcar (lambda (x) (* b x)) a))

(defmacro print/run-program (program args)
  (let ((g (gensym)))
    (once-only (program args)
      `(with-output-from-program (,g ,program ,args)
	 (format t "~A ~{~A~^ ~}~%" ,program ,args)
	 (format t "~A~%" ,g)
	 (values)))))
