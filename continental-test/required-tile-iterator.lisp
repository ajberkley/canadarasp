(defun wrt-homedir (filename)
  (let ((homedir (namestring (user-homedir-pathname))))
    (concatenate 'string homedir filename)))

(defun find-all-tile-ids (&optional (filename (wrt-homedir "canadarasp/continental-test/plot-generation/locations.txt")))
  (let (tile-ids)
    (with-open-file (str filename :direction :input)
      (read-line str)
      (loop :for line = (read-line str nil nil)
	 :for index :from 0
	 :while line
	 :do
	 (destructuring-bind (region location lon lat &optional model)
	     (mapcar (lambda (x) (string-trim '(#\Space) x)) (cl-ppcre:split "," line))
	   (declare (ignorable region model location))
	   (when (or (not model) (string= model *model*))
	     (pushnew (multiple-value-list (tile-id lon lat)) tile-ids :test #'equalp)))))
    tile-ids))

(defun only-required-tile-iterator ()
  "a tile-id is ('lat0:lat1:lon1:lon2' (lat0 lon0 lat1 lon1))"
  (let ((tile-ids (find-all-tile-ids)))
    (lambda ()
      (pop tile-ids))))
