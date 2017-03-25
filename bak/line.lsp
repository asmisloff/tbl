(defun line::len  (line)
  (vlax-get-property
    (vlax-ename->vla-object line)
    'length))

(defun line::equalp  (line1 line2)
  (equal (line::len line1)
	 (line::len line2)
	 *const::lin-eps*))

(defun line::linep (ent)
  (= (vlax-get-property (vlax-ename->vla-object ent) 'ObjectName)
     "AcDbLine"))

(defun line::i-j-k  (line)
  (mapcar '-
	  (vlax-safearray->list
	    (vlax-variant-value
	      (vlax-get-property
		(vlax-ename->vla-object line)
		'EndPoint)))
	  (vlax-safearray->list
	    (vlax-variant-value
	      (vlax-get-property
		(vlax-ename->vla-object line)
		'StartPoint)))))

(defun line::scalar-prod  (line1 line2)
  (apply '+
	 (mapcar '*
		 (line::i-j-k line1)
		 (line::i-j-k line2))))

(defun line::cos-alpha  (line1 line2)
  (/ (line::scalar-prod line1 line2)
     (*	(line::len line1)
	(line::len line2))))

(defun line::parallelp (line1 line2)
  (equal (abs (line::cos-alpha line1 line2))
	 1
	 *const::ang-eps*))

(defun line::ortogonalp (line1 line2)
  (setq c (line::cos-alpha line1 line2))
  (equal c 0 *const::ang-eps*))