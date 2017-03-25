(defun arc::normal  (arc)
  (vlax-safearray->list
    (vlax-variant-value
      (vlax-get-property
	(vlax-ename->vla-object arc)
	'Normal))))

(defun arc::arcp  (ent)
  (= (vlax-get-property (vlax-ename->vla-object ent) 'ObjectName)
     "AcDbArc"))

(defun arc::circp (ent)
  (= (vla-get-ObjectName (vlax-ename->vla-object ent))
     "AcDbCircle"))

(defun arc::center  (arc)
  (vlax-safearray->list
    (variant-value
      (vlax-get-property (vlax-ename->vla-object arc) 'Center))))

(defun arc::radius  (arc)
  (vlax-get-property (vlax-ename->vla-object arc) 'Radius))

(defun arc::start-angle	 (arc)
  (vlax-get-property (vlax-ename->vla-object arc) 'StartAngle))

(defun arc::end-angle  (arc)
  (vlax-get-property (vlax-ename->vla-object arc) 'EndAngle))

(defun arc::total-angle	 (arc)
  (vlax-get-property (vlax-ename->vla-object arc) 'TotalAngle))

(defun arc::start-point	 (arc)
  (vlax-safearray->list
    (variant-value
      (vlax-get-property
	(vlax-ename->vla-object arc)
	'StartPoint))))

(defun arc::end-point  (arc)
  (vlax-safearray->list
    (variant-value
      (vlax-get-property
	(vlax-ename->vla-object arc)
	'EndPoint))))

(defun arc::length (arc)
  ((vlax-get-property (vlax-ename->vla-object arc) 'ArcLength)))

(defun arc::equalp  (a1 a2)
  (and
    (equal (arc::radius a1)
	   (arc::radius a2)
	   *const::lin-eps*)
    (equal (arc::total-angle a1)
	   (arc::total-angle a2)
	   *const::ang-eps*)))

(defun arc::parallelp  (a1 a2)
  (la::vector-parallelp
    (arc::normal a1)
    (arc::normal a2)))

(defun arc::projectionaly-linked  (a1 a2)
  (and
    (arc::equalp a1 a2)
    (arc::parallelp a1 a2)
    (la::vector-parallelp
      (arc::normal a1)
      (la::make-vector
	(arc::center a1)
	(arc::center a2)))
    (vl-some '(lambda (V)
		(la::vector-parallelp
		  (arc::normal a1)
		  V))
	     (list (la::make-vector
		     (arc::start-point a1)
		     (arc::start-point a2))
		   (la::make-vector
		     (arc::start-point a1)
		     (arc::end-point a2))))))

(defun arc::coincide  (a1 a2)
  (and (arc::equalp a1 a2)
       (arc::parallelp a1 a2)
       (equal (arc::center a1)
	      (arc::center a2)
	      *const::lin-eps*)))