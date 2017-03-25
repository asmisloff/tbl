(defun estr::expl  (ent / ss i li)
  (command "_explode"
	   (vlax-vla-object->ename
	     (vla-copy
	       (vlax-ename->vla-object ent))))
  (util::sset->list (ssget "_P")))

(defun estr::get-ribs  (solid / res)
  (setq res '())
  (foreach item	 (estr::expl solid)
    (setq res (append (estr::expl item) res))
    (vla-delete (vlax-ename->vla-object item)))
  res)

(defun estr::ribs-equalp  (rib1 rib2)
  (or
    (and (line::linep rib1)
	 (line::linep rib2)
	 (line::equalp rib1 rib2)
	 (line::parallelp rib1 rib2))
    (and (arc::arcp rib1)
	 (arc::arcp rib2)
	 (or (arc::coincide rib1 rib2)
	     (arc::projectionaly-linked rib1 rib2)))))

(defun estr::squeeze  (entlist / res)
  (setq res '())
  (foreach ent	entlist
    (if	(not (vl-member-if
	       '(lambda (x) (estr::ribs-equalp ent x))
	       res))
      (setq res (cons ent res))
      (vla-delete (vlax-ename->vla-object ent))))
  res)

(defun estr::del-entities  (ent-list)
  (foreach ent	ent-list
    (vla-delete (vlax-ename->vla-object ent))))

(defun estr::layer  (ent)
  (vl-string-left-trim
    "$0$#"
    (vlax-get-property
      (vlax-ename->vla-object ent)
      'Layer)))

(defun estr::area  (ent)
  (if (= (vlax-get-property (vlax-ename->vla-object ent) 'ObjectName)
	 "AcDbRegion")
    (vlax-get-property (vlax-ename->vla-object ent) 'Area)
    0.0))

(defun estr::volume  (solid)
  (if (= (vlax-get-property
	   (vlax-ename->vla-object solid)
	   'ObjectName)
	 "AcDb3dSolid")
    (vlax-get-property (vlax-ename->vla-object solid) 'Volume)
    0.0))

(defun estr::principal-moments	(solid)
  (if (= (vlax-get-property
	   (vlax-ename->vla-object solid)
	   'objectname)
	 "AcDb3dSolid")
    (vlax-safearray->list (vlax-variant-value (vla-get-PrincipalMoments (vlax-ename->vla-object solid))))
    0.0))