(defun ucs::new	 (origin x-axis y-axis name)
  (vla-add *ucs-collection*
	   (vlax-3d-point origin)
	   (vlax-safearray-fill
	     (vlax-make-safearray vlax-vbdouble '(0 . 2))
	     x-axis)
	   (vlax-safearray-fill
	     (vlax-make-safearray vlax-vbdouble '(0 . 2))
	     y-axis)
	   name))

(defun ucs::x-axis (ucs alist)
  (vlax-put-property
    ucs
    'XVector
    (vlax-safearray-fill
	     (vlax-make-safearray vlax-vbdouble '(0 . 2))
	     alist)))

(defun ucs::delete  (ucs)
  (if ucs
    (vla-delete ucs)))

(defun ucs::matrix (ucs)
  (vlax-safearray->list
    (vlax-variant-value
      (vla-getucsmatrix ucs))))