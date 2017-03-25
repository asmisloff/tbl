(defun msr::bbox  (solid / minp maxp)
  (vla-getboundingbox
    (vlax-ename->vla-object solid)
    'minp
    'maxp)
  (util::sorted
    (mapcar '(lambda (x y) (num::round (- y x)))
	    (vlax-safearray->list minp)
	    (vlax-safearray->list maxp))
    '<))

(defun msr::rotated-copy  (solid tmatrix)
  (vla-transformby
    (vla-copy (vlax-ename->vla-object solid))
    tmatrix)
  (entlast))

(defun msr::measure-by-ucs  (ucs solid / tmatrix rc res)
  (setq	tmatrix	(vlax-tmatrix (la::transpone (ucs::matrix ucs)))
	rc	(msr::rotated-copy solid tmatrix)
	res	(msr::bbox rc))
  (estr::del-entities (list rc))
  res)

(defun msr::succesive-measurement
       (solid / faces biggest-face normal ribs lines res)
  (setq
    faces	 (estr::expl solid)
    biggest-face (util::get-maximum
		   '(lambda (x) (estr::area x))
		   faces)
    normal	 (arc::normal biggest-face)
    ribs	 (estr::expl biggest-face)
    lines	 (vl-remove-if-not 'line::linep ribs)
    res		 (list))
  (foreach line	 lines
    (setq ucs (ucs::new '(0 0 0) (line::i-j-k line) normal "#temp")
	  res (cons (msr::measure-by-ucs ucs solid)
		    res)))
  (setq res (cons (msr::bbox solid) res))
  (ucs::delete ucs)
  (estr::del-entities ribs)
  (estr::del-entities faces)
  (util::get-minimum '(lambda (x) (apply '* x)) res))