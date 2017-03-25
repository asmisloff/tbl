(defun tt::boxp	 (solid ribs / i j res)
  (setq	i   0
	j   1
	res (vl-every 'line::linep ribs))

  (while (and res
	      (< i (1- (length ribs))))
    (while (and	res
		(< j (length ribs)))
      (setq res	(line::ortogonalp
		  (nth i ribs)
		  (nth j ribs)))
      (setq j (+ j 1)))
    (setq i (+ i 1)
	  j (+ i 1)))

  (if res
    (util::sorted
      (mapcar '(lambda (x) (num::round (line::len x)))
	      ribs)
      '<)))

(defun tt::diskp (solid ribs)
  (if (vl-every 'arc::circp ribs)
    (msr::bbox solid)
    nil))
					;---------------------------------------------------------------------------------
(defun tt::parallelogrammp  (solid ribs)
  (if (and (vl-every 'line::linep ribs)
	   (= (length ribs) 3))
    (msr::succesive-measurement solid)
    nil))
					;---------------------------------------------------------------------------------
(defun tt::trianglep  (solid ribs)
  (if (and (vl-every 'line::linep ribs)
	   (= (length ribs) 4))
    (msr::succesive-measurement solid)
    nil))
					;---------------------------------------------------------------------------------
(defun tt::trapeziump  (solid ribs / two-and-only-two-lines-are-parallelp)
  (defun two-and-only-two-lines-are-parallelp  (lines / i j pairs)
    (setq i	0
	  j	1
	  pairs	(list))
    (while (< i (1- (length lines)))
      (while (< j (length lines))
	(setq pairs (cons (cons	(nth i lines)
				(nth j lines))
			  pairs)
	      j	    (1+ j)))
      (setq i (1+ i)
	    j (1+ i)))
    (= (length (vl-remove-if-not
		 '(lambda (x)
		    (line::parallelp
		      (car x)
		      (cdr x)))
		 pairs))
       1))
  (if (and (vl-every 'line::linep ribs)
	   (= (length ribs) 5)
	   (two-and-only-two-lines-are-parallelp ribs))
    (msr::succesive-measurement solid)
    nil))
					;---------------------------------------------------------------------------------
(defun tt::bended-rectp	 (solid		ribs	      /
			  arcs		lines	      thickness-by-layer
			  thickness-line	      height-lines
			  th		h)

  (setq	arcs		   (vl-remove-if-not 'arc::arcp ribs)
	lines		   (vl-remove-if-not 'line::linep ribs)
	thickness-by-layer (util::string->number (estr::layer (car ribs)))
	thickness-line	   (util::search
			     '(lambda (x)
				(equal (line::len x)
				       thickness-by-layer
				       *const::lin-eps*))
			     lines)
	height-lines	   (if (not (null arcs))
			     (vl-remove-if-not
			       '(lambda	(x)
				  (la::vector-parallelp
				    (line::i-j-k x)
				    (arc::normal (car arcs))))
			       lines)))
  (cond	((or
	   (null arcs)
	   (num::oddp
	     (length arcs))
	   (null thickness-line))
	 nil)
	((and
	   (vl-every '(lambda (x) (arc::parallelp x (car arcs)))
		     (cdr arcs))
	   (la::vector-ortogonalp
	     (arc::normal (car arcs))
	     (line::i-j-k thickness-line))
	   (= (length height-lines) 1))
	 (list (num::round
		 (setq th (line::len thickness-line)))
	       (num::round
		 (setq h (line::len (car height-lines))))
	       (num::round
		 (+ (/ (estr::volume solid)
		       th
		       h)
		    (/ th 2)))))
	(t nil)))
					;=================================================================================
					;=================================================================================
(setq *types*
       (list
	 (list 'tt::boxp "")
	 (list 'tt::trapeziump "Трапеция")
	 (list 'tt::parallelogrammp "Параллелограмм")
	 (list 'tt::trianglep "Треугольник")
	 (list 'tt::bended-rectp "Гнутая деталь")
	 (list 'tt::diskp "Диск")))
					;---------------------------------------------------------------------------------
(defun infer-type  (solid ribs / check res)
  (defun check	(alist)
    (cond ((null alist) (list nil "См. чертеж"))
	  ((setq res (apply (caar alist) (list solid ribs)))
	   (cons res (cdar alist)))
	  (t (check (cdr alist)))))
  (check *types*))