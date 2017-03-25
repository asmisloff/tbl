(defun la::make-vector	(p1 p2)
  (mapcar '- p2 p1))

(defun la::vector-norm	(alist)
  (sqrt
    (apply '+
	   (mapcar '(lambda (x) (expt x 2))
		   alist))))

(defun la::matrix-norm	(M)
  (la::vector-norm (mapcar 'la::vector-norm M)))

(defun la::vector/number  (V n)
  (mapcar '(lambda (x) (/ x n)) V))

(defun la::matrix/number  (M n)
  (mapcar '(lambda (x) (la::vector/number x n)) M))

(defun la::vector-equalp  (V1 V2 acc)
  (and (= (length V1) (length V2))
       (apply 'and (mapcar '(lambda (x y) (equal x y acc)) V1 V2))))

(defun la::matrix-equalp  (M1 M2 acc)
  (and (= (length M1) (length M2))
       (apply 'and
	      (mapcar '(lambda (X Y) (la::vector-equalp X Y acc)) M1 M2))))

(defun la::scalar-prod	(V1 V2)
  (apply '+
	 (mapcar
	   '(lambda (x y) (* x y))
	   V1
	   V2)))

(defun la::vector-cos-alpha  (V1 V2)
  (/ (la::scalar-prod V1 V2)
     (la::vector-norm V1)
     (la::vector-norm V2)))

(defun la::vector-ortogonalp  (V1 V2)
  (equal (la::scalar-prod V1 V2)
	 0.0
	 *const::ang-eps*))

(defun la::vector-parallelp  (V1 V2)
  (equal (abs (la::vector-cos-alpha V1 V2))
	 1.0
	 *const::ang-eps*))

(defun la::transpone  (M)		;look SICP, ex. 2.36
  (if (null (car M))
    '()
    (cons (apply 'list (mapcar 'car M))
	  (la::transpone (mapcar 'cdr M)))))

;;;alternative realization via "eval"
;;;------------------------------------------
;;;(defun la::transpone  (M)
;;;  (eval
;;;    (append
;;;      '(mapcar 'list)
;;;      (mapcar '(lambda (x) (cons 'list x))
;;;	      M))))