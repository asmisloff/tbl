(defun num::fix-double (double acc)
  (setq	mul   (expt 10.0 acc)
	fixed (/ (fix (* double mul))
		 mul))
  (if (>= (- double fixed)
	  (/ 0.5 mul))
    (+ fixed (/ 1.0 mul))
    fixed))

(defun num::evenp (n)
  (= (rem n 2) 0))

(defun num::oddp (n)
  (= (rem n 2) 1))

(defun num::betweenp (num a b)
  (and
    (>= num a)
    (<= num b)))

(defun num::round (f / i)
  (setq i (fix f))
  (if (>= (- f i) 0.5)
    (1+ i)
    i))