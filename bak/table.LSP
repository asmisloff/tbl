(setq *table* (list))

(defun tbl::add-row  (row / _row)
  (if (setq _row (util::search '(lambda (x) (row::eq x row)) *table*))
    (setq *table* (cons	(row::1+ _row)
			(vl-remove-if
			  '(lambda (x) (row::eq x row))
			  *table*)))
    (setq *table* (cons row *table*))))