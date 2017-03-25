(setq *table* (list))
(setq *_table* (list))

(defun tbl::add-row  (row / _row)
  (setq *_table* (cons row *_table*))
  (if (setq _row (util::search '(lambda (x) (row::eq x row)) *table*))
    (setq *table* (cons	(row::+ row _row)
			(vl-remove-if
			  '(lambda (x) (row::eq x row))
			  *table*)))
    (setq *table* (cons row *table*))))