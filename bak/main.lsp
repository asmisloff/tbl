(defun between-undo-marks  (proc / doc result)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq result (proc))
  (vla-EndUndoMark doc)
  result)

(defun select-solids-on-screen	()
  (ssget '((0 . "3DSOLID"))))

(defun fill-table  (sset / i)
  (setq *table* nil)
  (setq i 0)
  (while (< i (sslength sset))
    (tbl::add-row (row::make-row (ssname sset i)))
    (setq i (1+ i))))

(defun draw-table  (tbl m)
  (setq	dwg-table (vla-AddTable
		    *paper-space*
		    (vlax-3d-point 0 0)
		    1
		    (if	(= m 1)
		      5
		      6)
		    5
		    20))

  (vla-SetText dwg-table 0 0 "Поз.")
  (vla-SetText dwg-table 0 1 "Материал")
  (vla-SetText dwg-table 0 2 "Размеры")
  (vla-SetText dwg-table 0 3 "К-во")

  (vla-SetColumnWidth dwg-table 0 10)
  (vla-SetColumnWidth dwg-table 1 30)
  (vla-SetColumnWidth dwg-table 2 30)
  (vla-SetColumnWidth dwg-table 3 10)
  (cond	((= m 1)
	 (vla-SetText dwg-table 0 4 "Прим."))
	(T
	 (vla-SetText dwg-table 0 4 (strcat "x" (itoa m)))
	 (vla-SetColumnWidth dwg-table 4 12)
	 (vla-SetText dwg-table 0 5 "Прим.")))

  (foreach row	tbl
    (setq index (vlax-get-property dwg-table 'rows))
    (vla-InsertRows dwg-table index 5 1)
    (vla-SetText dwg-table index 0 index)
    (vla-SetText dwg-table index 1 (row::material row))
    (vla-SetText
      dwg-table
      index
      2
      (row::dims->str (row::dims row) "x"))
    (vla-SetText dwg-table index 3 (row::qty row))
    (cond ((> m 1)
	   (vla-SetText dwg-table index 4 (* (row::qty row) m))
	   (vla-SetText dwg-table index 5 (row::type row)))
	  (T
	   (vla-SetText dwg-table index 4 (row::type row))))))

(defun save-table-in-obl-files	(tbl / n last-material file)
  (setq n 1)
  (if (not (null tbl))
    (vl-mkdir "D:\\раскрой"))
  (foreach row	tbl
    (if	(/= last-material (row::material row))
      (progn
	(if (not (null file))
	  (close file))
	(setq last-material (row::material row)
	      file	    (open (strcat "D:\\раскрой\\" last-material ".obl") "w")
	      n		    1)))
    (write-line
      (strcat (itoa n)
	      "\t"
	      (strcat (itoa (cadr (row::dims row)))
		      "\t"
		      (itoa (caddr (row::dims row))))
	      "\t"
	      (itoa (row::qty row)))
      file)
    (setq n (1+ n)))
  (close file))

(defun save-table-in-csv-files	(tbl / last-material file)
  (if (not (null tbl))
    (vl-mkdir "Материалы"))
  (foreach row	tbl
    (if	(/= last-material (row::material row))
      (progn
	(if (not (null file))
	  (close file))
	(setq last-material (row::material row)
	      file	    (open (strcat "Материалы\\" last-material ".csv") "w"))
	(write-line "Материал\tРазмеры\tКол." file)))
    (write-line
      (strcat (row::material row)
	      "\t"
	      (strcat (itoa (car (row::dims row)))
		      "x"
		      (itoa (cadr (row::dims row)))
		      "x"
		      (itoa (caddr (row::dims row))))
	      "\t"
	      (itoa (row::qty row)))
      file))
  (close file))

;;;===============================================================================
;;;===============================================================================

(defun c:tbl  (/ m msg)
  (setup)
  (if (not (setq m (getint "Multiplicator <1>: ")))
    (setq m 1))

  (fill-table (select-solids-on-screen))
  (draw-table (util::sorted *table* 'row::<) m)
  'ok)

(defun c:tblf  ()
  (setup)
  (fill-table (select-solids-on-screen))
  (save-table-in-obl-files (util::sorted *table* 'row::<))
  'ok)

(defun c:tblcsv	 ()
  (setup)
  (fill-table (select-solids-on-screen))
  (save-table-in-csv-files (util::sorted *table* 'row::<))
  'ok)