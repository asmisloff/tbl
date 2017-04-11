(defun between-undo-marks  (proc / doc)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (proc)
  (vla-endundomark doc))

(setq *last-selected-solids* nil)

(defun select-solids-on-screen  ()
  (ssget '((0 . "3DSOLID"))))

(defun fill-table  (sset / i)
  (setq *table* nil)
  (setq i 0)
  (while (< i (sslength sset))
    (tbl::add-row (row::make-row (ssname sset i)))
    (setq i (1+ i))))

(defun draw-table  (tbl m)
  (setq dwg-table (vla-addtable
                    *paper-space*
                    (vlax-3d-point 0 0)
                    1
                    (if (= m 1)
                      5
                      6)
                    5
                    20))

  (vla-settext dwg-table 0 0 "Поз.")
  (vla-settext dwg-table 0 1 "Материал")
  (vla-settext dwg-table 0 2 "Размеры")
  (vla-settext dwg-table 0 3 "К-во")

  (vla-setcolumnwidth dwg-table 0 10)
  (vla-setcolumnwidth dwg-table 1 30)
  (vla-setcolumnwidth dwg-table 2 30)
  (vla-setcolumnwidth dwg-table 3 10)
  (cond ((= m 1)
         (vla-settext dwg-table 0 4 "Прим."))
        (t
         (vla-settext dwg-table 0 4 (strcat "x" (itoa m)))
         (vla-setcolumnwidth dwg-table 4 12)
         (vla-settext dwg-table 0 5 "Прим.")))

  (foreach row  tbl
    (setq index (vlax-get-property dwg-table 'rows))
    (vla-insertrows dwg-table index 5 1)
    (vla-settext dwg-table index 0 index)
    (vla-settext dwg-table index 1 (row::material row))
    (vla-settext
      dwg-table
      index
      2
      (row::dims->str (row::dims row) "x"))
    (vla-settext dwg-table index 3 (row::qty row))
    (cond ((> m 1)
           (vla-settext dwg-table index 4 (* (row::qty row) m))
           (vla-settext dwg-table index 5 (row::type row)))
          (t
           (vla-settext dwg-table index 4 (row::type row))))))

(defun draw-wall-panel-table  (tbl m)
  (setq dwg-table (vla-addtable
                    *paper-space*
                    (vlax-3d-point 0 0)
                    1
                    (if (= m 1)
                      5
                      6)
                    5
                    20))

  (vla-settext dwg-table 0 0 "Поз.")
  (vla-settext dwg-table 0 1 "Материал")
  (vla-settext dwg-table 0 2 "Размеры")
  (vla-settext dwg-table 0 3 "К-во")

  (vla-setcolumnwidth dwg-table 0 20)
  (vla-setcolumnwidth dwg-table 1 40)
  (vla-setcolumnwidth dwg-table 2 30)
  (vla-setcolumnwidth dwg-table 3 10)
  (cond ((= m 1)
         (vla-settext dwg-table 0 4 "Прим."))
        (t
         (vla-settext dwg-table 0 4 (strcat "x" (itoa m)))
         (vla-setcolumnwidth dwg-table 4 12)
         (vla-settext dwg-table 0 5 "Прим.")))

  (setq rp-cnt    0                     ; resopal panels count
        vn-cnt    0                     ; veneer panel count
        infer-art (lambda (row / mat doc-name room-number)
                    (setq mat (row::material row)
                          doc-name (vla-get-name (vla-get-ActiveDocument (vlax-get-acad-object)))
                          room-number (substr doc-name 1 (- (strlen doc-name) 4)))
                    (cond ((vl-string-search "Resopal" mat)
                           (strcat room-number "-ПР-" (int->str (setq rp-cnt (1+ rp-cnt)))))
                          ((vl-string-search "шпон" mat)
                           (strcat room-number "-ПШ-" (int->str (setq vn-cnt (1+ vn-cnt)))))
                          (t index)))
        int->str (lambda (i)
                   (if (< i 10)
                     (strcat "0" (itoa i))
                     (itoa i))))

  (foreach row  tbl
    (setq index (vlax-get-property dwg-table 'rows))
    (vla-insertrows dwg-table index 5 1)
    (vla-settext dwg-table index 0 (infer-art row))
    (vla-settext dwg-table index 1 (row::material row))
    (vla-settext
      dwg-table
      index
      2
      (row::dims->str (row::dims row) "x"))
    (vla-settext dwg-table index 3 (row::qty row))
    (cond ((> m 1)
           (vla-settext dwg-table index 4 (* (row::qty row) m))
           (vla-settext dwg-table index 5 (row::type row)))
          (t
           (vla-settext dwg-table index 4 (row::type row))))))


(defun save-table-in-obl-files  (tbl / n last-material file)
  (setq n 1)
  (if (not (null tbl))
    (vl-mkdir "d:/раскрой"))
  (foreach row  tbl
    (if (/= last-material (row::material row))
      (progn
        (if (not (null file))
          (close file))
        (setq last-material (row::material row)
              file          (open (strcat "d:/раскрой\\" last-material ".obl") "w")
              n             1)
        (write-line "Material 	Плита	Slab	" file)))
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

(defun save-table-in-csv-files  (tbl / last-material file)
  (if (not (null tbl))
    (vl-mkdir "d:/Материалы"))
  (foreach row  tbl
    (if (/= last-material (row::material row))
      (progn
        (if (not (null file))
          (close file))
        (setq last-material (row::material row)
              file          (open (strcat "d:/Материалы\\" last-material ".csv") "w"))
        (write-line "Material 	Плита	Slab	" file)))
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

(defun save-table-in-tc3-files  (tbl / n last-material file)
  (setq n 1)
  (if (not (null tbl))
    (vl-mkdir "d:/раскрой"))
  (foreach row  tbl
    (if (/= last-material (row::material row))
      (progn
        (if (not (null file))
          (close file))
        (setq last-material (row::material row)
              file          (open (strcat "d:/раскрой\\" last-material ".tc3") "w")
              n             1)
        (write-line "[Material]" file)
        (write-line "2800 2070 1 500 1 20 20 20 20 comment" file)
        (write-line "[Detail]" file)))
    (write-line
      (strcat (itoa (cadr (row::dims row)))
              " "
              (itoa (caddr (row::dims row)))
              " 1 "
              (itoa (row::qty row))
              " 1 ")
      file)
    (setq n (1+ n)))
  (close file))

;;;===============================================================================
;;;===============================================================================

(defun c:tbl  (/ m)
  (setup)
  (if (not (setq m (getint "Multiplicator <1>: ")))
    (setq m 1))
  (between-undo-marks
    (lambda ()
      (fill-table (setq *last-selected-solids* (select-solids-on-screen)))
      (draw-table (setq *table* (util::sorted *table* 'row::<)) m)))
  'ok)

(defun c:tblwp  (/ m)
  (setup)
  (if (not (setq m (getint "Multiplicator <1>: ")))
    (setq m 1))
  (between-undo-marks
    (lambda ()
      (fill-table (setq *last-selected-solids* (select-solids-on-screen)))
      (draw-wall-panel-table (setq *table* (util::sorted *table* 'row::<)) m)))
  'ok)

(defun c:tblf  ()
  (setup)
  (fill-table (setq *last-selected-solids* (select-solids-on-screen)))
  (save-table-in-obl-files (util::sorted *table* 'row::<))
  'ok)

(defun c:tblc  ()
  (setup)
  (fill-table (setq *last-selected-solids* (select-solids-on-screen)))
  (save-table-in-tc3-files (util::sorted *table* 'row::<))
  'ok)

(defun c:tblcsv  ()
  (setup)
  (fill-table (setq *last-selected-solids* (select-solids-on-screen)))
  (save-table-in-csv-files (util::sorted *table* 'row::<))
  'ok)

(defun highlight-position  (n)
  (command "_regenall")
  (if *table*
    (progn
      (setq handles (row::id
                      (nth (1- n)
                           *table*))
            lss     (util::sset->list
                      *last-selected-solids*)
            s       (vl-remove-if-not
                      '(lambda
                         (solid / handle)
                          (setq handle
                                 (vla-get-handle
                                   (vlax-ename->vla-object
                                     solid)))
                          (vl-some
                            '(lambda (h)
                               (= h handle))
                            handles))
                      lss))
      (foreach solid  lss
        (vla-highlight
          (vlax-ename->vla-object
            solid)
          :vlax-false))
      (foreach solid  s
        (vla-highlight
          (vlax-ename->vla-object
            solid)
          :vlax-true)))
    (princ "\nАктивная таблица пуста. Выполните команду tbl\n")))

(defun c:hp  (/ pos)
  (setq pos (getint "\nНомер позиции: "))
  (if (and (<= pos (length *table*))
           (> pos 0))
    (highlight-position pos)
    (princ "\nНет такой позиции.\n")))

(defun c:hpall  (/ pos leader)
  (setq pos 1)
  (while (<= pos (length *table*))
    (highlight-position pos)
    (setq points (vlax-make-safearray vlax-vbdouble '(0 . 5)))
    (print (strcat "Поз. " (itoa pos)))
    (setq p1 (getpoint "Стрелка: "))
    (if (not (null p1))
      (progn
        (setq p2 (getpoint "Текст: "))
        (if (not (null p2))
          (progn
            (vlax-safearray-fill points (append p1 p2))
            (setq leader (vla-addmleader *paper-space* points 0))
            (vla-put-textstring leader (itoa pos))))))
    (setq pos (1+ pos)))
  (command "_regenall"))

(defun c:mm  (/ s ss insp)
  (setup)
  (fill-table (setq ss (select-solids-on-screen)))
  (between-undo-marks
    (lambda ()
      (setq s (foreach row  *table*
                (strcat (car row)
                        "_"
                        (itoa (caadr row))
                        "х"
                        (itoa (cadadr row))
                        "х"
                        (itoa (cadr (cdadr row)))
                        " мм")))
      (command "-block"
               s
               (setq insp (getpoint "insertion point:"))
               (car (util::sset->list ss))
               "")
      (command "-insert" s insp "" "" "")
      (print s))))

 ;|«Visual LISP© Format Options»
(180 2 40 0 nil "end of " 100 9 1 1 0 nil T nil T)
;*** DO NOT add text below the comment! ***|;
