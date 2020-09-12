;; OCaml L-99 problems with Common Lisp
;; Miscellaneous Problems (91~)
(defpackage :l99-7
  (:use :cl)
  (:export :queens-positions))
(in-package :l99-7)


;; L-91 Eight queens problems
(defun mem (x l)
  (if (eql (find x l) nil)
    nil
    t))

(defun possible (row col used-rows usedD1 usedD2)
  (not (or (mem row used-rows)
           (mem (+ row col) usedD1)
           (mem (- row col) usedD2))))

(defun queens-positions (n)
  (labels ((aux (row col used-rows usedD1 usedD2)
             (if (> col n)
               (list (reverse used-rows))
               (append (if (< row n)
                         (aux (+ row 1) col used-rows usedD1 usedD2)
                         '())
                       (if (possible row col used-rows usedD1 usedD2)
                         (aux 1
                              (+ col 1)
                              (cons row used-rows)
                              (cons (+ row col) usedD1)
                              (cons (- row col) usedD2))
                         '())))))
    (aux 1 1 '() '() '())))

