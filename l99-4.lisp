;; OCaml L-99 problems with Common Lisp
;; Binary Tree (55)

(defpackage :l99-4
  (:use :cl)
  (:export :cbal-tree))
(in-package :l99-4)

(defstruct node* val l r)


;; L-55 Construct completely balanced binary trees
(defun add-trees-with (left right all)
  (labels ((add-right-tree (all l)
             (reduce #'(lambda (a r) (cons (make-node* :val #\x :l l :r r) a)) right :initial-value all)))
    (reduce #'add-right-tree left :initial-value all)))

(defun cbal-tree (n)
  (cond
    ((= n 0) '(nil))
    ((= (mod n 2) 1) (let* ((x (round (/ n 2)))
                            (t0 (cbal-tree x)))
                       (add-trees-with t0 t0 '())))
    (t
     (let* ((x (round (/ n 2)))
            (t1 (cbal-tree (- x 1)))
            (t2 (cbal-tree x)))
       (add-trees-with t1 t2 (add-trees-with t2 t1 '()))))))
