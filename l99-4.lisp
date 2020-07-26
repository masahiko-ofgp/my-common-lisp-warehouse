;; OCaml L-99 problems with Common Lisp
;; Binary Tree (55~56)

(defpackage :l99-4
  (:use :cl)
  (:export :cbal-tree
           :mirrorp
           :symmetricp))
(in-package :l99-4)

(defstruct node* val l r)
(defun node (val l r)
  (make-node* :val val :l l :r r))

(defstruct empty* val)
(defun empty ()
  (make-empty* :val nil))


;; L-55 Construct completely balanced binary trees
(defun add-trees-with (left right all)
  (labels ((add-right-tree (all l)
             (reduce #'(lambda (a r) (cons (node #\x l r) a))
                     right
                     :initial-value all)))
    (reduce #'add-right-tree left :initial-value all)))

(defun cbal-tree (n)
  (cond
    ((= n 0) (list (empty)))
    ((= (mod n 2) 1) (let* ((x (round (/ n 2)))
                            (t0 (cbal-tree x)))
                       (add-trees-with t0 t0 '())))
    (t
     (let* ((x (round (/ n 2)))
            (t1 (cbal-tree (- x 1)))
            (t2 (cbal-tree x)))
       (add-trees-with t1 t2 (add-trees-with t2 t1 '()))))))


;; L-56 Symmetric binary trees
(defun mirrorp (t1 t2)
  (cond
    ((and (typep t1 'empty*) (typep t2 'empty*)) t)
    ((and (typep t1 'node*) (typep t2 'node*))
     (let ((l1 (node*-l t1))
           (r1 (node*-r t1))
           (l2 (node*-l t2))
           (r2 (node*-r t2)))
       (and (mirrorp l1 r2) (mirrorp r1 l2))))
    (t nil)))

(defun symmetricp (bt)
  (cond
    ((typep bt 'empty*) t)
    (t
      (let ((l (node*-l bt))
            (r (node*-r bt)))
        (mirrorp l r)))))
