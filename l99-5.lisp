;; OCaml L-99 problems with Common Lisp
;; Multiway Trees (70C~73)
(in-package :cl-user)
(defpackage :l99-5
  (:use :cl)
  (:export :count-nodes
           :string-of-tree
           :tree-of-string
           :ipl
           :bottom-up
           :lispy))
(in-package :l99-5)

(defvar example-mult-tree
  '(#\a ((#\f ((#\g ())))
         (#\c ())
         (#\b ((#\d ())
               (#\e ()))))))


;; L-70-C Count the nodes of a multiway tree.
(defun count-nodes (tree)
  (reduce #'(lambda (n tr) (+ n (count-nodes tr)))
          (cadr tree)
          :initial-value 1))


;; L-70 Tree construction from a node string.
(defun add-string-of-tree (buff tree)
  (progn
    (vector-push (car tree) buff)
    (mapcan #'(lambda (sub) (add-string-of-tree buff sub)) (cadr tree))
    (vector-push #\^ buff)))

(defun string-of-tree (tree)
  (let ((buff (make-array 128 :fill-pointer 0)))
    (progn
      (add-string-of-tree buff tree)
      (concatenate 'string buff))))

(defun tree-of-substring (tree s i len)
  (if (or (>= i len)
          (char= (schar s i) #\^))
    (list (reverse tree) (+ i 1))
    (let* ((tmp (tree-of-substring '() s (+ i 1) len))
           (sub (car tmp))
           (j (cadr tmp)))
      (tree-of-substring (cons (list (schar s i) sub) tree) s j len))))

(defun tree-of-string (s)
  "e.g)
  * (tree-of-string \"afg^^c^bd^e^^^\")
  (#\a ((#\f ((#\g NIL)))
        (#\c NIL)
        (#\b ((#\d NIL)
              (#\e NIL)))))
  "
  (let ((tmp (tree-of-substring '() s 0 (length s))))
    (cond
      ((not (endp (car tmp))) (caar tmp))
      (t (error "tree-of-string")))))


;; L-71 Determine the internal path length of a tree.
(defun ipl-sub (len tree)
  (reduce #'(lambda (sm tr) (+ sm (ipl-sub (+ len 1) tr)))
          (cadr tree)
          :initial-value len))

(defun ipl (tree)
  (ipl-sub 0 tree))


;; L-72 Construct the bottom-up order sequence of the tree nodes.
(defun prepend-bottom-up (tree l)
  (reduce #'(lambda (tr ll) (prepend-bottom-up tr ll))
          (cadr tree)
          :initial-value (cons (car tree) l)
          :from-end t))

(defun bottom-up (tree)
  "e.g.)
  * (bottom-up '(#\a ((#\b ()))))
  (#\b #\a)
  "
  (prepend-bottom-up tree '()))


;; L-73 Lisp-like tree representation.
(defun add-lispy (buff tree)
  (cond
    ((null (cadr tree)) (vector-push (car tree) buff))
    (t
      (progn
        (vector-push #\( buff)
        (vector-push (car tree) buff)
        (mapcan #'(lambda (tr) (progn
                                 (vector-push #\Space buff)
                                 (add-lispy buff tr)))
                (cadr tree))
        (vector-push #\) buff)))))

(defun lispy (tree)
  "e.g.)
  * (lispy '(#\a ()))
  \"a\"
  * (lispy '(#\a ((#\b ()))))
  \"(a b)\"
  * (lispy example-mult-tree)
  \"(a (f g) c (b d e))\"
  "
  (let ((buff (make-array 128 :fill-pointer 0)))
    (progn
      (add-lispy buff tree)
      (concatenate 'string buff))))
