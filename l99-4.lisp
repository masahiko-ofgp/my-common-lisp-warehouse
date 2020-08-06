;; OCaml L-99 problems with Common Lisp
;; Binary Tree (55~63)

(defpackage :l99-4
  (:use :cl)
  (:export :cbal-tree
           :mirrorp
           :symmetricp
           :construct
           :sym-cbal-trees
           :hbal-tree
           :hbal-tree-nodes
           :count-leaves
           :leaves
           :internals
           :at-level
           :complete-binary-tree))
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


;; L-57 Binary search trees
(defun insert (tree x)
  (cond
    ((typep tree 'empty*) (node x (empty) (empty)))
    ((typep tree 'node*) (let ((y (node*-val tree))
                               (l (node*-l tree))
                               (r (node*-r tree)))
                           (cond
                             ((= x y) tree)
                             ((< x y) (node y (insert l x) r))
                             (t (node y l (insert r x))))))))
(defun construct (l)
  (reduce #'insert l :initial-value (empty)))


;; L-58 Generate-and-test paradigm
(defun sym-cbal-trees (n)
  (loop for i in (cbal-tree n) when (symmetricp i) :collect i))


;; L-59 Construct height-balanced binary trees
(defun hbal-tree (n)
  (cond
    ((= n 0) (list (empty)))
    ((= n 1) (list (node #\x (empty) (empty))))
    (t
      (let ((t1 (hbal-tree (- n 1)))
            (t2 (hbal-tree (- n 2))))
        (add-trees-with t1 t1 (add-trees-with t1 t2 (add-trees-with t2 t1 '())))))))


;; L-60 Construct height-balanced binary trees with a given number of nodes
(defun max-nodes (h)
  (- (ash 1 h) 1))

(defun min-nodes-loop (m0 m1 h)
  (if (<= h 1)
    m1
    (min-nodes-loop m1 (+ (+ m1 m0) 1) (- h 1))))

(defun min-nodes (h)
  (if (<= h 0)
    0
    (min-nodes-loop 0 1 h)))

(defun min-height (n)
  (truncate (/ (ceiling (log (+ n 1.0))) (log 2.0))))

(defun ceil-log2-loop (lg plus1 n)
  (if (= n 1)
    (if plus1
      (+ lg 1)
      lg)
    (ceil-log2-loop (+ lg 1) (or plus1 (/= 0 (logand n 1))) (truncate (/ n 2)))))

(defun ceil-log2 (n)
  (ceil-log2-loop 0 nil n))

(defun max-height-search (h mh mh1 n)
  (if (<= mh n)
    (max-height-search (+ h 1) mh1 (+ (+ mh1 mh) 1) n)
    (- h 1)))

(defun max-height (n)
  (max-height-search 0 0 1 n))

(defun fold-range (f init n0 n1)
  (if (> n0 n1)
    init
    (fold-range f (funcall f init n0) (+ n0 1) n1)))

(defun add-swap-left-right (trees)
  (reduce #'(lambda (a n)
              (cond
                ((typep n 'node*) (cons (node (node*-val n)
                                              (node*-r n)
                                              (node*-l n))
                                        a))
                ((typep n 'empty*) a)))
          trees
          :initial-value trees))

(defun hbal-tree-nodes-height (h n)
  (when (and (<= (min-nodes h) n) (<= n (max-nodes h)))
      (if (= h 0)
          (list (empty))
          (let* ((acc1 (add-hbal-tree-node '() (- h 1) (- h 2) n))
                 (acc2 (add-swap-left-right acc1)))
            (add-hbal-tree-node acc2 (- h 1) (- h 1) n)))))

(defun add-hbal-tree-node (l h1 h2 n)
  (let ((min-n1 (max (min-nodes h1) (- (- n 1) (max-nodes h2))))
        (max-n1 (min (max-nodes h1) (- (- n 1) (min-nodes h2)))))
    (fold-range #'(lambda (l n1)
                    (let ((t1 (hbal-tree-nodes-height h1 n1))
                          (t2 (hbal-tree-nodes-height h2 (- (- n 1) n1))))
                      (reduce #'(lambda (l t1)
                                  (reduce #'(lambda (l t2)
                                              (cons (node #\x t1 t2) l))
                                          t2 :initial-value l))
                              t1 :initial-value l)))
                l min-n1 max-n1)))

(defun rev-append (l1 l2)
  (append (reverse l1) l2))

(defun hbal-tree-nodes (n)
  (fold-range #'(lambda (l h) (rev-append (hbal-tree-nodes-height h n) l))
              '()
              (min-height n)
              (max-height n)))


;; L-61 Count the leaves of a binary tree.
(defun count-leaves (tree)
  (cond
    ((typep tree 'empty*) 0)
    ((typep tree 'node*)
     (if (and (typep (node*-l tree) 'empty*)
              (typep (node*-r tree) 'empty*))
         1
         (+ (count-leaves (node*-l tree))
            (count-leaves (node*-r tree)))))
    (t (error "Arguments type error"))))


;; L-61-A Collect the leaves of a binary tree in a list.
(defun leaves (tree)
  (labels ((aux (tree acc)
             (cond
               ((typep tree 'empty*) acc)
               ((typep tree 'node*)
                (if (and (typep (node*-l tree) 'empty*)
                         (typep (node*-r tree) 'empty*))
                    (cons (node*-val tree) acc)
                    (aux (node*-l tree) (aux (node*-r tree) acc)))))))
    (aux tree '())))


;; L-62 Collect internal nodes of a binary tree in a list.
(defun internals (tree)
  (labels ((aux (tree acc)
             (cond
               ((typep tree 'empty*) acc)
               ((typep tree 'node*)
                (if (and (typep (node*-l tree) 'empty*)
                         (typep (node*-r tree) 'empty*))
                  acc
                  (aux (node*-l tree) (cons (node*-val tree)
                                            (aux (node*-r tree) acc))))))))
    (aux tree '())))


;; L-62-B Collect the nodes at a given level in a list.
(defun at-level (tree level)
  (labels ((aux (tree acc counter)
             (cond
               ((typep tree 'empty*) acc)
               (t
                 (if (= counter level)
                   (cons (node*-val tree) acc)
                   (aux (node*-l tree)
                        (aux (node*-r tree) acc (+ counter 1))
                        (+ counter 1)))))))
    (aux tree '() 1)))


;; L-63 Construct a complete binary tree.
(defun split-n (l acc n)
  (cond
    ((zerop n) (list (reverse acc) l))
    ((endp l) (list (reverse acc) '()))
    (t
      (split-n (cdr l) (cons (car l) acc) (- n 1)))))

(defun myflatten (p c)
  (cond
    ((endp c) (mapcar #'(lambda (x) (node x (empty) (empty))) p))
    ((endp (cdr c)) (cons (node (car p) (car c) (empty))
                          (myflatten (cdr p) '())))
    (t
     (cons (node (car p) (car c) (cadr c))
           (myflatten (cdr p) (cddr c))))))

(defun complete-binary-tree (l)
  (cond
    ((null l) (empty))
    (t
      (labels ((aux (l ls)
                 (cond
                   ((endp ls) '())
                   (t
                     (let* ((tmp (split-n ls '() (ash 1 l)))
                            (p (car tmp))
                            (c (cadr tmp)))
                       (myflatten p (aux (+ l 1) c)))))))
        (car (aux 0 l))))))