;; OCaml L-99 problems with Common Lisp
;; Binary Tree (55~69)
(in-package :cl-user)
(defpackage :binary-tree
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
           :complete-binary-tree
           :example-layout-tree
           :layout-binary-tree-1
           :example-layout-tree-2
           :layout-binary-tree-2
           :layout-binary-tree-3
           :string-of-tree
           :tree-of-string
           :preorder
           :inorder
           :pre-in-tree
           :dotstring-of-tree
           :tree-of-dotstring))
(in-package :binary-tree)

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


;; L-64 Layout a binary tree (1).
(defun leaf (x) (node x (empty) (empty)))

(defvar example-layout-tree (node #\n
                                  (node #\k
                                        (node #\c
                                              (leaf #\a)
                                              (node #\h
                                                    (node #\g
                                                          (leaf #\e)
                                                          (empty))
                                                    (empty)))
                                        (leaf #\m))
                                  (node #\u
                                        (node #\p
                                              (empty)
                                              (node #\s
                                                    (leaf #\q)
                                                    (empty)))
                                        (empty))))
(defun layout-binary-tree-1 (tree)
  (labels ((layout (depth x-left tr)
             (cond
               ((typep tr 'empty*) (list (empty) x-left))
               ((typep tr 'node*)
                (let* ((tmp1 (layout (+ depth 1) x-left (node*-l tr)))
                       (ll (car tmp1))
                       (l-x-max (cadr tmp1))
                       (tmp2 (layout (+ depth 1) (+ l-x-max 1) (node*-r tr)))
                       (rr (car tmp2))
                       (r-x-max (cadr tmp2)))
                  (list (node (list (node*-val tr) l-x-max depth) ll rr) r-x-max)))
                (t (error "Invalid arg")))))
    (car (layout 1 1 tree))))


;; L-65 Layout binary tree (2).
(defvar example-layout-tree-2
  (node #\n
        (node #\k
              (node #\c
                    (leaf #\a)
                    (node #\e
                          (leaf #\d)
                          (leaf #\g)))
              (leaf #\m))
        (node #\u
              (node #\p
                    (empty)
                    (leaf #\q))
              (empty))))

(defun layout-binary-tree-2 (tree)
  (labels ((height (tr)
             (cond
               ((typep tr 'empty*) 0)
               ((typep tr 'node*) (+ 1 (max (height (node*-l tr))
                                            (height (node*-r tr)))))
               (t (error "height")))))
    (let ((tree-height (height tree)))
      (labels ((find-missing-left (depth tr)
                 (cond
                   ((typep tr 'empty*) (- tree-height depth))
                   ((typep tr 'node*) (find-missing-left (+ depth 1) (node*-l tr)))
                   (t (error "find-missing-left")))))
        (let ((translate-dst (- (ash 1 (find-missing-left 0 tree)) 1)))
          (labels ((layout (depth x-root tr)
                     (cond
                       ((typep tr 'empty*) (empty))
                       ((typep tr 'node*)
                        (let* ((spacing (ash 1 (- (- tree-height depth) 1)))
                               (ll (layout (+ depth 1)
                                           (- x-root spacing)
                                           (node*-l tr)))
                               (rr (layout (+ depth 1)
                                           (+ x-root spacing)
                                           (node*-r tr))))
                          (node (list (node*-val tr) x-root depth) ll rr)))
                        (t (error "layout")))))
            (layout 1 (- (ash 1 (- tree-height 1)) translate-dst) tree)))))))


;; L-66 Layout binary tree (3).
(defun layout-binary-tree-3 (tree)
  (labels ((translate-x (d tr)
             (cond
               ((typep tr 'empty*) (empty))
               ((typep tr 'node*) (node (list (car (node*-val tr))
                                              (+ (cadr (node*-val tr)) d)
                                              (caddr (node*-val tr)))
                                        (translate-x d (node*-l tr))
                                        (translate-x d (node*-r tr))))
               (t (error "translate-x"))))
           (dist (lr rl)
             (cond
               ((or (endp lr) (endp rl)) 0)
               (t (max (- (car lr) (car rl)) (dist (cdr lr) (cdr rl))))))
           (merge-profiles (p1 p2)
             (cond
               ((endp p1) p2)
               ((endp p2) p1)
               (t (cons (car p1) (merge-profiles (cdr p1) (cdr p2))))))
           (layout (depth tr)
             (cond
               ((typep tr 'empty*) (list '() (empty) '()))
               ((typep tr 'node*)
                (let* ((tmp1 (layout (+ depth 1) (node*-l tr)))
                       (tmp2 (layout (+ depth 1) (node*-r tr)))
                       (ll (car tmp1))
                       (lll (cadr tmp1))
                       (lr (caddr tmp1))
                       (rl (car tmp2))
                       (rrr (cadr tmp2))
                       (rr (caddr tmp2))
                       (d (+ (/ (dist lr rl) 2) 1))
                       (ll2 (mapcar #'(lambda (x) (- x d)) ll))
                       (lr2 (mapcar #'(lambda (x) (- x d)) lr))
                       (rl2 (mapcar #'(lambda (x) (+ x d)) rl))
                       (rr2 (mapcar #'(lambda (x) (+ x d)) rr)))
                  (list (cons 0 (merge-profiles ll2 rl2))
                        (node (list (node*-val tr) 0 depth)
                              (translate-x (- d) lll)
                              (translate-x d rrr))
                        (cons 0 (merge-profiles rr2 lr2))))))))
    (let* ((tmp (layout 1 tree))
           (l (car tmp))
           (tt (cadr tmp))
           (x-min (reduce #'min l :initial-value 0)))
      (translate-x (- 1 x-min) tt))))


;; L-67 A string representation of binary trees.
(defun string-of-tree (tree)
  (cond
    ((typep tree 'empty*) "_")
    ((typep tree 'node*)
     (let ((data (princ-to-string (node*-val tree))))
       (cond
         ((and (typep (node*-l tree) 'empty*)
               (typep (node*-r tree) 'empty*))
          data)
         (t
           (concatenate 'string
                        data
                        "("
                        (string-of-tree (node*-l tree))
                        ","
                        (string-of-tree (node*-r tree))
                        ")")))))))

(defun tree-of-string (str)
  "e.g.) * tree-of-string \"a(b(_,_)_)\" "
  (labels ((make (ofs s)
             (if (or (>= ofs (length s)) 
                     (char= (schar s ofs) (or #\, #\) #\_)))
               (list (empty) ofs)
               (let ((v (schar s ofs)))
                 (if (and (< (+ ofs 1) (length s))
                          (char= (schar s (+ ofs 1)) #\())
                   (let* ((tmp1 (make (+ ofs 2) s))
                          (l (car tmp1))
                          (ofs1 (cadr tmp1))
                          (tmp2 (make (+ ofs1 1) s))
                          (r (car tmp2))
                          (ofs2 (cadr tmp2)))
                     (list (node v l r) (+ ofs2 1)))
                   (if (char= v #\_)
                     (list (empty) (+ ofs 1))
                     (list (node v (empty) (empty)) (+ ofs 1))))))))
    (car (make 0 str))))


;; L-68 Preorder and inorder sequences of binary trees.
(defun preorder (tree)
  (cond
    ((typep tree 'empty*) '())
    ((typep tree 'node*) (cons (node*-val tree)
                               (append (preorder (node*-l tree))
                                       (preorder (node*-r tree)))))
    (t (error "Argument type error"))))

(defun inorder (tree)
  (cond
    ((typep tree 'empty*) '())
    ((typep tree 'node*) (append (inorder (node*-l tree))
                                 (cons (node*-val tree)
                                       (inorder (node*-r tree)))))
    (t (error "Argument type error"))))

(defun split-pre-in (p i x accp acci)
  (cond
    ((and (endp p) (endp i)) (list (list (reverse accp) (reverse acci))
                                   '(() ())))
    (t
      (let ((h1 (car p))
            (t1 (cdr p))
            (h2 (car i))
            (t2 (cdr i)))
        (if (equal x h2)
          (list (list (cdr (reverse (cons h1 accp))) t1)
                (list (reverse (cdr (cons h2 acci))) t2))
          (split-pre-in t1 t2 x (cons h1 accp) (cons h2 acci)))))))

(defun pre-in-tree (p i)
  (cond
    ((and (endp p) (endp i)) (empty))
    (t
      (let* ((h1 (car p))
             (tmp (split-pre-in p i h1 '() '()))
             (lp (caar tmp))
             (rp (cadar tmp))
             (li (caadr tmp))
             (ri (cadadr tmp)))
        (node h1 (pre-in-tree lp li) (pre-in-tree rp ri))))))


;; L-69 Dotstring representation of binary trees.
(defun dotstring-of-tree (tree)
  (cond
    ((typep tree 'empty*) ".")
    ((typep tree 'node*)
     (let ((data (princ-to-string (node*-val tree))))
       (cond
         ((and (typep (node*-l tree) 'empty*)
               (typep (node*-r tree) 'empty*))
          (concatenate 'string data "." "."))
         (t
           (concatenate 'string
                        data
                        (dotstring-of-tree (node*-l tree))
                        (dotstring-of-tree (node*-r tree)))))))))

(defun tree-of-dotstring (str)
  "e.g.) * tree-of-dotstring \"ab..c..\" "
  (labels ((make (ofs s)
             (if (or (>= ofs (length s)) 
                     (char= (schar s ofs) #\.))
               (list (empty) (+ ofs 1))
               (let ((v (schar s ofs)))
                 (if (< (+ ofs 1) (length s))
                   (let* ((tmp1 (make (+ ofs 1) s))
                          (l (car tmp1))
                          (ofs1 (cadr tmp1))
                          (tmp2 (make ofs1 s))
                          (r (car tmp2)))
                     (list (node v l r) (+ ofs1 1)))
                   (if (char= v #\.)
                     (list (empty) (+ ofs 1))
                     (list (node v (empty) (empty)) (+ ofs 1))))))))
    (car (make 0 str))))
