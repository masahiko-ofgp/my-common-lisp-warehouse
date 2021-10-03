;; OCaml L-99 problems with Common Lisp
;; Logic and Codes (46~50)
(in-package :cl-user)
(defpackage :logic-and-codes
  (:use :cl)
  (:export :.var.
           :.not.
           :.and.
           :.or.
           :table
           :table2
           :gray
           :huffman
           ))
(in-package :logic-and-codes)


(defclass var. ()
  ((val
    :initarg  :val
    :accessor val)))
(defun .var. (val)
  (make-instance 'var. :val val))


(defclass not. ()
  ((e
    :initarg  :e
    :accessor e)))
(defun .not. (e)
  (make-instance 'not. :e e))


(defclass and. ()
  ((e1
    :initarg  :e1
    :accessor e1)
   (e2
    :initarg :e2
    :accessor e2)))
(defun .and. (e1 e2)
  (make-instance 'and. :e1 e1 :e2 e2))


(defclass or. ()
  ((e1
    :initarg  :e1
    :accessor e1)
   (e2
    :initarg :e2
    :accessor e2)))
(defun .or. (e1 e2)
  (make-instance 'or. :e1 e1 :e2 e2))


(defgeneric get-ex (kind))
(defmethod get-ex ((kind var.))
  (with-accessors ((val val)) kind
    val))
(defmethod get-ex ((kind not.))
  (with-accessors ((e e)) kind
    e))
(defmethod get-ex ((kind and.))
  (with-accessors ((e1 e1) (e2 e2)) kind
    (list e1 e2)))
(defmethod get-ex ((kind or.))
  (with-accessors ((e1 e1) (e2 e2)) kind
    (list e1 e2)))


(defgeneric get-e1 (kind))
(defmethod get-e1 ((kind and.))
  (with-accessors ((e1 e1)) kind
    e1))
(defmethod get-e1 ((kind or.))
  (with-accessors ((e1 e1)) kind
    e1))


(defgeneric get-e2 (kind))
(defmethod get-e2 ((kind and.))
  (with-accessors ((e2 e2)) kind
    e2))
(defmethod get-e2 ((kind or.))
  (with-accessors ((e2 e2)) kind
    e2))


;; L-46,47 Truth tables for logical expressions (2 variables)
(defun evl (a val-a b val-b expr)
  (cond
    ((typep expr 'var.) (cond
                          ((equal (get-ex expr) a) val-a)
                          ((equal (get-ex expr) b) val-b)
                          (t (error "Invalid variable"))))
    ((typep expr 'not.) (not (evl a val-a b val-b (get-ex expr))))
    ((typep expr 'and.) (and (evl a val-a b val-b (get-e1 expr))
                             (evl a val-a b val-b (get-e2 expr))))
    ((typep expr 'or.) (or (evl a val-a b val-b (get-e1 expr))
                           (evl a val-a b val-b (get-e2 expr))))
    (t (error "Invalid variable"))))

(defun table (a b expr)
  "e.g.)
  * (defvar x (.and. (.var. #\a) (.or. (.var. #\a) (.var. #\b))))
  X
  * (table #\a #\b x)
  ((T T T) (T NIL T) (NIL T NIL) (NIL NIL NIL))
  "
  (list (list t t (evl a t b t expr))
        (list t nil (evl a t b nil expr))
        (list nil t (evl a nil b t expr))
        (list nil nil (evl a nil b nil expr))))


;; L-48 Truth tables for logical expressions.
(defun evl2 (val-vars expr)
  (cond
    ((typep expr 'var.) (loop for i in val-vars
                           when (eql (get-ex expr) (car i))
                           return (cadr i)))
    ((typep expr 'not.) (not (evl2 val-vars expr)))
    ((typep expr 'and.) (and (evl2 val-vars (get-e1 expr))
                             (evl2 val-vars (get-e2 expr))))
    ((typep expr 'or.) (or (evl2 val-vars (get-e1 expr))
                           (evl2 val-vars (get-e2 expr))))))
(defun table-make (val-vars vars expr)
  (cond
    ((endp vars) (list (list (reverse val-vars) (evl2 val-vars expr))))
    (t
     (append (table-make (cons (list (car vars) t) val-vars) (cdr vars) expr)
             (table-make (cons (list (car vars) nil) val-vars) (cdr vars) expr)))))
(defun table2 (vars expr)
  "e.g.)
  * (defvar a (.var. #\a))
  * (defvar b (.var. #\b))
  * (defvar c (.var. #\c))
  * (defvar ex (.or. (.and. a (.or. b c)) (.or. (.and. a b) (.and. a c))))
  * (table2 '(#\a #\b #\c) ex)
  "
  (table-make '() vars expr))
      

;; L-49 Gray code
(defun ^ (s1 s2) (concatenate 'string s1 s2))

(defun rev-append (l1 l2)
  (append (reverse l1) l2))

(defun gray (n)
  (labels ((gray-next-level (k l)
             (if (< k n)
                 (let* ((tmp (reduce #'(lambda (x y)
                                         (list (cons (^ "0" y) (car x))
                                               (cons (^ "1" y) (cadr x))))
                                     l
                                     :initial-value '(() ())))
                        (first-half (car tmp))
                        (second-half (cadr tmp)))
                   (gray-next-level (+ k 1) (rev-append first-half second-half)))
                 l)))
    (gray-next-level 1 '("0" "1"))))
    
    
;; L-50 Huffman code

;;;;;; Option Class ;;;;;;;;;;;;;;;;;;

(defclass some* ()
  ((val
    :initarg :val
    :accessor val)))
(defclass none* () nil)

(defgeneric unwrap (kind))
(defmethod  unwrap ((kind some*))
  (with-accessors ((val val)) kind
    val))
(defmethod unwrap ((kind none*)) nil)

(defun option (&optional x)
  (cond
    ((null x) (make-instance 'none*))
    (t
     (make-instance 'some* :val x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; Tree Class ;;;;;;;;;;;;;;;;;;

(defclass leaf ()
  ((val
    :initarg :val
    :accessor val)))
(defclass node ()
  ((v1
    :initarg :v1
    :accessor v1)
   (v2
    :initarg :v2
    :accessor v2)))

(defgeneric get-val (kind))
(defmethod get-val ((kind leaf))
  (with-accessors ((val val)) kind
    val))
(defmethod get-val ((kind node))
  (with-accessors ((v1 v1) (v2 v2)) kind
    (list v1 v2)))

(defun tree (x &optional y)
  (cond
    ((null y) (make-instance 'leaf :val x))
    (t (make-instance 'node :v1 x :v2 y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass pq ()
  ((data
    :initarg :data
    :initform (make-array 101 :initial-element '())
    :accessor data)
   (fst
    :initarg :fst
    :initform 101
    :accessor fst)))
(defmethod add ((q pq) p x)
  (progn
    (setf (elt (slot-value q 'data) p)
          (cons x (elt (slot-value q 'data) p)))
    (setf (slot-value q 'fst) (min p (slot-value q 'fst)))))
(defmethod get-min ((q pq))
  (if (= (slot-value q 'fst) 101)
      (option nil)
      (let ((l (elt (slot-value q 'data) (slot-value q 'fst))))
        (cond
          ((endp l) (error "False"))
          (t
           (let ((p (slot-value q 'fst)))
             (progn
               (setf (elt (slot-value q 'data) (slot-value q 'fst)) (cdr l))
               (loop
                  :while (and (< (slot-value q 'fst) 101)
                              (endp (elt (slot-value q 'data) (slot-value q 'fst))))
                  :do (setf (slot-value q 'fst) (+ 1 (slot-value q 'fst))))
               (option (list p (car l))))))))))


(defun huffman-tree (q)
  (let ((x (get-min q))
        (y (get-min q)))
    (cond
      ((and (typep x 'some*) (typep y 'some*)) (progn
                                                 (add q
                                                      (+ (car (unwrap x))
                                                         (car (unwrap y)))
                                                      (tree (cadr (unwrap x))
                                                            (cadr (unwrap y))))
                                                 (huffman-tree q)))
      ((and (typep x 'some*) (typep y 'none*)) (cadr (unwrap x)))
      ((and (typep x 'none*) (typep y 'some*)) (cadr (unwrap y)))
      (t (error "False")))))

(defun prefixes-of-tree (q &optional prefix)
  (cond
    ((typep q 'leaf) (list (list (get-val q) prefix)))
    ((typep q 'node) (let ((t0 (car (get-val q)))
                           (t1 (cadr (get-val q))))
                       (if (null prefix)
                         (append (prefixes-of-tree t0 "0")
                                 (prefixes-of-tree t1 "1"))
                         (append (prefixes-of-tree t0 (concatenate 'string  prefix "0"))
                                 (prefixes-of-tree t1 (concatenate 'string prefix "1"))))))))

(defun huffman (fs)
  (when (= (reduce #'(lambda (s p) (+ s (cadr p))) fs :initial-value 0) 100)
    (let ((q (make-instance 'pq)))
      (progn
        (dolist (i fs)
          (add q (cadr i) (tree (car i))))
        (prefixes-of-tree (huffman-tree q))))))
