;; OCaml L-99 problems with Common Lisp
;; Logic and Codes (46~49)

(defpackage :l99-3
  (:use :cl)
  (:export :.var.
           :.not.
           :.and.
           :.or.
           :table
           :table2
           :gray
           ))
(in-package :l99-3)


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
