;; OCaml L-99 problems with Common Lisp
;; Logic and Codes (46~47)

(defpackage :l99-3
  (:use :cl)
  (:export :var.
           :not.
           :and.
           :or.
           :get-ex
           :get-e1
           :get-e2
           :evl
           :table
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
