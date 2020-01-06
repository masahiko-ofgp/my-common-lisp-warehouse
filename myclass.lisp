(defpackage :myclass
  (:use :cl)
  (:export :main))
(in-package :myclass)

(defclass dog ()
  ((name
     :initarg :name
     :initform (error "required")
     :accessor name)))

(defclass person ()
  ((name
     :initarg :name
     :initform (error "required")
     :accessor name)
   (email
     :initarg :email
     :initform (error "required")
     :accessor email)))

(defgeneric get-name (kind))

(defmethod get-name ((kind person))
  (with-accessors ((n name)) kind
    (format t "~A~%" n)))

(defmethod get-name ((kind dog))
  (with-accessors ((n name)) kind
    (format t "~A~%" n)))

(defmethod get-email ((kind person))
  (with-accessors ((e email)) kind
    (format t "~A~%" e)))

(defun main ()
  (let ((tom (make-instance 'person :name "Tom" :email "tom@tom.com"))
        (hachi (make-instance 'dog :name "Hachi")))
    (get-name tom)
    (get-email tom)
    (get-name hachi)))
