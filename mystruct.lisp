(in-package :cl-user)
(defpackage :mystruct
  (:use :cl)
  (:export :main))
(in-package :mystruct)

(defstruct person name email)

(defstruct dog name)

(defun main ()
  (let ((tom (make-person :name "Tom" :email "tom@tom.com"))
        (hachi (make-dog :name "Hachi")))
    (progn
      (format t "~A~%" (typep tom 'person))
      (format t "~A~%" (person-name tom))
      (format t "~A~%" (person-email tom))
      (format t "~A~%" (typep hachi 'dog))
      (format t "~A~%" (dog-name hachi)))))
