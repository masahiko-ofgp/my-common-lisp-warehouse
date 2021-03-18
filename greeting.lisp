(in-package :cl-user)
(defpackage :greeting
  (:use :cl)
  (:export :hello))
(in-package :greeting)

(defun hello (&optional name)
  (cond
    ((null name) (hello "world"))
    (t (format t "Hello, ~A!!~%" name))))
