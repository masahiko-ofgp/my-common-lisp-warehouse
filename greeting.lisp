(defpackage :greeting
  (:use :cl)
  (:export :hello
           :hello2))
(in-package :greeting)

(defun hello ()
  (format t "Hello, world!!~%"))

(defun hello2 (name)
  (format t "Hello, ~A!!~%" name))
