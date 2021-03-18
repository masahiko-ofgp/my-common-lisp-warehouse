(in-package :cl-user)
(defpackage :factorial
  (:use :cl)
  (:export :fact))
(in-package :factorial)

(defun fact-cps (n cont)
  (if (zerop n)
    (funcall cont 1)
    (fact-cps (1- n) (lambda (x) (funcall cont (* n x))))))

;; factorial (continue passing style)
(defun fact (n)
  (fact-cps n #'(lambda (x) x)))
