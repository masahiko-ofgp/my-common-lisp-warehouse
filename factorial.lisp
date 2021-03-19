(in-package :cl-user)
(defpackage :factorial
  (:use :cl)
  (:export :fact))
(in-package :factorial)


(defun fact (n)
  (labels ((aux (n f)
             (if (zerop n)
               (funcall f 1)
               (aux (1- n) (lambda (x) (funcall f (* n x)))))))
    (aux n #'(lambda (x) x))))
