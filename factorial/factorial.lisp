(in-package :cl-user)
(defpackage :factorial
  (:use :cl)
  (:export :fact-normal
           :fact-cps))
(in-package :factorial)


(defun fact-normal (n)
  (if (<= n 1)
    1
    (* n (fact-normal (1- n)))))

(defun fact-cps (n)
  (labels ((aux (n f)
             (if (zerop n)
               (funcall f 1)
               (aux (1- n) (lambda (x) (funcall f (* n x)))))))
    (aux n #'(lambda (x) x))))
