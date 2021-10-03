(in-package :cl-user)
(defpackage :hanoi
  (:use :cl)
  (:export :run-hanoi))
(in-package :hanoi)


(defun hanoi-main (n a b c)
  (when (> n 0)
    (hanoi-main (- n 1) a c b)
    (format t "~A => ~A~%" a c)
    (hanoi-main (- n 1) b a c)))

(defun run-hanoi (disks)
  (hanoi-main disks #\A #\B #\C))
