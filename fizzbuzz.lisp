(defpackage :fizzbuzz
  (:use :cl)
  (:export :run-dotimes-fizzbuzz
           :run-loop-fizzbuzz))
(in-package :fizzbuzz)

(defun fzbz (n)
  (let ((fizzp (zerop (mod n 3)))
        (buzzp (zerop (mod n 5))))
    (cond
      ((and fizzp buzzp) "Fizzbuzz")
      (fizzp "Fizz")
      (buzzp "Buzz")
      (t n))))

;; dotimes
(defun run-dotimes-fizzbuzz (num)
  (dotimes (i num) (print (fzbz i))))

;; loop
(defun run-loop-fizzbuzz (num)
  (loop for i upto num do (print (fzbz i))))
