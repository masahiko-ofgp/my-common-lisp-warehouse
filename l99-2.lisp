;; OCaml L-99 problems with Common Lisp
;; Arithmetic (31~ )

(defpackage :l99-2
  (:use :cl)
  (:export :primep
           :primep2))
(in-package :l99-2)

;; L-31 Determine whether a given integer number is prime.

; version 1
(defun primep (n)
  (let* ((nn (abs n)))
    (labels ((is-not-divisor (d)
               (or (> (* d d) nn)
                   (and (/= (mod nn d) 0)
                        (is-not-divisor (+ d 1))))))
      (and (/= nn 1)
           (is-not-divisor 2)))))

; version 2
(defun filter (n l)
  (loop for i in l when (/= (mod i n) 0)
        collect i))

(defun primep2 (n)
  (let ((l (loop for i from 2 upto n collect i)))
    (labels ((make-primes (acc ls)
               (cond
                 ((endp ls) acc)
                 (t
                  (if (< (car ls) (sqrt n))
                      (make-primes (cons (car ls) acc) (filter (car ls) (cdr ls)))
                      (append (cons (car ls) acc) (cdr ls)))))))
      (let ((primes (make-primes '() l)))
        (unless (< n 2)
            (if (eql (find n primes) nil)
                nil
                t))))))
            