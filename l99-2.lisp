;; OCaml L-99 problems with Common Lisp
;; Arithmetic (31~34)

(defpackage :l99-2
  (:use :cl)
  (:export :primep
           :primep2
           :gcd-
           :ggcd
           :coprime
           :phi))
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

;; L-32 Determine the greatest common divisor of two positive integer.

; version 1
; e.g.)
; * (gcd- 12 24)
; 12
(defun gcd- (a b)
  (if (zerop b)
      a
      (gcd b (mod a b))))

; version 2
; e.g.)
; * (ggcd 12 24 36)
; 12
(defun ggcd (a b &rest r)
  (labels ((aux (a b)
             (if (zerop b)
                 a
                 (aux b (mod a b)))))
    (cond
      ((endp r) (aux a b))
      (t
       (reduce #'aux (cons a (cons b r)))))))


;; L-33 Determine whether two positive integer numbers are coprime.
(defun coprime (a b)
  (eql (gcd- a b) 1))


;; L-34 Calculate Euler's totient function φ(m).
(defun phi (n)
  (labels ((count-coprime (acc d)
             (if (< d n)
                 (count-coprime (if (coprime n d) (+ acc 1) acc) (+ 1 d))
                 acc)))
    (if (= n 1)
        1
        (count-coprime 0 1))))


;; L-35 Determine the prime factors of a given positive integer.
(defun factors (n)
  (labels ((aux (d n)
             (if (= n 1)
                 nil
                 (if (zerop (mod n d))
                     (cons d (aux d (/ n d)))
                     (aux (+ d 1) n)))))
    (aux 2 n)))