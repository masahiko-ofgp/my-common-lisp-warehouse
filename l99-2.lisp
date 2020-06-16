;; OCaml L-99 problems with Common Lisp
;; Arithmetic (31~ )

(defpackage :l99-2
  (:use :cl)
  (:export :primep))
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