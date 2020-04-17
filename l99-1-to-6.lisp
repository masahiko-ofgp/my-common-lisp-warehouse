;;; OCaml L-99 problems with Common Lisp

(defpackage :l99-1-to-6
  (:use :cl)
  (:export :lst
           :lst-two
           :at
           :leng
           :revrs
           :palindromep))
(in-package :l99-1-to-6)


;; L-01 Return the last element of a list.
(defun lst (l)
  (when (typep l 'list)
    (cond
      ((null l) nil)
      ((eql (cdr l) nil) (car l))
      (t (lst (cdr l))))))


;; L-02 Find the last but one (last and penultimate) elements of a list.
(defun lst-two (l)
  (when (typep l 'list)
    (cond
      ((or (null l) (null (cdr l))) nil)
      ((null (cddr l)) l)
      (t (lst-two (cdr l))))))


;; L-03 Find the k's element of a list.
(defun at (l k)
  (when (typep l 'list)
    (cond
      ((null l) nil)
      (t (if (= k 1)
             (car l)
             (at (cdr l) (1- k)))))))


;; L-04 Find the number of elements of a list.
(defun leng (l)
  (when (typep l 'list)
    (labels ((aux (n ls)
               (if (null ls)
                   n
                   (aux (1+ n) (cdr ls)))))
    (aux 0 l))))

;; L-05 Reverse a list.
(defun revrs (l)
  (when (typep l 'list)
    (labels ((aux (acc ls)
               (if (null ls)
                   acc
                   (aux (cons (car ls) acc) (cdr ls)))))
    (aux '() l))))
      

;; L-06 Find out whether a list is a palindrome.
(defun palindromep (l)
  (when (typep l 'list)
    (equal l (revrs l))))
