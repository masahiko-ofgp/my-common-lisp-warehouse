;;; OCaml L-99 problems with Common Lisp

(defpackage :l99
  (:use :cl)
  (:export :lst
           :lst-two
           :at
           :leng
           :revrs
           :palindromep
           :flltn
           :compress))
(in-package :l99)


;; L-01 Return the last element of a list.
(defun lst (l)
  (when (listp l)
    (cond
      ((null l) nil)
      ((eql (cdr l) nil) (car l))
      (t (lst (cdr l))))))


;; L-02 Find the last but one (last and penultimate) elements of a list.
(defun lst-two (l)
  (when (listp l)
    (cond
      ((or (null l) (null (cdr l))) nil)
      ((null (cddr l)) l)
      (t (lst-two (cdr l))))))


;; L-03 Find the k's element of a list.
(defun at (l k)
  (when (listp l)
    (cond
      ((null l) nil)
      (t (if (= k 1)
             (car l)
             (at (cdr l) (1- k)))))))


;; L-04 Find the number of elements of a list.
(defun leng (l)
  (when (listp l)
    (labels ((aux (n ls)
               (if (null ls)
                   n
                   (aux (1+ n) (cdr ls)))))
    (aux 0 l))))

;; L-05 Reverse a list.
(defun revrs (l)
  (when (listp l)
    (labels ((aux (acc ls)
               (if (null ls)
                   acc
                   (aux (cons (car ls) acc) (cdr ls)))))
    (aux '() l))))
      

;; L-06 Find out whether a list is a palindrome.
(defun palindromep (l)
  (when (listp l)
    (equal l (revrs l))))


;; L-07 Flatten a nested list structure.
(defun flltn (l)
  (when (listp l)
    (labels ((aux (acc ls)
                  (cond
                    ((null ls) acc)
                    ((atom (car ls)) (aux (cons (car ls) acc) (cdr ls)))
                    (t (aux (aux acc (car ls)) (cdr ls))))))
      (revrs (aux '() l)))))


;; L-08 Eliminate consecutive duplicates of list elements.
(defun compress (l)
  (when (listp l)
    (cond
      ((or (null l) (null (cdr l))) l)
      ((equal (car l) (cadr l)) (compress (cdr l)))
      ((not (equal (car l) (cadr l))) (cons (car l) (compress (cdr l))))
      (t l))))


;; L-09 Pack consecutive duplicates of list elements into sublists.
(defun pack (l)
  (when (listp l)
    (labels ((aux (current acc ls)
               (cond
                 ((null ls) ls)
                 ((null (cdr ls)) (cons (cons (car ls) current) acc))
                 ((equal (car ls) (cadr ls)) (aux (cons (car ls) current) acc (cdr ls)))
                 (t (aux '() (cons (cons (car ls) current) acc) (cdr ls))))))
      (revrs (aux '() '() l)))))