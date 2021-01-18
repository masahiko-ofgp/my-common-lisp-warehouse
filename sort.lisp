(defpackage :sort
  (:use :cl)
  (:export :isort
           :bsort))
(in-package :sort)

(defun %isort (elem lst)
  (cond
    ((endp lst) (list elem))
    (t
      (if (<= elem (car lst))
        (cons elem lst)
        (cons (car lst) (%isort elem (cdr lst)))))))

(defun isort (lst)
  "Insertion Sort"
  (cond
    ((endp lst) '())
    (t (%isort (car lst) (isort (cdr lst))))))


(defun %nil-to-zero (a)
  (if (null a)
    0
    a))

(defun bsort (lst)
  "Bubble Sort"
  (labels ((aux (l)
             (let ((x (car l))
                   (x2 (cadr l))
                   (xs (cddr l)))
               (cond
                 ((or (endp l) (endp (cdr l))) l)
                 (t
                   (if (> x (%nil-to-zero x2))
                       (cons x2 (aux (cons x xs)))
                       (cons x (aux (cons x2 xs)))))))))
    (let ((tmp (aux lst)))
        (if (equal tmp lst)
          tmp
          (bsort tmp)))))
