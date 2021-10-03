(in-package :cl-user)
(defpackage :sort
  (:use :cl)
  (:export :isort
           :bsort
           :qsort
           :qsort2
           :msort))
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


(defun %nil-to-zero (a) (if a a 0))
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


(defun qsort (lst)
  "Quick Sort"
  ;(let ((pivot (car lst)))
  (let ((pivot (nth (round (/ (length lst) 2)) lst)))
    (if (cdr lst)
      (nconc (qsort (remove-if-not #'(lambda (x) (< x pivot)) lst))
             (remove-if-not #'(lambda (x) (= x pivot)) lst)
             (qsort (remove-if-not #'(lambda (x) (> x pivot)) lst)))
      lst)))

(defun %partition (p l)
  (labels ((aux (p y n l)
             (if (endp l)
               (list (nreverse y) (nreverse n))
               (if (funcall p (car l))
                 (aux p (cons (car l) y) n (cdr l))
                 (aux p y (cons (car l) n) (cdr l))))))
    (let ((result (aux p '() '() l)))
      result)))
(defun qsort2 (lst)
  "Quick Sort version 2"
  (cond
    ((endp lst) lst)
    (t
      (let* ((result (%partition #'(lambda (x) (< x (car lst))) (cdr lst)))
             (l (car result))
             (r (cadr result)))
        (nconc (qsort2 l) (cons (car lst) (qsort2 r)))))))


(defun %merge (l1 l2)
  (cond
    ((endp l1) l2)
    ((endp l2) l1)
    (t
      (if (< (car l1) (car l2))
        (cons (car l1) (%merge (cdr l1) l2))
        (cons (car l2) (%merge l1 (cdr l2)))))))
(defun msort (lst)
  "Merge Sort"
  (cond
    ((or (endp lst) (= (length lst) 1)) lst)
    (t
      (let* ((mid (round (/ (length lst) 2)))
             (l (msort (subseq lst 0 mid)))
             (r (msort (subseq lst mid))))
        (%merge l r)))))
