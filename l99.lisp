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
           :compress
           :pack
           :encode
           :encode2
           :decode
           :duplicate
           :replicate
           :drop
           :split
           :slice
           :slice2
           :rotate
           :remove-at))
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


;; L-10 Run-length encoding of a list.
(defun encode (l)
  (when (listp l)
    (labels ((aux (cont acc ls)
                  (cond
                    ((null ls) '())
                    ((null (cdr ls)) (cons (cons (+ cont 1) (car ls)) acc))
                    ((equal (car ls) (cadr ls)) (aux (+ cont 1) acc (cdr ls)))
                    (t (aux 0 (cons (cons (+ cont 1) (car ls)) acc) (cdr ls))))))
      (revrs (aux 0 '() l)))))


;; L-11/13 Modified run-length encoding.
(defstruct one value)
(defstruct many value)

(defun encode2 (l)
  (when (listp l)
    (labels ((rle (cnt x)
               (if (= cnt 1)
                   (make-one :value elem)
                   (make-many :value (cons cnt x))))
             (aux (cnt acc ls)
                  (cond
                    ((null ls) '())
                    ((null (cdr ls)) (cons (rle (+ cnt 1) (car ls)) acc))
                    ((equal (car ls) (cadr ls)) (aux (+ cnt 1) acc (cdr ls)))
                    (t (aux 0 (cons (rle (+ cnt 1) (car ls)) acc) (cdr ls))))))
    (revrs (aux 0 '() l)))))


;; L-12 Decode a run-length encoded list.
(defun decode (l)
  (when (listp l)
    (labels ((many (acc n x)
               (if (= n 0)
                   acc
                   (many (cons x acc) (- n 1) x)))
             (aux (acc ls)
               (cond
                 ((null ls) acc)
                 ((and (atom (car ls)) (not (null (cdr ls)))) (aux (cons (car ls) acc) (cdr ls)))
                 (t (aux (many acc (caar ls) (cdar ls)) (cdr ls))))))
      (aux '() (revrs l)))))


;; L-14 Duplicate the elements of a list.
(defun duplicate (l)
  (when (listp l)
    (cond
      ((null l) '())
      (t (cons (car l) (cons (car l) (duplicate (cdr l))))))))


;; L-15 Replicate the elements of a list given number of times.
(defun replicate (l n)
  (when (listp l)
    (labels ((prepend (nn acc x)
               (if (= nn 0)
                   acc
                   (prepend (- nn 1) (cons x acc) x)))
             (aux (acc ls)
               (cond
                 ((null ls) acc)
                 (t (aux (prepend n acc (car ls)) (cdr ls))))))
      (aux '() (revrs l)))))


;; L-16 Drop every N'th element from a list.
(defun drop (l n)
  (when (listp l)
    (labels ((aux (i ls)
               (cond
                 ((null ls) '())
                 ((= i n) (aux 1 (cdr ls)))
                 (t (cons (car ls) (aux (+ i 1) (cdr ls)))))))
      (aux 1 l))))


;; L-17 Split a list into two parts. The length of the first part is given.
(defun split (l n)
  (when (listp l)
    (labels ((aux (i acc ls)
               (cond
                 ((null ls) (list (revrs acc) '()))
                 ((= i 0) (list (revrs acc) (cdr ls)))
                 (t (aux (- i 1) (cons (car ls) acc) (cdr ls))))))
      (aux n '() l))))


;; L-18 Extract a slice from a list. (Not tail recursive)
(defun slice (l i k)
  (when (listp l)
    (labels ((take (n ls)
               (cond
                 ((or (null ls) (= n 0)) '())
                 (t (cons (car ls) (take (- n 1) (cdr ls))))))
             (drop (n ls)
               (cond
                 ((null ls) '())
                 ((= n 0) ls)
                 (t (drop (- n 1) (cdr ls))))))
      (take (+ 1 (- k i)) (drop i l)))))


;; L-18-2
(defun fold-until (f acc n l)
  (when (listp l)
    (cond
      ((null l) (list acc '()))
      ((= n 0) (list acc (cdr l)))
      (t (fold-until f (funcall f acc (car l)) (- n 1) (cdr l))))))

(defun slice2 (l i k)
  (let* ((ls (cdr (fold-until (lambda (x y) (declare (ignore x y)) '()) '() i l)))
         (taken (car (fold-until (lambda (acc h) (cons h acc)) '() (+ (- k i) 1) ls))))
    ; WIP
    (flltn (revrs taken))))


;; L-19 Rotate a list N places to the left.
(defun rotate (l n)
  (when (listp l)
    (let* ((len (leng l))
           (nn (if (= len 0)
                   0
                   (mod (mod n (+ len len)) len))))
      (if (= nn 0)
          l
          (let* ((ab (split l n)))
            ; WIP
            (flltn (append (cdr ab) (car ab))))))))


;; L-20 Remove the K'th element from a list.
(defun remove-at (n l)
  (when (listp l)
    (cond
      ((null l) '())
      ((= n 0) (cdr l))
      (t (cons (car l) (remove-at (- n 1) (cdr l)))))))