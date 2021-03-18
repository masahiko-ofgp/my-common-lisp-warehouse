;; OCaml L-99 problems with Common Lisp
;; Arithmetic (31~41)
(in-package :cl-user)
(defpackage :l99-2
  (:use :cl)
  (:export :primep
           :make-primes-list
           :primep2
           :gcd-
           :ggcd
           :coprime
           :phi
           :factors
           :factors2
           :phi-improved
           :timeit
           :all-primes
           :goldbach
           :golcbach-limit))
(in-package :l99-2)

;; L-31 Determine whether a given integer number is prime.
;; version 1
(defun primep (n)
  (let* ((nn (abs n)))
    (labels ((is-not-divisor (d)
               (or (> (* d d) nn)
                   (and (/= (mod nn d) 0)
                        (is-not-divisor (+ d 1))))))
      (and (/= nn 1)
           (is-not-divisor 2)))))


;; version 2
(defun filter (n l)
  (if (null l)
      nil
      (if (= (mod (car l) n) 0)
          (filter n (cdr l))
          (cons (car l) (filter n (cdr l))))))

(defun range (s e)
  (if (> s e)
      nil
      (loop for i from s upto e collect i)))

(defun make-primes-list (n)
  "Can be used with L-39.
   e.g.)
   * (length (make-primes-list 7920))
   1000
  "
  (labels ((aux (acc ls)
             (cond
               ((endp ls) acc)
               (t
                (if (< (car ls) (sqrt n))
                    (aux (cons (car ls) acc) (filter (car ls) (cdr ls)))
                    (append (cons (car ls) acc) (cdr ls)))))))
    (let ((primes (aux '() (range 2 n))))
      (sort primes '<))))

(defun primep2 (n)
  (unless (< n 2)
    (if (eql (find n (make-primes-list n)) nil)
        nil
        t)))


;; L-32 Determine the greatest common divisor of two positive integer.
;; version 1
(defun gcd- (a b)
  "e.g.)
  * (gcd- 12 24)
  12
  "
  (if (zerop b)
      a
      (gcd b (mod a b))))

;; version 2
(defun ggcd (a b &rest r)
  "e.g.)
  * (ggcd 12 24 36)
  12
  "
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


;; L-36 Determine the prime factors of a given positive integer (2).
(defun factors2 (n)
  (labels ((aux (d n)
             (cond
               ((= n 1) nil)
               (t
                (if (zerop (mod n d))
                    (let* ((l (aux d (/ n d))))
                      (if (eql (caar l) d)
                          (cons (list (caar l) (+ (cadar l) 1)) (cdr l))
                          (cons (list d 1) l)))
                    (aux (+ d 1) n))))))
    (aux 2 n)))


;; L-37 Calculate Euler's totient function φ(m) (improved).
(defun pow (n p)
  (if (< p 1)
      1
      (* n (pow n (- p 1)))))

(defun phi-improved (n)
  (labels ((aux (acc l)
             (cond
               ((endp l) acc)
               (t
                (let ((p (caar l))
                      (m (cadar l)))
                  (aux (* (* (- p 1) (pow p (- m 1))) acc) (cdr l)))))))
    (aux 1 (factors2 n))))


;; L-38 Compare the two methods of calculating Euler's totient function.
;; * (timeit #'phi 10090)
;; * (timeit #'phi-improved 10090)
(defun timeit (f n)
  (let ((run-time (get-internal-run-time))
        (real-time (get-internal-real-time)))
    (progn
      (funcall f n)
      (format t "Run Time: ~,6F~%" (/ (- (get-internal-run-time)
                                         run-time)
                                      internal-time-units-per-second))
      (format t "Real Time: ~,6F~%" (/ (- (get-internal-real-time)
                                          real-time)
                                       internal-time-units-per-second)))))


;; L-39 A list of prime numbers.
(defun all-primes (a b)
  (if (> a b)
      nil
      (let ((rst (all-primes (+ a 1) b)))
        (if (primep a)
            (cons a rst)
            rst))))


;; L-40 Goldbach's conjecture.
(defun goldbach (n)
  (labels ((aux (d)
             (if (and (primep d) (primep (- n d)))
                 (list d (- n d))
                 (aux (+ d 1)))))
    (aux 2)))


;; L-41 A list of Goldbach compositions.
(defun goldbach-list (a b)
  (if (> a b)
      '()
      (if (= (mod a 2) 1)
          (goldbach-list (+ a 1) b)
          (cons (list a (goldbach a)) (goldbach-list (+ a 2) b)))))

(defun goldbach-limit (a b lim)
  (let ((l (goldbach-list a b)))
    (loop for x in l when (and (> (caadr x) lim) (> (cadadr x) lim))
         collect x)))
