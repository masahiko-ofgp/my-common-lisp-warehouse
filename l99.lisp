;;; OCaml L-99 problems with Common Lisp
;; Working with lists (1 ~ 28)
;; L-27 is two patterns. (OCaml version and Haskell version)

(defpackage :l99
  (:use :cl)
  (:export :lst
           :lst-two
           :at
           :leng
           :revrs
           :palindromep
           :flttn
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
           :remove-at
           :insert-at
           :range
           :rand-select
           :lotto-select
           :permutation
           :extract
           :group-ml
           :group-hs
           :length-sort
           :frequency-sort))
(in-package :l99)


;; L-01 Return the last element of a list.
(defun lst (l)
  (when (listp l)
    (cond
      ((endp l) nil)
      ((endp (cdr l)) (car l))
      (t (lst (cdr l))))))


;; L-02 Find the last but one (last and penultimate) elements of a list.
(defun lst-two (l)
  (when (listp l)
    (cond
      ((endp (or l (cdr l))) nil)
      ((endp (cddr l)) l)
      (t (lst-two (cdr l))))))


;; L-03 Find the k's element of a list.
(defun at (l k)
  (when (listp l)
    (cond
      ((endp l) nil)
      (t (if (= k 1)
             (car l)
             (at (cdr l) (1- k)))))))


;; L-04 Find the number of elements of a list.
(defun leng (l)
  (when (listp l)
    (labels ((aux (n ls)
               (if (endp ls)
                   n
                   (aux (1+ n) (cdr ls)))))
    (aux 0 l))))

;; L-05 Reverse a list.
(defun revrs (l)
  (when (listp l)
    (labels ((aux (acc ls)
               (if (endp ls)
                   acc
                   (aux (cons (car ls) acc) (cdr ls)))))
    (aux '() l))))
      

;; L-06 Find out whether a list is a palindrome.
(defun palindromep (l)
  (when (listp l)
    (equal l (revrs l))))


;; L-07 Flatten a nested list structure.
(defun flttn (l)
  (when (listp l)
    (labels ((aux (acc ls)
                  (cond
                    ((endp ls) acc)
                    ((atom (car ls)) (aux (cons (car ls) acc) (cdr ls)))
                    (t (aux (aux acc (car ls)) (cdr ls))))))
      (revrs (aux '() l)))))


;; L-08 Eliminate consecutive duplicates of list elements.
(defun compress (l)
  (when (listp l)
    (cond
      ((endp (or l (cdr l))) l)
      ((equal (car l) (cadr l)) (compress (cdr l)))
      ((not (equal (car l) (cadr l))) (cons (car l) (compress (cdr l))))
      (t l))))


;; L-09 Pack consecutive duplicates of list elements into sublists.
(defun pack (l)
  (when (listp l)
    (labels ((aux (current acc ls)
               (cond
                 ((endp ls) ls)
                 ((endp (cdr ls)) (cons (cons (car ls) current) acc))
                 ((equal (car ls) (cadr ls)) (aux (cons (car ls) current) acc (cdr ls)))
                 (t (aux '() (cons (cons (car ls) current) acc) (cdr ls))))))
      (revrs (aux '() '() l)))))


;; L-10 Run-length encoding of a list.
(defun encode (l)
  (when (listp l)
    (labels ((aux (cont acc ls)
                  (cond
                    ((endp ls) '())
                    ((endp (cdr ls)) (cons (cons (+ cont 1) (car ls)) acc))
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
                   (make-one :value x)
                   (make-many :value (cons cnt x))))
             (aux (cnt acc ls)
                  (cond
                    ((endp ls) '())
                    ((endp (cdr ls)) (cons (rle (+ cnt 1) (car ls)) acc))
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
                 ((endp ls) acc)
                 ((and (atom (car ls)) (not (endp (cdr ls)))) (aux (cons (car ls) acc) (cdr ls)))
                 (t (aux (many acc (caar ls) (cdar ls)) (cdr ls))))))
      (aux '() (revrs l)))))


;; L-14 Duplicate the elements of a list.
(defun duplicate (l)
  (when (listp l)
    (cond
      ((endp l) '())
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
                 ((endp ls) acc)
                 (t (aux (prepend n acc (car ls)) (cdr ls))))))
      (aux '() (revrs l)))))


;; L-16 Drop every N'th element from a list.
(defun drop (l n)
  (when (listp l)
    (labels ((aux (i ls)
               (cond
                 ((endp ls) '())
                 ((= i n) (aux 1 (cdr ls)))
                 (t (cons (car ls) (aux (+ i 1) (cdr ls)))))))
      (aux 1 l))))


;; L-17 Split a list into two parts. The length of the first part is given.
(defun split (l n)
  (when (listp l)
    (labels ((aux (i acc ls)
               (cond
                 ((endp ls) (list (revrs acc) '()))
                 ((= i 0) (list (revrs acc) (cdr ls)))
                 (t (aux (- i 1) (cons (car ls) acc) (cdr ls))))))
      (aux n '() l))))


;; L-18 Extract a slice from a list. (Not tail recursive)
(defun slice (l i k)
  (when (listp l)
    (labels ((take (n ls)
               (cond
                 ((or (endp ls) (= n 0)) '())
                 (t (cons (car ls) (take (- n 1) (cdr ls))))))
             (drop (n ls)
               (cond
                 ((endp ls) '())
                 ((= n 0) ls)
                 (t (drop (- n 1) (cdr ls))))))
      (take (+ 1 (- k i)) (drop i l)))))


;; L-18-2
(defun fold-until (f acc n l)
  (when (listp l)
    (cond
      ((endp l) (list acc '()))
      ((= n 0) (list acc (cdr l)))
      (t (fold-until f (funcall f acc (car l)) (- n 1) (cdr l))))))

(defun slice2 (l i k)
  (let* ((ls (cdr (fold-until (lambda (x y) (declare (ignore x y)) '()) '() i l)))
         (taken (car (fold-until (lambda (acc h) (cons h acc)) '() (+ (- k i) 1) ls))))
    (flttn (revrs taken))))


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
            (flttn (append (cdr ab) (car ab))))))))


;; L-20 Remove the K'th element from a list.
(defun remove-at (n l)
  (when (listp l)
    (cond
      ((endp l) '())
      ((= n 0) (cdr l))
      (t (cons (car l) (remove-at (- n 1) (cdr l)))))))


;; L-21 Insert an element at a given position into a list.
(defun insert-at (x n l)
  (when (listp l)
    (cond
      ((endp l) (list x))
      ((= n 0) (cons x l))
      (t (cons (car l) (insert-at x (- n 1) (cdr l)))))))


;; L-22 Create a list containing all integers within a given range.
(defun range (a b)
  (labels ((aux (acc high low)
             (if (>= high low)
                 (aux (cons high acc) (- high 1) low)
                 acc)))
    (if (< a b)
        (aux '() b a)
        (revrs (aux '() a b)))))


;; L-23 Extract a given number of randomly selected elements from a list.
(defun rand-select (l n)
  (when (listp l)
    (labels ((extract (acc nn ls)
               (cond
                 ((endp ls) (error "Not Found"))
                 ((= nn 0) (cons (car ls) (append acc (cdr ls))))
                 (t (extract (cons (car ls) acc) (- nn 1) (cdr ls)))))
             (extract-rand (ls len)
               (extract '() (random len) ls))
             (aux (nn acc ls len)
               (if (= nn 0)
                   acc
                   (let* ((tmp (extract-rand ls len))
                          (picked (car tmp))
                          (rest (cdr tmp)))
                     (aux (- nn 1) (cons picked acc) rest (- len 1))))))
      (let ((len (leng l)))
        (aux (min n len) '() l len)))))


;; L-24 Lotto: Draw N different random numbers from the set 1..M.
(defun lotto-select (n m)
  (rand-select (range 1 m) n))


;; L-25 Generate a random permutation of the elements of a list.
(defun permutation (l)
  (when (listp l)
    (labels ((extract (acc n ls)
               (cond
                 ((endp ls) (error "Not found"))
                 ((= n 0) (cons (car ls) (append acc (cdr ls))))
                 (t (extract (cons (car ls) acc) (- n 1) (cdr ls)))))
             (extract-rand (ls len)
               (extract '() (random len) ls))
             (aux (acc ls len)
               (if (= len 0)
                   acc
                   (let* ((tmp (extract-rand ls len))
                          (picked (car tmp))
                          (rest (cdr tmp)))
                     (aux (cons picked acc) rest (- len 1))))))
      (aux '() l (leng l)))))


;; L-26 Generate the combinations of K distinct objects chosen from the N
;; elements of a list
(defun extract (k l)
  (when (listp l)
    (if (<= k 0)
        '(())
        (cond
          ((endp l) '())
          (t
           (let ((with-h (mapcar #'(lambda (ls) (cons (car l) ls))
                                 (extract (- k 1) (cdr l))))
                 (without-h (extract k (cdr l))))
             (append with-h without-h)))))))

;; L-27 Group the elements of a set into disjoint subsets.
; OCaml version L99-27
;    :use revrs and flttn
(defun group-ml (l sizes)
  (let* ((initial (mapcar #'(lambda (size) (list size)) sizes)))
    (labels ((prepend (p l)
               (labels ((emit (l acc) (cons l acc))
                        (inner-aux (f acc l)
                          (cond
                            ((endp l) (funcall f '() acc))
                            (t
                             (let* ((n (caar l))
                                    (ll (cdar l))
                                    (accm
                                      (if (> n 0)
                                          (funcall f (cons (list (- n 1) (cons p ll)) (cdr l)) acc)
                                          acc)))
                               (inner-aux #'(lambda (ls acc) (funcall f (cons (car l) ls) acc)) accm (cdr l)))))))
                 (inner-aux #'emit '() l)))
             (aux (l)
               (cond
                 ((endp l) (list initial))
                 (t
                  (let* ((hd (car l))
                         (tl (cdr l)))
                    (mapcan #'(lambda (r) (prepend hd r)) (aux tl)))))))
      (let* ((all (aux l))
             (complete (loop for i in all when (and (zerop (caar i)) (zerop (caadr i)))
                             collect (loop for (nil j) in i
                                           collect j))))
        (loop for (i j) in complete
              collect (list (flttn i) (flttn j)))))))

; Haskell versoin L99-27.
(defun combination (n l)
  (cond
    ((= n 0) (list (list '() l)))
    ((endp l) '())
    (t
     (let ((ts (loop for (ys nil) in (combination (- n 1) (cdr l))
                     collect (list (cons (car l) ys) (cdr l))))
           (ds (loop for (ys zs) in (combination n (cdr l))
                     collect (list ys (cons (car l) zs)))))
       (append ts ds)))))

(defun group-hs (sizes l)
  (cond
    ((endp sizes) '(()))
    (t
     (loop for (g rs) in (combination (car sizes) l)
           collect (loop for gs in (group-hs (cdr sizes) rs)
                         collect (cons g gs))))))


;; L99-28 Sorting a list of lists according to length of sublists.
(defun insert (cmp e l)
  (cond
    ((endp l) (list e))
    (t
     (if (<= (funcall cmp e (car l)) 0)
         (cons e l)
         (cons (car l) (insert cmp e (cdr l)))))))

(defun srt (cmp l)
  (cond
    ((endp l) '())
    (t (insert cmp (car l) (srt cmp (cdr l))))))

(defun compare (x y)
  (cond
    ((= x y) 0)
    ((> x y) 1)
    (t -1)))

(defun length-sort (l)
  (let* ((ll (mapcar #'(lambda (l) (list (length l) l)) l))
         (lll (srt #'(lambda (a b) (compare (car a) (car b))) ll)))
    (mapcan #'cdr lll)))

(defun rle (l)
  (labels ((aux (cnt acc ls)
             (cond
               ((endp ls) acc)
               ((= (length ls) 1) (cons (list (car ls) (1+ cnt)) acc))
               (t
                (let* ((a (car ls))
                       (b (cadr ls))
                       (tl (cdr ls)))
                  (if (= a b)
                      (aux (1+ cnt) acc tl)
                      (aux 0 (cons (list a (1+ cnt)) acc) tl)))))))
  (aux 0 '() l)))

(defun frequency-sort (l)
  (let* ((lengths (mapcar #'length l))
         (freq (rle (srt #'compare lengths)))
         (by-freq (mapcar #'(lambda (l) (list (cadr (assoc (length l) freq)) l)) l))
         (sorted (srt #'(lambda (a b) (compare (car a) (car b))) by-freq)))
    (mapcar #'cdr sorted)))
