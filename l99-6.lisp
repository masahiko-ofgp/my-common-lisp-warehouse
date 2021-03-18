;; L99 problems with Common Lisp
;; Graphs (80~)
(in-package :cl-user)
(defpackage l99-6
  (:use cl)
  (:export :graph-to-adj
           :adj-to-graph
           :pair-of-fri
           :fir-of-string))
(in-package :l99-6)

(defstruct graph nodes edges)
(defstruct adjacency l) ;May change


(defvar example-graph
  (make-graph :nodes '(#\b #\c #\d #\f #\g #\h #\k)
              :edges '((#\b #\c) (#\b #\f) (#\c #\f) (#\f #\k) (#\g #\h))))


;; WIP: L-80 Conversions
(defun graph-to-adj (g)
  (cond
    ((not (typep g 'graph)) (error "graph-of-adj"))
    ((endp (graph-nodes g)) (make-adjacency :l '()))
    (t
      (labels ((aux (node edge)
                 (cond
                   ((equal node (car edge)) (cadr edge))
                   ((equal node (cadr edge)) (car edge))
                   (t nil))))
        (let ((tmp (loop for n in (graph-nodes g)
                         collect (loop for e in (graph-edges g)
                                       when (not (null (aux n e)))
                                       collect (aux n e)))))
          (make-adjacency :l (loop for n in (graph-nodes g)
                                   for tm in tmp
                                   collect (list n tm))))))))

(defun equal-pair (p1 p2)
  (if (or (and (equal (car p1) (car p2)) (equal (cadr p1) (cadr p2)))
          (and (equal (cadr p1) (car p2)) (equal (car p1) (cadr p2))))
    t
    nil))

(defun adj-to-graph (adj)
  (cond
    ((not (typep adj 'adjacency)) (error "adj-to-graph"))
    ((endp (adjacency-l adj)) (make-graph :nodes '() :edges '()))
    (t
      (let* ((nodes (loop for (h nil) in (adjacency-l adj) collect h))
             (tmp (loop for (h tl) in (adjacency-l adj)
                        append (mapcar #'(lambda (x) (list h x)) tl)))
             (edges (remove-duplicates tmp
                                       :test #'equal-pair
                                       :from-end t)))
        (make-graph :nodes nodes :edges edges)))))

(defun pair-of-fri (a b)
  (if (eql a b)
    (format nil "~A" a)
    (format nil "~A-~A" a b)))

(defun fri-of-string (fri)
  (cond
    ((endp fri) nil)
    (t (concatenate 'string
                    (format nil "~A" (car fri))
                    (fri-of-string (cdr fri))))))
;;(defun graph-to-fri (g))
;;(defun fri-to-graph (f))
;;(defun adj-to-fri (a))
;;(defun fri-to-adj (f))
