;; L99 problems with Common Lisp
;; Graphs (80~)

(defpackage l99-6
  (:use cl)
  (:export :graph-to-adj))
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

;;(defun adj-to-graph (a))
;;(defun graph-to-fri (g))
;;(defun fri-to-graph (f))
;;(defun adj-to-fri (a))
;;(defun fri-to-adj (f))
