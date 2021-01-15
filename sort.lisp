(defun my-sort-internal (elem lst)
  (cond
    ((endp lst) (list elem))
    (t
      (if (<= elem (car lst))
        (cons elem lst)
        (cons (car lst) (my-sort-internal elem (cdr lst)))))))

(defun my-sort (lst)
  (cond
    ((endp lst) '())
    (t (my-sort-internal (car lst) (my-sort (cdr lst))))))

