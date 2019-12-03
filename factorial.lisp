(defun fact-cc (n cont)
  (if (zerop n)
    (funcall cont 1)
    (fact-cc (1- n) (lambda (x) (funcall cont (* n x))))))

;; factorial main
(defun fact (n)
  (fact-cc n #'(lambda (x) x)))
