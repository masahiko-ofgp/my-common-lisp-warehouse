(defun fizzbuzz (n)
  (let ((fizzp (zerop (mod n 3)))
        (buzzp (zerop (mod n 5))))
    (cond
      ((and fizzp buzzp) "Fizzbuzz")
      (fizzp "Fizz")
      (buzzp "Buzz")
      (t n))))

;; dotimes
(defun run-dotimes-fizzbuzz (num)
  (dotimes (i num) (print (fizzbuzz i))))

;; do
(defun run-do-fizzbuzz (num)
  (do ((i 0 (1+ i)))
      ((= i num) i)
    (print (fizzbuzz i))))

;; loop
(defun run-loop-fizzbuzz (num)
  (loop for i upto num do (print (fizzbuzz i))))
