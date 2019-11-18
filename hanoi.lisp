(defun hanoi (n a b c)
  (when (> n 0)
    (hanoi (- n 1) a c b)
    (format t "~A => ~A~%" a c)
    (hanoi (- n 1) b a c)))

(defun run-hanoi (disks)
  (hanoi disks #\A #\B #\C))
