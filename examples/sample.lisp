

(defun sum (n)
  (if (<= n 0) 0 (+ n (sum (- n 1))))
)

(defun fib (n)
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)) ))
)
