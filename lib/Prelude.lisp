(defun even (x) (eq 0 (mod x 2)))
(defun odd (x) (eq 1 (mod x 2)))

(defun reduce (fn ini xs)
  (if (null xs) ini
    (reduce fn (fn ini (car xs)) (cdr xs))))

(defun map (fn xs)
  (if (null xs) xs (cons (fn (car xs)) (map fn (cdr xs)))))

(defun filter (fn xs)
  (if (null xs) xs
    (if (fn (car xs)))
      (cons (car xs) (filter fn (cdr xs)))
))

(defun reverse (xs)
  (defun loop (xs acc)
    (if (null xs) acc (loop (cdr xs) (cons (car xs) acc))))
  (loop xs nil))
