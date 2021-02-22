(defun even (x) (eq 0 (mod x 2)))
(defun odd (x) (eq 1 (mod x 2)))

(defun id (a) a)
(defun flip (f)
  (lambda (x y) (f y x)))

(defun inc (n) (+ 1 n))

;; [a -> b, b -> c, ... y -> z] -> (a -> z)
(defun compose (fns)
  (foldr (lambda (f g)
    (lambda (a) (f (g a)))) id fns))

;; List operations

;; [a] -> [a]
(defun reverse (xs)
  (defun loop (xs acc)
    (if (null xs) acc
      (loop (cdr xs) (cons (car xs) acc))
  ))
  (loop xs nil))

;; (b -> a -> b) -> b -> [a]
(defun foldl (fn ini xs)
  (if (null xs) ini
    (foldl fn (fn ini (car xs)) (cdr xs))))

;; (a -> b -> b) -> b -> [a]
(defun foldr (fn in xs)
  (foldl (flip fn) in (reverse xs)))

;; (a -> b) -> [a] -> [b]
(defun map (fn xs)
  (if (null xs) xs
    (cons (fn (car xs)) (map fn (cdr xs)))
))

;; (a -> Boolean) -> [a] -> [a]
(defun filter (fn xs)
  (if (null xs) xs
    (if (fn (car xs)))
      (cons (car xs) (filter fn (cdr xs)))
))

;; Number list specialiations

(defun sum (xs)
  (foldl + 0 xs))

(defun product (xs)
  (foldl * 1 xs))
