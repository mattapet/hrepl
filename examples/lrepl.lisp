(defun eval (input) input)

(defun print' (output) (write-line output))

(defun read' () (
  (write "lrepl> ")
  (read-line)
))

(defun loop ()
  (let ((input (read')))
    (if (eq input ":quit")
      (print' "bye")
      (
        (print' input)
        (loop)))
))
