
(require 'cl-lib)

(defun kkr-e ()
  '("empty"))

(defun kkr-a (across)
  (list "across" across))

(defun kkr-d (down)
  (list "down" down))

(defun kkr-da (down across)
  (list "downacross" down across))

(defun kkr-v ()
  '("value" (1 2 3 4 5 6 7 8 9)))

(defun kkr-vv (values)
  (list "value" (delete-dup values)))

(defun kkr-draw (cell)
  (cond
    ((equal "empty" (car cell)) "   -----  ")
    ((equal "across" (car cell)) (format "   --\\%2d  " (cl-second cell)))
    ((equal "down" (car cell)) (format "   %2d\\--  " (cl-second cell)))
    ((equal "downacross" (car cell)) (format "   %2d\\%2d  " (cl-second cell) (cl-third cell)))
    (t cell)))