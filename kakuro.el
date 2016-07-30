(setq lexical-binding t)

(require 'cl-lib)

(defun kkr-e ()
  '(:empty))

(defun kkr-a (across)
  (list :across across))

(defun kkr-d (down)
  (list :down down))

(defun kkr-da (down across)
  (list :downacross down across))

(defun kkr-v ()
  '(:value (1 2 3 4 5 6 7 8 9)))

(defun kkr-vv (values)
  (list :value (sort (cl-remove-duplicates values) '<)))

(defun kkr-draw-values (cell)
  (concat " "
          (mapconcat (lambda (x)
                       (if (member x (cl-second cell))
                         (number-to-string x)
                         "."))
                     (number-sequence 1 9)
                     "")))

(defun kkr-draw-value (cell)
  (if (= 1 (length (cl-second cell)))
    (format "     %d    " (car (cl-second cell)))
    (kkr-draw-values cell)))

(defun kkr-draw (cell)
  (cond
    ((equal :empty (car cell)) "   -----  ")
    ((equal :across (car cell)) (format "   --\\%2d  " (cl-second cell)))
    ((equal :down (car cell)) (format "   %2d\\--  " (cl-second cell)))
    ((equal :downacross (car cell)) (format "   %2d\\%2d  " (cl-second cell) (cl-third cell)))
    ((equal :value (car cell)) (kkr-draw-value cell))
    (t cell)))

(defun kkr-draw-row (row)
  (concat
    (mapconcat 'kkr-draw row "")
    "\n"))

(defun kkr-product (colls)
  (cond
    ((= 0 (length colls)) '())
    ((= 1 (length colls)) (mapcar 'list (car colls)))
    (t (let ((head (car colls))
             (tail-prod (kkr-product (cdr colls))))
         (apply 'append (mapcar (lambda (x)
                                  (mapcar (lambda (y)
                                            (append (list x) y))
                                          tail-prod))
                                head))))))

(defun kkr-all-different (nums)
  (= (length nums) (length (cl-remove-duplicates nums))))

(defun kkr-permute-all (vs target)
  (let ((values (mapcar 'cl-second vs)))
    (cl-remove-if-not (lambda (x) 
                        (= target (apply '+ x)))
                      (kkr-product values))))

(defun kkr-transpose (m)
  (apply 'cl-mapcar 'list m))

(defun kkr-is-possible (cell n)
  (member n (cl-second cell)))

(defun kkr-solve-step (cells total)
  (let* ((final (- (length cells) 1))
         (p1 (kkr-permute-all cells total))
         (p2 (cl-remove-if-not (lambda (x)
                                 (kkr-is-possible (nth final cells) (nth final x)))
                               p1))
         (perms (cl-remove-if-not 'kkr-all-different p2)))
    (cl-mapcar (lambda (x) (kkr-vv x))
               (kkr-transpose perms))))

