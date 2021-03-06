(setq lexical-binding t)

(require 'cl-lib)
(require 'dash)

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

(defun kkr-draw-grid (grid)
  (mapconcat 'kkr-draw-row grid ""))

(defun kkr-product (colls)
  (cond
    ((= 0 (length colls)) '())
    ((= 1 (length colls)) (mapcar 'list (car colls)))
    (t (let ((head (car colls))
             (tail-prod (kkr-product (cdr colls))))
         (-mapcat (lambda (x)
                    (--map (append (list x) it) tail-prod))
                  head)))))

(defun kkr-all-different (nums)
  (= (length nums) (length (cl-remove-duplicates nums))))

(defun kkr-permute-all (vs target)
  (let ((values (mapcar 'cl-second vs)))
    (--filter (= target (apply '+ it))
              (kkr-product values))))

(defun kkr-transpose (m)
  (apply 'cl-mapcar 'list m))

(defun kkr-is-possible (cell n)
  (member n (cl-second cell)))

(defun kkr-solve-step (cells total)
  (let* ((final (- (length cells) 1))
         (p1 (kkr-permute-all cells total))
         (p2 (--filter (kkr-is-possible (nth final cells) (nth final it))
                       p1))
         (perms (-filter 'kkr-all-different p2)))
    (cl-mapcar (lambda (x) (kkr-vv x))
               (kkr-transpose perms))))

(defun kkr-gather-values (line)
  (-partition-by (lambda (x) (equal :value (car x))) line))

(defun kkr-pair-targets-with-values (line)
  (-partition-all 2 (kkr-gather-values line)))

(defun kkr-solve-step (cells total)
  (let* ((final (- (length cells) 1))
         (p1 (kkr-permute-all cells total))
         (p2 (--filter (kkr-is-possible (nth final cells) (nth final it)) p1))
         (perms (-filter 'kkr-all-different p2)))
    (cl-mapcar (lambda (x) (kkr-vv x)) (kkr-transpose perms))))

(defun kkr-solve-pair (f pair)
  (let* ((nvs (car pair))
         (vs (cl-second pair))) 
    (if (null vs)
      nvs
      (append nvs (kkr-solve-step vs (funcall f (-last-item nvs)))))))

(defun kkr-solve-line (line f)
  (let ((pairs (kkr-pair-targets-with-values line)))
    (-mapcat (-partial 'kkr-solve-pair f) pairs)))

(defun kkr-solve-row (row)
  (kkr-solve-line row (lambda (x) (car (last x)))))

(defun kkr-solve-column (column)
  (kkr-solve-line column (lambda (x) (cl-second x))))

(defun kkr-solve-grid (grid)
  (let* ((g1 (cl-mapcar 'kkr-solve-row grid))
         (g2 (kkr-transpose g1))
         (g3 (cl-mapcar 'kkr-solve-column g2)))
    (kkr-transpose g3)))

(defun kkr-solver (grid)
  (print (kkr-draw-grid grid))
  (let ((g (kkr-solve-grid grid)))
    (if (equal g grid)
      g
      (kkr-solver g))))

