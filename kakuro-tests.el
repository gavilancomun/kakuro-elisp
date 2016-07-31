(setq lexical-binding t)

(require 'package)
(package-initialize)

(require 'cl-lib)
(require 'dash)

(add-to-list 'load-path ".")
(load-file "kakuro.el")

(ert-deftest kkr-test-empty ()
             ""
             (should (equal "   -----  " (kkr-draw (kkr-e)))))

(ert-deftest kkr-test-across ()
             ""
             (should (equal "   --\\ 5  " (kkr-draw (kkr-a 5)))))

(ert-deftest kkr-test-down ()
             ""
             (should (equal "    4\\--  " (kkr-draw (kkr-d 4)))))

(ert-deftest kkr-test-da ()
             ""
             (should (equal "    3\\ 4  " (kkr-draw (kkr-da 3 4)))))

(ert-deftest kkr-test-v ()
             ""
             (should (equal " 123456789" (kkr-draw (kkr-v)))))

(ert-deftest kkr-test-vv ()
             ""
             (should (equal " 12......." (kkr-draw (kkr-vv '(1 2))))))

(ert-deftest kkr-test-row ()
             ""
             (let ((line (list (kkr-da 3 4) (kkr-v) (kkr-vv '(1 2)) (kkr-d 4) (kkr-e) (kkr-a 5) (kkr-vv '(4)) (kkr-vv '(1)))))
               (should (equal "    3\\ 4   123456789 12.......    4\\--     -----     --\\ 5       4         1    \n" (kkr-draw-row line)))))

(ert-deftest kkr-test-products ()
             ""
             (let ((data '((1 2) (10) (100 200 300)))
                   (expected '((1 10 100) (1 10 200) (1 10 300) (2 10 100) (2 10 200) (2 10 300))))
               (should (equal expected (kkr-product data)))))

(ert-deftest kkr-test-permutes ()
             ""
             (let ((results (kkr-permute-all (list (kkr-v) (kkr-v) (kkr-v)) 6)))
               (should (equal 10 (length results)))
               (should (equal 6 (length (cl-remove-if-not 'kkr-all-different results))))))

(ert-deftest kkr-test-transpose ()
             ""
             (let* ((ints '((1 2 3 4) (1 2 3 4) (1 2 3 4)))
                    (tr (kkr-transpose ints)))
               (should (equal (length ints) (length (car tr))))
               (should (equal (length (car ints)) (length tr)))))

(ert-deftest kkr-test-is-possible ()
             ""
             (let ((vc (kkr-vv '(1 2 3))))
               (should (kkr-is-possible vc 2))
               (should-not (kkr-is-possible vc 4))))

(ert-deftest kkr-test-value-equality ()
             ""
             (should (equal (kkr-v) (kkr-v)))
             (should (equal (kkr-vv '(1 2)) (kkr-vv '(1 2)))))

(ert-deftest kkr-test-solvestep ()
             ""
             (let ((result (kkr-solve-step (list (kkr-vv '(1 2)) (kkr-v)) 5)))
               (should (equal (kkr-vv '(1 2)) (cl-first result)))
               (should (equal (kkr-vv '(3 4)) (cl-second result)))))

(ert-deftest kkr-test-takewhile ()
             ""
             (should (equal '(0 1 2 3) (-take-while (lambda (x) (< x 4)) (number-sequence 0 9)))))

(ert-deftest kkr-test-drop ()
             ""
             (should (equal '(5 6) (-drop 4 '(1 2 3 4 5 6)))))

(ert-deftest kkr-test-take ()
             ""
             (should (equal '(1 2 3 4) (-take 4 '(1 2 3 4 5 6)))))

(ert-deftest kkr-test-gather ()
             ""
             (let* ((line (list (kkr-da 3 4) (kkr-v) (kkr-v) (kkr-d 4) (kkr-e) (kkr-a 4) (kkr-v) (kkr-v)))
                    (result (kkr-gather-values line)))
               (should (equal 4 (length result)))
               (should (equal (kkr-da 3 4) (caar result)))
               (should (equal (kkr-d 4) (caar (cddr result))))
               (should (equal (kkr-e) (cl-second (car (cddr result)))))
               (should (equal (kkr-a 4) (cadr (cdar (cddr result)))))))

(ert-deftest kkr-test-pair-targets ()
             ""
             (let* ((line (list (kkr-da 3 4) (kkr-v) (kkr-v) (kkr-d 4) (kkr-e) (kkr-a 4) (kkr-v) (kkr-v)))
                    (result (kkr-pair-targets-with-values line)))
               (should (equal 2 (length result)))
               (should (equal (kkr-da 3 4) (car (caar result))))
               (should (equal (kkr-d 4) (caar (cl-second result))))
               (should (equal (kkr-e) (cl-second (car (cl-second result)))))
               (should (equal (kkr-a 4) (cadr (cdar (cl-second result)))))))

(ert-deftest kkr-test-solve-pair ()
             ""
             (let* ((line (list (kkr-da 3 4) (kkr-v) (kkr-v) (kkr-d 4) (kkr-e) (kkr-a 4) (kkr-v) (kkr-v)))
                    (pairs (kkr-pair-targets-with-values line))
                    (pair (car pairs))
                    (result (kkr-solve-pair 'cl-second pair)))
               (should (equal 3 (length result)))
               (should (equal (kkr-vv '(1 2)) (cl-second result)))
               (should (equal (kkr-vv '(1 2)) (cl-third result)))))

