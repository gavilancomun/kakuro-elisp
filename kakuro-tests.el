
(require 'cl-lib)

(add-to-list 'load-path ".")
(load "kakuro.el")

(ert-deftest pp-test-quote ()
       "Tests the rendering of `quote' symbols in `pp-to-string'."
       (should (equal (pp-to-string '(quote quote)) "'quote"))
       (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
       (should (equal (pp-to-string '('a 'b)) "('a 'b)\n")))

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

