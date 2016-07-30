
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

(ert-deftest kkr-test-da()
             ""
             (should (equal "    3\\ 4  " (kkr-draw (kkr-da 3 4)))))

