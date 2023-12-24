;; bear-test.el

(require 'ert)
(require 'bear)

(ert-deftest simple-test ()
  (should (= (+ 2 3) 5)))
