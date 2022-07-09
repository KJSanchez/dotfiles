

(ert-deftest test-divide-by-zero ()
  (should-error (/ 1 0)
                :type 'arith-error))


(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))
