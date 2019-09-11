(in-package img-genner/tests)
(use-package :img-genner)
(deftest poly-interpolator-test
  (let ((xs #(1 3 5 7))
        (ys #(1 3 2 5)))
    (testing "Discrimination"
      (ok (=
           (print (poly-linear-interpolator xs ys 3))
           3.0
           ))
      (ok (= (poly-linear-interpolator xs ys 1.5) 1.5))
      )
    )
  )
(run-suite *package*)
