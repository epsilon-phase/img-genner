(in-package img-genner/tests)
(use-package :img-genner)
(deftest poly-interpolator-test
  (testing "Scalar Discrimination"
    (let ((xs #(1 3 5 7))
          (ys #(1 3 2 5)))
      (ok (=
           (print (poly-linear-interpolator xs ys 3))
           3.0
           ))
      (ok (= (poly-linear-interpolator xs ys 1.5) 1.5))
      (ok (= (poly-linear-interpolator xs ys 4.0) 2.5))
      (ok (= (poly-linear-interpolator xs ys 0.0) 0.0))
      (ok (= (poly-linear-interpolator xs ys 11.0) 11.0))
      ))
  (testing "Vector test"
    (let ((x #(0 2 4 6))
          (y #(#(0.0 0.0) #(2.0 2.0) #(5.0 5.0) #(8.0 8.0))))
      (ok (equalp #(1.0 1.0)
                  (poly-linear-interpolator x y 1.0)))
      (ok (equalp #(2.0 2.0) (poly-linear-interpolator x y 2.0)))
      (ok (equalp #(3.5 3.5) (poly-linear-interpolator x y 3.0)))
    ))
  )
