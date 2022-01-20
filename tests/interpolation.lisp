(in-package img-genner/tests)
(use-package :img-genner)
(deftest poly-interpolator-test
  (testing "Scalar Discrimination"
    (let ((xs #(1 3 5 7))
          (ys #(1 3 2 5)))
      (ok (=
           (print (poly-linear-interpolator xs ys 3))
           3.0
           ) "Specified point")
      (ok (= (poly-linear-interpolator xs ys 1.5) 1.5)
          "Middle 1")
      (ok (= (poly-linear-interpolator xs ys 4.0) 2.5)
          "Middle 2")
      (ok (= (poly-linear-interpolator xs ys 0.0) 0.0)
          "Extrapolation -")
      (ok (= (poly-linear-interpolator xs ys 11.0) 11.0)
          "Extrapolation +")
      ))
  (testing "Vector test"
    (let ((x #(0 2 4 6))
          (y #(#(0.0 0.0) #(2.0 2.0) #(5.0 5.0) #(8.0 8.0))))
      (ok (equalp #(1.0 1.0)
                  (poly-linear-interpolator x y 1.0))
          "Middle specified vector")
      (ok (equalp #(2.0 2.0) (poly-linear-interpolator x y 2.0))
          "Endpoint specified vector")
      (ok (equalp #(3.5 3.5) (poly-linear-interpolator x y 3.0))
          "Middle specified vector")
    ))
  )
(deftest interpolate-4-test
    (testing "Basics"
             (ok (equalp (interpolate-4 0.0 0.0 2.0 4.0 5.0 4.0) 2))
             (ok (equalp (interpolate-4 1.0 0.0 2.0 4.0 5.0 4.0) 4))
             (ok (equalp (interpolate-4 1.0 1.0 2.0 4.0 5.0 4.0) 4))
             (ok (equalp (interpolate-4 0.0 1.0 2.0 4.0 5.0 4.0) 5))))
