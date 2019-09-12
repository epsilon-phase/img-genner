(in-package img-genner/test)
(use-package :img-genner)
(defun distance(a)
  (sqrt (+ (expt (aref a 0 0) 2)
           (expt (aref a 1 0) 2))))
(deftest circle-stuff
  (testing "radius dimension"
    (loop for i from 3 to 20
          with c = (make-ellipse 0.0 0.0 3.0 3.0)
          do(ok (not
                 (remove-if (lambda(x) (> 0.001 (abs (- 3 (distance x)))))
                            (get-points c :max-degree i))
                 ))
          )
          )
  )
(deftest rectangle-stuff
  (testing "Rectangle Points"
    (ok (equalp '(#2a((0.0)(0.0)(0.0))
                  #2A((1.0)(0.0)(0.0))
                  #2A((1.0)(-1.0)(0.0))
                  #2A((0.0)(-1.0)(0.0))
                  )
                (get-points (make-rectangle 0.0 0.0 1.0 1.0))))))
(deftest intersection-stuff
  (testing "Intersection exists"
    (ok (equalp #2a((0.0)(0.5)(0.0))
                (get-intersection 0.0 0.0 0.0 1.0 -0.5 0.5 0.5 0.5))
        "MidLine")
    (ok (equalp #2a((0.0)(0.0)(0.0))
                (get-intersection 0.0 0.0 0.0 1.0
                                  0.0 0.0 1.0 0.0))
        "Line segment end")
    )
  (testing "No intersection"
    (ng (get-intersection 0.0 0.0 0.5 0.5 1.0 1.0 1.0 0.0))
    )
  )
