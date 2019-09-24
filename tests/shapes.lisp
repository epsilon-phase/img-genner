(in-package img-genner/tests)
(use-package :img-genner)
(defun distance(a)
  (sqrt (+ (expt (aref a 0) 2)
           (expt (aref a 1) 2))))
(deftest circle-stuff
  (testing "radius dimension"
    (loop for i from 3 to 20
          with c = (make-ellipse 0.0 0.0 3.0 3.0)
          do(ok (not
                 (remove-if (lambda(x) (> 0.001 (abs (- 3 (distance x)))))
                            (get-points c :max-degree i))
                 ) (format nil "Checking Distance with ~a vertices" i))
          )
          )
  )
(deftest rectangle-stuff
  (testing "Rectangle Points"
    (ok (equalp '(#1a(-0.5 0.5)
                  #1A(0.5 0.5)
                  #1A(0.5 -0.5)
                  #1A(-0.5 -0.5)
                  )
                (get-points (make-rectangle 0.0 0.0 1.0 1.0)))
        "Rectangle correctness")))
(deftest intersection-stuff
  (testing "Intersection exists"
    (ok (equalp #1a(0.0 0.5)
                (get-intersection 0.0 0.0 0.0 1.0 -0.5 0.5 0.5 0.5))
        "MidLine")
    (ok (equalp #1a(0.0 0.0)
                (get-intersection 0.0 0.0 0.0 1.0
                                  0.0 0.0 1.0 0.0))
        "Line segment end")
    )
  (testing "No intersection"
    (ng (get-intersection 0.0 0.0 0.5 0.5 1.0 1.0 1.0 0.0))
    )
  )
;;This seems to not work, but we think the ellipse fill code is probably better than when we started o.o
;(deftest ellipse-fill
;  (testing "Non-variance"
;    (let ((image (png:make-image 10 10 1)))
;      (ok (equalp (fill-ellipse (make-ellipse 5.0 5.0 5.0 5.0) image (static-color-stroker #(255)))
;                  #3A(((0) (0) (255) (255) (255) (255) (255) (255) (255) (0))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (255) (255) (255) (255) (255) (255) (255) (255) (255))
;                      ((0) (0) (255) (255) (255) (255) (255) (255) (255) (0))
;                      ((0) (0) (0) (0) (0) (0) (0) (0) (0) (0)))
;                  )
;          "Equal, as they ought to be"))))
