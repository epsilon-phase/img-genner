(in-package "img-genner")
(defun lerp(p1 p2 f)
  (typecase p1
    (vector
     (map 'vector
          (lambda (a b)
            (+
             (* (- 1 f) a) (* f b)))
          p1 p2))
    (t (+ (* (- 1 f) p1) (* f p2)))
    )
  )
(defun poly-linear-interpolator(xs ys x)
  (loop for i from 1 to (1- (length xs))
        for x1 = (aref xs (1- i)) then (aref xs (1- i))
        for x2 = (aref xs i) then (aref xs i)
        when (<= x1 x x2)
          do(return (lerp (aref ys (1- i))
                          (aref ys i)
                          (/ (- x x1)
                             (- x2 x1))
                          ))))
(export '(poly-linear-interpolator))
