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
  (cond
    ((< x (aref xs 0))
     (lerp (aref ys 0) (aref ys 1) (/ (- x (aref xs 0)) (- (aref xs 1) (aref xs 0)))))
    ((> x (aref xs (1- (length xs))))
     (lerp (aref ys (- (length ys) 2))
           (aref ys (- (length ys) 1))
           (/ (- x (aref xs (- (length xs) 2)))
              (- (aref xs (1- (length xs)))
                 (aref xs (- (length xs) 2))))))
    (t
     (loop for i from 1 to (1- (length xs))
           for x1 = (aref xs (1- i)) then (aref xs (1- i))
           for x2 = (aref xs i) then (aref xs i)
           when (<= x1 x x2)
             do(return (lerp (aref ys (1- i))
                             (aref ys i)
                             (/ (- x x1)
                                (- x2 x1))
                             ))))))
(export '(poly-linear-interpolator))
