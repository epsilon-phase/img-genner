(in-package img-genner)
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

#+sbcl
(declaim (sb-ext:maybe-inline line-index-interpolator))
(defun line-index-interpolator(x1 y1 x2 y2 &optional (buffer nil))
  (declare (optimize (speed 3))
           (type fixnum x1 y1 x2 y2)
           (type (or null (vector cons)) buffer)
           )
  (if buffer
      (setf (fill-pointer buffer) 0))
  (flet ((absdist (a b) (declare (type fixnum a))(abs (- a b))))
    (let* ((dx (absdist x2 x1))
           (dy (- (absdist y2 y1)))
           (sx (if (< x1 x2) 1 -1))
           (sy (if (< y1 y2) 1 -1))
           (err (+ dx dy))
           (r (if buffer buffer
                  (make-array dx
                              :adjustable t :fill-pointer 0
                              :initial-element nil))))
      (declare (type fixnum err dy dx ))
      (loop until (and (= x1 x2) (= y1 y2))
            for e2 fixnum = (* 2 err) then (* 2 err)
            do(vector-push-extend `(,x1 . ,y1) r)
            finally (return r)
            when (>= e2 dy)
              do(incf err dy)
              and do(incf x1 sx)
            when (<= e2 dx)
              do (incf err dx)
              and do (incf y1 sy)
            )
      ))
  )
(export '(poly-linear-interpolator line-index-interpolator))
