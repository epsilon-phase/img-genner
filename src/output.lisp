(require 'png)
(in-package "img-genner")
(use-package 'png)
#|
 | Stroker is what we're calling it and yes.
 |
 | It is *that* lewd.
 |#
(defun static-color-stroker(color)
  (lambda (i x y t)
    (declare (ignore t))
    (loop for i across color
          for z = 0 then (+ z 1)
          do(setf (aref i x y z) (aref color z))
          )
    ))
(defun calculate-bounding-box(segments)
  (let ((top-left (make-array 2 :initial-contents `(,most-positive-single-float ,most-negative-single-float )))
        (bottom-right (make-array 2 :initial-contents `(,most-negative-single-float ,most-positive-single-float))))
    (loop for i in segments
          for x = (aref i 0 0) then (aref i 0 0)
          for y = (aref i 1 0) then (aref i 1 0)
          when (> x (aref bottom-right 0))
            do(setf (aref bottom-right 0) x)
          when (< x (aref top-left 0))
            do(setf (aref top-left 0) x)
          when (> y (aref top-left 1))
            do(setf (aref top-left 1) y)
          when (< y (aref bottom-right 1))
            do(setf (aref bottom-right 1) y)
          )
    (list top-left bottom-right)
    )
  )
;TODO move this to shapes.lisp :)
#|
 |Translated from https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
 |#
(defun orientation(a b c)
  (let ((val (- (* (- (aref b 1) (aref a 1)) (- (aref c 0) (aref b 0)))
                (* (- (aref b 0) (aref a 0)) (- (aref c 1) (aref b 1))))))
  (if (or (zerop val) (< single-float-negative-epsilon val single-float-epsilon))
      0;colinear
      (if (> val 0)
          1;clockwise
          2);counter-clockwise
      )
    )
  )
(defun on-segment(a b c)
  "Determines if b lies on the line segment ac"
  (flet ((p-x (v) (aref v 0))
         (p-y (v) (aref v 1)))
    (and (< (p-x b) (max (p-x a) (p-x c)))
         (> (p-x b) (min (p-x a) (p-x c)))
         (< (p-y b) (max (p-y a) (p-y c)))
         (> (p-y b) (min (p-y a) (p-y c))))
    ))
(defun intsersects-p(a b c d);TODO Make this return the location of the intersection
  (let ((o1 (orientation a b c))
        (o2 (orientatino a b d))
        (o3 (orientation c d a))
        (o3 (orientation c d b)))
    (if (and (not (= o1 o2)) (not (= o3 o4)))
        t
        (or
         (and (zerop o1) (on-segment a c b))
         (and (zerop o2) (on-segment a d b))
         (and (zerop o3) (on-segment c a d))
         (and (zerop o4) (on-segment c b d))
         )
        )
    )
  )
(defun fill(segments image stroker)
  (let* ((dim (calculate-bounding-box segments))
         (startx (truncate (aref (first dim) 0)))
         (endx (truncate (aref (second dim) 0)))
         (starty (truncate (aref (second dim) 1)))
         (endy (truncate (aref (first dim) 1)))
         (inside nil)
         )
    (loop for y from starty upto endy
          do(loop for x from startx upto endx
                  do();TODO finish this function
                  )
          )
    )
  )
