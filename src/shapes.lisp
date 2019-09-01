(in-package "img-genner")

(defclass shape() ())
(defgeneric bounds(s))
(defclass circle(ellipse)
  ((center :initform #2A((0.0)(0.0)(0.0)))
   (radius :initform #(1.0 1.0)))
  )
(defclass rectangle(shape)
  ((topleft :initform #2A((0.0)(0.0)(0.0)))
   (width :initform 1.0)
   (height :initform 1.0)))
(defgeneric get-segments(shape &key max-degree))
(defmethod get-segments((shape ellipse) &key (max-degree 10))
  (with-slots (center radius) shape
    (loop for i from 0 to max-degree
          for angle = 0.0 then (* i (/ (* 3.1415 2) max-degree))
          with x = 0.0
          with y = 0.0
          do(progn
              (setf y (+ (aref center 0 1) (* (sin angle) (aref radius 1))))
              x (+ (aref center 0 0) (* (cos angle) (aref radius 0))))
          collect (make-array '(3 1) :initial-contents `((,x)(,y)(0.0))))
    ))
(defmethod get-segments((shape rectangle) &key (max-degree 4))
  (declare (ignore max-degree))
  (with-slots (topleft width height)
      shape
    (with-array-items ((ox 0 0) (oy 1 0)) topleft
      (list
       (point ox oy)
       (point (+ ox width) oy)
       (point (+ ox width) (- oy height))
       (point ox (- oy height))
      )
    )))
(defun get-intersection(ax ay bx by cx cy dx dy)
  (declare (debug 3))
  "Returns the point where the lines intersect, or nil if they don't"
  (let* ((s1-x (- bx ax))
         (s1-y (- by ay))
         (s2-x (- dx cx))
         (s2-y (- dy cy))
         )
        (let ((s (/
                  (+ (* -1 s1-y (- ax cx))
                     (* s1-x (- ay cy)))
                  (+ (* -1 s2-x s1-y)
                     (* s1-x s2-y))
                  ))
              (t- (/
                  (- (* s2-x (- ay cy))
                     (* s2-y (- ax cx)))
                  (+ (* -1 s2-x s1-y)
                     (* s1-x s2-y))
                  )))
          (if (and (>= s 0) (<= s 1) (>= t- 0) (<= t- 1))
              (point (+ ax (* t- s1-x)) (+ ay (* t- s1-y)))
              nil))
    )
  )
(print (get-intersection 0.0 0.0 0.0 1.0 -0.5 0.5 0.5 0.5))
(print (get-intersection 0.0 0.0 0.0 1.0 0.0 0.5 0.5 0.5))
(defun get-lines(segments)
  "Return a list of cons pairs of points representing lines
in a closed path"
  (push `(,(first segments) . ,(last segments))
        (loop for i in segments
              for j in (rest segments)
              collect `(,i . ,j)
              )
        )
  )
;(print (macroexpand-1 '(with-array-items ((a 1 1) (b 1 2)) array (setf a 2 b 3))))
