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

;(print (macroexpand-1 '(with-array-items ((a 1 1) (b 1 2)) array (setf a 2 b 3))))
