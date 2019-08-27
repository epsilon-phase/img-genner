(in-package "img-genner")

(defclass shape() ())
(defgeneric bounds(s))
(defclass circle(shape)
  ((center :initform #2A((0.0)(0.0)(0.0)))
   (radius :initform #(1.0 1.0)))
  )
(defgeneric get-segments(shape &key max-degree))
(defmethod get-segments((shape circle) &key (max-degree 10))
  (with-slots (center radius) shape
    (loop for i from 0 to max-degree
          for angle = 0.0 then (* i (/ (* 3.1415 2) max-degree))
          with x = 0.0
          with y = 0.0
          do(progn
              (setf y (+ (aref center 0 1) (* (sin angle) (aref radius 1))))
              x (+ (aref center 0 0) (* (cos angle) (aref radius 0))))
          collect (make-array '(3 1) :element-type 'single-float :initial-contents `((,x)(,y)(0.0))))
  ))
(print (macroexpand '(with-array-items ((a 1 1) (b 1 2)) array (setf a 2 b 3))))
