(in-package "img-genner")

(defclass shape() ())
(defgeneric bounds(s))
(defclass ellipse(shape)
  ((center :initform #2A((0.0)(0.0)(0.0)) :initarg :center)
   (radius :initform #(1.0 1.0) :initarg :radius))
  )
(defun make-ellipse(center-x center-y radius-x radius-y)
  "Convenience function for ellipse-creation."
  (let ((a (make-instance 'ellipse)))
    (setf (slot-value a 'center)
          (point center-x center-y)
          (slot-value a 'radius)
          (vector radius-x radius-y))
    a
    )
  )
(defclass rectangle(shape)
  ((topleft :initform #2A((0.0)(0.0)(0.0)))
   (width :initform 1.0 )
   (height :initform 1.0)))
(defun make-rectangle(x y width height)
  (let ((a (make-instance 'rectangle)))
    (setf (slot-value a 'topleft) (point x y)
          (slot-value a 'width) width
          (slot-value a 'height) height)
    a
    ))
(defmethod bounds((c ellipse))
  (with-slots (center radius) c
    (list
     (vector (- (aref center 0 0) (svref radius 0))
             (+ (aref center 1 0) (svref radius 1)))
     (vector (+ (aref center 0 0) (svref radius 0))
             (- (aref center 1 0) (svref radius 1))))))
(defgeneric get-segments(shape &key max-degree))
(defgeneric get-points(shape &key max-degree))
(defmethod get-points((shape ellipse) &key (max-degree 10))
  (with-slots (center radius) shape
    (loop for i from 0 to max-degree
          for angle = 0.0 then (* i (/ (* 3.1415 2) max-degree))
          with x = 0.0
          with y = 0.0
          do(progn
              (setf y (+ (aref center 1 0) (* (sin angle) (aref radius 1)))
              x (+ (aref center 0 0) (* (cos angle) (aref radius 0)))))
          collect (make-array '(3 1) :initial-contents `((,x)(,y)(0.0))))
    ))
(defmethod get-points((shape rectangle) &key (max-degree 4))
  (declare (ignore max-degree))
  (with-slots (topleft width height)
      shape
      (list
       (point (aref topleft 0 0) (aref topleft 1 0))
       (point (+ (aref topleft 0 0) width) (aref topleft 1 0))
       (point (+ (aref topleft 0 0) width) (- (aref topleft 1 0) height))
       (point (aref topleft 0 0) (- (aref topleft 1 0) height))
      )
    ))
(defun slope(ax ay bx by)
  (if (= ax bx) 
      (/ (- by ay) (- bx ax)))
  )
(defun colinearp(ax ay bx by cx cy dx dy)
  (if (= (slope cx cy dx dy)
         (slope ax ay bx by));then find the y-offset
      (= (- ay (* ax (slope ax ay bx by)))
         (- cy (* cx (slope cx cy dx dy))))
      )
  )
(defun get-intersection(ax ay bx by cx cy dx dy)
  "Returns the point where the lines intersect, or nil if they don't(Or if they're collinear)"
  (declare (debug 3))
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
        ))
(defun get-lines(segments)
  "Return a list of cons pairs of points representing lines
in a closed path"
  (cons `(,(first segments) . ,(car (last segments)))
        (loop for i in segments
              for j in (rest segments)
              collect `(,i . ,j)
              )
        )
  )
(defmethod get-segments :around ((s shape) &key (max-degree nil))
  (get-lines (if max-degree
                 (get-points s :max-degree max-degree)
                 (get-points s))))

;(print (macroexpand-1 '(with-array-items ((a 1 1) (b 1 2)) array (setf a 2 b 3))))

(export '(ellipse rectangle make-ellipse get-segments get-points make-rectangle get-intersection))
