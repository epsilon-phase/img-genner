(in-package img-genner)

(defclass shape()
  ((rotation :initform 0.0 :initarg :rotation
             :type single-float
             :documentation "The amount by which the shape is rotated around the origin")
   (origin :initform (point 0.0 0.0) :initarg :origin :type (simple-array single-float (2))
           :documentation "The point which all vertices are relative to."))
  (:documentation "The base shape class"))
(defgeneric bounds(s))
(defgeneric move-to(shape x y)
  (:documentation "Set the origin of the shape to the specified coordinates."))
(defgeneric get-segments(shape &key max-degree)
  (:documentation "Retrieve the line segment point pairs that comprise the outline of the shape"))
(defgeneric get-points(shape &key max-degree)
  (:documentation "Get the points comprising the outline of the shape"))
(let ((theta-last 0.0)
      (s-theta 0.0)
      (c-theta 1.0))
  (defun adjust-point(x y theta)
    (declare (type single-float theta)
             (type (or single-float fixnum) x y))
    "Rotate the point @c((x,y)) @c(theta) radians around the origin, returning it as @c((values x y))"
    (when (/= theta theta-last)
        (setf theta-last theta
              c-theta (cos theta)
              s-theta (sin theta)))
    (values (- (* c-theta x) (* s-theta y))
            (+ (* s-theta x) (* c-theta y)))))
(defun rotate-around(shape point theta)
  (declare (type (simple-array single-float (3 1)) point)
           (type single-float theta))
  "Rotate a shape around a given point by the angle specified. Destructively modifies shape."
  (multiple-value-bind (ex ey)
      (adjust-point (- (aref (slot-value shape 'origin) 0)
                       (aref point 0))
                    (- (aref (slot-value shape 'origin) 1)
                       (aref point 1))
                    theta)
    (setf (slot-value shape 'origin) (incf-point (point ex ey) point))
    (incf (slot-value shape 'rotation) theta))
  shape)
(defun to-shape-space(point shape)
  "Convert global coordinates to local coordinates."
  (declare (type (simple-array single-float (2)) point))
  (let ((point (sub-point point (slot-value shape 'origin))))
    (multiple-value-call #'point (adjust-point (aref point 0)
                                               (aref point 1)
                                               (slot-value shape 'rotation))))
  )
(defclass ellipse(shape)
  ((radius :initform #1a(1.0 1.0)
           :type (simple-array single-float (2))
           :initarg :radius
           :documentation "A array of 2 floats that specifies the x and y axis radius respectively"))
  (:documentation "An ellipse with the given radii")
  )
(defmethod move-to((e shape) x y)
  (setf (aref (slot-value e 'origin) 0) (coerce 'single-float x)
        (aref (slot-value e 'origin) 1) (coerce 'single-float y)))
(defun make-ellipse(center-x center-y radius-x radius-y)
  "Convenience function for ellipse-creation."
  (let ((a (make-instance 'ellipse)))
    (setf (slot-value a 'origin)
          (point center-x center-y)
          (slot-value a 'radius)
          (vector radius-x radius-y))
    a
    )
  )
(defclass rectangle(shape)
  (
   (width :initform 1.0 :type single-float :initarg :width
          :documentation "The width of the rectangle")
   (height :initform 1.0 :type single-float :initarg :height
           :documentation "The height of the rectangle"))
  (:documentation "A rectangle"))
(defmethod move-to((r rectangle) x y)
  (setf (aref (slot-value r 'origin) 0) (coerce 'single-float x)
        (aref (slot-value r 'origin) 1) (coerce 'single-float y)))
(defun make-rectangle(x y width height)
  (let ((a (make-instance 'rectangle)))
    (setf (slot-value a 'origin) (point x y)
          (slot-value a 'width) width
          (slot-value a 'height) height)
    a
    ))
(defmethod bounds((c ellipse))
  (with-slots (origin radius) c
    (list
     (vector (- (aref origin 0) (aref radius 0))
             (+ (aref origin 1) (aref radius 1)))
     (vector (+ (aref origin 0) (aref radius 0))
             (- (aref origin 1) (aref radius 1))))))
(defmethod bounds((r rectangle))
  (with-slots (origin width height) r
    (list (vector (aref origin 0 0) (aref origin 1))
          (vector (+ width (aref origin 0))
                  (- height (aref origin 1))))
    ))
(defgeneric inside-shape(point shape))
(defmethod inside-shape(p (e ellipse))
  (let* ((p2 (to-shape-space p e)))
    (<=
     (+ (/
         (expt (aref p2 0) 2)
         (expt (svref (slot-value e 'radius) 0) 2))
        (/ (expt (aref p2 1) 2)
           (expt (svref (slot-value e 'radius) 1) 2)))
     1.0)))
(defmethod inside-shape(p (r rectangle))
  (let ((p (to-shape-space p r)))
    (with-slots (width height) r
        (and
         (<= (- (/ width 2.0)) (aref p 0) (/ width 2.0))
         (>= (- (/ height 2.0)) (aref p 1) (/ height 2.0)))))
  )
;;Handle adjusting the points obtained by both the origin(translation) and
;;the rotation.
(defmethod get-points :around ((s shape)&key (max-degree 10))
  "Calculate the points offset by rotation and origin"
  (with-slots (origin rotation) s
    (let ((p (call-next-method s :max-degree max-degree)))
      (loop for i in p
            do(multiple-value-bind (ex ey)
                  (adjust-point (aref i 0) (aref i 1) rotation)
                (setf (aref i 0) ex
                      (aref i 1) ey)
                (incf-point i origin)
                ))
      p
      )
  ))
(defmethod get-points((shape ellipse) &key (max-degree 10))
  "Create a list of max-degree points which comprise an ellipse."
  (with-slots (radius rotation) shape
    (loop for i from 0 to max-degree
          for angle = 0.0 then (* i (/ (* 3.1415 2) max-degree))
          collect(make-array 2 :element-type 'single-float
                               :initial-contents `(,(* (aref radius 0) (cos angle))
                                                   ,(* (aref radius 1) (sin angle))
                                                   )))
    ))
(defmethod get-points((shape rectangle) &key (max-degree 4))
  "Create a list of 4 points which comprise a rectangle."
  (declare (ignore max-degree))
  (with-slots (width height)
      shape
    (map 'list #'point
         (list (- (/ width 2.0)) (/ width 2.0) (/ width 2.0) (- (/ width 2.0)))
         (list (/ height 2.0) (/ height 2.0) (- (/ height 2.0)) (- (/ height 2.0)))
         )
    )
   ; (multiple-value-bind (e-width e-height)
   ;     (adjust-point width height rotation)
   ;   (list
   ;    (point (aref topleft 0 0) (aref topleft 1))
   ;    (point (+ (aref topleft 0 0) e-width) (aref topleft 1))
   ;    (point (+ (aref topleft 0 0) e-width) (- (aref topleft 1) e-height))
   ;    (point (aref topleft 0 0) (- (aref topleft 1) e-height))
   ;    )
   ;   )
    )

(defun colinearp(ax ay bx by cx cy dx dy)
  (declare (type (or fixnum single-float);Does this have effect?
                 ax ay
                 bx by
                 dx dy))
  (or (and (= ax bx) (= cx dx))
      (and (= ay by) (= cy dy)))
  )
(defun get-intersection(ax ay bx by cx cy dx dy)
  "Returns the point where the lines intersect, or nil if they don't.
Avoids lines that have zero determinants"
  ;(declare (type single-float
                 ;ax ay
                 ;bx by
                 ;cx cy
                 ;dx dy)
                                        ;(optimize 3))
  (if (colinearp ax ay bx by cx cy dx dy) nil
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
        )))
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
(defmethod get-segments ((s shape) &key (max-degree nil))
  (get-lines (if max-degree
                 (get-points s :max-degree max-degree)
                 (get-points s))))

;(print (macroexpand-1 '(with-array-items ((a 1 1) (b 1 2)) array (setf a 2 b 3))))
(Declaim (ftype (function (t t t) (values single-float single-float)) adjust-point))
(export '(ellipse rectangle make-ellipse get-segments get-points make-rectangle get-intersection rotate-around))
