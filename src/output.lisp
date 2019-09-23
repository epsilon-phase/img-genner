(require :png)
(in-package "img-genner")
;TODO add assertions for color type checking.
(defun set-pixel(image x y color)
  "Bounds respecting color setting"
  (declare (type fixnum x y))
  (if (and
       (< 0 x (array-dimension image 1))
       (< 0 y (array-dimension image 0))
       (= (array-dimension color 0) (array-dimension image 2)))
      (loop for i across color
            for z = 0 then (1+ z)
            do(setf (aref image (min (1- (array-dimension image 0))
                                     (- (1- (array-dimension image 0)) y))
                          x z)
                    i)))
  )

(defun set-pixel-component(image x y c color)
  "Bounds respecting color setting, but more convenient for gradients"
  (declare (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type fixnum x y c))
  (if (and
       (< 0 x (array-dimension image 1))
       (< 0 y (array-dimension image 0))
       (< c (array-dimension image 2)))
      (setf (aref image (min (1- (array-dimension image 0)) (- (1- (array-dimension image 0)) y)) x c) color)
      )
  )
(defun static-color-stroker(color)
  (lambda (i x y frac)
    (declare (ignore frac)
             (type fixnum x y)
             (dynamic-extent color))
    (set-pixel i x y color)
    ))
(defun gradient-color-stroker(c1 c2)
  "This returns a function that can be used to color a line/line-like object with a gradient
based on how far the coordinate is along the line"
  (lambda (i x y frac)
    (declare (type fixnum x y))
    (loop for a across c1
          for b across c2
          for z = 0 then (1+ z)
          do(set-pixel-component i x y z
                  (coerce (truncate
                           (+ (* a (- 1 frac))
                              (* b frac)))
                          '(unsigned-byte 8)))
          )
    )
  )
(defun radial-gradient-stroker(c1 c2 center-x center-y maxradius
                               &optional (distance-function #'distance2d))
  "Creates a radial gradient stroker centered on a given coordinate, with a scale up to max radius"
  (lambda (i x y frac)
    (declare (ignore frac)
             (type fixnum x y))
    (loop for a across c1
          for b across c2
          for z = 0 then (1+ z)
          with d = (min 1.0
                        (/ (funcall distance-function x y center-x center-y)
                           maxradius))
          do(set-pixel-component i x y z
                  (coerce (truncate
                           (+ (* a (- 1 d)) (* b d)))
                          '(unsigned-byte 8))))))

(defun calculate-bounding-box(segments)
  "Calculate the bounding box for the given line segments"
  (loop for (i . _) in segments
        for x = (aref i 0 0) then (aref i 0 0)
        for y = (aref i 1 0) then (aref i 1 0)
        maximizing x into max-x
        minimizing y into min-y
        maximizing y into max-y
        minimizing x into min-x
        finally (return
                  (list (vector min-x max-y)
                        (vector max-x min-y))
                  )))

(defun stroke-h-line(image stroker start end)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3 1)) start end)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type function stroker))
  (let ((sx (the fixnum (truncate (aref start 0 0))))
        (ex (the fixnum (truncate (aref end 0 0))))
        (y  (the fixnum (truncate (aref start 1 0)))))
;    (format t "stroking color from ~a to ~a \n" sx ex)
    (loop for i from sx to ex
          do (funcall stroker image i y 0.0)
          ))
  )
(defun line-pairs(l)
  "Generate pairs of lines that can be stroked in order to fill a polygon."
  (loop for i in l
        for index = 0 then (1+ index)
        with a = nil
        when (evenp index)
          do (setf a i)
        when (and (oddp index) (not (equal a i)))
          collect `(,a . ,i)))
(defun stroke-line(ax ay bx by image stroker)
  (declare (type (simple-array (unsigned-byte 8) (* * *)))
           (type function stroker)
           (type single-float ax ay bx by)
           (optimize speed))
  (let ((x1 (truncate ax))
        (y1 (truncate ay))
        (x2 (truncate bx))
        (y2 (truncate by)))
    (declare (type fixnum x1 y1 x2 y2))
                                        ;    (format t "(~a,~a)->(~a,~a)" ax ay bx by)
    (let* ((dist-x (abs (- x1 x2)))
           (dist-y (abs (- y1 y2)))
           (steep (> dist-y dist-x)))
      (when steep
        (psetf x1 y1 y1 x1
               x2 y2 y2 x2))
      (when (< x2 x1)
        (psetf x1 x2 x2 x1
               y1 y2 y2 y1))
;      (format t "x1=~a y1=~a x2=~a y2=~a" x1 y1 x2 y2)
 ;     (terpri)
      (let* ((delta-x (- x2 x1))
             (delta-y (abs (- y2 y1)))
             (err (floor delta-x 2))
             (y-step (if (< y1 y2) 1 -1))
             (y y1))
        (declare (type fixnum err delta-y y-step delta-x y))
        (loop for x from x1 to x2
              do(progn
                ;  (format t "~a,~a +~a ~~~a" x y y-step err)
  ;                (terpri)
                  (if steep
                      (funcall stroker image y x 0.0)
                      (funcall stroker image x y 0.0))
                  (setf err (- err delta-y))
                  (when (< err 0)
                      (incf y y-step)
                      (incf err delta-x)))))))
  image
  )
#| TODO: This duplicates a crazy amount of work. It could be made better by
 |       keeping a range of line segments sorted by their min-y and max-y and
 |       updating it for each row stroked.
 |#
(defun fill-polygon(lines image stroker)
  (let* ((dim (calculate-bounding-box lines))
         (startx (truncate (aref (first dim) 0)))
         (endx (1+ (truncate (aref (second dim) 0))))
         (starty (truncate (aref (second dim) 1)))
         (endy (truncate (aref (first dim) 1))))
    (loop for y from starty upto endy
          do(map 'list
                 (lambda (x)
                   (stroke-h-line image stroker (car x) (cdr x))
                   )
                 (line-pairs
                  (sort (remove-if-not #'identity
                                       (map 'list
                                            (lambda (x)
                                              (get-intersection startx y endx y
                                                                (aref (car x) 0 0)
                                                                (aref (car x) 1 0)
                                                                (aref (cdr x) 0 0)
                                                                (aref (cdr x) 1 0)))
                                            lines))
                        #'compare-points)))))
  image)
#|(defun fill-ellipse(ellipse image stroker)
  (with-slots (radius center) ellipse
    (if (zerop (slot-value ellipse 'rotation))
     (loop for y from 0 to (aref radius 1)
          with cx = (truncate (aref center 0 0))
          with cy = (truncate (aref center 1 0))
          with ry = (truncate (expt (aref radius 1) 2))
          with rx = (truncate (expt (aref radius 0) 2))
          do(loop for x from 0 to (sqrt (* rx (- 1 (/ (* y y) ry))))
                  do(progn
                        (funcall stroker image (+ cx x) (+ cy y) 0.0)
                        (funcall stroker image (- cx x) (+ cy y) 0.0)
                        (funcall stroker image (- cx x) (- cy y) 0.0)
                        (funcall stroker image (+ cx x) (- cy y) 0.0))
                  )
           )
       (loop for y from (* -1 (aref radius 1)) to (aref radius 1)
           with cx = (truncate (aref center 0 0))
           with cy = (truncate (aref center 1 0))
           with rx = (truncate (expt (aref radius 0) 2))
           with ry = (truncate (expt (aref radius 1) 2))
           do(loop for x from (* -1 (sqrt (* rx (- 1 (/ (* y y)
                                                        ry)) )))
                                 to (sqrt (* rx (- 1 (/ (* y y) ry))))
                   do(progn
                       (multiple-value-bind (ex ey)
                           (adjust-point x y (slot-value ellipse 'rotation))
                         (funcall stroker image (+ cx (truncate ex))
                                  (+ cy (truncate ey)) 0.0))
                       (multiple-value-bind (ex ey) (adjust-point (- x) y
                                                                  (slot-value ellipse 'rotation))
                         (funcall stroker image (+ cx (truncate ex))
                                  (+ cy (truncate ey)) 0.0
                                  )
                         )
                       (multiple-value-bind (ex ey) (adjust-point (- x) (- y) (slot-value ellipse 'rotation))
                         (funcall stroker image (+ cx (truncate ex)) (+ cy (truncate ey)) 0.0))
                       )))))
  image)
|#
(defun fill-ellipse(ellipse image stroker)
  (declare (type ellipse ellipse)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type function stroker)
           (optimize speed))
  (with-slots (origin radius rotation) ellipse
    (Declare (type (simple-array single-float (3 1)) origin)
             (type (simple-array single-float (2)) radius))
    (loop for y from (- (aref radius 1)) to (aref radius 1) by 0.5
          with cx = (aref origin 0 0)
          with cy = (aref origin 1 0)
          do(let ((x (/ (* (aref radius 0) (sqrt (abs (- (expt (aref radius 1) 2) (* y y)))))
                        (aref radius 1))))
              (multiple-value-bind (ex1 ey1) (adjust-point x y rotation)
                (multiple-value-bind (ex2 ey2) (adjust-point (- 0 x) y rotation)
                  (stroke-line (+ ex1 cx) (+ ey1 cy) (+ cx ex2) (+ cy ey2) image stroker)
                  )
                (multiple-value-bind (ex2 ey2) (adjust-point (- x) (- y) rotation)
                  (stroke-line (+ ex1 cx) (+ ey1 cy) (+ cx ex2) (+ cy ey2) image stroker)
                  )
                )
              )
    ))
  image)
(defun fill-triangle(a b c image stroker)
  (declare (type (simple-array single-float (3 1)) a b c)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type function stroker)
           (optimize speed))
  (flet ((edge-function (d e p)
           (declare (type (simple-array single-float (3 1)) d e p))
           (- (* (- (aref p 0 0) (aref d 0 0))
                 (- (aref e 1 0) (aref d 1 0)))
              (* (- (aref p 1 0) (aref d 1 0))
                 (- (aref e 0 0) (aref d 0 0))))))
    (let* ((min-x (min (aref a 0 0) (aref b 0 0) (aref c 0 0)))
           (max-x (max (aref a 0 0) (aref b 0 0) (aref c 0 0)))
           (min-y (min (aref a 1 0) (aref b 1 0) (aref c 1 0)))
           (max-y (max (aref a 1 0) (aref b 1 0) (aref c 1 0))))
      (loop for y from min-y to max-y
            with p = (point 0.0 0.0)
            do(loop for x from min-x to max-x
                    do (setf (aref p 0 0) (+ x 0.5)
                             (aref p 1 0) (+ y 0.5))
                    with w0 = 0 with w1 = 0 with w2 = 0
                    do(setf w0 (edge-function b c p)
                            w1 (edge-function c a p)
                            w2 (edge-function a b p))
                    when (and (> w0 0) (> w1 0) (> w2 0))
                      do(funcall stroker image (truncate x 1) (truncate y 1) 0.0)
            )
            )))
  image)
(defun fill-rectangle(rectangle image stroker)
  (declare (type Rectangle rectangle))
  (if (zerop (slot-value rectangle 'rotation))
      (fill-rectangle-sloppy rectangle image stroker)
      (destructuring-bind (a b c d) (get-points rectangle)
        (fill-triangle a b c image stroker)
        (fill-triangle c d a image stroker)
        ))
  image)
(defun fill-rectangle-sloppy(rectangle image stroker)
  (with-slots (origin width height rotation) rectangle
    (loop
      repeat height
      for y = height then (1- y)
      with ty = (aref origin 1 0)
      with tx = (aref origin 0 0)
      do(multiple-value-bind (e-width1 e-height1)
            (adjust-point width (- y) rotation)
          (multiple-value-bind (e-width2 e-height2)
              (adjust-point 0 (- y) rotation)
            (stroke-line (+ tx e-width2)
                         (- ty e-height2)
                         (+ tx e-width1)
                         (- ty e-height1)
                         image stroker)
            ))))
  image)

(defgeneric fill-shape(shape image stroker))
(defmethod fill-shape((r rectangle) image stroker)
;;  (if (zerop (slot-value r 'rotation))
      (fill-rectangle r image stroker)
 ;;     (fill-polygon (get-segments r) image stroker)
 ;;     )
  )
(defmethod fill-shape((e ellipse) image stroker)
  (fill-ellipse e image stroker))
(defmethod fill-shape((p t) image stroker)
  (fill-polygon p image stroker))

(export '(fill-shape radial-gradient-stroker gradient-stroker static-clispcolor-stroker
          fill-rectangle fill-ellipse fill-rectangle-sloppy fill-triangle))
