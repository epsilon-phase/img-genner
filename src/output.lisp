(require 'png)
(in-package "img-genner")

(defun set-pixel(image x y color)
  "Bounds respecting color setting"
  (declare (type fixnum x y))
  (if (and
       (< x (array-dimension image 1))
       (< y (array-dimension image 0))
       (= (array-dimension color 0) (array-dimension image 2)))
      (loop for i across color
            for z = 0 then (1+ z)
            do(setf (aref image (- (1- (array-dimension image 0)) y) x z) i)))
  )

(defun set-pixel-component(image x y c color)
  "Bounds respecting color setting, but more convenient for gradients"
  (if (and
       (< x (array-dimension image 1))
       (< y (array-dimension image 0))
       (< c (array-dimension image 2)))
      (setf (aref image y x c) color)
      )
  )
(defun interpolate(max a b frac)
  "'naive' linear interpolation"
  (let ((f (max 0 (min 1 frac))))
    (+ (* a (- 1 frac))
       (* b frac))))
(defun lerp(p1 p2 f)
  (typecase p1
    (vector
     (map 'vector
       (lambda (a b)
         (+
          (* (- 1 f) a) (* f b)))
       p1 p2))
    (t (+ (* (- 1 f) p1) (* f b)))
    )
  )
(defun static-color-stroker(color)
  (lambda (i x y frac)
    (declare (ignore frac))
    (set-pixel i x y color)
    ))
(defun gradient-color-stroker(c1 c2)
  "This returns a function that can be used to color a line/line-like object with a gradient
based on how far the coordinate is along the line"
  (lambda (i x y frac)
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
(defun radial-gradient-stroker(c1 c2 center-x center-y maxradius)
  "Creates a radial gradient stroker centered on a given coordinate, with a scale up to max radius"
  (lambda (i x y frac)
    (declare (ignore frac))
    (loop for a across c1
          for b across c2
          for z = 0 then (1+ z)
          with d = (min 1.0
                        (max 0
                             (/ (sqrt (+
                                       (expt (- x center-x) 2)
                                       (expt (- y center-y) 2)
                                       ))
                                maxradius)
                             ))
          do(set-pixel-component i x y z
                  (coerce (truncate
                           (+ (* a (- 1 d)) (* b d)))
                          '(unsigned-byte 8))
                  )
          )
    )
  )
(defun calculate-bounding-box(segments)
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
                  )
;          when (> x (aref bottom-right 0))
;            do(setf (aref bottom-right 0) x)
;          when (< x (aref top-left 0))
;            do(setf (aref top-left 0) x)
;          when (> y (aref top-left 1))
;            do(setf (aref top-left 1) y)
;          when (< y (aref bottom-right 1))
;            do(setf (aref bottom-right 1) y)
          )
  )

(defun stroke-h-line(image stroker start end)
  (let ((sx (truncate (aref start 0 0)))
        (ex (truncate (aref end 0 0)))
        (y  (truncate (aref start 1 0))))
;    (format t "stroking color from ~a to ~a \n" sx ex)
    (loop for i from sx to ex
          for frac = 0.0 then (/ (- i sx)
                                 (- ex
                                    sx))
          do (funcall stroker image i y frac)
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
(print  (line-pairs '(1 2 3 4)))
#| TODO: This duplicates a crazy amount of work. It could be made better by
 |       keeping a range of line segments sorted by their min-y and max-y and
 |       updating it for each row stroked.
 |#
(defun fill-shape(lines image stroker)
  (let* ((dim (calculate-bounding-box lines))
         (startx (truncate (aref (first dim) 0)))
         (endx (1+ (truncate (aref (second dim) 0))))
         (starty (truncate (aref (second dim) 1)))
         (endy (truncate (aref (first dim) 1)))
;         (lines (get-lines segments))
         )
                                        ;(format t "Looking for intersections between y coords ~A ~A" starty endy)

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
                        #'compare-points)))
          )
    ))
(export '(fill-shape radial-gradient-stroker gradient-stroker static-color-stroker))
