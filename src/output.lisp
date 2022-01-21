(require :png)
(in-package img-genner)
                                        ;TODO add assertions for color type checking.
#+sbcl
(declaim (sb-ext:maybe-inline correct-indices swap-pixel swap-pixel-2 get-pixel))
(defun correct-indices(width height x y)
  (declare
           (type fixnum height width x y)
           (optimize speed))
  (values
   (the fixnum (min (1- width) (max x 0)))
   (the fixnum (min (1- height)
        (- (1- height) y))))
  )
(defun set-pixel(image x y color)
  "Bounds respecting color setting"
  (declare (type fixnum x y)
 ;          (type (vector (unsigned-byte *) *) color)
           (type (simple-array unsigned-byte (* * *)))
           )
  (if (and
       (< -1 x (array-dimension image 1))
       (< -1 y (array-dimension image 0))
       (= (array-dimension color 0) (array-dimension image 2)))
      (loop for i across color
            for z = 0 then (1+ z)
            do(setf (aref image (min (1- (array-dimension image 0))
                                     (- (1- (array-dimension image 0)) y))
                          x z)
                    i)))
  )
(declaim (sb-ext:maybe-inline get-pixel))
(defun get-pixel(image x y &optional (result-vec nil))
  (declare (type fixnum x y)
           (type (or
                  (simple-array (unsigned-byte 8) (* * 3))
                  (simple-array (unsigned-byte 8) (* * 4)))
                 image)
           (type (or null (simple-array (unsigned-byte 8) (3))
                     (simple-array (unsigned-byte 8) (4)))
                 result-vec)
           (optimize (speed 3) (safety 0)))
  (let ((color (if result-vec result-vec (make-array (array-dimension image 2) :element-type '(unsigned-byte 8))))
        (x (min (1- (array-dimension image 1)) (max x 0)))
        (y (min (1- (array-dimension image 0))
                (- (1- (array-dimension image 0)) y))))
    (declare (type fixnum x y)
             (type (simple-array (unsigned-byte 8) (3)) color))
    (if (= 3 (array-dimension image 2))
        (progn (setf (aref color 0) (aref image y x 0)
                     (aref color 1) (aref image y x 1)
                     (aref color 2) (aref image y x 2))
               color)
        (loop for i fixnum from 0 below (array-dimension color 0)
              do(setf (aref color i) (aref image y x i))
              finally (return color)
              )
        )))
;(declaim (ftype (function ((simple-array #1=(unsigned-byte 8) (* * 3)) fixnum fixnum &optional t) (values (simple-array #1# (3)))) get-pixel))
(defun swap-pixel(image x1 y1 x2 y2)
  (declare (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type fixnum x1 y1 x2 y2)
           (optimize speed)
           (inline correct-indices))
  (multiple-value-bind (x1 y1) (correct-indices (array-dimension image 1) (array-dimension image 0) x1 y1)
    (multiple-value-bind (x2 y2) (correct-indices (array-dimension image 1) (array-dimension image 0) x2 y2)
      (loop for i from 0 below 3
            do(psetf (aref image y1 x1 i) (aref image y2 x2 i)
                     (aref image y2 x2 i) (aref image y1 x1 i)))
      )
    ))
(defun swap-pixel-2(image1 x1 y1 image2 x2 y2)
  (declare (type (simple-array (unsigned-byte 8) (* * *)) image1 image2)
           (type fixnum x1 y1 x2 y2)
           (optimize speed)
           (inline correct-indices))
  (multiple-value-bind (x1 y1) (correct-indices (array-dimension image1 1) (array-dimension image1 0) x1 y1)
    (multiple-value-bind (x2 y2) (correct-indices (array-dimension image2 1) (array-dimension image2 0) x2 y2)
      (loop for i from 0 below 3
            do(psetf (aref image2 y2 x2 i) (aref image1 y1 x1 i)
                     (aref image1 y1 x1 i) (aref image1 y2 x2 i))))))
(defun copy-region(source dest width height x1 y1
                   x2 y2)
  (loop for y from 0 below height
        do(loop for x from 0 below width
                do(loop for c from 0 below 3
                        do(setf (aref dest (+ y2 y) (+ x2 x) c)
                                (aref source (+ y1 y) (+ x1 x) c)))))
  )
(defun partition-image(image mode element)
  "Partition an image into two different parts.
Mode must be either :row or :column, and determines the axis of the split
Element being the row or column where it starts copying to the second image"
  (let* ((dim-x-1 (ecase mode
                    (:row (array-dimension image 1))
                    (:column (min element (array-dimension image 1)))))
         (dim-x-2 (ecase mode
                    (:row (array-dimension image 1))
                    (:column (- (array-dimension image 1) element))))

         (dim-y-1 (ecase mode
                    (:row element)
                    (:column (array-dimension image 0))))
         (dim-y-2 (ecase mode
                    (:row (- (array-dimension image 0) element))
                    (:column (array-dimension image 0))))
         (im1 (make-array (list dim-y-1 dim-x-1 (array-dimension image 2))
                          :element-type '(unsigned-byte 8)))
         (im2 (make-array (list dim-y-2 dim-x-2 (array-dimension image 2))
                          :element-type '(unsigned-byte 8)))
         )
    (copy-region image im1 dim-x-1 dim-y-1 0 0 0 0)
    (copy-region image im2 dim-x-2 dim-y-2
                 (if (eq mode :column) dim-x-1 0)
                 (if (eq mode :row) dim-y-1 0) 0 0
                 )
    (values im1 im2)
    )

  )

(declaim (inline get-pixel-component set-pixel-component))
(defun set-pixel-component(image x y c color)
  "Bounds respecting color setting, but more convenient for gradients"
  (declare (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type fixnum x y c))
  (if (and
       (< 0 x (array-dimension image 1))
       (< 0 y (array-dimension image 0))
       (< c (array-dimension image 2)))
      (setf (aref image (min (1- (array-dimension image 0))
                             (- (1- (array-dimension image 0)) y) )
                  x c) color)
      )
  )
(defun get-pixel-component(image x y c)
  (aref image (min (1- (array-dimension image 0))
                   (- (1- (array-dimension image 0)) y))
        x c))
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
        for x = (aref i 0) then (aref i 0)
        for y = (aref i 1) then (aref i 1)
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
           (type (simple-array single-float (2)) start end)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type function stroker))
  (let ((sx (the fixnum (truncate (aref start 0))))
        (ex (the fixnum (truncate (aref end 0))))
        (y  (the fixnum (truncate (aref start 1)))))
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
(defun stroke-line(image ax ay bx by stroker)
  "Stroke a line from ax ay to bx by"
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
                                                                (aref (car x) 0)
                                                                (aref (car x) 1)
                                                                (aref (cdr x) 0)
                                                                (aref (cdr x) 1)))
                                            lines))
                        #'compare-points)))))
  image)

(defun fill-ellipse(ellipse image stroker)
  (declare (type ellipse ellipse)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type function stroker)
           (optimize speed))
  (with-slots (origin radius rotation) ellipse
    (Declare (type (simple-array single-float (2)) origin)
             (type (simple-array single-float (2)) radius))
    (loop for y from (- (aref radius 1)) to (aref radius 1) by 0.5
          with cx = (aref origin 0)
          with cy = (aref origin 1)
          do(let ((x (/ (* (aref radius 0) (sqrt (abs (- (expt (aref radius 1) 2) (* y y)))))
                        (aref radius 1))))
              (multiple-value-bind (ex1 ey1) (adjust-point x y rotation)
                (multiple-value-bind (ex2 ey2) (adjust-point (- 0 x) y rotation)
                  (stroke-line image (+ ex1 cx) (+ ey1 cy) (+ cx ex2) (+ cy ey2) stroker)
                  )
                (multiple-value-bind (ex2 ey2) (adjust-point (- x) (- y) rotation)
                  (stroke-line image (+ ex1 cx) (+ ey1 cy) (+ cx ex2) (+ cy ey2) stroker)
                  )
                )
              )
    ))
  image)
(defun fill-triangle(a b c image stroker)
  (declare (type (simple-array single-float (2)) a b c)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type function stroker)
           (optimize speed))
  (flet ((edge-function (d e p)
           (declare (type (simple-array single-float (2)) d e p))
           (- (* (- (aref p 0) (aref d 0))
                 (- (aref e 1) (aref d 1)))
              (* (- (aref p 1) (aref d 1))
                 (- (aref e 0) (aref d 0))))))
    (let* ((min-x (min (aref a 0) (aref b 0) (aref c 0)))
           (max-x (max (aref a 0) (aref b 0) (aref c 0)))
           (min-y (min (aref a 1) (aref b 1) (aref c 1)))
           (max-y (max (aref a 1) (aref b 1) (aref c 1))))
      (loop for y from min-y to max-y
            with p = (point 0.0 0.0)
            do(loop for x from min-x to max-x
                    do (setf (aref p 0) (+ x 0.25)
                             (aref p 1) (+ y 0.25))
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
  (destructuring-bind (a b c d) (get-points rectangle)
    (fill-triangle a b c image stroker)
    (fill-triangle c d a image stroker)
    )
  image)
(defun fill-rectangle-sloppy(rectangle image stroker)
  (with-slots (origin width height rotation) rectangle
    (loop
      repeat height
      for y = height then (1- y)
      with ty = (aref origin 1)
      with tx = (aref origin 0)
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
      (fill-rectangle r image stroker)
  )
(defmethod fill-shape((e ellipse) image stroker)
  (fill-ellipse e image stroker))
(defmethod fill-shape ((p polygon) image stroker)
  (loop for (a b c) in (img-genner/triangularization:earclip (get-points p))
        do(fill-triangle a c b image stroker)))
(defmethod fill-shape((p t) image stroker)
  (fill-polygon p image stroker))
(export '(fill-shape radial-gradient-stroker gradient-stroker static-color-stroker
          fill-rectangle fill-ellipse fill-rectangle-sloppy fill-triangle))

                                        ;Actual image saving/loading stuff here :)

(defun load-image(pathname)
  "Load an image, detecting the type from the filename"
  (let  ((ext (subseq pathname (1+ (position #\. pathname :from-end t)) )))
    (cond
      ((string= ext "png") (png:8-bit-image (png:decode-file pathname)))
      ((or (string= ext "jpg")
           (string= ext "jpeg")) (multiple-value-bind (image height width)
                               (jpeg:decode-image pathname :colorspace-conversion t)
                                   (loop with im = (make-array (list height width
                                                                     (/ (array-dimension image 0)
                                                                        (* height width)))
                                                         :element-type '(unsigned-byte 8))
                                   for c across image
                                   for i = 0 then (1+ i)
                                   do(setf (row-major-aref im i)
                                           c)
                                   finally (return ;For some reason, cl-jpeg loads it in bgr order
                                             (loop for y from 0 below height
                                                   do(loop for x from 0 below width
                                                           do(psetf (aref im y x 0)
                                                                    (aref im y x 2)
                                                                    (aref im y x 2)
                                                                    (aref im y x 0)))
                                                   finally (return im)))))))))
(defun save-image(image filename &key (quality 64))
  "Save an image to the given filename, selecting png or jpeg compression based on the extension. The quality number is Mysterious, and only applies to the jpeg encoder.
If the filename is actually a stream, then it will be written to the stream as a "
  (if (not (stringp filename))
      (png:encode image filename)
      (let ((ext (subseq filename (1+ (position #\. filename :from-end t)))))
        (cond
          ((string= ext "png") (png:encode-file image filename))
          ((string= ext "jpg")
                                        ;CL-JPEG seems unable to enable us to produce
                                        ;images with reasonable loss, but, eh.
                                        ;Worth including.
           (loop with im = (jpeg:allocate-buffer (array-dimension image 0)
                                                 (array-dimension image 1)
                                                 3)
                 for i from 0 below (array-total-size im)
                 do(setf (aref im i)
                         (row-major-aref image
                                         (case (mod i 3)
                                           (0 (+ 2 i))
                                           (2 (- i 2))
                                           (1 i)))
                         )
                 finally (jpeg:encode-image filename im 3 (array-dimension image 0)
                                            (array-dimension image 1)
                                            :q-factor quality)
                 )
           )
          )
        )))
(defparameter *default-color* (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
(defun make-image(width height &optional (default-color *default-color*))
  "Create a new image of the specified width and height and fill it with default-color"
  (declare (type fixnum width height)
           (type (simple-array (unsigned-byte 8)) default-color)
           (optimize speed))
  (loop with image = (make-array `(,height ,width ,(array-dimension default-color 0)) :element-type '(unsigned-byte 8))
        for y from 0 below (array-dimension image 0)
        do(loop for x from 0 below (array-dimension image 1)
                do(setf (aref image y x 0) (aref default-color 0)
                        (aref image y x 1) (aref default-color 1)
                        (aref image y x 2) (aref default-color 2)))
        finally (return image)))

(defun add-alpha-channel(image &optional (default-alpha 255))
  (let ((result (make-array `(,(array-dimension image 0) ,(array-dimension image 1) 4))))
    (loop for y from 0 below (array-dimension image 0)
          do(loop for x below (array-dimension image 1)
                  do(loop for c from 0 below (array-dimension image 2)
                          do(setf (aref result y x c) (aref image y x c)))
                  do(setf (aref result y x 3) default-alpha)))
    result))
(declaim (inline region-average-color))
(defun region-average-color(image x1 y1 x2 y2)
  (loop with r fixnum = 0
        with b fixnum = 0
        with g fixnum = 0
        with count = 0
        for y from y1 to y2
        do(loop for x from x1 to x2
                do(incf count)
                do(setf r (+ r (aref image y x 0))
                        g (+ g (aref image y x 1))
                        b (+ b (aref image y x 2))))
        finally(setf count (max 1 count))
        finally(return (vector (floor r count) (floor g count) (floor b count)))))
(defun antialias(image)
  (loop with result = (copy-image image)
        for y from 0 below (array-dimension image 0)
        for max-y = (min (1- (array-dimension image 0)) (+ y 1))
        for min-y = (max 0 (1- y))
        do(loop for x from 0 below (array-dimension image 1)
                for max-x = (min (1- (array-dimension image 1)) (+ x 1))
                for min-x = (max 0 (1- x))
                do(loop for i from 0
                        for c across (region-average-color image min-x min-y max-x max-y)
                        do(setf (aref result y x i) c))

                )
        finally(return result)))

(export '(save-image load-image make-image antialias))
