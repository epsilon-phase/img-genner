(in-package :img-genner)
(declaim (optimize  (debug 3)))
(defun quick-get-pixel(image x y)
  (declare (type (simple-array (unsigned-byte 8) (* * 3)) image)
           (type fixnum x y) (optimize (speed 3)))
  (values (aref image y x 0) (aref image y x 1) (aref image y x 2)))

(defun compare-colors-bytewise(c1 c2)
  (declare (type (vector (unsigned-byte 8) 3) c1 c2))
  (loop for a across c1
        for b across c2
          until (not (= a b))
        finally (return (< a b))))
(defun compare-colors-magnitude(c1 c2)
  (declare (type (simple-array (unsigned-byte 8)) c1 c2)
           (optimize (speed 3) (safety 0)))
  (< (+ (aref c1 0) (aref c1 1) (aref c1 2))
     (+ (aref c2 0) (aref c2 1) (aref c2 2))))
;  (loop for i fixnum across c1
;        for j fixnum across c2
;        summing i into c3 fixnum
;        summing j into c4 fixnum
;        finally (return (< c3 c4))))
(declaim (inline compare-colors-magnitude))
(defun split-n-length(input l)
  "split a sequence into segments of at most l elements"
  (let ((len (length input)))
    (loop for start = 0 then (+ start l)
          for end = (min len (+ start l)) then (min len (+ start l))
          collect (subseq input start end)
          until (= end len)
          )
    ))
(defun sort-along-line(image line &optional (comparison #'compare-colors-bytewise))
  "Sort according to the values of pixels in image along the coordinate pairs in line
using the comparison function passed"
  (declare (optimize (speed 3))
           (type (simple-array (unsigned-byte 8) (* * 3)) image)
           (type (vector cons) line)
           (type (function (vector vector)
                           boolean)
                 comparison)
           (inline get-pixel swap-pixel)
           )
  (let ((comb-length (length line))
        (sorted nil)
        (shrink 1.3)
        (color-a (make-array 3 :element-type '(unsigned-byte 8)))
        (color-b (make-array 3 :element-type '(unsigned-byte 8))))
    (loop for gap fixnum = comb-length then (floor gap shrink)
                                        ;Implementation of Comb sort, as it is fast,
                                        ;lightweight, and very easy to write
          when (<= gap 1)
            do(setf gap 1
                    sorted t)
          do(loop for i = 0 then (1+ i)
                  for j = (+ i gap) then (1+ j)
                  while (< (+ i gap) comb-length)
                  for (ax . ay) = (aref line i) then (aref line i)
                  for (bx . by) = (aref line j) then (aref line j)
                  when (funcall comparison
                                (get-pixel image ax ay color-a)
                                (get-pixel image bx by color-b))
                    do(progn (swap-pixel image ax ay bx by)
                             (setf sorted nil))
                  )
          while (not sorted))))
(defun ordinal-pixel-sort(image &key (comparison #'compare-colors-bytewise)
                                  (segment-length 20) (direction :left))
  (declare (optimize (speed 2))
           (type (simple-array (unsigned-byte 8) (* * *)) image))
  (flet ((line (start)
           "start is the x or y coordinate to use"
           (multiple-value-bind (offset-x offset-y start-x start-y)
               (ecase direction
                 (:left (values -1 0 (1- (array-dimension image 1)) start))
                 (:right (values 1 0 0 start))
                 (:up (values 0 1 start 0))
                 (:down (values 0 -1 start (1- (array-dimension image 0))))
                 )
             (loop with r = (make-array 100 :adjustable t :fill-pointer 0)
                   for x fixnum = start-x then (+ x offset-x)
                   for y fixnum = start-y then (+ y offset-y)
                   while (and (>= x 0) (< x (array-dimension image 1))
                              (>= y 0) (< y (array-dimension image 0)))
                   do(vector-push-extend (cons x y) r)
                   finally (return r)
                   )
             )))
    (loop for i from 0 below (array-dimension image (ecase direction (:up 1) (:down 1) (:left 0) (:right 0)))
          do(loop for l in (split-n-length (line i) segment-length)
                  do(sort-along-line image l comparison))
          )
    ))
(defun central-pixel-sort (image cx cy
                           &key
                            (comparison #'compare-colors-bytewise)
                             (segment-length 20))
  (declare (optimize (speed 2))
           (type fixnum cx cy segment-length)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (inline line-index-interpolator))
  (let ((buffer (make-array 30 :adjustable t :fill-pointer 0)))
    (loop for y from 0 below (array-dimension image 0)
          do(loop for i in (split-n-length (line-index-interpolator 0 y cx cy buffer) segment-length)
                  do(sort-along-line image i comparison)
                  )
          do(loop for i in (split-n-length
                            (line-index-interpolator (1- (array-dimension image 1))
                                                     y cx cy buffer)
                            segment-length)
                  do(sort-along-line image i comparison))
          )
    (loop for x from 0 below (array-dimension image 1)
          do(loop for i in (split-n-length (line-index-interpolator x 0 cx cy buffer)
                                           segment-length)
                  do(sort-along-line image i comparison)
                  )
          do(loop for i in (split-n-length
                            (line-index-interpolator x (1- (array-dimension image 0))
                                                     cx cy buffer)
                                           segment-length)
                  do(sort-along-line image i comparison)
                  )
          )
    ))
(defun fuck-it-up-pixel-sort (image cx cy
                           &key
                             (comparison #'compare-colors-bytewise)
                             (segment-length 20))
  (declare (type fixnum cx cy segment-length)
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (optimize (speed 2)))
  (loop for y from 0 below (array-dimension image 0)
        do(loop for x from 0 below (array-dimension image 1)
                do(loop for i in (split-n-length (line-index-interpolator x y cx cy) segment-length)
                        do(sort-along-line image i comparison)
                        ))
        )
  )
(defun swap-tiles(image tile-width tile-height x1 y1 x2 y2)
  "Swap a segment of an image consisting of tile-width by tile-height pixels, the first starting at x1,y1 and the second starting at x2,y2"
  (declare (type (simple-array (unsigned-byte 8) (* * 3)) image)
           (type fixnum tile-width tile-height x1 y1 x2 y2)
           (optimize speed)
           (inline swap-pixel))
  (dotimes (y tile-height)
    (dotimes (x tile-width)
      (declare (type fixnum x y))
      (swap-pixel image (the fixnum (+ x1 x))
                  (the fixnum (+ y1 y))
                  (the fixnum (+ x2 x))
                  (the fixnum (+ y2 y))))))
(defun swap-tiles-2(tile-width tile-height image1 x1 y1 image2 x2 y2)
  (declare (type fixnum tile-width tile-height x1 y1 x2 y2)
           (type (simple-array (unsigned-byte 8) (* * 3)) image1 image2))
  (dotimes (y tile-height)
    (dotimes (x tile-width)
      (declare (type fixnum x y))
      (swap-pixel-2 image1 (+ x x1) (+ y y1)
                    image2 (+ x x2) (+ y y2))))
  )
(defun range-vector(start end)
  (loop with r = (make-array (- end start) :element-type 'fixnum :fill-pointer 0)
        for i from start below end do(vector-push i r)
        finally (return r)))
(defun scramble-vector(r)
  "Take a vector r and scramble it using the \"random\" function. Destructively modifies r"
  ;Fisher Yates/Knuth Shuffle implementation
  (loop for i from (1- (fill-pointer r)) above 1
        for j = (random i) then (random i)
        do(psetf (aref r i) (aref r j)
                 (aref r j) (aref r i))
        finally (return r)
        ))
(defun scramble-image(image tile-width tile-height)
  "Swap tiles of (tile-width x tile-height) in the image."
  (declare (type (simple-array (unsigned-byte 8) (* * 3)) image)
           (type fixnum tile-width tile-height)
           (optimize speed))
  (let* ((columns (floor  (array-dimension image 1) tile-width))
        (rows (floor  (array-dimension image 0) tile-height))
        (tile-vec (scramble-vector (range-vector 0 (* columns rows)))))
    (declare (type fixnum columns rows))
    (flet ((tile-x (index)
             (* tile-width (mod index columns)))
           (tile-y (index)
             (declare (optimize speed)
                      (type fixnum index))
             (* tile-height (floor index columns))))
      (loop for i = 0 then (1+ i)
            for j across tile-vec
            do(swap-tiles image tile-width tile-height
                          (tile-x i) (tile-y i)
                          (tile-x j) (tile-y j))))))
(defun scramble-image-2(tile-width tile-height image1 image2)
  "Scramble two images into each other with a specified tile size
It is an error to specify images that are of different dimensions"
  (declare (type fixnum tile-width tile-height)
           (type (simple-array (unsigned-byte 8) (* * 3)) image1 image2))
  (when (not (and (= (array-dimension image1 0) (array-dimension image2 0))
                  (= (array-dimension image1 1) (array-dimension image1 1))))
      (error "Image1 and image2 have differing sizes. This is not supported"))
  (let* ((columns (floor  (array-dimension image1 1) tile-width))
         (rows (floor  (array-dimension image1 0) tile-height))
         (tile-vec (scramble-vector
                    (loop with r = (make-array 1 :adjustable t :fill-pointer 0 :element-type 'list)
                          for i across (range-vector 0 (* columns rows))
                          do(progn (vector-push-extend (list image1 i) r)
                                   (vector-push-extend (list image2 i) r))
                          finally (return r)))))
    (flet ((tile-x (index)
             (* tile-width (mod index columns)))
           (tile-y (index)
             (declare (optimize speed)
                      (type fixnum index))
             (* tile-height (floor index columns))))
      (print (aref tile-vec 0))
      (loop for idx from 1 below (length tile-vec)
            for j = (aref tile-vec (1- idx)) then (aref tile-vec (1- idx))
            for i = (aref tile-vec idx) then (aref tile-vec idx)

            do(destructuring-bind (im1 tile1) i
                (destructuring-bind (im2 tile2) j
                (swap-tiles-2 tile-width tile-height
                            im1 (tile-x tile1) (tile-y tile1)
                            im2 (tile-x tile2) (tile-y tile2)))))
    )
    ))
(defun intensify-blur(image offset)
  "Performs a simple linear blur from left to right on the specified interval"
  (declare (optimize (speed 2))
           (type (simple-array (unsigned-byte 8) (* * *)) image)
           (type fixnum offset))
  (loop for y fixnum from 0 below (array-dimension image 0)
        with start-x = (whenz (< offset 0) (abs offset))
        with end-x = (- (array-dimension image 1)(whenz (> offset 0) offset))
        do(loop for x fixnum from start-x below end-x
                for x-off fixnum = (+ x offset)
                do(loop for c fixnum from 0 below (array-dimension image 2)
                        do(setf (aref image y x c)
                                (floor (+ (aref image y x c)
                                          (aref image y x-off c))
                                       2))
                        )
                )
          finally (return image)
        )
  )

(defun intensify-blur-nd(im offset)
  "Performs a simple linear blur from left to right on the specified interval. Non-destructive, different than intensify-blur"
  (declare (optimize (speed 2))
           (type (simple-array (unsigned-byte 8) (* * *)) im)
           (type fixnum offset))
  (loop with image = (the (simple-array (unsigned-byte 8) (* * *)) (copy-image im))
        for y fixnum from 0 below (array-dimension im 0)
        with start-x = (whenz (< offset 0) (abs offset))
        with end-x = (+ (array-dimension image 1)(whenz (< offset 0) offset))
        do(loop for x fixnum from start-x below end-x
                for x-off fixnum = (+ x offset)
                do(loop for c fixnum from 0 below (array-dimension image 2)
                        do(setf (aref image y x c)
                                (floor (+ (aref im y x c)
                                          (aref im y x-off c))
                                       2))
                        )
                )
        finally (return image)
        ))
(defun set-pixel-rgb(image x y r g b)
  (declare (type fixnum x y)
           (type (unsigned-byte 8) r g b)
           (type (simple-array (unsigned-byte 8) ))
           (optimize speed))
  (setf (aref image y x 0) r
        (aref image y x 1) g
        (aref image y x 2) b))
(defun rgb-to-hsl(r g b)
  (let* ((r (/ r 255.0))
         (g (/ g 255.0))
         (b (/ b 255.0))
         (mx (max r g b))
         (mn (min r g b))
         (l (/ (+ mx mn) 2.0))
         (h 0.0)
         (s 0.0))
    (if (= mx mn)
        (setf s 0.0 h 0.0)
        (let ((d (- mx mn)))
          (setf s (if (> l 0.5) (/ d (- 2 mx mn)) (/ d (+ mn mx))))
          (setf h
                (case mx
                  (r (+ (/ (- g b) d)
                        (if (< g b) 6 0)))
                  (b (+ 2 (/ (- b r) d)))
                  (b (+ 4 (/ (- r g) d)))))
          ))
    (vector h s l)))

(defun apply-vector(function vector)
  (apply function (coerce vector 'list)))
(defun map-region(image x y width height func)
  (declare (type (simple-array (unsigned-byte 8) (* * 3)) image)
           (type fixnum x y width height)
           (type (function ) func))
  (loop for ix from x below (+ width x)
        do(loop for iy from y below (+ height y)
                for r1 = (aref image iy ix 0)
                for g1 = (aref image iy ix 1)
                for b1 = (aref image iy ix 2)
                do(multiple-value-bind (r g b)
                      (funcall func r1 g1 b1)
                    (set-pixel-rgb image ix iy r g b))))
  )

(defun to-fractional-color(r g b)
  (map 'vector (lambda (a) (/ a 255.0)) (list r g b)))
(defun from-fractional-color(r g b)
  (map 'vector (lambda (a) (floor (* a 255))) (list r g b)))

(export '(compare-colors-bytewise sort-along-line compare-colors-magnitude ordinal-pixel-sort
          central-pixel-sort fuck-it-up-pixel-sort scramble-image scramble-image-2 intensify-blur
          intensify-blur-nd rgb-to-hsl))
