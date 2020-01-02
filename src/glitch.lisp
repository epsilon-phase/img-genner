(in-package :img-genner)
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
           (optimize (speed 3)))
  (loop for i fixnum across c1
        for j fixnum across c2
        summing i into c3 fixnum
        summing j into c4 fixnum
        finally (return (< c3 c4))))
(defun split-n-length(input l)
  "split a sequence into segments of at most l elements"
  (let ((len (length input)))
    (loop for start = 0 then (+ start l)
          for end = (min len (+ start l)) then (min len (+ start l))
          collect (subseq input start end)
          until (= end len)
          )
    ))
(let ((color-a (make-array 3 :element-type '(unsigned-byte 8)))
      (color-b (make-array 3 :element-type '(unsigned-byte 8))))
  (defun sort-along-line(image line &optional (comparison #'compare-colors-bytewise))
    (declare (optimize (speed 2))
             (type (simple-array (unsigned-byte 8) (* * 3)) image)
             (type (vector cons) line)
             (type (function (vector vector)
                             boolean)
                   comparison)
             )
    (let ((comb-length (length line))
          (sorted nil)
          (shrink 1.3))
      (loop for gap fixnum = comb-length then (floor gap shrink)
            when (<= gap 1)
              do(setf gap 1
                      sorted t)
            do(loop with color-a = (make-array 3 :element-type '(unsigned-byte 8))
                    with color-b = (make-array 3 :element-type '(unsigned-byte 8))
                    for i = 0 then (1+ i)
                    for j = (+ i gap) then (1+ j)
                    while (< (+ i gap) comb-length)
                    for (ax . ay) = (aref line i) then (aref line i)
                    for (bx . by) = (aref line j) then (aref line j)
                    when (funcall comparison (get-pixel image ax ay color-a) (get-pixel image bx by color-b))
                      do(progn (swap-pixel image ax ay bx by)
                               (setf sorted nil))
                    )
            while (not sorted)))))
(defun ordinal-pixel-sort(image &key (comparison #'compare-colors-bytewise)
                                  (segment-length 20) (direction :left))
  (declare (optimize (speed 2))
           (type (simple-array (unsigned-byte *) (* * *)) image))
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
           (type (simple-array unsigned-byte (* * *)) image))
  (let ((buffer (make-array 30 :adjustable t :fill-pointer 0)))
    (loop for y from 0 below (array-dimension image 0)
          do(loop for i in (split-n-length (line-index-interpolator 0 y cx cy buffer) segment-length)
                  do(sort-along-line image i comparison)
                  )
          do(loop for i in (split-n-length (line-index-interpolator (1- (array-dimension image 1)) y cx cy buffer) segment-length)
                  do(sort-along-line image i comparison))
          )
    (loop for x from 0 below (array-dimension image 1)
          do(loop for i in (split-n-length (line-index-interpolator x 0 cx cy buffer) segment-length)
                  do(sort-along-line image i comparison)
                  )
          do(loop for i in (split-n-length (line-index-interpolator x (1- (array-dimension image 0))  cx cy buffer) segment-length)
                  do(sort-along-line image i comparison)
                  )
          )
    ))
(defun fuck-it-up-pixel-sort (image cx cy
                           &key
                             (comparison #'compare-colors-bytewise)
                             (segment-length 20))
  (declare (type fixnum cx cy segment-length)
           (type (simple-array unsigned-byte (* * *)) image)
           (optimize (speed 2)))
  (loop for y from 0 below (array-dimension image 0)
        do(loop for x from 0 below (array-dimension image 1)
                do(loop for i in (split-n-length (line-index-interpolator x y cx cy) segment-length)
                        do(sort-along-line image i comparison)
                        ))
        )
  )
(export '(compare-colors-bytewise sort-along-line compare-colors-magnitude ordinal-pixel-sort
          central-pixel-sort fuck-it-up-pixel-sort))
