(in-package img-genner)

(defmacro with-array-items(items array &body body)
  (let ((g (gensym)))
    `(let ((,g ,array))(symbol-macrolet ,(loop for i in items
                           collect (list (first i)  (cons 'aref (cons g (rest i))))
                           )
     ,@body)))
  )
(defun copy-vector-extend(vec item)
  "Make a copy of a vector, increasing its size if necessary and adding a new item"
  (let ((v (make-array (1+ (array-dimension vec 0))
                       :element-type (array-element-type vec)
                       :fill-pointer 0
                       :adjustable t)))
    (loop for i across vec
          do(vector-push-extend i v))
    (vector-push-extend item v)
    v))
(defun flat-array-to-image(arr height width)
  (let ((r (make-array (list height width (/ (array-total-size arr) (* height width))) :element-type '(unsigned-byte 8))))
    (loop with b = (array-storage-vector arr)
          for i from 0 below (array-dimension arr 0)
          do(setf (aref b i) (aref arr i)))
    r
    ))
(defun copy-image(image)
  (declare (type (simple-array t (* * *)) image))
  (loop with new = (make-array (array-dimensions image) :element-type (array-element-type image))
        for y from 0 below (array-dimension image 0)
        do(loop for x from 0 below (array-dimension image 1)
                do(loop for c from 0 below (array-dimension image 2)
                        do(setf (aref new y x c)
                                (aref image y x c))))
        finally (return new)))

(defmacro whenv(test value &body body)
  (if (> 1 (length body))
      `(if ,test (progn ,@body) ,value)
      `(if ,test ,@body ,value)))
(defmacro unlessv(test value body)
  (if (> 1 (length body))
      `(if ,test ,value (progn ,@body))
      `(if ,test ,value ,@body)))
(defmacro whenz(test  &body body)
  "Returns body when test is not nil"
  (if (> 1 (length body))
      `(if ,test (progn ,@body) 0)
      `(if ,test ,@body 0)))
(defmacro unlessz(test &body body)
  "Returns zero unless test returns nil"
  (if (> 1 (length body))
      `(if ,test 0 (progn ,@body))
      `(if ,test 0 ,@body)))


