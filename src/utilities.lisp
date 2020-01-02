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
(print (macroexpand '(with-array-items ((a 1 2)(b 2 3)) c (setf a 2 b 5))))
