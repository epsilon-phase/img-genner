(in-package img-genner)

(defmacro with-array-items(items array &body body)
  (let ((g (gensym)))
    `(let ((,g ,array))(symbol-macrolet ,(loop for i in items
                           collect (list (first i)  (cons 'aref (cons g (rest i))))
                           )
     ,@body)))
  )
(defun copy-vector-extend(vec item)
  (let ((v (make-array (1+ (array-dimension vec 0))
                       :element-type (array-element-type vec)
                       :fill-pointer 0
                       :adjustable t)))
    (loop for i across vec
          do(vector-push-extend i v))
    (vector-push-extend item v)
    v))
(defun convex(a b c)
  "returns true if the angle abc is convex"
  (declare (type (simple-array single-float (2)) a b c))
  (let ((a (sub-point a b))
        (c (sub-point c b)))
    (< (atan (- (aref a 1) (aref c 1))
             (- (aref a 0) (aref c 0)))
       pi)))
(print (macroexpand '(with-array-items ((a 1 2)(b 2 3)) c (setf a 2 b 5))))
