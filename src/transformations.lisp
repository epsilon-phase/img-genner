(in-package "img-genner")

; From https://rosettacode.org/wiki/Matrix_multiplication#Common_Lisp
(defun mmul (A B)
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (C (make-array `(,m ,l) :initial-element 0.0 )))
    (loop for i from 0 to (- m 1) do
      (loop for k from 0 to (- l 1) do
        (setf (aref C i k)
              (loop for j from 0 to (- n 1)
                    sum (* (aref A i j)
                           (aref B j k))))))
    C))

(defclass transformation()
  ((combined :initform (make-transf-matrix))
   (objects :initform (list (make-transf-matrix)))))

(defstruct transf-matrix
  (kind 'iden :type symbol)
  (matrix (make-array '(3 3) 
                                       :initial-contents '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0)))
   :type (simple-array t (3 3))))
(defun make-rotation(angle)
  "Create a rotation matrix"
  (make-transf-matrix :kind 'rot :matrix
                      (make-array '(3 3)

                                  :initial-contents `((,(cos angle) ,(sin angle) 0.0)
                                                      (,(* -1.0 (sin angle)) ,(cos angle) 0.0)
                                                      (0.0 0.0 1.0)))))
(defun make-identity()
  "Create an identity matrix :p"
  (make-transf-matrix :kind 'iden))
(defun make-translation(x y)
  (let ((r (make-transf-matrix :kind 'trans)))
    (setf (aref (transf-matrix-matrix r) 0 2) x)
    (setf (aref (transf-matrix-matrix r) 1 2) y)
    r))
(defun make-scale(sx sy)
  (make-transf-matrix :kind 'scale
                      :matrix (make-array '(3 3)
                                          :initial-contents `((,sx 0.0 0.0)(0.0 ,sy 0.0) (0.0 0.0 1.0))
                                                       )))
(defun explain-matrix(tra)
  (let ((mat (transf-matrix-matrix tra))
        (kind (transf-matrix-kind tra)))
    (case kind
      ((iden) "Identity matrix")
      ((rot)
       (format nil "Rotation by ~a degrees"
               (* (/ 180 pi) (acos (aref mat 0 0)))))
      ((trans) ; :3
       (format nil "Translated by ~a across x and ~a across y"
               (aref mat 0 2)
               (aref mat 1 2))
       )
      ((scale)
       (format nil "Scaled by ~a across x and ~a across y"
               (aref mat 0 0)
               (aref mat 1 1)))
      ((shear)
       (format nil "Sheared by a factor of ~a across x and a factor of ~a across y"
               (aref mat 0 1)
               (aref mat 1 0))))))
(defgeneric transform(transform transformee))
(defun add-transform(transformation trans)
  (push trans (slot-value transformation 'objects))
  (setf (slot-value transformation 'combined)
        (make-transf-matrix :kind 'combined :matrix
                            (reduce #'mmul (map 'list #'(lambda (x) (transf-matrix-matrix x))
                                                (slot-value transformation 'objects)))))
  )

(defun point(x y)
  (make-array '(3 1) :element-type 'single-float :initial-contents `((,(coerce x 'single-float)) (,(coerce y 'single-float)) (0.0))))

(defmethod transform((tr transformation) (m array))
  (mmul (transf-matrix-matrix  (slot-value tr 'combined)) m)
  )


#|
(with-transform transform &restbody)
|#
(defmacro with-transform(tr &body body)
  (loop for i in body
        collect `(transform ,tr ,i))
  )
                                        ;TODO remove most of the transform code,
                                        ;it's both not used and not pleasant to
                                        ;work with, in our opinion

(defun compare-points(a b)
  (if (= (aref a 0 0) (aref b 0 0))
      (< (aref a 1 0) (aref b 1 0))
      (< (aref a 0 0) (aref b 0 0))
      ))
(defun distance2d(x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2)
           (expt (- y1 y2) 2))))
(defun chess-distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))
(defun biggest-magnitude(x1 y1 x2 y2)
  (max  (abs (- x1 x2)) (abs (- y2 y1))))
(defun point-distance(a b)
  (distance2d (aref a 0 0) (aref a 1 0) (aref b 0 0) (aref b 1 0)))
(defun add-point(a b)
  (point (+ (aref a 0 0) (aref b 0 0))
         (+ (aref b 1 0) (aref b 1 0))))
(defparameter *distance-functions* (list #'chess-distance #'distance2d #'biggest-magnitude))
(export '(chess-distance biggest-magnitude *distance-functions*))
