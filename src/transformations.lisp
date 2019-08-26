(in-package "img-genner")

; From https://rosettacode.org/wiki/Matrix_multiplication#Common_Lisp
(defun mmul (A B)
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (C (make-array `(,m ,l) :initial-element 0.0 :element-type 'single-float)))
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
  (matrix (make-array '(3 3) :element-type 'single-float
                                       :initial-contents '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0)))
   :type (simple-array single-float (3 3))))
(defun make-rotation(angle)
  "Create a rotation matrix"
  (make-transf-matrix :kind 'rot :matrix
                      (make-array '(3 3)
                                  :element-type 'single-float
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
                                          :element-type 'single-float
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
(let ((a (make-rotation (coerce (* pi .25) 'single-float))))
  (print a)
  (print (explain-matrix a)
          ))
(let ((tranf (make-instance 'transformation)))
  (loop for i from 0 to 10
        do(add-transform tranf
                         (case (truncate (random 3))
                           ((0) (make-rotation (coerce (random (* 2 pi)) 'single-float)))
                           ((1) (make-translation (random 1.3) (random 1.3)))
                           ((2) (make-scale (random 1.3) (random 1.3)))
                           )
                         ))
  (print tranf)
  (print (slot-value tranf 'combined))
  (map nil #'print (slot-value tranf 'objects))
  (print (mmul
          (transf-matrix-matrix (slot-value tranf 'combined))
          (make-array '(3 1) :initial-contents '((3.0) (2.0) (0.0))))))

(defun point(x y)
  (the single-float x)
  (the single-float y)
  (make-array '(3 1) :element-type 'single-float :initial-contents `((,x) (,y) (0.0))))

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

