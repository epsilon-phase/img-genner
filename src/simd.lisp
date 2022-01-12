#| 
 | This file implements a few routines from the img-genner package that capable of being used with sb-simd
 | to provide some rather impressive acceleration compared to the otherwise non-simd optimized functions.
 | Or at least that's the aspiration here.
 |#


(defpackage #:img-genner/simd
  (:use common-lisp sb-simd)
  (:export diff-image diff-image-slow))
(in-package #:img-genner/simd)
(require :sb-simd)
(use-package :sb-simd)

(declaim (sb-ext:maybe-inline get-pixel))
(defun get-pixel(image x y)
  (declare (type (simple-array u8 (* * 3)) image)
           (type s64 x y))
  (values (u8-aref image y x 0) (u8-aref image y x 1) (u8-aref image y x 2)))

#|
 | 
 |#
(defun diff-image(i1 i2 &optional result-image)
  (declare (type (simple-array u8 (* * 3)) i1 i2)
           (type (or (simple-array u8 (* * 3)) null) result-image)
           (optimize speed (safety 0)))
  (let ((r (if result-image
               result-image
               (make-array (list (array-dimension i1 0) (array-dimension i1 1) 3) :element-type 'u8 :initial-element 0))))
    (declare (type (simple-array u8 (* * 3)) r))
    (do-vectorized (x 0 (1- (the u64 (array-total-size i1))))
      (setf (u8-row-major-aref r x)
            (u8- (u8-row-major-aref i1 x)
                  (u8-row-major-aref i2 x))))
    r))


(defun diff-image-slow(i1 i2 &optional result-image)
  (declare (type (simple-array u8 (* * 3)) i1 i2)
           (type (or null (simple-array u8 (* * 3))) result-image)
           (optimize speed (safety 0)))
  (loop with result = (if result-image result-image (make-array (list (array-dimension i1 0) (array-dimension i1 1) 3) :element-type 'u8 :initial-element 0))
        for y from 0 below (array-dimension i1 0)
        do(loop for x from 0 below (array-dimension i1 1)
                do(setf (aref result y x 0)
                        (- (aref i1 y x 0) (aref i2 y x 0))
                        (aref result y x 1)
                        (- (aref i1 y x 1) (aref i2 y x 1))
                        (aref result y x 2)
                        (- (aref i1 y x 2) (aref i2 y x 2))))
        finally(return result)))

                                        ; We have a suspicion that this would work better with the image changed into f32s 
(defun upscale-x2-linear-simd(image)
  (declare (type (simple-array u8 (* * 3))))
  (loop with result = (img-genner:make-image (* (array-dimension image 1) 2) (* (array-dimension image 0) 2))
        for y from 0 below (array-dimension image 0)
        do(do-vectorized (x 0 (1- (array-dimension image 1)))
            (setf (u8-aref result (* y 2) (* x 2) 0) (f32)
        finally (return result)))
(export '(diff-image diff-image-slow))
