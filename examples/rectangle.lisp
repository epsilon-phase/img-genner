(in-package img-genner/examples)
(defun clock-face( hour minute second &optional (size 400) (pathname "clock.png"))
  (let ((center (make-array 2 :element-type 'single-float
                              :initial-contents `(,(/ size 2.0) ,(/ size 2.0))))
        (hour-hand
          (img-genner:make-rectangle (/ size 2.0)
                                     (+ (/ size 2.0) (/ size 8.0))
                                     10.0
                                     (/ size 4.0)))
        (minute-hand (img-genner:make-rectangle (/ size 2.0)
                                                (+ (/ size 2.0)(/ size 4.0))
                                                5.0
                                                (/ size 2.0)))
        (second-hand (img-genner:make-rectangle (/ size 2.0)
                                                (+ (/ size 2.0)(/ size 4.0))
                                                3.0
                                                (/ size 2.0)))
        (image (make-image size size))
        (minute (- minute))
        (hour (- hour))
        (second (- second)))
    (loop for i from 0 to 59
          with shape = (make-instance 'img-genner:rectangle :width 5.0 :height 10.0
                                                            :origin (img-genner:point (/ size 2.0)
                                                                                      (- size 5.0)))
          do(img-genner:fill-shape shape image
                                       (img-genner:static-color-stroker
                                        (if (zerop (mod i 5)) #(255 255 255) #(200 200 200))))
          do(img-genner:rotate-around shape (img-genner:point (/ size 2.0) (/ size 2.0)) 0.104))
    (img-genner:rotate-around hour-hand
                              center (+ (* hour 2 (/ 3.14 12))
                                        (* minute 2 (/ 3.14 12 60))))
    (img-genner:rotate-around minute-hand center
                              (+ (* minute 2 (/ 3.14 60))
                                 (* second 2 (/ 3.14 60 60))))
    (img-genner:rotate-around second-hand center
                                 (* second 2 (/ 3.14 60)))
    (img-genner:fill-rectangle hour-hand image (img-genner:static-color-stroker #(0 0 255)))
    (img-genner:fill-shape minute-hand image (img-genner:static-color-stroker #(255 0 0)))
    (img-genner:fill-shape second-hand image (img-genner:static-color-stroker #(255 255 0)))
    (save-image image pathname)
    )
  )
(export '(clock-face))
