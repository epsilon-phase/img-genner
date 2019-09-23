(in-package img-genner/examples)
(defun clock-face( hour minute second &optional (size 100) (pathname "clock.png"))
  (let ((center (make-array '(3 1) :element-type 'single-float
                                   :initial-contents `((,(/ size 2.0))(,(/ size 2.0))(0.0))))
        (hour-hand
          (img-genner:make-rectangle (/ size 2.0)
                                     (+ (/ size 2.0) (/ size 8.0))
                                     10.0
                                     (/ size 4)))
        (minute-hand (img-genner:make-rectangle (/ size 2.0)
                                                (+ (/ size 2)(/ size 4.0))
                                                5.0
                                                (/ size 2.0)))
        (second-hand (img-genner:make-rectangle (/ size 2.0)
                                                (+ (/ size 2.0)(/ size 4.0))
                                                3.0
                                                (/ size 2.0)))
        (image (png:make-image size size 3))
        (minute (- minute))
        (hour (- hour))
        (second (- second)))
    (img-genner:rotate-around hour-hand
                              center (+ (* hour 2 (/ 3.14 12))
                                        (* minute 2 (/ 3.14 12 60))))
    (img-genner:rotate-around minute-hand center
                              (+ (* minute 2 (/ 3.14 60))
                                 (* second 2 (/ 3.14 60 60))))
    (img-genner:rotate-around second-hand center
                                 (* second 2 (/ 3.14 60)))
    (img-genner:fill-rectangle hour-hand image (img-genner:static-color-stroker #(0 0 255)))
    (img-genner:fill-rectangle minute-hand image (img-genner:static-color-stroker #(255 0 0)))
    (img-genner:fill-rectangle second-hand image (img-genner:static-color-stroker #(255 255 0)))
    (png:encode-file image pathname)
    )
  )
(export '(clock-face))
