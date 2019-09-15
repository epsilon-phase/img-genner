(require 'png)
(in-package img-genner/examples)
(use-package :img-genner)
(defun ellipse-test()
  (let ((a (img-genner:make-ellipse 50.0 50.0 50.0 50.0))
        (b (png:make-image 101 101 3)))
    (fill-shape (get-segments a) b (static-color-stroker #(255 0 0)))
    (with-open-file (f "solid-ellipse.png" :direction
                       :output :element-type '(unsigned-byte 8)
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
      (png:encode b f))
    (fill-shape (get-segments a :max-degree 20) b
                (radial-gradient-stroker #(255 0 0 0) #(0 255 0) 50 50 50))
    (with-open-file (f "radial-ellipse.png" :direction :output
                                            :element-type '(unsigned-byte 8)
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
      (png:encode b f))
    ))
(defun more-ellipses()
  (let ((a (loop repeat 8
                 for i = 20.0 then (+ i 35)
                 collect (make-ellipse i 20.0 20.0 20.0)))
        (colors (list (vector 255 0 0) (vector 0 255 0) (vector 0 0 255) (vector 255 0 255)
                      (vector #xb0 #x0b #x1e)))
        (b (png:make-image 40 400 3)))
    (setf (cdr (last colors)) colors)
    (loop for i in a
          for c in colors
          do(fill-shape (get-segments i :max-degree 20)
                        b
                        (static-color-stroker c))
          )
    (with-open-file (f "circles.png" :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (png:encode b f)
      )
    ))
(defun nested-ellipse()
  (let ((a (make-ellipse 50.0 50.0 50.0 50.0))
        (b (make-ellipse 50.0 51.0 25.0 25.0))
        (img (png:make-image 100 101 3))
        )
    (fill-shape (append (get-segments a :max-degree 20)
                        (get-segments b :max-degree 4))
                img
                (static-color-stroker #(255 0 0))
                )
    (with-open-file (f "nested.png" :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
      (png:encode img f))
    ))
(defun specialized-filler()
  (let ((m (make-ellipse 40.0 22.0 20.0 20.0))
        (b (make-ellipse 40.0 150.0 40.0 20.0))
        (r (make-rectangle 20.0 80.0 15.0 30.0))
        (image (png:make-image 300 80 3))
        )
    (fill-ellipse m image (static-color-stroker #(255 0 0)))
    (fill-ellipse-loopless b image (static-color-stroker #(0 255 0)))
    (fill-rectangle r image (static-color-stroker #(0 255 255)))
    (with-open-file (f "specialized.png" :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
      (png:encode image f))))
(export '(ellipse-test more-ellipses nested-ellipse specialized-filler))
