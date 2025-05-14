(in-package img-genner/examples)

(defun run-ffmpeg-example()
  "Demonstrate how to run ffmpeg to generate video files"
  (let* ((program (sb-ext:run-program "ffmpeg" 
                                      '(
                                        ;; Use the image2pipe option for reading stdin
                                        "-f" "image2pipe"
                                        ;; Read from stdin
                                        "-i" "-"
                                        ;; Assume we want to overwrite the output file if it exists
                                        ;; This answers yes to every prompt that you pass it
                                        "-y"
                                        ;; The output file :3
                                        "ellipse-gradient.mp4")
                                      ;; Create an input stream for the ffmpeg process to read from
                                      :input :stream
                                      ;; Search the $PATH variable instead of assuming an absolute path
                                      :search t
                                      ;; Don't wait for the process to finish before continuing the lisp code
                                      :wait nil))
         ;; Grab the input stream for the ffmpeg program.
         (input-strm (sb-ext:process-input program))
         
         (image (img-genner:make-image 640 480 
                                       (img-genner:rgb 0 0 0))))
    (loop for frame from 0 to 360
          ;; The images are stored in row-major order, so hieght, width, pixel-element
          with image-center-x = (/ (array-dimension image 1) 2)
          with image-center-y = (/ (array-dimension image 0) 2)
          with orange = (img-genner:rgb 255 255 0)
          with purple = (img-genner:rgb 255 0 255)
          for other-color = (img-genner:color-interpolate orange purple (/ frame 360.0))
          ;;For silly reasons, this needs grass fed floating point numbers passed in. I will fix that at some point
          with ellipse = (img-genner:make-ellipse image-center-x
                                                  image-center-y
                                                  ;; The radii of the ellipse
                                                  200.0 200.0)
          ;; The effective angle in radians
          for angle = (* (/ frame 60.0) (* 2 pi))
          ;; The center of the gradient
          for cx = (+ 320 (* 50 (cos angle)))
          for cy = (+ 240 (* 50 (sin angle)))
          ;; The brush to stroke with
          for stroker = (img-genner:radial-gradient-stroker 
                         (img-genner:rgb 255 0 0)
                         other-color
                         cx cy
                         100.0)
          ;;Clear the image on every frame. It's best to reuse image buffers when possible
          do(img-genner:clear-image image (img-genner:rgb 0 0 0))
          ;;Fill the ellipse
          do(img-genner:fill-ellipse ellipse image
                                     stroker)
          ;;Print a helpful message on the progress of the writing
          do(format t "Frame ~a~%" frame)
          ;;Write the frame to the ffmpeg input stream
          do(img-genner:save-image image input-strm))
    ;;Close the ffmpeg input stream, causing it to finish reading and flush whatever's there. 
    ;; This likely happens anyway, but this way we ensure that it finishes
    (close input-strm)
    ))
(export '(run-ffmpeg-example))