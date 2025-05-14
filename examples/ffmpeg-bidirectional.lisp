(in-package img-genner/examples)
(defmacro with-fifo-file(var &body body)
  "fifo nodes in common lisp are annoying as hell because most implementations
are blocking and therefore require harranguing to get to function"
  (let ((fifo-name (string var))
        (fifo-var (gensym)))
    `(progn
       (sb-ext:run-program "mkfifo"
                           '(,fifo-name)
                           :search t)
       (unwind-protect ,@body
         (delete-file ,fifo-name)))))
(defun mosaify-input-video(input-path-1 input-path-2 
                           output-path
                           tile-width
                           tile-height)
  (with-fifo-file input-1
    (with-fifo-file input-2
      (let* ((input-1 (sb-ext:run-program 
                       "ffmpeg"
                         `(;; Ignore all the annoying questions it
                           ;; might ask us. Might make it work with
                           ;; fifo nodes
                           "-y"
                         "-i" ,input-path-1
                           ;; image2pipe, our good friend, just writes
                           ;;image headers+data into the same stream
                         "-f" "image2pipe"
                           ;; write it as a png
                         "-c:v" "png"
                           ;;Write to the stringified symbol name.
                         "INPUT-1")
                       :output nil
                       :search t
                       :wait nil))
             (input-2 (sb-ext:run-program
                        "ffmpeg"
                        `("-y"
                         "-i" ,input-path-2
                         "-f" "image2pipe"
                         "-c:v" "png" "INPUT-2")
                       :output nil
                       :search t
                       :wait nil))
             (output (sb-ext:run-program 
                      "ffmpeg"
                      `("-f" "image2pipe"
                        "-i" "-" "-y"
                        ,output-path)
                      :wait nil
                      :input :stream
                      :search t ))
             (output-stream (sb-ext:process-input output)))
        ;; If we don't do this then it will block until there is data
        (sleep 1)
        (let (;;We could avoid all this if run-program allowed for specifying the element type of the streams, but literally
              ;;every library I've tried to get around this issue
              ;;doesn't work on character streams
              (input-1-stream (open "INPUT-1" :element-type '(unsigned-byte 8)))
              (input-2-stream (open "INPUT-2" :element-type '(unsigned-byte 8))))
          (format t "Reading from ~a stream~%" (stream-element-type input-1-stream)) 
          (unwind-protect (loop
                            for frame from 0
            for image-1 = (pngload:data (pngload:load-stream input-1-stream))
            for image-2 = (pngload:data (pngload:load-stream input-2-stream))
            do(print "Read frame ~i~%" frame)
            do(img-genner:save-image
               ;; Mosaify attempts to replace every tile of an image
               ;; with the nearest match tile of another image.
               (img-genner:mosaify
                ;; The first image specified is the source of tiles
                image-2
                ;; The second is the one it is matched against
                image-1
                ;;And these are the tile widths.
                tile-width tile-height) 
               ;; Look at glitch.lisp for more fun functions
               output-stream)
                            ;; Ideally this will stop the program
                            ;; when one or both streams are empty.
                            ;;
                            ;; Given fifo nodes' behavior I'm not so
                            ;; certain
            when (not (or (listen input-1-stream)
                          (listen input-2-stream)))
            do(return nil)
            )
            (close output-stream)
            (sb-ext:process-kill input-1 9)
            (sb-ext:process-kill input-2 9)
            (sb-ext:process-kill output 9)))))))