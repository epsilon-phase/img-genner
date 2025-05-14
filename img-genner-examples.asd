(defsystem "img-genner-examples"
  :version "0.0.0"
  :author "Violet White"
  :depends-on ("img-genner" "png-read" "alexandria")
  :components ((:file "examples/package")
               (:file "examples/ellipse")
               (:file "examples/rectangle")
               (:file "examples/ffmpeg")
               (:file "examples/ffmpeg-bidirectional")))
