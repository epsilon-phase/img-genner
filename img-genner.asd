(defsystem "img-genner"
  :version "0.0.1"
  :author "Violet White"
  :licence "MIT"
  :depends-on ("pngload" "zpng" "cl-jpeg"
                         "alexandria"
                         "pcall"
                     )
  :components ((:file "src/package")
               (:file "src/triangularization/triangles")
               (:file "src/utilities")
               (:file "src/transformations")
               (:file "src/colors")
               (:file "src/interpolation")
               (:file "src/shapes")
               (:file "src/output")
               (:file "src/glitch")
               ))
