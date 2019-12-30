(defsystem "img-genner"
  :version "0.0.1"
  :author "Violet White"
  :licence "Nonviolent Public License"
  :depends-on ("png")
  :components ((:file "src/package")
               (:file "src/utilities")
               (:file "src/transformations")
               (:file "src/colors")
               (:file "src/interpolation")
               (:file "src/shapes")
               (:file "src/output")
               (:file "src/glitch")
               ))
