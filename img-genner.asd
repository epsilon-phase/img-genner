(defsystem "img-genner"
  :version "0.0.1"
  :author "Violet White"
  :licence "Nonviolent Public License"
  :depends-on ("png" "rove")
  :components ((:file "src/package")
               (:file "src/utilities")
               (:file "src/transformations")
               (:file "src/colors")
               (:file "src/shapes")
               (:file "src/output")
               ))
(defsystem "img-genner-examples"
  :version "0.0.0"
  :author "Violet White"
  :depends-on ("img-genner"))
