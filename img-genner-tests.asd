(defsystem "img-genner-tests"
  :depends-on ("img-genner" "rove" "png")
  :components ((:file "tests/package")
               (:file "tests/interpolation")
               (:file "tests/colors")
               (:file "tests/shapes")
               (:file "tests/runthem"))
  )
