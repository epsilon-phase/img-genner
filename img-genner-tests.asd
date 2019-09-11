(defsystem "img-genner-tests"
  :depends-on ("img-genner" "rove")
  :components ((:file "tests/package")
               (:file "tests/interpolation"))
  )
