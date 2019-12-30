(defpackage img-genner
  (:use common-lisp)
  (:documentation "docstring"))

(defpackage img-genner/triangularization
  (:use common-lisp)
  (:documentation "Triangulation utilities.")
  (:export earclip complex-polygon-p fan-triangulate))
