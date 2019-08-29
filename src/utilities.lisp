(in-package "img-genner")

(defmacro with-array-items(items array &body body)
  `(symbol-macrolet ,(loop for i in items
                           collect (list (first i)  (cons 'aref (rest i)))
                           )
     ,@body)
  )
