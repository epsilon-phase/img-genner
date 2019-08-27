(in-package 'img-genner)

(defmacro with-array-items(items array &body body)
  (loop for i in items
        for head =  (first i) then (first i)
        for tail = (rest i) then (rest i)
        do(setf body (subst (append `(aref ,array) tail) head body))
        )
  body
  )
