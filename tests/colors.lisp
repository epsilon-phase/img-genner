(in-package img-genner/tests)
(use-package :img-genner)

(deftest color-test
  (testing "Byte order"
    (ok (equalp #(255 255 0) (get-color "Yellow"))
        "Byte order correct"
        )
    )
  (testing "alpha"
    (ok (equalp #(255 255 0 255) (get-color "Yellow" t)))
    "Alpha extension works :)"
    )
  )

(deftest color-comparison-test
    (testing "First byte"
             (ok (not (compare-colors-bytewise #(3 0 0) #(2 0 0))))
             (ok (compare-colors-bytewise #(2 0 0) #(3 0 0)))
             )
  (testing "Second byte"
           (ok (not (compare-colors-bytewise #(2 3 0) #(2 2 0))))
           (ok  (compare-colors-bytewise #(2 2 0) #(2 3 0)))
           )
  )
