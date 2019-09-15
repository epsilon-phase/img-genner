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
