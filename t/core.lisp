(uiop:define-package #:openrpc-tests/core
  (:use #:cl)
  (:import-from #:hamcrest/rove
                #:contains
                #:assert-that)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest))
(in-package #:openrpc-tests/core)


(deftest test-pet-shop-example
  (testing "Just example"
    (ok (equal (length "Foo")
               3))))
