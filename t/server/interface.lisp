(uiop:define-package #:openrpc-tests/server/interface
  (:use #:cl)
  (:import-from #:hamcrest/rove
                #:contains
                #:assert-that)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest)
  (:import-from #:openrpc-server
                #:type-to-schema)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:hamcrest/matchers
                #:has-hash-entries))
(in-package #:openrpc-tests/server/interface)


(deftest test-simple-types-schema ()
  (testing "Schema for integer"
    (assert-that (type-to-schema 'integer)
                 (has-hash-entries "type" "integer")))

  (testing "Schema for string"
    (assert-that (type-to-schema 'string)
                 (has-hash-entries "type" "string"))))


(deftest test-member-renders-as-enum ()
  (testing "When I use MEMBER in type, then it is transformed to enum."
    (assert-that (type-to-schema '(member :place :flowers :catering))
                 (has-hash-entries "type" "string"
                                   "enum" (list "PLACE" "FLOWERS" "CATERING")))))
