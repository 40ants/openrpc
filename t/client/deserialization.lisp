(uiop:define-package #:openrpc-tests/client-deserialization
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:serapeum
                #:dict))
(in-package #:openrpc-tests/client-deserialization)


(defclass test-class ()
  ())


(deftest test-one-of-deserialization ()
  (let* ((result-spec (dict "oneOf"
                            (list
                             (dict "type" "null")
                             (dict "type" "object"
                                   "properties"
                                   (dict "foo"
                                         (dict "type" "string"))
                                   "x-cl-class" "FOO"
                                   "x-cl-package" "OPENRPC-TESTS/CLIENT-DESERIALIZATION"))))
         (classes-cache (dict))
         (transform
           (openrpc-client/core::generate-result-transformation
            'class-name
            'raw-response
            result-spec
            classes-cache)))
    (ok (equal transform
               '(when raw-response
                 (FUNCALL #'MAKE-INSTANCE 'FOO
                  :FOO (GETHASH "foo" RAW-RESPONSE)))))))
