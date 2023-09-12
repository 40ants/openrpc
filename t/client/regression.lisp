(uiop:define-package #:openrpc-tests/client/regression
  (:use #:cl)
  (:import-from #:rove
                #:deftest)
  (:import-from #:openrpc-client/core
                #:%generate-client
                #:retrieve-spec))
(in-package #:openrpc-tests/client/regression)


(deftest client-regression ()
  (let ((spec
          (rove:testing "Reading spec"
            (retrieve-spec
             ;; TODO: repeat this test for all subfolders of #P"t/client"
             (asdf:system-relative-pathname :openrpc-tests
                                            (make-pathname :directory '(:relative "t" "client" "regress-data" "multiple-types")
                                                           :name "spec"
                                                           :type "json"))))))
    (rove:ok spec)
    
    (multiple-value-bind (client-class class-definitions methods)
        (rove:testing "Generating client"
          (%generate-client 'the-class spec :export-symbols nil))

      ;; TODO: Here we need to compare results with forms read from:
      ;; - #P"t/client/regress-data/multiple-types/client-class.lisp"
      ;; - #P"t/client/regress-data/multiple-types/class-definitions.lisp"
      ;; - #P"t/client/regress-data/multiple-types/methods.lisp"
      (rove:ok client-class)
      (rove:ok class-definitions)
      (rove:ok methods))))
