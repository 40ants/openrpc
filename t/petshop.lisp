(uiop:define-package #:openrpc-tests/petshop
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:clack.test)
  (:import-from #:rove
                #:testing
                #:ok
                #:deftest)
  (:import-from #:openrpc-server
                #:define-api)
  (:import-from #:openrpc-server/clack
                #:make-clack-app)
  (:import-from #:clack.test
                #:localhost)
  (:import-from #:clack.test
                #:testing-app)
  (:import-from #:openrpc-client
                #:generate-client)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:bordeaux-threads
                #:*default-special-bindings*)
  (:import-from #:bordeaux-threads-2)
  (:import-from #:openrpc-example)
  (:import-from #:openrpc-example/server
                #:pets-api
                #:*pets*))
(in-package #:openrpc-tests/petshop)


(defmacro with-empty-pet-store (() &body body)
  `(progn
     (clrhash *pets*)
     ,@body))


(deftest test-pet-shop-example
  (with-empty-pet-store ()
    (testing-app "Checking PetShop"
        (make-clack-app pets-api)
      (let* ((url (localhost "/openrpc.json"))
             (test-package (make-package "test-package1" :use (list :cl)))
             (api-symbol (intern "PETSHOP" test-package)))
        (unwind-protect 
             (let* ((*package* test-package))
               (testing "Client classes creation"
                 (eval `(generate-client ,api-symbol ,url))
                
                 (let ((client (uiop:symbol-call test-package "MAKE-PETSHOP")))
                   (jsonrpc:client-connect client :url (localhost "/") :mode :http)
                  
                   (testing "Initially, there is no pets"
                     (ok (null (uiop:symbol-call test-package :list-pets client)))
                     ;; Now add a 9 cats
                     (loop for i from 1 upto 9
                           do (uiop:symbol-call test-package :create-pet client
                                                (format nil "Cat ~A" i)
                                                "cat"))

                     (multiple-value-bind (first-page get-second-page)
                         (uiop:symbol-call test-package :list-pets client :limit 4)
                       (ok (length= 4 first-page))
                       (ok (equal (mapcar (find-symbol "PET-NAME" test-package)
                                          first-page)
                                  (list "Cat 1" "Cat 2" "Cat 3" "Cat 4")))
                       ;; Now we'll check if our pagination works
                       (ok get-second-page)
                       (multiple-value-bind (second-page get-third-page)
                           (funcall get-second-page)
                         (ok (length= 4 second-page))
                         (ok (equal (mapcar (find-symbol "PET-NAME" test-package)
                                            second-page)
                                    (list "Cat 5" "Cat 6" "Cat 7" "Cat 8")))
                         (ok get-third-page)
                         (multiple-value-bind (third-page get-fourth-page)
                             (funcall get-third-page)
                           (ok (length= 1 third-page))
                           (ok (equal (mapcar (find-symbol "PET-NAME" test-package)
                                              third-page)
                                      (list "Cat 9")))
                           ;; There is no pages anymore
                           (ok (null get-fourth-page)))))))))
          (delete-package test-package))))))
