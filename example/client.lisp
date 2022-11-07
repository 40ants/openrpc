(uiop:define-package #:openrpc-example/client
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:openrpc-client
                #:generate-client))
(in-package #:openrpc-example/client)

;; (generate-client petshop
;;                  "http://localhost:8000/openrpc.json")
;; 
;; Here we use spec from the disk, because during the build
;; Example server is not available yet:
(generate-client petshop
                 (asdf:system-relative-pathname :openrpc-example
                                                "example/autogenerated-spec.json"))



(defun make-test-client ()
  (let ((cl (make-petshop)))
    (jsonrpc:client-connect cl :url "http://localhost:8000/" :mode :http)
    cl))
