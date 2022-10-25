(uiop:define-package #:openrpc-example/client
  (:use #:cl)
  (:import-from #:openrpc-client
                #:generate-client))
(in-package #:openrpc-example/client)

(generate-client petshop
                 "http://localhost:8000/openrpc.json")


(defun make-test-client ()
  (let ((cl (make-petshop)))
    (jsonrpc:client-connect cl :url "http://localhost:8000/" :mode :http)
    cl))
