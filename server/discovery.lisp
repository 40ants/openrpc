(uiop:define-package #:openrpc-server/discovery
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:openrpc-server/vars
                #:*server*)
  (:import-from #:openrpc-server/content-descriptor
                #:make-content-descriptor)
  (:import-from #:openrpc-server/method
                #:get-params-as-content-descriptors
                #:get-method-result-as-content-descriptor)
  (:import-from #:openrpc-server/interface
                #:make-info))
(in-package #:openrpc-server/discovery)


(defun generate-methods (mapper)
  (loop for name being the hash-key of mapper
        using (hash-value func)
        collect (let ((method (make-hash-table :test #'equal)))
                  (setf (gethash "name" method)
                        name)
                  (cond
                    ((string= name "rpc.discover")
                     (setf (gethash "params" method)
                           #())
                     (setf (gethash "result" method)
                           (make-content-descriptor "OpenRPC Schema"
                                                    :reference "https://raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json")))
                    (t
                     (setf (gethash "params" method)
                           (get-params-as-content-descriptors name))
                     (setf (gethash "result" method)
                           (get-method-result-as-content-descriptor name))
                     ;; Require to pass arguments as a dictionary.
                     ;; This way we'll be able to process keyword
                     ;; arguments of our methods.
                     (setf (gethash "paramStructure" method)
                           "by-name")))
                  method)))


(defun rpc-discover (server args)
  (declare (ignore args))
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "methods" response)
          (generate-methods (jsonrpc/mapper::exposable-mapper *server*)))
    (setf (gethash "openrpc" response)
          "1.0.0")
    (setf (gethash "info" response)
          (make-info server))
    response))


