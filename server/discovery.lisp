(uiop:define-package #:openrpc-server/discovery
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:jsonrpc/mapper)
  (:import-from #:lack.request)
  (:import-from #:openrpc-server/vars
                #:*current-request*)
  (:import-from #:openrpc-server/content-descriptor
                #:make-content-descriptor)
  (:import-from #:openrpc-server/method
                #:method-description
                #:method-deprecated
                #:method-summary
                #:get-params-as-content-descriptors
                #:get-method-result-as-content-descriptor)
  (:import-from #:openrpc-server/interface
                #:make-info)
  (:import-from #:openrpc-server/api
                #:get-method-info
                #:*current-api*
                #:api)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:alexandria
                #:when-let))
(in-package #:openrpc-server/discovery)


(defun generate-methods (api mapper)
  (check-type api api)

  (loop for name being the hash-key of mapper
          using (hash-value func)
        for method = (let ((method (make-hash-table :test #'equal)))
                       (setf (gethash "name" method)
                             name)
                       (cond
                         ((string= name "rpc.discover")
                          (setf (gethash "params" method)
                                #())
                          (setf (gethash "result" method)
                                (make-content-descriptor "OpenRPC Schema"
                                                         :reference "https://raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json"))
                          method)
                         (t
                          (when-let ((method-info (get-method-info api name)))
                            (setf (gethash "params" method)
                                  (or (get-params-as-content-descriptors api name)
                                      #()))
                            (setf (gethash "result" method)
                                  (get-method-result-as-content-descriptor api name))
                            
                            (when (method-summary method-info)
                              (setf (gethash "summary" method)
                                    (method-summary method-info)))
                            (when (method-description method-info)
                              (setf (gethash "description" method)
                                    (method-description method-info)))
                            (when (method-deprecated method-info)
                              (setf (gethash "deprecated" method)
                                    (method-deprecated method-info)))
                            
                            ;; Require to pass arguments as a dictionary.
                            ;; This way we'll be able to process keyword
                            ;; arguments of our methods.
                            (setf (gethash "paramStructure" method)
                                  "by-name")
                            method))))
        when method
          collect method))


(defun rpc-discover (server args)
  (declare (ignore args))

  (unless (boundp '*current-api*)
    (error "Please, call rpc-discover in context of running application."))
  
  (let ((response (make-hash-table :test 'equal))
        (req *current-request*))
    (setf (gethash "methods" response)
          (generate-methods *current-api* (jsonrpc/mapper::exposable-mapper server)))
    (setf (gethash "openrpc" response)
          "1.0.0")
    (setf (gethash "info" response)
          (make-info *current-api* server))
    (setf (gethash "servers" response)
          (list
           (dict "name" "default"
                 "url" (format nil "https://~A/"
                               (lack.request:request-server-name req))
                 )))
    response))


