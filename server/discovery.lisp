(uiop:define-package #:openrpc-server/discovery
  (:use #:cl)
  (:import-from #:lack.request)
  (:import-from #:openrpc-server/vars
                #:*current-request*)
  (:import-from #:openrpc-server/api
                #:api-methods)
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
                #:when-let)
  (:export
   #:generate-spec))
(in-package #:openrpc-server/discovery)


(defun generate-methods (api)
  (check-type api api)

  (loop for name being the hash-key of (api-methods api)
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


(declaim (ftype (function (api &key (:server-url string)) hash-table)
                generate-spec))
(defun generate-spec (api &key (server-url "http://localhost:5000/"))
  "Returns a dictionary with a full OpenRPC specification of the API.
   This specification can be published on the web or saved to the JSON or YAML file.

   Also, you might want to use this function in the REPL to inspect the spec like this:

   ```
   FOO> (serapeum:toggle-pretty-print-hash-table)
   T
   FOO> (openrpc-server/discovery:generate-spec test)
   (SERAPEUM:DICT
     \"methods\" '((SERAPEUM:DICT
                   \"name\" \"foo\"
                   \"params\" '((SERAPEUM:DICT
                                \"name\" \"bar\"
                                \"schema\" (SERAPEUM:DICT
                                           \"type\" \"string\")))
                   \"result\" (SERAPEUM:DICT
                              \"name\" \"foo_result\"
                              \"schema\" (SERAPEUM:DICT
                                         \"type\" \"integer\"))
                   \"paramStructure\" \"by-name\"))
     \"openrpc\" \"1.0.0\"
     \"info\" (SERAPEUM:DICT
              \"title\" \"Default API\"
              \"version\" \"0.1.0\")
     \"servers\" '((SERAPEUM:DICT
                   \"name\" \"default\"
                   \"url\" \"http://localhost:5000/\")))
   ```
"

  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "methods" response)
          (generate-methods api))
    (setf (gethash "openrpc" response)
          "1.0.0")
    (setf (gethash "info" response)
          (make-info api))
    (setf (gethash "servers" response)
          (list
           (dict "name" "default"
                 "url" server-url)))
    response))


(defun rpc-discover (server args)
  (declare (ignore server args))

  (unless (boundp '*current-api*)
    (error "Please, call rpc-discover in context of running application."))

  (let ((req *current-request*))
    (generate-spec *current-api*
                   :server-url (format nil "https://~A/"
                                       (lack.request:request-server-name req)))))


