(uiop:define-package #:openrpc-server/clack
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:yason)
  (:import-from #:lack.request)
  (:import-from #:jsonrpc/server
                #:bind-server-to-transport)
  (:import-from #:jsonrpc/transport/websocket
                #:websocket-transport)
  (:import-from #:jsonrpc/transport/http
                #:http-transport)
  (:import-from #:openrpc-server/discovery
                #:rpc-discover)
  (:import-from #:openrpc-server/vars
                #:*current-request*)
  (:import-from #:openrpc-server/api
                #:api-server
                #:*current-api*
                #:api-methods)
  (:import-from #:openrpc-server/method
                #:method-thunk)
  (:import-from #:websocket-driver
                #:websocket-p)
  ;; On CCL prometheus.cl does not load propertly
  ;; See this issue:
  ;; https://github.com/deadtrickster/prometheus.cl/issues/2
  #-ccl
  (:import-from #:clack-prometheus
                #:with-prometheus-stats)
  (:import-from #:clack-cors
                #:make-cors-middleware)
  (:export
   #:make-clack-app
   #:app-middlewares
   #:debug-on
   #:debug-off))
(in-package #:openrpc-server/clack)


(defvar *allow-debug-header* nil
  "If set to true, then JSON-RPC request will be processed with interactive debugger enabled
   in case if X-Debug-On header is present in the request.")


(defun return-spec (server &key indent-json)
  (list 200
        (list :content-type "application/json")
        (list (yason:with-output-to-string* (:indent indent-json)
                (yason:encode (rpc-discover server nil))))))


(defun process-request (env server websocket websocket-app http http-app indent-json)
  (cond
    ((and websocket
          (websocket-p env))
     (funcall websocket-app env))
    ((and (string-equal (getf env :path-info)
                        "/openrpc.json")
          (eql (getf env :request-method)
               :get))
     (return-spec server :indent-json indent-json))
    (http
     (funcall http-app env))))


(defgeneric make-clack-app (api &key)
  (:documentation "Should return an app suitable for passing to clackup.

                   You can define a method to redefine application.
                   But to add middlewares it is more convenient to define a method for
                   APP-MIDDLEWARES generic-function."))

(defgeneric app-middlewares (api)
  (:documentation "Should return an plist of middlewared to be applied to the Clack application.

                   Keys should be a keyword with middleware name. And value is a function accepting
                   a Clack application as a single argument and returning a new application.

                   Middlewares are applied to the app from left to right. This makes it possible to
                   define an :around method which will inject or replace a middleware or original app.

                   Default method defines two middlewares with keys :CORS and :PROMETHEUS.
                   (Prometheus middleware works on SBCL but not on CCL).
                   To wrap these middlewares, add your middlewares to the end of the list.
                   To add your middleware inside the stack - push it to the front."))


(defmethod make-clack-app ((api t)
                           &key
                           (http t)
                           (websocket t)
                           (indent-json nil))
  "Returns an Clack application to serve JSON-RPC API."
  
  (unless (or http websocket)
    (error "Argument :http, :websocket or both should be set to T."))
  
  (let ((server (jsonrpc:make-server))
        (http-transport (make-instance 'http-transport))
        (websocket-transport (when websocket
                               (make-instance 'websocket-transport))))
    ;; Only one transport can be used for message processing loop,
    ;; but we need to call both because this function sets a callback
    ;; for message dispatching inside the transport.
    (when http
      (bind-server-to-transport server http-transport))
    ;; This transport will be used for message processing loop.
    (when websocket
      (bind-server-to-transport server websocket-transport))

    (jsonrpc:expose server "rpc.discover"
                    (lambda (args)
                      (rpc-discover server args)))

    ;; This will allow us to replace methods in the JSON-RPC
    ;; server when a new version of methods is defined using
    ;; DEFINE-API-METHOD macro:
    (setf (api-server api)
          server)

    (loop for name being the hash-key of (api-methods api)
          using (hash-value method-info)
          do (jsonrpc:expose server name
                             (method-thunk method-info)))

    (let ((websocket-app (when websocket
                           (jsonrpc/transport/websocket:make-clack-app websocket-transport)))
          (http-app (when http
                      (jsonrpc/transport/http:make-clack-app http-transport))))
      (lambda (env)
        (let ((*current-api* api)
              (*current-request* (lack.request:make-request env)))
          (process-request env server websocket websocket-app http http-app indent-json))))))


(defun process-debug-header (app)
  (flet ((debug-header-processor (env)
           (cond ((and *allow-debug-header*
                       (gethash "x-debug-on"
                                (getf env :headers)))
                  (let ((jsonrpc:*debug-on-error* t))
                    (funcall app env)))
                 (t
                  (funcall app env)))))
    #'debug-header-processor))


(defun return-allowed-headers (env response-headers)
  (declare (ignore env response-headers))
  (let ((result (or (uiop:getenv "CORS_ALLOWED_HEADERS")
                    clack-cors:*default-allowed-headers*
                    "Content-Type,Authorization")))
    (if *allow-debug-header*
        (if result
            (concatenate 'string result ",X-Debug-On")
            result)
        result)))


(defmethod app-middlewares ((api t))
  (flet ((cors-middleware (app)
           (make-cors-middleware
            app
            :allowed-origin (or (uiop:getenv "CORS_ALLOWED_ORIGIN")
                                clack-cors:*default-allowed-origin*
                                "*")
            :allowed-headers #'return-allowed-headers
            :allowed-methods (or (uiop:getenv "CORS_ALLOWED_METHODS")
                                 clack-cors:*default-allowed-methods*
                                 "POST"))))
    (list
     :debug #'process-debug-header
     :cors #'cors-middleware
     #-ccl
     :prometheus
     #-ccl
     #'with-prometheus-stats)))


(defun debug-on ()
  (setf *allow-debug-header* t)
  (values))


(defun debug-off ()
  (setf *allow-debug-header* nil)
  (values))
