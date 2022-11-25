(uiop:define-package #:openrpc-server/clack
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:yason)
  (:import-from #:lack.request)
  (:import-from #:jsonrpc/class
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
                #:api-methods
                #:default-api)
  (:import-from #:openrpc-server/method
                #:method-thunk)
  (:import-from #:websocket-driver
                #:websocket-p)
  (:export
   #:make-clack-app))
(in-package #:openrpc-server/clack)


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


(defun make-clack-app (&key
                         (api default-api)
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
