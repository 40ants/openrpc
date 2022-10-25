(uiop:define-package #:openrpc-server/clack
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:jsonrpc/class
                #:bind-server-to-transport)
  (:import-from #:jsonrpc/transport/websocket
                #:websocket-transport)
  (:import-from #:jsonrpc/transport/http
                #:http-transport)
  (:import-from #:openrpc-server/discovery
                #:rpc-discover)
  (:import-from #:openrpc-server/method
                #:*methods*)
  (:import-from #:openrpc-server/vars
                #:*server*)
  (:export #:make-clack-app))
(in-package #:openrpc-server/clack)


;; (defun make-api-app ()
;;   (let ((server (jsonrpc:make-server))
;;         (http-transport (make-instance 'http-transport))
;;         (websocket-transport (make-instance 'websocket-transport)))
;;     ;; Only one transport can be used for message processing loop,
;;     ;; but we need to call both because this function sets a callback
;;     ;; for message dispatching inside the transport.
;;     (bind-server-to-transport server http-transport)
;;     ;; This transport will be used for message processing loop.
;;     (bind-server-to-transport server websocket-transport)

;;     (jsonrpc:expose server "rpc.discover"
;;                     (lambda (args)
;;                       (rpc-discover server args)))

;;     (loop for name being the hash-key of *methods*
;;           using (hash-value func)
;;           do (jsonrpc:expose server name func))

;;     (setf *server* server)
    
;;     (let ((websocket-app (jsonrpc/transport/websocket:make-clack-app websocket-transport))
;;           (http-app (jsonrpc/transport/http:make-clack-app http-transport)))
;;       (lambda (env)
;;         (reblocks/session:with-session (env)
;;           (cond
;;             ((wsd:websocket-p env)
;;              (funcall websocket-app env))
;;             ((and (string-equal (getf env :path-info)
;;                                 "/openrpc.json")
;;                   (eql (getf env :request-method)
;;                        :get))
;;              (list 200
;;                    (list :content-type "application/json")
;;                    (list (yason:with-output-to-string* ()
;;                            (yason:encode (rpc-discover server nil))))))
;;             (t
;;              (funcall http-app env))))))))

(defun make-clack-app (&key (http t)
                            (websocket t))
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

    (loop for name being the hash-key of *methods*
          using (hash-value func)
          do (jsonrpc:expose server name func))

    (setf *server* server)
    
    (let ((websocket-app (when websocket
                           (jsonrpc/transport/websocket:make-clack-app websocket-transport)))
          (http-app (when http
                      (jsonrpc/transport/http:make-clack-app http-transport))))
      (lambda (env)
        (cond
          ((and websocket
                (wsd:websocket-p env))
           (funcall websocket-app env))
          ((and (string-equal (getf env :path-info)
                              "/openrpc.json")
                (eql (getf env :request-method)
                     :get))
           (list 200
                 (list :content-type "application/json")
                 (list (yason:with-output-to-string* ()
                         (yason:encode (rpc-discover server nil))))))
          (http
           (funcall http-app env)))))))
