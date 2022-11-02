(uiop:define-package #:openrpc-client/error
  (:use #:cl)
  (:import-from #:serapeum
                #:pretty-print-hash-table)
  (:export
   #:rpc-error
   #:rpc-error-code
   #:rpc-error-message
   #:rpc-error-func-name
   #:rpc-error-func-arguments))
(in-package #:openrpc-client/error)


(define-condition rpc-error (error)
  ((code :initarg :code
         :reader rpc-error-code)
   (message :initarg :message
            :reader rpc-error-message)
   (func-name :initarg :func-name
              :reader rpc-error-func-name)
   (func-arguments :initarg :func-arguments
                   :reader rpc-error-func-arguments))
  (:report (lambda (condition stream)
             (format stream "RPC method ~S called with ~A returned error. Code: ~A, message: ~S"
                     (rpc-error-func-name condition)
                     (let ((args (rpc-error-func-arguments condition)))
                       (typecase args
                         (hash-table
                          (with-output-to-string (s)
                            (pretty-print-hash-table args s)))
                         (t
                          args)))
                     (rpc-error-code condition)
                     (rpc-error-message condition)))))
