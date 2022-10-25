(uiop:define-package #:openrpc-server/errors
  (:use #:cl)
  (:import-from #:jsonrpc/errors))
(in-package #:openrpc-server/errors)


(defun return-error (message &key (code -1))
  "Raises an error to interrupt processing and return status to the caller."
  (error 'jsonrpc/errors:jsonrpc-callback-error
         :message message
         :code code))
