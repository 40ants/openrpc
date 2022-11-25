(uiop:define-package #:openrpc-server/errors
  (:use #:cl)
  (:import-from #:jsonrpc)
  (:import-from #:jsonrpc/errors)
  (:export
   #:return-error))
(in-package #:openrpc-server/errors)


(defun return-error (message &key (code -1)
                               (error-class 'jsonrpc/errors:jsonrpc-callback-error))
  "Raises an error to interrupt processing and return status to the caller."
  (error error-class
         :message message
         :code code))
