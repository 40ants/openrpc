(uiop:define-package #:openrpc-server/utils
  (:use #:cl))
(in-package #:openrpc-server/utils)


(defun sym-to-string (sym)
  (string-downcase (symbol-name sym)))

