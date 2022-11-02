(uiop:define-package #:openrpc-server/utils
  (:use #:cl)
  (:import-from #:str
                #:replace-all))
(in-package #:openrpc-server/utils)


(defun sym-to-string (sym)
  (string-downcase (symbol-name sym)))


(defun sym-to-api-string (sym)
  "Makes a string which can be used as method argument name or a key in the object.

   Here we need to replace '-' with '_', because languages not so advanced as Common Lisp
   are unable to work with variable names having '-' symbol"
  (replace-all "-" "_" (sym-to-string sym) ))

