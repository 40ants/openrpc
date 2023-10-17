(uiop:define-package #:openrpc-server/api
  (:use #:cl)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:export #:define-api
           #:*current-api*
           #:api
           #:api-version
           #:api-title
           #:api-methods))
(in-package #:openrpc-server/api)


(defvar-unbound *current-api*
  "Points to a current API object when processing any RPC method.")


(defclass api ()
  ((methods :initform (make-hash-table :test 'equal)
            :reader api-methods
            :documentation "Returns a hash-table containing meta-information about all API methods.

                            Returned hash-table has key strings having methods names and internal
                            objects of class `method-info` as values. I'm not sure if we need to
                            export functions to manipulate with method info objects manually.
                            Use OPENRPC-SERVER/METHOD:DEFINE-RPC-METHOD macro to add or update RPC methods.")
   (title :initform "Default API"
          :initarg :title
          :reader api-title
          :documentation "Returns a title of the API.")
   (version :initform "0.1.0"
            :initarg :version
            :reader api-version
            :documentation "Returns a version of the API.")
   (server :initform nil
           :accessor api-server)))


(defmacro define-api ((name &key
                              (title "Default API")
                              (version "0.1.0")))
  `(progn
     (defclass ,name (api)
       ())

     (defvar ,name (make-instance ',name
                                  :title ,title
                                  :version ,version))))


(defgeneric add-api-method (api method-name method-info)
  (:method ((api api) method-name method-info)
    (setf (gethash method-name (api-methods api))
          method-info)))


(defgeneric get-method-info (api name)
  (:method ((api api) name)
    (gethash name (api-methods api))))


(define-api (default-api))

