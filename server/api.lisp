(uiop:define-package #:openrpc-server/api
  (:use #:cl)
  (:export #:define-api
           #:*current-api*
           #:api
           #:api-version
           #:api-title
           #:api-methods))
(in-package #:openrpc-server/api)


(defvar *current-api*)


(defclass api ()
  ((methods :initform (make-hash-table :test 'equal)
            :reader api-methods)
   (title :initform "Default API"
          :initarg :title
          :reader api-title)
   (version :initform "0.1.0"
            :initarg :version
            :reader api-version)
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

