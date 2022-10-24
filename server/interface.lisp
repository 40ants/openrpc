(uiop:define-package #:openrpc-server/interface
  (:use #:cl)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:alexandria
                #:length=))
(in-package #:openrpc-server/interface)


(defgeneric transform-result (object)
  (:documentation "Prepares object for serialization before responding to RPC call.

Result should be list, hash-map or a value of primitive type.")
  (:method ((object list))
    (mapcar #'transform-result object)))


(defgeneric primitive-type-p (type)
  (:method ((type t))
    nil)
  (:method ((type (eql 'integer)))
    t)
  (:method ((type (eql 'string)))
    t))


(defgeneric type-to-schema (type)
  (:method ((type t))
    (cond
      ((primitive-type-p type)
       (dict "type"
             (string-downcase (symbol-name type))))
      ;; Non paginated results:
      ((and (listp type)
            (symbolp (car type))
            (string-equal (car type)
                          "list-of"))
       (unless (length= 2 type)
         (error "Type definition ~S should have this form (LIST-OF ~A)."
                type
                (or (second type)
                    "SOME-TYPE")))
       (dict "type" "array"
             "items" (type-to-schema (second type))))
      ;; Paginated results:
      ((and (listp type)
            (symbolp (car type))
            (string-equal (car type)
                          "paginated-list-of"))
       (unless (length= 2 type)
         (error "Type definition ~S should have this form (PAGINATED-LIST-OF ~A)."
                type
                (or (second type)
                    "SOME-TYPE")))
       (dict "type" "object"
             "properties" (dict "items" (dict "type" "array"
                                              "items" (type-to-schema (second type)))
                                "next-page-key" (dict "type" "string"))
             "required" (list "items")
             "x-paginated-list" t))
      (t
       (error "Type ~S is not supported. Please, define ~S method for it."
              type
              'type-to-schema)))))


(defgeneric make-info (server)
  (:method ((server jsonrpc:server))
    (let ((info (make-hash-table :test 'equal)))
      (setf (gethash "title" info)
            "Experimental API")
      (setf (gethash "version" info)
            "0.1.0")
      info)))
