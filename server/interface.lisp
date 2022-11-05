(uiop:define-package #:openrpc-server/interface
  (:use #:cl)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:openrpc-server/api
                #:api-version
                #:api-title
                #:api)
  (:export
   #:type-to-schema
   #:transform-result
   #:primitive-type-p
   #:make-info))
(in-package #:openrpc-server/interface)


(defgeneric transform-result (object)
  (:documentation "Prepares object for serialization before responding to RPC call.

Result should be list, hash-map or a value of primitive type.")
  (:method ((object list))
    (mapcar #'transform-result object))
  
  (:method ((object vector))
    (map 'vector #'transform-result object))
  
  (:method ((object string))
    object)
  
  (:method ((object integer))
    object)
  
  (:method ((object (eql t)))
    object)
  
  ;; (:method ((object integer))
  ;;   object)
  )


(defgeneric primitive-type-p (type)
  (:documentation "Should return t for type if it's name matched to simple types supported by [JSON-SCHEMA](https://json-schema.org/).

Argument TYPE is a symbol.")
  (:method ((type t))
    nil)
  (:method ((type (eql 'integer)))
    t)
  (:method ((type (eql 'string)))
    t)
  (:method ((type (eql 'null)))
    t)
  (:method ((type (eql 'boolean)))
    t))


(defgeneric type-to-schema (type)
  (:documentation "This method is called for all types for which PRIMITIVE-TYPE-P generic-function
returns NIL.

It should return as hash-table with JSON-SCHEMA corresponding to type. Keys of the dictionary should
be strings. It is convenient to use [`SERAPEUM:DICT`][SERAPEUM:DICT] for building the result.")
  (:method ((type t))
    (cond
      ((primitive-type-p type)
       (dict "type"
             (string-downcase (symbol-name type))))
      ;; Scheme for these multiple types should be generated:
      ;; Here TYPE might be (OR NULL STRING)
      ((and (listp type)
            (symbolp (car type))
            (string-equal (car type)
                          "or"))
       (loop for subtype in (cdr type)
             for subtype-schema = (type-to-schema subtype)
             collect subtype-schema into subtype-schemas
             finally (return (dict "oneOf"
                                   subtype-schemas))))
      ;; Non paginated results:
      ((and (listp type)
            (symbolp (car type))
            (or (string-equal (car type)
                              "list-of")
                ;; Support for SERAPEUM:SOFT-LIST-OF
                (string-equal (car type)
                              "soft-list-of")))
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
                                "next_page_key" (dict "oneOf"
                                                      (list (dict "type" "string")
                                                            (dict "type" "int"))))
             "required" (list "items")
             "x-paginated-list" t))
      ((find-class type)
       (type-to-schema (find-class type)))
      (t
       (error "Type ~S is not supported. Please, define ~S method for it."
              type
              'type-to-schema)))))


(defgeneric make-info (api server)
  (:documentation "Returns a basic information about API for [info section](https://spec.open-rpc.org/#info-object) of OpenRPC spec.")
  (:method ((api api) (server jsonrpc:server))
    (let ((info (make-hash-table :test 'equal)))
      (setf (gethash "title" info)
            (api-title api))
      (setf (gethash "version" info)
            (api-version api))
      info)))
