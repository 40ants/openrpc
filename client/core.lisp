(uiop:define-package #:openrpc-client/core
  (:use #:cl)
  (:import-from #:kebab
                #:to-lisp-case)
  (:import-from #:log)
  (:import-from #:yason)
  (:import-from #:jsonrpc/class)
  (:import-from #:str)
  (:import-from #:dexador)
  (:import-from #:alexandria
                #:length=
                #:make-keyword
                #:appendf
                #:read-file-into-string
                #:copy-hash-table)
  (:import-from #:usocket
                #:connection-refused-error)
  (:import-from #:openrpc-client/error
                #:rpc-error)
  (:import-from #:serapeum
                #:fmt)
  (:export #:generate-client))
(in-package #:openrpc-client/core)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-method-description (spec)
    (let* ((original-name (gethash "name" spec))
           (name (intern (normalize-name original-name)))
           (params-spec (gethash "params" spec))
           (lambda-list (generate-lambda-list params-spec)))
      `(format stream "- ~S~%"
               ',(cons name
                       lambda-list))))

  
  (defun generate-client-class (class-name spec &key export-symbols)
    (let* ((make-func-name (alexandria:symbolicate "MAKE-" class-name))
           (method-descriptions (mapcar #'generate-method-description
                                        (gethash "methods" spec)))
           (result `((defclass ,class-name (jsonrpc/class:client)
                       ())
                     (defun ,make-func-name ()
                       (make-instance ',class-name))
                     (defmethod describe-object ((client ,class-name) stream)
                       (format stream "Supported RPC methods:~2%")
                       ,@method-descriptions))))
      (when export-symbols
        (appendf result
                 `((export ',class-name)
                   (export ',make-func-name))))
      (values result)))

  (defun normalize-name (string)
    (string-upcase
     (to-lisp-case
      (str:replace-all "." "-" string))))

  (declaim (ftype (function (hash-table) cons) schema-to-type))
  (defun schema-to-type (schema)
    "Convert JSON types to CL types. Supports one or multiple types."
    (let ((type (gethash "type" schema))
          (type-list nil))
      (declare (type (or string cons) type)
               (list type-list))
      (flet ((%schema-to-type (type)
               (cond ((string-equal type "integer") (push 'integer type-list))
                     ((string-equal type "number")
		      (push 'double-float type-list)
		      (push 'integer type-list))
                     ((string-equal type "string") (push 'string type-list))
                     ((string-equal type "array") (push 'list type-list))
                     ((string-equal type "null") (push 'null type-list))
                     (t
                      (error "Type ~S is not supported yet."
                             type)))))
        (if (stringp type)
            (%schema-to-type type)
            (mapc #'%schema-to-type type)))
      (nreverse type-list)))

  (defun generate-generic-lambda-list (params)
    (loop for param in params
          for required = (gethash "required" param)
          if required
          collect param into required-params
          else
          collect param into keyword-params
          finally (return (append (loop for param in required-params
                                        for name = (intern (normalize-name
                                                            (gethash "name" param)))
                                        collect name)
                                  (when keyword-params
                                    (list '&key))
                                  (loop for param in keyword-params
                                        for name = (intern (normalize-name
                                                            (gethash "name" param)))
                                        collect name)))))

  (declaim (ftype (function (cons) cons) expand-list))
  (defun expand-list (lambda-list)
    "Expand required parameters to all possible combinations of types.
     If required parameters have one type, the result is same as input."
    (let ((lambda-lists nil))
      (declare (list lambda-lists))
      (flet ((add-to-lists (element lists)
               (if lists
                   (mapcar (lambda (list)
                             (reverse (cons element (reverse list))))
                           lists)
                   (list (list element)))))
        (mapc (lambda (elements)
                (let ((name (car elements))
                      (types (cdr elements)))
                  (declare (symbol name) (list types))
                  (if (= 1 (length types))
                      (setf lambda-lists (add-to-lists elements lambda-lists))
                      (setf lambda-lists
                            (mapcan (lambda (type)
                                      (add-to-lists (list name type) lambda-lists))
                                    types)))))
              lambda-list))
      lambda-lists))

  (declaim (ftype (function (list) (values list &optional)) generate-lambda-list))
  (defun generate-lambda-list (params)
    "Generate lambda list from parameter list."
    (loop for param in params
          for required = (gethash "required" param)
          if required
          collect param into required-params
          else
          collect param into keyword-params
          finally (let ((required-parameter
                          (loop for param in required-params
                                for name = (intern (normalize-name
                                                    (gethash "name" param)))
                                for type = (schema-to-type (gethash "schema" param))
                                if (atom type)
                                  collect (list name type)
                                else
                                  collect (cons name type)))
                        (keyword-parameter
                          (when keyword-params
                            (cons '&key
                                  (loop for param in keyword-params
                                        for name = (intern (normalize-name
                                                            (gethash "name" param)))
                                        for default = nil
                                        for given-name = (alexandria:symbolicate name "-GIVEN-P")
                                        collect (list name default given-name))))))
                    (declare (list required-parameter keyword-parameter))
                    (return (if required-parameter
                                (if keyword-parameter
                                    (mapcar (lambda (required-combination)
                                              (append required-combination keyword-parameter))
                                            (expand-list required-parameter))
                                    (expand-list required-parameter))
                                (when keyword-parameter
                                  (list keyword-parameter)))))))

  (defun generate-arguments-collector (params)
    (loop for param in params
          for required = (gethash "required" param)
          if required
          collect param into required-params
          else
          collect param into keyword-params
          finally (return `(let ((args (make-hash-table :test 'equal)))
                             ,@(loop for param in required-params
                                     for original-name = (gethash "name" param)
                                     for name = (intern (normalize-name original-name))
                                     collect `(setf (gethash ,original-name args)
                                                    ,name))
                             ;; But Keywords arguments should be added to the dict
                             ;; only if they were given:
                             ,@(loop for param in keyword-params
                                     for original-name = (gethash "name" param)
                                     for name = (intern (normalize-name original-name))
                                     for given-name = (alexandria:symbolicate name "-GIVEN-P")
                                     collect `(when ,given-name
                                                (setf (gethash ,original-name args)
                                                      ,name)))
                             ;; Returning the dictionary with all given arguments
                             args))))
  
  (defun get-or-create-class (x-cl-class schema classes-cache &key export-symbols)
    (let* ((class-name (alexandria:symbolicate (string-upcase x-cl-class)))
           (existing-code (gethash class-name classes-cache)))
      (unless existing-code
        (loop with properties = (gethash "properties" schema)
              for name being the hash-key of properties
              for name-symbol = (alexandria:symbolicate (string-upcase
                                                         (to-lisp-case name)))
              for name-keyword = (alexandria:make-keyword name-symbol)
              for reader-func = (alexandria:symbolicate class-name
                                                        "-"
                                                        name-symbol)
              collect `(export ',reader-func) into slot-reader-exports
              collect `(,name-symbol :initform nil
                                     :initarg ,name-keyword
                                     :reader ,reader-func) into slots
              collect `(format stream " ~A=~S"
                               ',name-symbol
                               (,reader-func obj)) into slot-printers
              finally (let ((class-definition
                              `((defclass ,class-name ()
                                  (,@slots))
                                (defmethod print-object ((obj ,class-name) stream)
                                  (print-unreadable-object (obj stream :type t)
                                    ,@slot-printers)))))
                        (setf (gethash class-name classes-cache)
                              (append class-definition
                                      (when export-symbols
                                        (list `(export ',class-name)))
                                      (when export-symbols
                                        slot-reader-exports))))))
      class-name))

  (defun null-type-p (spec)
    (equal (gethash "type" spec)
           "null"))

  (defun generate-result-transformation (api-class-name result-symbol spec-or-schema classes-cache
                                         &key export-symbols)
    (let* ((schema (or (gethash "schema" spec-or-schema)
                       spec-or-schema))
           (one-of (gethash "oneOf" schema))
           (props (gethash "properties" schema))
           (x-cl-class (gethash "x-cl-class" schema))
           (x-cl-package (gethash "x-cl-package" schema))
           (paginated-list (gethash "x-paginated-list" schema))
           (type (gethash "type" schema)))
      (cond
        ((and (length= 2 one-of)
              ;; This is a special case were we supporting for
              ;; optional arguments or fields where one of
              ;; possible values is null.
              ;;
              ;; TODO: to support really automatic choose of one from many
              ;; possible types, we need either to analyze required properties
              ;; of their schemas and to build a matcher which will analyze
              ;; properties of received data object and choose correct class.
              ;; 
              ;; The other way is to support "discriminator" like users of OpenAPI
              ;; do: https://redocly.com/docs/resources/discriminator/
              ;; However I didn't find any materials pointing that sombody use
              ;; this technique with OpenRPC.
              (find-if #'null-type-p one-of))
         (let* ((other-type (find-if-not #'null-type-p one-of))
                (element-transformation
                  (generate-result-transformation api-class-name
                                                  result-symbol
                                                  other-type
                                                  classes-cache
                                                  :export-symbols export-symbols)))
           `(when ,result-symbol
              ,element-transformation)))
        (paginated-list
         (let ((element-transformation
                 (generate-result-transformation api-class-name
                                                 'item
                                                 (gethash "items"
                                                          (gethash "items"
                                                                   (gethash "properties" schema)))
                                                 classes-cache
                                                 :export-symbols export-symbols)))
           `(let ((next-page-key (gethash "next_page_key" ,result-symbol)))
              (values-list
               (append
                (list
                 (loop for item in (gethash "items" ,result-symbol)
                       collect ,element-transformation))
                (when next-page-key
                  (list
                   (flet ((retrieve-next-page ()
                            (let ((new-args (copy-hash-table args)))
                              (setf (gethash "page_key" new-args)
                                    next-page-key)
                              (retrieve-data new-args))))
                     #'retrieve-next-page))))))))
        ((and x-cl-class
              ;; If object class is unknown, we'll use it's value as is:
              (not (and (string= x-cl-class "T")
                        (string= x-cl-package "COMMON-LISP"))))
         (let ((class-name (get-or-create-class x-cl-class
                                                schema
                                                classes-cache
                                                :export-symbols export-symbols)))
           (when (eql class-name api-class-name)
             (error "API defines class ~S which clashesh with a name of symbol you choose for API class. Please, choose another name." class-name))

           `(funcall #'make-instance
                     ',class-name
                     ;; Now we need to extract parameters from raw-response
                     ;; For each parameter we need to apply
                     ;; GENERATE-RESULT-TRANSFORMATION again.
                     ,@(loop for prop-name being the hash-key of props
                             using (hash-value prop-schema)
                             for prop-name-as-key = (make-keyword
                                                     (string-upcase
                                                      (to-lisp-case prop-name)))
                             collect prop-name-as-key
                             collect (generate-result-transformation
                                      api-class-name
                                      `(gethash ,prop-name ,result-symbol)
                                      prop-schema
                                      classes-cache
                                      :export-symbols export-symbols)))))
        ((and (gethash "items" schema)
	      (stringp type)
              (string-equal type "array"))
         (let ((element-transformation
                 (generate-result-transformation api-class-name
                                                 'item
                                                 (gethash "items" schema)
                                                 classes-cache
                                                 :export-symbols export-symbols)))
           `(loop for item in ,result-symbol
                  collect ,element-transformation)))
        (t
         ;; In simple cases we don't need to apply any transformations
         result-symbol))))

  (declaim (ftype (function (symbol symbol list cons string (or symbol cons))
                            cons)
                  generate-defmethods))
  (defun generate-defmethods (name class-name lambda-list arguments-collector
                             original-name result-transformation)
    "Generate either a defmethod with only client as argument or for each given
lambda-list a separate defmethod."
    (flet ((generate-defmethod (&optional lambda-list-element)
             (let ((args-collect (gensym)))
               `(defmethod ,name ,(cons `(client ,class-name) lambda-list-element)
                  (let* ((,args-collect ,arguments-collector))
                    (labels ((retrieve-data (args)
                               (let ((raw-response (rpc-call client ,original-name args)))
                                 ,result-transformation)))
                      (retrieve-data ,args-collect)))))))
      (if lambda-list
          (mapcar #'generate-defmethod lambda-list)
          (list (generate-defmethod)))))

  (declaim (ftype (function (symbol hash-table hash-table &key (:export-symbols t))
                            cons)
                  generate-method))
  (defun generate-method (class-name spec classes-cache &key export-symbols)
    "Generate generic and specialized methods for certain class."
    (let* ((original-name (gethash "name" spec))
           (name (intern (normalize-name original-name)))
           (params-spec (gethash "params" spec))
           (summary (gethash "summary" spec))
           (description (gethash "description" spec))
           (result-spec (gethash "result" spec))
           (result-transformation (generate-result-transformation class-name
                                                                  'raw-response
                                                                  result-spec
                                                                  classes-cache
                                                                  :export-symbols export-symbols))
           (lambda-list (generate-lambda-list params-spec))
           (generic-lambda-list (generate-generic-lambda-list params-spec))
           (generic-body (when summary
                           (list (list :documentation
                                       (fmt "~A~@[~2%~A~]"
                                            summary
                                            description)))))
           (arguments-collector (generate-arguments-collector params-spec))
           (result (cons
                    `(defgeneric ,name (client ,@generic-lambda-list)
                       ,@generic-body)
                    (generate-defmethods name class-name lambda-list arguments-collector
                                         original-name result-transformation))))
      (when export-symbols
        (push `(export ',name)
              result))
      (values result)))

  (defun retrieve-data-from-url (url)
    (handler-bind ((connection-refused-error
                     (lambda (condition)
                       (declare (ignore condition))
                       (error "Unable to generate client because URL ~S is unavailable."
                              url))))
      (dex:get url)))
  
  (defun retrieve-data-from-path (path)
    (cond
      ((probe-file path)
       (read-file-into-string path))
      (t
       (error "Unable to generate client because PATH ~S does not exists."
              path))))

  (defun retrieve-spec (url-or-path)
    (yason:parse 
     (etypecase url-or-path
       (pathname (retrieve-data-from-path url-or-path))
       (string
        (cond
          ((str:starts-with-p "http" url-or-path)
           (retrieve-data-from-url url-or-path))
          (t
           (retrieve-data-from-path url-or-path))))))))


(defgeneric rpc-call (client func-name arguments)
  (:method ((client t) func-name (arguments t))
    (handler-bind ((dexador.error:http-request-internal-server-error
                     (lambda (condition)
                       (let* ((body (dex:response-body condition))
                              (response (yason:parse body))
                              (error (gethash "error" response))
                              (code (gethash "code" error))
                              (message (gethash "message" error)))
                         (log:error "Received error response"
                                    code
                                    message)
                         (error 'rpc-error
                                :code code
                                :message message
                                :func-name func-name
                                :func-arguments arguments)))))
      (jsonrpc/class:call client func-name arguments))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %generate-client (class-name spec &key (export-symbols t))
    (let* ((client-class (generate-client-class class-name spec :export-symbols export-symbols))
           (object-classes
             ;; The map from package::symbol to a code which defines
             ;; a class for some complex object used as argument or
             ;; result in an API:
             (make-hash-table :test 'equal))
           (methods (loop for method-spec in (gethash "methods" spec)
                          appending (generate-method class-name method-spec object-classes
                                                     :export-symbols export-symbols)))
           (class-definitions
             (loop for def being the hash-value of object-classes
                   ;; Here each def contains a list of DEFCLASS + one or more methods.
                   appending def)))
      (values client-class
              class-definitions
              methods))))


(defmacro generate-client (class-name url-or-path &key (export-symbols t))
  "Generates Common Lisp client by OpenRPC spec.

   CLASS-NAME is the name of a API class. Also, a corresponding MAKE-<CLASS-NAME> function
   is created.

   URL-OR-PATH argument could be a string with HTTP URL of a spec, or a pathname
   if a spec should be read from the disc."
  (let* ((spec (retrieve-spec (eval url-or-path))))
    (multiple-value-bind (client-class class-definitions methods)
        (%generate-client class-name spec :export-symbols export-symbols)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@client-class
         ,@class-definitions
         ,@methods))))
