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

  (defun schema-to-type (schema)
    (let ((type (gethash "type" schema)))
      (cond
        ((string-equal type "integer") 'integer)
        ((string-equal type "number") 'double-float)
        ((string-equal type "string") 'string)
        ((string-equal type "array") 'list)
        (t
         (error "Type ~S is not supported yet."
                type)))))

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
  
  (defun generate-lambda-list (params)
    (loop for param in params
          for required = (gethash "required" param)
          if required
          collect param into required-params
          else
          collect param into keyword-params
          finally (return (append (loop for param in required-params
                                        for name = (intern (normalize-name
                                                            (gethash "name" param)))
                                        for type = (schema-to-type (gethash "schema" param))
                                        collect (list name type))
                                  (when keyword-params
                                    (list '&key))
                                  (loop for param in keyword-params
                                        for name = (intern (normalize-name
                                                            (gethash "name" param)))
                                        for default = nil
                                        for given-name = (alexandria:symbolicate name "-GIVEN-P")
                                        collect (list name default given-name))))))
  
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

  (defun make-plist-from (raw-response)
    (loop for name being the hash-key of raw-response
          using (hash-value value)
          for name-as-keyword = (alexandria:make-keyword
                                 (string-upcase
                                  (to-lisp-case name)))
          appending (list name-as-keyword value)))

  (defun generate-result-transformation (api-class-name result-symbol spec-or-schema classes-cache
                                         &key export-symbols)
    (let* ((schema (or (gethash "schema" spec-or-schema)
                       spec-or-schema))
           (x-cl-class (gethash "x-cl-class" schema))
           (paginated-list (gethash "x-paginated-list" schema))
           (type (gethash "type" schema)))
      (cond
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
        (x-cl-class
         (let ((class-name (get-or-create-class x-cl-class
                                                schema
                                                classes-cache
                                                :export-symbols export-symbols)))
           (when (eql class-name api-class-name)
             (error "API defines class ~S which clashesh with a name of symbol you choose for API class. Please, choose another name." class-name))

           `(apply #'make-instance
                   ',class-name
                   ;; Now we need to extract parameters from raw-response
                   (make-plist-from ,result-symbol))))
        ((string-equal type "array")
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
  
  
  (defun generate-method (class-name spec classes-cache &key export-symbols)
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
           (result (list
                    `(defgeneric ,name (client ,@generic-lambda-list)
                       ,@generic-body)
                    
                    `(defmethod ,name ((client ,class-name) ,@lambda-list)
                       (let* ((args ,arguments-collector))
                         (labels ((retrieve-data (args)
                                    (let ((raw-response (rpc-call client ,original-name args)))
                                      ,result-transformation)))
                           (retrieve-data args)))))))
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


(defmacro generate-client (class-name url-or-path &key (export-symbols t))
  "Generates Common Lisp client by OpenRPC spec.

   CLASS-NAME is the name of a API class. Also, a corresponding MAKE-<CLASS-NAME> function
   is created.

   URL-OR-PATH argument could be a string with HTTP URL of a spec, or a pathname
   if a spec should be read from the disc."
  (let* ((spec (retrieve-spec (eval url-or-path)))
         (client-class (generate-client-class class-name spec :export-symbols export-symbols))
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
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@client-class
       ,@class-definitions
       ,@methods)))
