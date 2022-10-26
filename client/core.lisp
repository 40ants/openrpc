(uiop:define-package #:openrpc-client/core
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:kebab)
  (:import-from #:jsonrpc)
  (:import-from #:jsonrpc/class)
  (:import-from #:str)
  (:import-from #:dexador)
  (:import-from #:alexandria
                #:copy-hash-table)
  (:import-from #:usocket
                #:connection-refused-error)
  (:export #:@index
           #:@readme
           #:generate-client))
(in-package #:openrpc-client/core)


(defsection @example (:title "Example chapter")
  "Please, fill this documentation with real docs.")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun generate-method-description (spec)
    (let* ((original-name (gethash "name" spec))
           (name (intern (normalize-name original-name)))
           (params-spec (gethash "params" spec))
           (lambda-list (generate-lambda-list params-spec)))
      `(format stream "- ~S~%"
               ',(cons name
                       lambda-list))))

  
  (defun generate-client-class (class-name spec)
    (let ((make-func-name (alexandria:symbolicate "MAKE-" class-name))
          (method-descriptions (mapcar #'generate-method-description
                                       (gethash "methods" spec))))
      `((defclass ,class-name (jsonrpc/class:client)
          ())
        (defun ,make-func-name ()
          (make-instance ',class-name))

        (defmethod describe-object ((client ,class-name) stream)
          (format stream "Supported RPC methods:~2%")
          ,@method-descriptions))))

  (defun normalize-name (string)
    (string-upcase
     (kebab:to-lisp-case
      (str:replace-all "." "-" string))))

  (defun schema-to-type (schema)
    (let ((type (gethash "type" schema)))
      (cond
        ((string-equal type "integer") 'integer)
        ((string-equal type "string") 'string)
        (t
         (error "Type ~S is not supported yet."
                type)))))

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
  
  (defun get-or-create-class (x-cl-class schema classes-cache)
    (let* ((class-name (alexandria:symbolicate (string-upcase x-cl-class)))
           (existing-code (gethash class-name classes-cache)))
      (unless existing-code
        (loop with properties = (gethash "properties" schema)
              for name being the hash-key of properties
              for name-symbol = (alexandria:symbolicate (string-upcase name))
              for name-keyword = (alexandria:make-keyword name-symbol)
              for reader-func = (alexandria:symbolicate class-name
                                                        "-"
                                                        name-symbol)
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
                              class-definition))))
      class-name))

  (defun make-plist-from (raw-response)
    (loop for name being the hash-key of raw-response
          using (hash-value value)
          for name-as-keyword = (alexandria:make-keyword (string-upcase name))
          appending (list name-as-keyword value)))

  (defun generate-result-transformation (result-symbol spec-or-schema classes-cache)
    (let* ((schema (or (gethash "schema" spec-or-schema)
                       spec-or-schema))
           (x-cl-class (gethash "x-cl-class" schema))
           (paginated-list (gethash "x-paginated-list" schema))
           (type (gethash "type" schema)))
      (cond
        (paginated-list
         (let ((element-transformation
                 (generate-result-transformation 'item
                                                 (gethash "items"
                                                          (gethash "items"
                                                                   (gethash "properties" schema)))
                                                 classes-cache)))
           `(let ((next-page-key (gethash "next-page-key" ,result-symbol)))
              (values-list
               (append
                (list
                 (loop for item in (gethash "items" ,result-symbol)
                       collect ,element-transformation))
                (when next-page-key
                  (list
                   (flet ((retrieve-next-page ()
                            (let ((new-args (copy-hash-table args)))
                              (setf (gethash "page-key" new-args)
                                    next-page-key)
                              (retrieve-data new-args))))
                     #'retrieve-next-page))))))))
        (x-cl-class
         (let ((class-name (get-or-create-class x-cl-class
                                                schema
                                                classes-cache)))
           `(apply #'make-instance
                   ',class-name
                   ;; Now we need to extract parameters from raw-response
                   (make-plist-from ,result-symbol))))
        ((string-equal type "array")
         (let ((element-transformation
                 (generate-result-transformation 'item (gethash "items" schema) classes-cache)))
           `(loop for item in ,result-symbol
                  collect ,element-transformation)))
        (t
         ;; In simple cases we don't need to apply any transformations
         result-symbol))))
  
  
  (defun generate-method (class-name spec classes-cache)
    (let* ((original-name (gethash "name" spec))
           (name (intern (normalize-name original-name)))
           (params-spec (gethash "params" spec))
           (result-spec (gethash "result" spec))
           (result-transformation (generate-result-transformation 'raw-response result-spec classes-cache))
           (lambda-list (generate-lambda-list params-spec))
           (arguments-collector (generate-arguments-collector params-spec)))
      `(defmethod ,name ((client ,class-name) ,@lambda-list)
         (let* ((args ,arguments-collector))
           (labels ((retrieve-data (args)
                      (let ((raw-response (rpc-call client ,original-name args)))
                        ,result-transformation)))
             (retrieve-data args)))))))


(defgeneric rpc-call (client func-name arguments)
  (:method ((client t) func-name (arguments t))
    (jsonrpc:call client func-name arguments)))


(defmacro generate-client (class-name url)
  (handler-bind ((connection-refused-error
                   (lambda (condition)
                     (declare (ignore condition))
                     (error "Unable to generate client because URL ~S is unavailable."
                            url))))
    (let* ((spec (yason:parse (dex:get url)))
           (client-class (generate-client-class class-name spec))
           (object-classes
             ;; The map from package::symbol to a code which defines
             ;; a class for some complex object used as argument or
             ;; result in an API:
             (make-hash-table :test 'equal))
           (methods (loop for method-spec in (gethash "methods" spec)
                          collect (generate-method class-name method-spec object-classes)))
           (class-definitions
             (loop for def being the hash-value of object-classes
                   ;; Here each def contains a list of DEFCLASS + one or more methods.
                   appending def)))
      `(progn
         ,@client-class
         ,@class-definitions
         ,@methods))))
