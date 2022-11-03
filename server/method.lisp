(uiop:define-package #:openrpc-server/method
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:alexandria
                #:length=
                #:copy-hash-table
                #:make-keyword
                #:symbolicate)
  (:import-from #:lambda-fiddle
                #:with-destructured-lambda-list)
  (:import-from #:openrpc-server/utils
                #:sym-to-api-string)
  (:import-from #:openrpc-server/content-descriptor
                #:make-content-descriptor)
  (:import-from #:openrpc-server/interface
                #:transform-result
                #:type-to-schema)
  (:import-from #:openrpc-server/api
                #:api
                #:get-method-info
                #:add-api-method
                #:default-api
                #:api-server)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-callback-error)
  (:export
   #:define-rpc-method))
(in-package #:openrpc-server/method)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass parameter ()
    ((name :initarg :name
           :type string
           :reader parameter-name)
     (type :initarg :type
           :type string
           :reader parameter-type)
     (required :initarg :required
               :initform nil
               :type boolean
               :accessor parameter-required)
     (summary :initarg :summary
              :initform nil
              :type (or null string)
              :accessor parameter-summary)
     (description :initarg :description
                  :initform nil
                  :type (or null string)
                  :accessor parameter-description)
     (deprecated :initarg :deprecated
                 :initform nil
                 :type boolean
                 :accessor parameter-deprecated)))

  (defmethod print-object ((obj parameter) stream)
    (print-unreadable-object (obj stream :type t)
      (format stream "~S ~S~A"
              (parameter-name obj)
              (parameter-type obj)
              (if (parameter-required obj)
                  " required"
                  ""))))
  
  (defclass result ()
    ((type :initarg :type
           :reader result-type)))

  (defmethod print-object ((obj result) stream)
    (print-unreadable-object (obj stream :type t)
      (format stream "~S"
              (result-type obj))))
  
  (defclass method-info ()
    ((thunk :initarg :thunk
            :reader method-thunk)
     (params :initarg :params
             :reader method-params)
     (result :initarg :result
             :reader method-result)
     (summary :initarg :summary
              :initform nil
              :type (or null string)
              :reader method-summary)
     (description :initarg :description
                  :initform nil
                  :type (or null string)
                  :reader method-description)
     (deprecated :initarg :deprecated
                 :initform nil
                 :type boolean
                 :reader method-deprecated)))

  (defmethod print-object ((obj method-info) stream)
    (print-unreadable-object (obj stream :type t)
      (format stream "(~{~S~^, ~}) -> ~S"
              (method-params obj)
              (method-result obj))))


  (defparameter *info-form-types*
    (list :param :result :summary :description :deprecated))
  
  (defun separate-method-info-forms (body)
    (loop for form in body
          for info-form-p = (and (consp form)
                                 (keywordp (first form))
                                 (member (first form)
                                         *info-form-types*))
          if info-form-p
            collect form into method-info-forms
          else
            collect form into new-body
          finally (return (values method-info-forms
                                  new-body))))

  (defun simplify-arg (arg)
    (etypecase arg
      (symbol arg)
      (cons (car arg))))


  (defun make-lambda-list (required-args optional-args keyword-args)
    (append required-args
            (when optional-args
              (list* '&optional
                     optional-args))
            (when keyword-args
              (list* '&key
                     keyword-args))))

  (defun sort-params (method-name params required-args optional-args keyword-args)
    "Accepts PARAMS list and reorders it such that first go required args, then optional and finally keyword."

    (let* ((simplified-optional-args (mapcar #'simplify-arg optional-args))
           (simplified-keyword-args (mapcar #'simplify-arg keyword-args))
           (positional-args (append required-args
                                    optional-args))
           (sorted-params
             (stable-sort
              (copy-list params) #'<
              :key (lambda (param)
                     (let* ((name (parameter-name param))
                            (pos (position name positional-args)))
                       (cond
                         (pos pos)
                         ((member name simplified-keyword-args)
                          ;; There is no positional arg with this index,
                          ;; so, we'll use it for keyword args
                          (length positional-args))
                         (t
                          (error "Parameter ~S not found among method arguments: ~S"
                                 name
                                 (make-lambda-list required-args optional-args keyword-args)))))))))
      (let ((not-documented
              (set-difference (append required-args
                                      simplified-optional-args
                                      simplified-keyword-args)
                              (mapcar #'parameter-name sorted-params))))
        (when not-documented
          (error "Some parameters of function ~S are not documented: ~{~S~^, ~}"
                 method-name
                 not-documented)))
      sorted-params))
  

  (defun make-method-info (method-name thunk info-forms required-args optional-args keyword-args)
    (loop with params = nil
          with result = nil
          with summary = nil
          with description = nil
          with deprecated = nil
          for form in info-forms
          for form-type = (first form)
          do (ecase form-type
               (:summary
                (setf summary (second form)))
               (:description
                (setf description (second form)))
               (:deprecated
                (unless (length= 1 form)
                  (error "Form ~S should have only one element."
                         form))
                (setf deprecated t))
               (:param (push (destructuring-bind (name type &optional summary &rest rest)
                                 (cdr form)
                               (apply #'make-instance
                                      'parameter
                                      :name name
                                      :type type
                                      :summary summary
                                      rest))
                             params))
               (:result (setf result
                              (make-instance 'result
                                             :type (second form)))))
          finally (return
                    (let ((sorted-params
                            (sort-params method-name
                                         params
                                         required-args
                                         optional-args
                                         keyword-args)))
                      (unless result
                        (error "Method definition lack metadata about result. Please, add (:result <type>) form."))
                      
                      (loop for param in sorted-params
                            when (member (parameter-name param)
                                         required-args)
                              do (setf (parameter-required param)
                                       t))
                      (make-instance 'method-info
                                     :thunk thunk
                                     :params sorted-params
                                     :result result
                                     :summary summary
                                     :description description)))))

  (defun make-call-form (name keyword-arg-keys keyword-arg-names optional-arg-names required-args-getters)
    `(etypecase args
       (hash-table
        ;; If arguments were given in a hash-table, then we need
        ;; to figure out which are positional and which are keywords
        ;; and to transform key names to symbols:
        (let ((new-args
                (append (list ,@required-args-getters)
                        (loop for optional-name in (list ,@optional-arg-names)
                              for (value present-p) = (multiple-value-list
                                                       (gethash optional-name args))
                              if present-p
                                collect value into values
                              else
                                do (return values))
                        (loop for key-name in (list ,@keyword-arg-names)
                              for key-symbol in (list ,@keyword-arg-keys)
                              for (value present-p) = (multiple-value-list
                                                       (gethash key-name args))
                              when present-p
                                append (list key-symbol value)))))
          (apply ',name new-args))
        )
       (list
        (apply ',name args))))

  (defun make-wrapper-form (call-form paginated-result)
    (let ((wrapper-body
            (cond
              (paginated-result
               `(multiple-value-bind (result next-page-key)
                    ,call-form
                  (let ((response (dict "items" (transform-result result))))
                    (when next-page-key
                      (setf (gethash "next-page-key" response)
                            next-page-key))
                    response)))
              (t
               `(transform-result ,call-form)))))
      `(handler-bind ((error (lambda (c)
                               (when (and jsonrpc:*debug-on-error*
                                          ;; Errors returned explicitly by user should not
                                          ;; invoke debugger:
                                          (not (typep c 'jsonrpc-callback-error)))
                                 (invoke-debugger c)))))
         (with-log-unhandled (:errors-to-ignore '(jsonrpc-callback-error))
           ,wrapper-body)))))


(defun get-params-as-content-descriptors (api name)
  (let ((method-info (get-method-info api name)))
    (loop for param in (method-params method-info)
          collect (make-content-descriptor (sym-to-api-string (parameter-name param))
                                           :schema (type-to-schema (parameter-type param))
                                           :required (parameter-required param)
                                           :summary (parameter-summary param)
                                           :description (parameter-description param)
                                           :deprecated (parameter-deprecated param)))))


(defun get-method-result-as-content-descriptor (api name)
  (check-type api api)
  
  (let* ((method-info (get-method-info api name))
         (result (method-result method-info))
         (type (result-type result)))
    (make-content-descriptor (concatenate 'string name "_result")
                             :schema (type-to-schema type))))


(defmacro define-rpc-method (name args &body body)
  "Macro to define RPC method.

   All arguments should have corresponding (:param arg type) form in the BODY.

   Also, there should be one (:result type) form in the BODY."
  (destructuring-bind (api name)
      (etypecase name
        (symbol (list default-api name))
        (list name))
    (with-destructured-lambda-list (:required required-args
                                    :optional optional-args
                                    :key keyword-args)
                                   args
      (multiple-value-bind (info-forms body)
          (separate-method-info-forms body)
        (let* ((paginated-result
                 (loop for form in info-forms
                       when (and (eql (first form)
                                      :result)
                                 (consp (second form))
                                 (string-equal (symbol-name (car (second form)))
                                               "paginated-list-of"))
                         do (return t)))
               (optional-args (mapcar #'simplify-arg optional-args))
               (keyword-args (mapcar #'simplify-arg keyword-args))
               (name-as-string (sym-to-api-string name))
               (wrapper-name (symbolicate "%RPC-" name))
               (required-args-getters (loop for arg in required-args
                                            for name = (sym-to-api-string arg)
                                            collect `(multiple-value-bind (value present-p)
                                                         (gethash ,name args)
                                                       (unless present-p
                                                         (error "Argument ~A is required but not supplied by a client."
                                                                ',arg))
                                                       value)))
               (optional-arg-names (mapcar #'sym-to-api-string optional-args))
               (keyword-arg-names (mapcar #'sym-to-api-string keyword-args))
               (keyword-arg-keys (mapcar #'make-keyword keyword-args))
               (call-form (make-call-form name keyword-arg-keys keyword-arg-names optional-arg-names required-args-getters))
               (wrapper-form (make-wrapper-form call-form paginated-result)))

          `(flet ((,wrapper-name (args)
                    ,wrapper-form))
            
             (defun ,name ,args
               ,@body)

             (add-api-method ,api
                             ,name-as-string
                             (make-method-info ',name
                                               #',wrapper-name
                                               ',info-forms
                                               ',required-args
                                               ',optional-args
                                               ',keyword-args))
            
             (when (api-server ,api)
               (log:info "Exposing new version of ~A method."
                         ,name-as-string)
               (jsonrpc:expose (api-server ,api)
                               ,name-as-string
                               #',wrapper-name))))))))

