(uiop:define-package #:openrpc-server/method
  (:use #:cl)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:openrpc-server/vars
                #:*server*)
  (:import-from #:alexandria
                #:length=
                #:copy-hash-table
                #:make-keyword
                #:symbolicate)
  (:import-from #:lambda-fiddle
                #:with-destructured-lambda-list)
  (:import-from #:openrpc-server/utils
                #:sym-to-string)
  (:import-from #:openrpc-server/content-descriptor
                #:make-content-descriptor)
  (:import-from #:openrpc-server/interface
                #:transform-result
                #:type-to-schema))
(in-package #:openrpc-server/method)


(defvar *methods* (make-hash-table :test 'equal)
  "This hash keeping all methods.")

(defvar *method-info* (make-hash-table :test 'equal)
  "Hash to keep additional information about RPC method signatures.")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass parameter ()
    ((name :initarg :name
           :initform nil
           :reader parameter-name)
     (type :initarg :type
           :initform nil
           :reader parameter-type)
     (required :initarg :required
               :initform nil
               :accessor parameter-required)))

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
    ((params :initarg :params
             :reader method-params)
     (result :initarg :result
             :reader method-result)))

  (defmethod print-object ((obj method-info) stream)
    (print-unreadable-object (obj stream :type t)
      (format stream "(~{~S~^, ~}) -> ~S"
              (method-params obj)
              (method-result obj))))
  
  (defun separate-method-info-forms (body)
    (loop with params = nil
          with result = nil
          with rest-body = nil
          for form in body
          for info-form-p = (and (consp form)
                                 (keywordp (first form))
                                 (member (first form)
                                         '(:param :result)))
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

  (defun sort-params (params required-args optional-args keyword-args)
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
          (error "Some parameters of function are not documented: ~{~S~^, ~}"
                 not-documented)))
      sorted-params))
  

  (defun make-method-info (info-forms required-args optional-args keyword-args)
    (loop with params = nil
          with result = nil
          for form in info-forms
          for form-type = (first form)
          do (ecase form-type
               (:param (push (make-instance 'parameter
                                            :name (second form)
                                            :type (third form))
                             params))
               (:result (setf result
                              (make-instance 'result
                                             :type (second form)))))
          finally (return (let ((sorted-params
                                  (sort-params params
                                               required-args
                                               optional-args
                                               keyword-args)))
                            (loop for param in sorted-params
                                  when (member (parameter-name param)
                                               required-args)
                                  do (setf (parameter-required param)
                                           t))
                            (make-instance 'method-info
                                           :params sorted-params
                                           :result result)))))

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


;; TODO: check if needed
(defun get-params-as-content-descriptors (name)
  (let ((method-info (gethash name *method-info*)))
    (loop for param in (method-params method-info)
          collect (make-content-descriptor (sym-to-string (parameter-name param))
                                           :type (sym-to-string (parameter-type param))
                                           :required (parameter-required param)))))


;; TODO: check if needed
(defun get-method-result-as-content-descriptor (name)
  (let* ((method-info (gethash name *method-info*))
         (result (method-result method-info))
         (type (result-type result)))
    (make-content-descriptor (concatenate 'string name "-result")
                             :schema (type-to-schema type)
                             ;; :type (sym-to-string (result-type result))
                             )))


(defmacro define-rpc-method (name args &body body)
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
             (name-as-string (string-downcase (symbol-name name)))
             (wrapper-name (symbolicate "%RPC-" name))
             (required-args-getters (loop for arg in required-args
                                          for name = (sym-to-string arg)
                                          collect `(multiple-value-bind (value present-p)
                                                       (gethash ,name args)
                                                     (unless present-p
                                                       (error "Argument ~A is required but not supplied by a client."
                                                              ',arg))
                                                     value)))
             (optional-arg-names (mapcar #'sym-to-string optional-args))
             (keyword-arg-names (mapcar #'sym-to-string keyword-args))
             (keyword-arg-keys (mapcar #'make-keyword keyword-args))
             (call-form (make-call-form name keyword-arg-keys keyword-arg-names optional-arg-names required-args-getters))
             (wrapper-form (make-wrapper-form call-form paginated-result)))

        `(flet ((,wrapper-name (args)
                  ,wrapper-form))
           
           (defun ,name ,args
             ,@body)

           (setf (gethash ,name-as-string *methods*)
                 #',wrapper-name)
           
           (setf (gethash ,name-as-string *method-info*)
                 (make-method-info ',info-forms
                                   ',required-args
                                   ',optional-args
                                   ',keyword-args))
           
           (when *server*
             (jsonrpc:expose *server* ,name-as-string
                             #',wrapper-name)))))))

