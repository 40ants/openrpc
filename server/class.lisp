(uiop:define-package #:openrpc-server/class
  (:use #:cl)
  (:import-from #:openrpc-server/interface
                #:slots-to-exclude
                #:transform-result
                #:type-to-schema)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:yason)
  (:import-from #:closer-mop
                #:standard-slot-definition
                #:slot-definition-initfunction
                #:slot-definition-type
                #:slot-definition-name
                #:class-slots
                #:ensure-finalized)
  (:import-from #:local-time
                #:format-timestring
                #:timestamp)
  (:import-from #:openrpc-server/utils
                #:sym-to-api-string))
(in-package #:openrpc-server/class)


(defmethod type-to-schema ((class class))
  (let* ((class (ensure-finalized class))
         (class-name (class-name class))
         (slots (class-slots class))
         (to-exclude (slots-to-exclude class))
         (required-properties nil)
         (properties (loop with result = (make-hash-table :test 'equal
                                                          :size (length slots))
                           for slot in slots
                           for slot-name = (slot-definition-name slot)
                           for api-attr-name = (sym-to-api-string slot-name)
                           for has-default = (slot-definition-initfunction slot)
                           for should-be-exluded = (member slot-name to-exclude
                                                           :test #'string-equal)
                           for required = (not has-default)
                           when (and (not should-be-exluded)
                                     required)
                           do (push api-attr-name required-properties)
                           unless should-be-exluded
                           do (setf (gethash api-attr-name result)
                                    (type-to-schema slot))
                           finally (return result)))
         (description (documentation class t))
         (schema (dict "type" "object"
                       "properties" properties
                       "required" (or required-properties
                                      #())
                       "x-cl-class" (symbol-name class-name)
                       "x-cl-package" (package-name (symbol-package class-name)))))
    (when description
      (setf (gethash "description" schema)
            description))
    (values schema)))


(defmethod type-to-schema ((slot standard-slot-definition))
  (let* ((type (slot-definition-type slot))
         (description (documentation slot t))
         (schema (type-to-schema type)))
    (when description
      (setf (gethash "description" schema)
            description))
    (values schema)))


(defmethod transform-result ((object standard-object))
  (let* ((class (class-of object))
         (class (ensure-finalized class))
         (slots (class-slots class))
         (to-exclude (slots-to-exclude class)))
    (loop with result = (make-hash-table :test 'equal
                                         :size (length slots))
          for slot in slots
          for slot-name = (slot-definition-name slot)
          for field-name = (sym-to-api-string slot-name)
          for should-be-excluded = (member slot-name to-exclude
                                           :test #'string-equal)
          when (and (not should-be-excluded)
                    (slot-boundp object slot-name))
          do (let ((slot-value (slot-value object
                                           slot-name))
                   (slot-type (slot-definition-type slot)))
               ;; For boolean slots we automatically coerce
               ;; values to the yason:false or T to exclude cases
               ;; when null is returned instead of false:
               (when (eql slot-type 'boolean)
                 (setf slot-value
                       (cond
                         ((eql slot-value yason:false)
                          yason:false)
                         (slot-value
                          t)
                         (t
                          yason:false))))
               (setf (gethash field-name result)
                     (transform-result slot-value)))
          finally (return result))))


(defmethod transform-result ((ts timestamp))
  (format-timestring nil ts))
