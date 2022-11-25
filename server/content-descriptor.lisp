(uiop:define-package #:openrpc-server/content-descriptor
  (:use #:cl)
  (:import-from #:alexandria
                #:copy-hash-table))
(in-package #:openrpc-server/content-descriptor)


(defun make-content-descriptor (name &key type reference required
                                       (schema nil schema-given-p)
                                       summary
                                       description
                                       deprecated)
  (let ((result (make-hash-table :test 'equal))
        (schema (if schema
                    (copy-hash-table schema)
                    (make-hash-table :test 'equal))))
    (unless (or type reference schema-given-p)
      (error "Reference or type or schema should be given."))
    
    (cond
      (type
       (setf (gethash "type" schema)
             type))
      (reference
       (setf (gethash "$ref" schema)
             reference)))

    (setf (gethash "name" result)
          name
          (gethash "schema" result)
          schema)

    (when required
      (setf (gethash "required" result)
            t))
    
    (when summary
      (setf (gethash "summary" result)
            summary))
    
    (when description
      (setf (gethash "description" result)
            description))

    (when deprecated
      (setf (gethash "deprecated" result)
            t))

    result))

