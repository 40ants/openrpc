((defgeneric test-one-of
     (jsonrpc/client:client value)
   (:documentation "Test method for oneOf schema"))
 (defmethod test-one-of ((jsonrpc/client:client the-class) (value string))
   (let* ((#:g1
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "value" openrpc-client/core::args) value)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testOneOf"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g1))))
 (defmethod test-one-of ((jsonrpc/client:client the-class) (value integer))
   (let* ((#:g2
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "value" openrpc-client/core::args) value)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testOneOf"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g2))))
 (defgeneric test-any-of
     (jsonrpc/client:client value)
   (:documentation "Test method for anyOf schema"))
 (defmethod test-any-of ((jsonrpc/client:client the-class) (value string))
   (let* ((#:g3
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "value" openrpc-client/core::args) value)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testAnyOf"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g3))))
 (defmethod test-any-of ((jsonrpc/client:client the-class) (value integer))
   (let* ((#:g4
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "value" openrpc-client/core::args) value)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testAnyOf"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g4))))
 (defmethod test-any-of ((jsonrpc/client:client the-class) (value null))
   (let* ((#:g5
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "value" openrpc-client/core::args) value)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testAnyOf"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g5))))
 (defgeneric test-all-of
     (jsonrpc/client:client value)
   (:documentation "Test method for allOf schema"))
 (defmethod test-all-of ((jsonrpc/client:client the-class) (value hash-table))
   (let* ((#:g6
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "value" openrpc-client/core::args) value)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testAllOf"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g6))))
 (defgeneric test-implicit-object
     (jsonrpc/client:client config)
   (:documentation
    "Test method for implicit object type (properties without type)"))
 (defmethod test-implicit-object
            ((jsonrpc/client:client the-class) (config hash-table))
   (let* ((#:g7
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "config" openrpc-client/core::args) config)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testImplicitObject"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g7))))
 (defgeneric test-ref
     (jsonrpc/client:client data)
   (:documentation "Test method for $ref schema"))
 (defmethod test-ref ((jsonrpc/client:client the-class) (data t))
   (let* ((#:g8
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "data" openrpc-client/core::args) data)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testRef"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g8))))
 (defgeneric test-enum
     (jsonrpc/client:client status)
   (:documentation "Test method for enum schema without type"))
 (defmethod test-enum ((jsonrpc/client:client the-class) (status t))
   (let* ((#:g9
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "status" openrpc-client/core::args) status)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testEnum"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g9))))
 (defgeneric test-const
     (jsonrpc/client:client version)
   (:documentation "Test method for const schema"))
 (defmethod test-const ((jsonrpc/client:client the-class) (version t))
   (let* ((#:g10
           (let ((openrpc-client/core::args (make-hash-table :test 'equal)))
             (setf (gethash "version" openrpc-client/core::args) version)
             openrpc-client/core::args)))
     (labels ((openrpc-client/core::retrieve-data (openrpc-client/core::args)
                (let ((openrpc-client/core::raw-response
                       (openrpc-client/core::rpc-call jsonrpc/client:client
                                                      "testConst"
                                                      openrpc-client/core::args)))
                  openrpc-client/core::raw-response)))
       (openrpc-client/core::retrieve-data #:g10)))))
