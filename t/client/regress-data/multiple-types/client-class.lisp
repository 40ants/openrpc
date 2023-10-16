((defclass the-class (jsonrpc/class:client) nil)
 (defun make-the-class () (make-instance 'the-class))
 (defmethod describe-object ((openrpc-client/core::client the-class) stream)
   (openrpc-client/core::generate-method-descriptions
    (class-of openrpc-client/core::client) stream)))
