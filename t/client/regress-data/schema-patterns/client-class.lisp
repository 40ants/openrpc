((defclass the-class (jsonrpc/client:client) nil)
 (defun make-the-class () (make-instance 'the-class))
 (defmethod describe-object ((jsonrpc/client:client the-class) stream)
   (openrpc-client/core::generate-method-descriptions
    (class-of jsonrpc/client:client) stream)))
