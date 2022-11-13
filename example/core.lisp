(uiop:define-package #:openrpc-example
  (:use #:cl)
  (:nicknames #:openrpc-example/core)
  (:import-from #:openrpc-example/server)
  (:import-from #:openrpc-example/client))
(in-package #:openrpc-example)
