(uiop:define-package #:openrpc-server/server
  (:use #:cl)
  (:import-from #:openrpc-server/discovery)
  (:import-from #:openrpc-server/method)
  (:import-from #:openrpc-server/interface))
(in-package #:openrpc-server/server)
