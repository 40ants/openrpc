(uiop:define-package #:openrpc-client
  (:use #:cl)
  (:import-from #:openrpc-client/core
                #:generate-client)
  (:export #:generate-client))
(in-package #:openrpc-client)
