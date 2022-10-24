#-asdf3.1 (error "System OPENRPC requires ASDF 3.1")
(defsystem "openrpc-client"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "client"
  :depends-on ("openrpc-client/core")
  :description "OpenRPC client implementation for Common Lisp."
  :homepage "https://40ants.com/openrpc/"
  :source-control (:git "https://github.com/40ants/openrpc")
  :in-order-to ((test-op (test-op openrpc-tests))))

