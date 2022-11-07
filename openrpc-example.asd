#-asdf3.1 (error "System OPENRPC requires ASDF 3.1")
(defsystem "openrpc-example"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "example"
  :depends-on ("clack-handler-hunchentoot"
               "openrpc-example/server"
               "openrpc-example/client")
  :description "Example JSON-RPC server and client."
  :homepage "https://40ants.com/openrpc/"
  :source-control (:git "https://github.com/40ants/openrpc")
  :in-order-to ((test-op (test-op openrpc-tests))))
