#-asdf3.1 (error "System OPENRPC requires ASDF 3.1")
(defsystem "openrpc-ci"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :pathname "ci"
  :depends-on ("openrpc-server/ci"
               "openrpc-client/ci"
               "openrpc-docs/ci")
  :description "CI for Common Lisp OpenRPC library."
  :homepage "https://40ants.com/openrpc/"
  :source-control (:git "https://github.com/40ants/openrpc")
  :in-order-to ((test-op (test-op openrpc-tests))))

