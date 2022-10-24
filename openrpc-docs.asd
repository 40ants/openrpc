#-asdf3.1 (error "System OPENRPC requires ASDF 3.1")
(defsystem "openrpc-docs"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "docs"
  :depends-on ("openrpc-docs/index"
               "openrpc-docs/changelog")
  :description "Documentation for Common Lisp OpenRPC library."
  :homepage "https://40ants.com/openrpc/"
  :source-control (:git "https://github.com/40ants/openrpc")
  :in-order-to ((test-op (test-op openrpc-tests))))

