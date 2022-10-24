#-asdf3.1 (error "System OPENRPC requires ASDF 3.1")
(defsystem "openrpc"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :pathname "src"
  :depends-on ("openrpc/core"
               "openrpc/changelog")
  :description "40Ants Common Lisp projects."
  :homepage "https://40ants.com/openrpc/"
  :source-control (:git "https://github.com/40ants/openrpc")
  :in-order-to ((test-op (test-op openrpc-tests))))

