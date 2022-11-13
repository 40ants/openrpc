#-asdf3.1 (error "System OPENRPC requires ASDF 3.1")
(defsystem "openrpc-server"
  :author "Alexander Artemenko"
  :license "BSD"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "server"
  :depends-on ("log4cl-extras"
               "openrpc-server/server"
               "openrpc-server/class"
               "openrpc-server/discovery")
  :description "OpenRPC server implementation for Common Lisp."
  :homepage "https://40ants.com/openrpc/"
  :source-control (:git "https://github.com/40ants/openrpc")
  :in-order-to ((test-op (test-op openrpc-tests))))

(register-system-packages "log4cl" '(#:log))
(register-system-packages "lack-request" '(#:lack.request))
