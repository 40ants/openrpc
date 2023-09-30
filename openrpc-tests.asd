(defsystem openrpc-tests
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               "openrpc-tests/petshop"
               "openrpc-tests/server/interface"
               "openrpc-tests/client/deserialization"
               "openrpc-tests/client/generation"
               "openrpc-tests/client/regression")
  :description "Test system for OPENRPC."

  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))


(register-system-packages "clack-test" '(#:clack.test))
