(uiop:define-package #:openrpc-docs/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:openrpc-server/docs
                #:@server)
  (:import-from #:openrpc-client/docs
                #:@client)
  (:export #:@index
           #:@readme))
(in-package #:openrpc-docs/index)


(defmethod docs-config ((system (eql (asdf:find-system "openrpc-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "OpenRPC for Common Lisp"
                    :external-links (("openrpc" . "https://spec.open-rpc.org/")
                                     ("jsonrpc" . "https://github.com/cxxxr/jsonrpc")
                                     ("clack" . "https://github.com/fukamachi/clack"))
                    :ignore-words ("GIT"
                                   "BSD"
                                   "RPC"
                                   "JSON-RPC"
                                   "PAGE-KEY"
                                   "SQL"
                                   "WHERE"
                                   "LIMIT"
                                   "JSON"
                                   "JSON-SCHEMA"
                                   "API"
                                   "CLACK:CLACKUP"
                                   "Clack"
                                   "URL"
                                   "CL"
                                   "REPL"
                                   "OPENRPC-CLIENT"
                                   "ASDF"))
  "
[![](https://github-actions.40ants.com/40ants/openrpc/matrix.svg)](https://github.com/40ants/openrpc/actions)

This framework is built on top of [`JSON-RPC`][jsonrpc] and [`Clack`][clack]. Comparing to JSON-RPC library,
it provides these key features:

- Automatic [OpenRPC spec][openrpc] generation.
- Automatic JSON-RPC client building by OpenRPC spec. This includes creation of Common Lisp classes and methods
  for making RPC requests and returning native CL objects.
- On both server and client sides your code looks very lispy, all JSON marshalling is done under the hood. 
"
  (@server section)
  (@client section))


(defsection-copy @readme @index)
