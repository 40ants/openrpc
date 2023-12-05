(defpackage #:openrpc-server/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/critic)
  (:import-from #:40ants-ci/jobs/run-tests
                #:run-tests)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/steps/sh
                #:sections
                #:sh))
(in-package #:openrpc-server/ci)


(defworkflow server-ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  ;; :cache t
  :jobs ((40ants-ci/jobs/linter:linter :check-imports t)
         (run-tests
          :os ("ubuntu-latest"
               "macos-latest")
          :quicklisp (;; Quicklisp is not working yet, because
                      ;; there is old JSONRPC where no "jsonrpc/transport/http" system. 
                      ;; "quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 ;; On CCL there are some strange network errors both on ubuntu and OSX
                 "ccl-bin/1.12.1"
                 "clisp"
                 "ecl")
          :exclude (
                    ;; For some reason CLISP of OSX does not support threading
                    ;; and bordeaux-threads fails to compile
                    (:os "macos-latest" 
                     :lisp "clisp")
                    ;; ECL on OSX fails to compile prometheus-gc/sbcl
                    (:os "macos-latest" 
                     :lisp "ecl"))
          :coverage t
          :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
                   dist ultralisp http://dist.ultralisp.org
                   {% endifequal %}")))

