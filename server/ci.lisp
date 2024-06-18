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
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter :check-imports t)
         (run-tests
          :os ("ubuntu-latest"
               "macos-13")
          :quicklisp (;; Quicklisp is not working yet, because
                      ;; there is old JSONRPC where no "jsonrpc/server" system. 
                      ;; "quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 ;; If somebody cares about these or other implementations
                 ;; you can join as a maintainer:
                 ;; "ccl-bin"
                 ;; "clisp"
                 ;; "ecl"
                 )
          ;; :exclude (
          ;;           ;; For some reason CLISP of OSX does not support threading
          ;;           ;; and bordeaux-threads fails to compile
          ;;           (:os "macos-13" 
          ;;            :lisp "clisp")
          ;;           ;; ECL on OSX fails to compile prometheus-gc/sbcl
          ;;           (:os "macos-13" 
          ;;            :lisp "ecl"))
          :coverage t
          :qlfile "{% ifequal env.quicklisp_dist \"ultralisp\" %}
                   dist ultralisp http://dist.ultralisp.org
                   {% endifequal %}")))

