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
               "macos-latest")
          :quicklisp (;; Quicklisp is not working yet, because
                      ;; there is old JSONRPC where no "jsonrpc/transport/http" system. 
                      ;; "quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 "ccl-bin/1.12.1")
          :exclude ((:os "ubuntu-latest"
                         ;; On Ubuntu tests fail with this error:
                         ;; The condition Address family for hostname not supported (error #-9) during nameserver operation in getaddrinfo occurred with errno: 0.
                     :lisp "ccl-bin/1.12.1"))
          :coverage t
          :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
                   dist ultralisp http://dist.ultralisp.org
                   {% endifequal %}")))

