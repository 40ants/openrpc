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
               ;; "macos-latest"
               )
          :quicklisp (;; "quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 ;; "ccl-bin"
                 )
          :coverage t
          ;; TODO: We don't need this because we depend on a custom version
          ;; of jsonrpc from the qlfile:
          ;; :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
          ;;          dist ultralisp http://dist.ultralisp.org
          ;;          {% endifequal %}"
          )))

