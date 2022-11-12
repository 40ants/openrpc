(defpackage #:openrpc-docs/ci
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
(in-package #:openrpc-docs/ci)


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :by-cron "0 10 * * 1"
  ;; :cache t
  :jobs ((40ants-ci/jobs/linter:linter :check-imports t)
         (40ants-ci/jobs/docs:build-docs)))
