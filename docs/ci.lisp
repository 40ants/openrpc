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


(defparameter *asdf-version* "3.3.5.1"
  "At some point installation of the latest roswell version was broken:
   https://github.com/roswell/roswell/issues/497")


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :by-cron "0 10 * * 1"
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-version *asdf-version*)))
