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


(defparameter *asdf-version* "3.3.5.1"
  "At some point installation of the latest roswell version was broken:
   https://github.com/roswell/roswell/issues/497")


(defworkflow server-ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  ;; :cache t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-version *asdf-version*)
         (run-tests
          :os ("ubuntu-latest"
               ;; "macos-latest"
               )
          :quicklisp (;; "quicklisp"
                      "ultralisp")
          :lisp ("sbcl-bin"
                 ;; "ccl-bin"
                 )
          :asdf-version *asdf-version*
          :coverage t
          ;; TODO: We don't need this because we depend on a custom version
          ;; of jsonrpc from the qlfile:
          ;; :qlfile "{% ifequal quicklisp_dist \"ultralisp\" %}
          ;;          dist ultralisp http://dist.ultralisp.org
          ;;          {% endifequal %}"
          )))

