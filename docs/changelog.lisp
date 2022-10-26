(uiop:define-package #:openrpc-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:openrpc-docs/changelog)


(defchangelog (:ignore-words ("40ANTS-DOC"
                              "ASDF"
                              "OSX"))
  (0.1.0 2022-10-13
         "- Initial version."))
