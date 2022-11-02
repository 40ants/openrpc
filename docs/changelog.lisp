(uiop:define-package #:openrpc-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:openrpc-docs/changelog)


(defchangelog (:ignore-words ("40ANTS-DOC"
                              "ASDF"
                              "OSX"))
  (0.3.0 2022-10-30
         "- Method and its params now support such metadata as :summary :description and :deprecated.
          - Schemas for CL classes can have :description if documentation id defined for class or its slots.
          - Function OPENRPC-CLIENT:GENERATE now exports methods, classes and their slot readers by default.
          - All methods, their arguments and object keys now use underscore instead of dash to make them more
            convenient to use from other languages.")
  (0.2.0 2022-10-25
         "- Support client generation from a file on a filesystem.")
  (0.1.0 2022-10-13
         "- Initial version."))
