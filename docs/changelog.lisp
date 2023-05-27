(uiop:define-package #:openrpc-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:openrpc-docs/changelog)


(defchangelog (:ignore-words ("40ANTS-DOC"
                              "ASDF"
                              "API"
                              "CL"
                              "HTTP"
                              "JSON"
                              "OSX"))
  (0.5.0 2023-05-27
         "
## Changes

- OPENRPC-SERVER/CLACK:MAKE-CLACK-APP now is a generic-function.

## Additions

- Added a way to modify Clack middlewares applied to the app. This way you can add your own middlewares or routes to your application. See details in the OPENRPC-SERVER/CLACK:APP-MIDDLEWARES generic-function documentation.
- Added OPENRPC-SERVER/CLACK:DEBUG-ON and OPENRPC-SERVER/CLACK:DEBUG-OFF functions. They turn on support for `X-Debug-On` HTTP header. Use this header to turn on interative debugger only for choosen requests.
- Added generic-function OPENRPC-SERVER/INTERFACE:SLOTS-TO-EXCLUDE which allows to list some slots to be hidden from all data-structures. For example, you might want to exclude password-hashes or some other sensitive information.
- Added support for `(MEMBER :foo :bar ...)` datatype. Such objects are represented as string with enum values in JSON schema.

# Fixes

- Fixes type of the next-page key in the response of paginated methods.
")
  (0.4.0 2022-11-07
         "- Fixed usage of default API when api is not specified to define-rpc-method macro.
          - Fixed most imports.")
  (0.3.0 2022-10-30
         "- Method and its params now support such metadata as :summary :description and :deprecated.
          - Schemas for CL classes can have :description if documentation id defined for class or its slots.
          - Macro OPENRPC-CLIENT:GENERATE-CLIENT now exports methods, classes and their slot readers by default.
          - All methods, their arguments and object keys now use underscore instead of dash to make them more
            convenient to use from other languages.")
  (0.2.0 2022-10-25
         "- Support client generation from a file on a filesystem.")
  (0.1.0 2022-10-13
         "- Initial version."))
