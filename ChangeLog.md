<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2022-11-07)

* Fixed usage of default `API` when api is not specified to define-rpc-method macro.

* Fixed most imports.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2022-10-30)

* Method and its params now support such metadata as :summary :description and :deprecated.

* Schemas for `CL` classes can have :description if documentation id defined for class or its slots.

* Macro [`openrpc-client:generate-client`][3710] now exports methods, classes and their slot readers by default.

* All methods, their arguments and object keys now use underscore instead of dash to make them more
  convenient to use from other languages.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2022-10-25)

* Support client generation from a file on a filesystem.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0 (2022-10-13)

* Initial version.


[3710]: https://40ants.com/openrpc/#x-28OPENRPC-CLIENT-2FCORE-3AGENERATE-CLIENT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
