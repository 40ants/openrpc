<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E12-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.12.0 (2025-06-22)

<a id="changes"></a>

### Changes

Macro [`openrpc-server:define-api`][5255] now allows to pass class slots definition
like you can do with `DEFCLASS` macro.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E11-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.0 (2025-06-19)

<a id="fixes"></a>

### Fixes

Now if class's slot has type `BOOLEAN`, it's value is propertly serialized as "true" or "false". Previously instead of "false" a "null" was returned.

<a id="changes"></a>

### Changes

If slot name has some lowercased symbols, then we don't downcase the name when serializing or deserializing this slot. This allows to work with `API`'s which require someStrangeCased attributes. Here is an example how to use this feature:

```
(defclass call-response ()
  ((content :type (soft-list-of content)
            :initarg :content)
   (|isError| :type boolean
              :initform nil
              :initarg :is-error)))
```
<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E4-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.4 (2024-02-04)

<a id="fix"></a>

### Fix

Required parameters of type boolean false can be supplied now.
Since yason:*parse-json-booleans-as-symbols* is set, only yason:false maps into json false.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E3-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.3 (2023-11-21)

<a id="fix"></a>

### Fix

Add necessary helpers for simple example in Readme.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.2 (2023-11-17)

<a id="fixes"></a>

### Fixes

Fixed loading error occured in some cases when jsonrpc/transport/http was not found.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.1 (2023-10-17)

<a id="additions"></a>

### Additions

* Support object and boolean type for required parameters.
* Add regression tests for generated `DESCRIBE-OBJECT` method.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0 (2023-10-17)

<a id="changes"></a>

### Changes

Generic-function [`openrpc-server/interface:make-info`][e28d] now accepts only one argument - object of class [`openrpc-server/api:api`][5d14].

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E3-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.3 (2023-10-16)

<a id="fixes"></a>

### Fixes

A function `generate-method-descriptions` has been added. This function uses the mop
for generating the method descriptions at run-time. Previously this happend at compile-time.
The `generate-method-descriptions` is called from the generated specialized `describe-object`
method. Now the output of `describe-object` should be correct again and show all generated,
excluding the describe-object method itself.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.2 (2023-10-15)

<a id="fixes"></a>

### Fixes

* Generate additional method with integer class specializer (till now it was
  only double-float) for required parameter of type number.
* Allow required parameter of type array without an items slot.
  The result will not be transformed.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.1 (2023-09-24)

<a id="fixes"></a>

### Fixes

Fix support for multiple parameter types, (particularly for required parameters):

```json
"params": [
  {
    "name": "name",
    "schema":
    {
      "type": ["string", "null"],
      "maxLength": 255
    },
    "required": true,
    "summary": "User name."
  }
]
```
<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0 (2023-08-19)

<a id="backward-incompatible-fixes"></a>

### Backward Incompatible Fixes

Fixed how `oneOf` type is processed when there are only two subtypes like that:

```json
"oneOf": [
  {
    "type": "null"
  },
  {
    "type": "object",
    "properties": {
      "project_name": {
        "type": "object",
        "properties": {},
        "required": [],
        "x-cl-class": "T",
        "x-cl-package": "COMMON-LISP"
      },
```
Previously in this case openrpc-client generated code which returned a hash-table.
Now it will return a common-lisp object or `NIL`.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0 (2023-08-16)

<a id="backward-incompatible-fixes"></a>

### Backward Incompatible Fixes

Nested dataclasses now handled propertly in the client. Previously, nested objects were parsed as hash-maps.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E7-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.1 (2023-08-11)

<a id="fixes"></a>

### Fixes

Fixed location for autogenerated generic function and added docstrings taken from Open`RPC` spec.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0 (2023-08-09)

<a id="backward-incompatible-changes"></a>

### Backward Incompatible Changes

* Generic-function [`openrpc-server/interface:slots-to-exclude`][6155] now matches slot names before transforming
  them to camel_case. Now you can return slot names as they are given in lisp classes.

<a id="fixes"></a>

### Fixes

* Now client `API` is generated correctly when you call [`openrpc-client:generate-client`][3710] macro
  with `:export-symbols nil` argument.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2023-06-09)

<a id="additions"></a>

### Additions

* Float, Double float and Ratio types are supported. They are represented as a "number" type in `JSON` schema.

<a id="x-28OPENRPC-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2023-05-27)

<a id="changes"></a>

### Changes

* [`openrpc-server/clack:make-clack-app`][1d3d] now is a generic-function and it requires `API` instance as a first argument.
  Previously `API` instance was optional.

<a id="additions"></a>

### Additions

* Added a way to modify Clack middlewares applied to the app. This way you can add your own middlewares or routes to your application. See details in the [`openrpc-server/clack:app-middlewares`][a0d7] generic-function documentation.
* Added [`openrpc-server/clack:debug-on`][b0b1] and [`openrpc-server/clack:debug-off`][58fd] functions. They turn on support for `X-Debug-On` `HTTP` header. Use this header to turn on interative debugger only for choosen requests.
* Added generic-function [`openrpc-server/interface:slots-to-exclude`][6155] which allows to list some slots to be hidden from all data-structures. For example, you might want to exclude password-hashes or some other sensitive information.
* Added support for `(MEMBER :foo :bar ...)` datatype. Such objects are represented as string with enum values in `JSON` schema.

<a id="fixes"></a>

### Fixes

* Fixes type of the next-page key in the response of paginated methods.

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
[5d14]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FAPI-3AAPI-20CLASS-29
[5255]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FAPI-3ADEFINE-API-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[a0d7]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3AAPP-MIDDLEWARES-20GENERIC-FUNCTION-29
[58fd]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3ADEBUG-OFF-20FUNCTION-29
[b0b1]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3ADEBUG-ON-20FUNCTION-29
[1d3d]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3AMAKE-CLACK-APP-20GENERIC-FUNCTION-29
[e28d]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FINTERFACE-3AMAKE-INFO-20GENERIC-FUNCTION-29
[6155]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FINTERFACE-3ASLOTS-TO-EXCLUDE-20GENERIC-FUNCTION-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
