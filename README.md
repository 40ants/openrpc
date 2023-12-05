<a id="x-28OPENRPC-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# OpenRPC for Common Lisp

[![](https://github-actions.40ants.com/40ants/openrpc/matrix.svg)][4bbd]

This framework is built on top of [JSON-RPC][c597] and [`Clack`][75f7]. Comparing to `JSON-RPC` library,
it provides these key features:

* Automatic [OpenRPC spec][3160] generation.
* Automatic `JSON-RPC` client building by Open`RPC` spec. This includes creation of Common Lisp classes and methods
  for making `RPC` requests and returning native `CL` objects.
* On both server and client sides your code looks very lispy, all `JSON` marshalling is done under the hood.

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40SERVER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Server

<a id="openrpc-server-asdf-system-details"></a>

### OPENRPC-SERVER ASDF System Details

* Description: Open`RPC` server implementation for Common Lisp.
* Licence: `BSD`
* Author: Alexander Artemenko
* Homepage: [https://40ants.com/openrpc/][348e]
* Source control: [GIT][76b5]
* Depends on: [alexandria][8236], [clack-cors][087e], [clack-prometheus][babd], [closer-mop][61a4], [jsonrpc][a9bd], [lack-request][6a02], [lambda-fiddle][3ad9], [local-time][46a1], [log4cl][7f8b], [log4cl-extras][691c], [serapeum][c41d], [str][ef7f], [websocket-driver][4f50], [yason][aba2]

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40DEFINING-METHODS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Defining Methods

Here is an example of openrpc-server `ASDF` system allows to define
`JSON-RPC` methods and data-structures they return.

Let's see how we can define an [`api`][5d14] for usual PetShop example.

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40SIMPLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Simple Example

First, we will operate on usual Common Lisp class:

```
(defclass pet ()
  ((id :initarg :id
       :type integer
       :reader pet-id)
   (name :initarg :name
         :type string
         :reader pet-name)
   (tag :initarg :tag
        :type string
        :reader pet-tag)))
```

Next we define some helpers:

```lisp
(defvar *pets* (make-hash-table :test 'equal))

(defun get-new-id ()
  (1+
   (if (zerop (hash-table-count *pets*))
       0
       (apply #'max (alexandria:hash-table-keys *pets*)))))
```

Now we can define an `RPC` method to create a new pet:

```
(openrpc-server:define-rpc-method create-pet (name tag)
  (:summary "Short method docstring.")
  (:description "Lengthy description of the method.")
  (:param name string "Pet's name"
          :description "This is a long description of the parameter.")
  (:param tag string "Old param, don't use it anymore." :deprecated t)
  (:result pet)
  (let* ((new-id (get-new-id))
         (pet (make-instance 'pet
                             :id new-id
                             :name name
                             :tag tag)))
    (setf (gethash new-id *pets*)
          pet)
    pet))
```
Here we should explicitly specify type for each parameter and result's type.

Pay attention, the result type is `PET`. [`openrpc-server`][24be] system takes care on serializing
objects and you can retrieve an Open`RPC` spec for any type, using [`type-to-schema`][4de0] generic-function:

```
CL-USER> (serapeum:toggle-pretty-print-hash-table)
T
CL-USER> (openrpc-server:type-to-schema 'pet)
(SERAPEUM:DICT
  "type" "object"
  "properties" (SERAPEUM:DICT
                 "id" (SERAPEUM:DICT
                        "type" "integer"
                       )
                 "name" (SERAPEUM:DICT
                          "type" "string"
                         )
                 "tag" (SERAPEUM:DICT
                         "type" "string"
                        )
                )
  "required" '("tag" "name" "id")
  "x-cl-class" "PET"
  "x-cl-package" "COMMON-LISP-USER"
 )
```
This method is used to render response requests to `/openrpc.json` handle of your [`api`][5d14].

There is also a second generic-function which transform class instance into simple datastructures
according to a scheme. For example, here is how we can serialize our pet:

```
CL-USER> (openrpc-server:transform-result
          (make-instance 'pet :name "Bobik"))
(SERAPEUM:DICT
  "name" "Bobik"
 ) 
CL-USER> (openrpc-server:transform-result
          (make-instance 'pet
                         :name "Bobik"
                         :tag "the dog"))
(SERAPEUM:DICT
  "name" "Bobik"
  "tag" "the dog"
 )
```
<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40LISTS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Returning Lists

To return result as a list of objects of some kind, use `(:result (list-of pet))` form:

```
(openrpc-server:define-rpc-method list-pets ()
  (:result (list-of pet))
  (retrieve-all-pets))
```
<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40PAGINATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Paginated Results

Sometimes your system might operate on a lot of objects and you don't want to return all of them at once.
For this case, framework supports a [keyset pagination][7ba8]. To use
it, your method should accept `LIMIT` argument and `PAGE-KEY` argument. And if there are more results, than
method should return as a second value the page key for retrieving the next page.

In this simplified example, we'll return `(list 1 2 3)` for the first page, `(list 4 5 6)` for the second and
`(list 7 8)` for the third. Pay attention how `VALUES` form is used for first two pages but omitted for the third:

```
(openrpc-server:define-rpc-method list-pets (&key (limit 3) page-key)
  (:param limit integer)
  (:param page-key integer)
  (:result (paginated-list-of integer))

  (cond
    ((null page-key)
     (values (list 1 2 3)
             3))
    ((= page-key 3)
     (values (list 4 5 6)
             6))
    (t
      (list 7 8))))
```
Of cause, in the real world application, you should use `PAGE-KEY` and `LIMIT` arguments in the `WHERE` `SQL` clause.

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40STARTING-SERVER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Using Clack to Start Server

Framework is based on Clack. Use [`make-clack-app`][1d3d] to create an application suitable for serving with `CLACK:CLACKUP`.

Then just start the web application as usual.

```
(clack:clackup (make-clack-app)
               :address interface
               :port port)
```
Also, you might use any Lack middlewares. For example, here is how "mount" middleware can be used
to make [`api`][5d14] work on `/api/` `URL` path, while the main application is working on other `URL` paths:

```
(defparameter *app*
  (lambda (env)
    '(200 (:content-type "text/plain") ("Hello, World!"))))

(clack:clackup
 (lambda (app)
   (funcall (lack.util:find-middleware :mount)
            app
            "/api"
            (make-clack-app)))
 *app*)
```
<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40SPEC-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### OpenRPC Spec

The key feature of the framework, is an automatic `OpenRPC spec` generation.

When you have your [`api`][5d14] up and running, spec will be available on `/openrpc.json` path.
For our example project it will looks like:

```json
{
  "methods": [
    {
      "name": "rpc.discover",
      "params": [],
      "result": {
        "name": "OpenRPC Schema",
        "schema": {
          "$ref": "https://raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json"
        }
      }
    },
    {
      "name": "list-pets",
      "params": [
        {
          "name": "page-key",
          "schema": {
            "type": "integer"
          }
        },
        {
          "name": "limit",
          "schema": {
            "type": "integer"
          }
        }
      ],
      "result": {
        "name": "list-pets-result",
        "schema": {
          "type": "object",
          "properties": {
            "items": {
              "type": "array",
...
```
<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### API

<a id="x-28OPENRPC-SERVER-2FAPI-3AAPI-20CLASS-29"></a>

#### [class](a7d4) `openrpc-server/api:api` ()

<a id="x-28OPENRPC-SERVER-2FAPI-3A-2ACURRENT-API-2A-20-28VARIABLE-29-29"></a>

#### [variable](d40b) `openrpc-server/api:*current-api*` -unbound-

Points to a current [`api`][5d14] object when processing any `RPC` method.

<a id="x-28OPENRPC-SERVER-2FAPI-3ADEFINE-API-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](04bc) `openrpc-server/api:define-api` (NAME &KEY (TITLE "Default API") (VERSION "0.1.0"))

<a id="x-28OPENRPC-SERVER-2FAPI-3AAPI-METHODS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20OPENRPC-SERVER-2FAPI-3AAPI-29-29"></a>

#### [reader](43ae) `openrpc-server/api:api-methods` (api) (= (make-hash-table :test 'equal))

Returns a hash-table containing meta-information about all [`api`][5d14] methods.

Returned hash-table has key strings having methods names and internal
objects of class `method-info` as values. I'm not sure if we need to
export functions to manipulate with method info objects manually.
Use [`openrpc-server/method:define-rpc-method`][b2e4] macro to add or update `RPC` methods.

<a id="x-28OPENRPC-SERVER-2FAPI-3AAPI-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20OPENRPC-SERVER-2FAPI-3AAPI-29-29"></a>

#### [reader](de63) `openrpc-server/api:api-title` (api) (:TITLE = "Default API")

Returns a title of the [`api`][5d14].

<a id="x-28OPENRPC-SERVER-2FAPI-3AAPI-VERSION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20OPENRPC-SERVER-2FAPI-3AAPI-29-29"></a>

#### [reader](9c2b) `openrpc-server/api:api-version` (api) (:version = "0.1.0")

Returns a version of the [`api`][5d14].

<a id="x-28OPENRPC-SERVER-2FMETHOD-3ADEFINE-RPC-METHOD-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](a138) `openrpc-server/method:define-rpc-method` name args &body body

Macro to define `RPC` method.

All arguments should have corresponding (:param arg type) form in the `BODY`.

Also, there should be one (:result type) form in the `BODY`.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3ATYPE-TO-SCHEMA-20GENERIC-FUNCTION-29"></a>

#### [generic-function](40da) `openrpc-server/interface:type-to-schema` type

This method is called for all types for which [`primitive-type-p`][edf5] generic-function
returns `NIL`.

It should return as hash-table with `JSON-SCHEMA` corresponding to type. Keys of the dictionary should
be strings. It is convenient to use `SERAPEUM:DICT` for building the result.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3ATRANSFORM-RESULT-20GENERIC-FUNCTION-29"></a>

#### [generic-function](ef94) `openrpc-server/interface:transform-result` object

Prepares object for serialization before responding to `RPC` call.

Result should be list, hash-map or a value of primitive type.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3APRIMITIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>

#### [generic-function](64a4) `openrpc-server/interface:primitive-type-p` type

Should return t for type if it's name matched to simple types supported by [JSON-SCHEMA][686b].

Argument `TYPE` is a symbol.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3AMAKE-INFO-20GENERIC-FUNCTION-29"></a>

#### [generic-function](a55b) `openrpc-server/interface:make-info` api

Returns a basic information about `API` for [info section][5f59] of Open`RPC` spec.

<a id="x-28OPENRPC-SERVER-2FERRORS-3ARETURN-ERROR-20FUNCTION-29"></a>

#### [function](ff5b) `openrpc-server/errors:return-error` message &key (code -1) (error-class 'jsonrpc/errors:jsonrpc-callback-error)

Raises an error to interrupt processing and return status to the caller.

<a id="x-28OPENRPC-SERVER-2FCLACK-3AMAKE-CLACK-APP-20GENERIC-FUNCTION-29"></a>

#### [generic-function](bea6) `openrpc-server/clack:make-clack-app` api &key http websocket indent-json

Should return an app suitable for passing to clackup.

You can define a method to redefine application.
But to add middlewares it is more convenient to define a method for
[`app-middlewares`][a0d7] generic-function.

<a id="x-28OPENRPC-SERVER-2FCLACK-3AAPP-MIDDLEWARES-20GENERIC-FUNCTION-29"></a>

#### [generic-function](d7c0) `openrpc-server/clack:app-middlewares` api

Should return an plist of middlewared to be applied to the Clack application.

Keys should be a keyword with middleware name. And value is a function accepting
a Clack application as a single argument and returning a new application.

Middlewares are applied to the app from left to right. This makes it possible to
define an :around method which will inject or replace a middleware or original app.

Default method defines two middlewares with keys `:CORS` and `:PROMETHEUS`.
(Prometheus middleware works on `SBCL` but not on `CCL`).
To wrap these middlewares, add your middlewares to the end of the list.
To add your middleware inside the stack - push it to the front.

<a id="x-28OPENRPC-SERVER-2FCLACK-3ADEBUG-ON-20FUNCTION-29"></a>

#### [function](c9e9) `openrpc-server/clack:debug-on`

<a id="x-28OPENRPC-SERVER-2FCLACK-3ADEBUG-OFF-20FUNCTION-29"></a>

#### [function](8e7b) `openrpc-server/clack:debug-off`

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3ASLOTS-TO-EXCLUDE-20GENERIC-FUNCTION-29"></a>

#### [generic-function](866a) `openrpc-server/interface:slots-to-exclude` type

You can define a method for this generic function to exclude some slots from being shown in the `JSON` schema.

Pay attention that this generic-function is called with class not with objects to be serialized.
We need this because at the moment of generation [`api`][5d14] methods and Open`RPC` spec we know nothing about
objects except their classes.

Methods of this function should return a list of strings. Given slots will be excluded from the spec
and will not be serialized. Strings are compared in case-insensitive mode.

<a id="x-28OPENRPC-CLIENT-2FDOCS-3A-3A-40CLIENT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Client

<a id="openrpc-client-asdf-system-details"></a>

### OPENRPC-CLIENT ASDF System Details

* Description: Open`RPC` client implementation for Common Lisp.
* Licence: `BSD`
* Author: Alexander Artemenko
* Homepage: [https://40ants.com/openrpc/][348e]
* Source control: [GIT][76b5]
* Depends on: [alexandria][8236], [dexador][8347], [jsonrpc][a9bd], [kebab][5186], [log4cl][7f8b], [serapeum][c41d], [str][ef7f], [usocket][636b], [yason][aba2]

[`openrpc-client`][4c76] `ASDF` system provides a way to build `CL` classes and methods for working with `JSON-RPC` `API`.
All you need is to give it an `URL` and all code will be created in compile-time as a result of macro-expansion.

<a id="generating"></a>

### Generating

For example, this macro call:

```

(generate-client petshop
                 "http://localhost:8000/openrpc.json")
```
Will generate the whole bunch of classes and methods:

```
(defclass petshop (jsonrpc/class:client) nil)

(defun make-petshop () (make-instance 'petshop))

(defmethod describe-object ((openrpc-client/core::client petshop) stream)
  (format stream "Supported RPC methods:~2%")
  (format stream "- ~S~%" '(rpc-discover))
  (format stream "- ~S~%"
          '(list-pets &key (page-key nil page-key-given-p)
            (limit nil limit-given-p)))
  (format stream "- ~S~%" '(create-pet (name string) (tag string)))
  (format stream "- ~S~%" '(get-pet (id integer))))

(defclass pet nil
  ((id :initform nil :initarg :id :reader pet-id)
   (name :initform nil :initarg :name :reader pet-name)
   (tag :initform nil :initarg :tag :reader pet-tag)))

(defmethod print-object ((openrpc-client/core::obj pet) stream)
  (print-unreadable-object (openrpc-client/core::obj stream :type t)
    (format stream " ~A=~S" 'id (pet-id openrpc-client/core::obj))
    (format stream " ~A=~S" 'name (pet-name openrpc-client/core::obj))
    (format stream " ~A=~S" 'tag (pet-tag openrpc-client/core::obj))))

(defmethod rpc-discover ((openrpc-client/core::client petshop))
  ...)

(defmethod list-pets
    ((openrpc-client/core::client petshop)
     &key (page-key nil page-key-given-p) (limit nil limit-given-p))
  ...)

(defmethod create-pet
    ((openrpc-client/core::client petshop) (name string) (tag string))
  ...)

(defmethod get-pet ((openrpc-client/core::client petshop) (id integer))
  ...)
```
<a id="using"></a>

### Using

When client is generated, you need to make an instance of it and to connect
it to the server:

```
(let ((cl (make-petshop)))
    (jsonrpc:client-connect cl :url "http://localhost:8000/" :mode :http)
    cl)
```
You can use any transport, supported by `JSONRPC` library.

`DESCRIBE-OBJECT` method is defined for a client, so you might see which methods are supported right in the `REPL`:

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (defvar *client* (make-test-client))
#<PETSHOP {1007AB2B13}>

OPENRPC-EXAMPLE/CLIENT> (describe *client*)
Supported RPC methods:

- (RPC-DISCOVER)
- (LIST-PETS &KEY (PAGE-KEY NIL PAGE-KEY-GIVEN-P)
             (LIMIT NIL LIMIT-GIVEN-P))
- (CREATE-PET (NAME STRING) (TAG STRING))
- (GET-PET (ID INTEGER))
```
And then to call these methods as usually you do in Common Lisp. Pay attention, that
the library returns not `JSON` dictionaries, but ready to use `CL` class instances:

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (create-pet *client* "Bobik" "the dog")
#<PET  ID=1 NAME="Bobik" TAG="the dog">

OPENRPC-EXAMPLE/CLIENT> (create-pet *client* "Murzik" "the cat")
#<PET  ID=2 NAME="Murzik" TAG="the cat">

OPENRPC-EXAMPLE/CLIENT> (create-pet *client* "Homa" "the hamster")
#<PET  ID=3 NAME="Homa" TAG="the hamster">
```
Now, pay attention how pagination does work.

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (list-pets *client* :limit 2)
(#<PET  ID=1 NAME="Bobik" TAG="the dog">
 #<PET  ID=2 NAME="Murzik" TAG="the cat">)
#<FUNCTION (FLET OPENRPC-CLIENT/CORE::RETRIEVE-NEXT-PAGE :IN LIST-PETS) {1006D1F3CB}>
```
This call has returned a list of objects as the first value and a closure, which can
be called to retrive the next page. Let's retrieve it now!

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (funcall #v167:1)
(#<PET  ID=3 NAME="Homa" TAG="the hamster">)
```
Now this is the last page and there is now a closure to retrieve the next page. Learn more how
to implement pagination on server-side in the [`Paginated Results`][5c31] section.

<a id="x-28OPENRPC-CLIENT-2FCORE-3AGENERATE-CLIENT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro](c95d) `openrpc-client/core:generate-client` class-name url-or-path &key (export-symbols t)

Generates Common Lisp client by Open`RPC` spec.

`CLASS-NAME` is the name of a `API` class. Also, a corresponding `MAKE`-<CLASS-NAME> function
is created.

`URL-OR-PATH` argument could be a string with `HTTP` `URL` of a spec, or a pathname
if a spec should be read from the disc.

<a id="x-28OPENRPC-DOCS-2FCONTRIBUTING-3A-3A-40ASK-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Our Ask...

If you use this or find value in it, please consider contributing in one or more of the following ways:

1. Sponsor project at [Patreon][2180] or [Boosty][ae59] and make a contribution.
2. Star it!
3. Share posts about it in social networks!
4. Fix an issue.
5. Add a feature (post a proposal in an issue first!).

<a id="x-28OPENRPC-DOCS-2FCONTRIBUTING-3A-3A-40CONTRIBUTORS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Contributors

These people have contributed to Open`RPC`. I'm so grateful to them!

* [Kilian M. Haemmerle][5863]


[348e]: https://40ants.com/openrpc/
[4c76]: https://40ants.com/openrpc/#x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22openrpc-client-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[24be]: https://40ants.com/openrpc/#x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22openrpc-server-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[5d14]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FAPI-3AAPI-20CLASS-29
[a0d7]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3AAPP-MIDDLEWARES-20GENERIC-FUNCTION-29
[1d3d]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3AMAKE-CLACK-APP-20GENERIC-FUNCTION-29
[5c31]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FDOCS-3A-3A-40PAGINATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[edf5]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FINTERFACE-3APRIMITIVE-TYPE-P-20GENERIC-FUNCTION-29
[4de0]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FINTERFACE-3ATYPE-TO-SCHEMA-20GENERIC-FUNCTION-29
[b2e4]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FMETHOD-3ADEFINE-RPC-METHOD-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[ae59]: https://boosty.to/40ants
[76b5]: https://github.com/40ants/openrpc
[4bbd]: https://github.com/40ants/openrpc/actions
[c95d]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/client/core.lisp#L488
[d40b]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/api.lisp#L14
[a7d4]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/api.lisp#L18
[43ae]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/api.lisp#L19
[de63]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/api.lisp#L27
[9c2b]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/api.lisp#L31
[04bc]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/api.lisp#L39
[c9e9]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/clack.lisp#L180
[8e7b]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/clack.lisp#L185
[bea6]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/clack.lisp#L66
[d7c0]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/clack.lisp#L73
[ff5b]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/errors.lisp#L9
[a55b]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/interface.lisp#L163
[ef94]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/interface.lisp#L20
[64a4]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/interface.lisp#L52
[866a]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/interface.lisp#L68
[40da]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/interface.lisp#L81
[a138]: https://github.com/40ants/openrpc/blob/4d5adcc72e7e11260d0ffb0ae56b18408ae26676/server/method.lisp#L313
[c597]: https://github.com/cxxxr/jsonrpc
[75f7]: https://github.com/fukamachi/clack
[5863]: https://github.com/kilianmh
[686b]: https://json-schema.org/
[8236]: https://quickdocs.org/alexandria
[087e]: https://quickdocs.org/clack-cors
[babd]: https://quickdocs.org/clack-prometheus
[61a4]: https://quickdocs.org/closer-mop
[8347]: https://quickdocs.org/dexador
[a9bd]: https://quickdocs.org/jsonrpc
[5186]: https://quickdocs.org/kebab
[6a02]: https://quickdocs.org/lack-request
[3ad9]: https://quickdocs.org/lambda-fiddle
[46a1]: https://quickdocs.org/local-time
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[636b]: https://quickdocs.org/usocket
[4f50]: https://quickdocs.org/websocket-driver
[aba2]: https://quickdocs.org/yason
[3160]: https://spec.open-rpc.org/
[5f59]: https://spec.open-rpc.org/#info-object
[7ba8]: https://use-the-index-luke.com/no-offset
[2180]: https://www.patreon.com/bePatron?u=33868637

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
