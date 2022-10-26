<a id="x-28OPENRPC-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# OpenRPC for Common Lisp

[![](https://github-actions.40ants.com/40ants/openrpc/matrix.svg)][4bbd]

This framework is built on top of [`JSON-RPC`][c597] and [`Clack`][75f7]. Comparing to `JSON-RPC` library,
it provides these key features:

* Automatic [OpenRPC spec][3160] generation.

* Automatic `JSON-RPC` client building by Open`RPC` spec. This includes creation of Common Lisp classes and methods
  for making `RPC` requests and returning native `CL` objects.

* On both server and client sides your code looks very lispy, all `JSON` marshalling is done under the hood.

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40SERVER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Server

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40DEFINING-METHODS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Defining Methods

Here is an example of openrpc-server `ASDF` system allows to define
`JSON-RPC` methods and data-structures they return.

Let's see how we can define an `API` for usual PetShop example.

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
Now we can define an `RPC` method to create a new pet:

```
(openrpc-server:define-rpc-method create-pet (name tag)
  (:param name string)
  (:param tag string)
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

But to make this work with our pet class, we have to define a two more methods.
First one will tell framework to which `JSON-SCHEMA` should be mapped objects of this type:

```
(defmethod type-to-schema ((type (eql 'pet)))
  (dict "type" "object"
        "properties" (dict "id" (type-to-schema 'integer)
                           "name" (type-to-schema 'string)
                           "tag" (type-to-schema 'string))
        "required" (list "id" "name" "tag")
        "x-cl-class" (symbol-name type)
        "x-cl-package" (package-name (symbol-package type))))
```
And second method should transform pet instance into simple datastructures
according to scheme. Later result of this transformation will be serialized
to `JSON`:

```
(defmethod transform-result ((obj pet))
  (dict "id" (pet-id obj)
        "name" (pet-name obj)
        "tag" (pet-tag obj)))
```
Probably, someday framework will generate these methods automatically, using
types from `DEFCLASS` form.

<a id="x-28OPENRPC-SERVER-2FDOCS-3A-3A-40LISTS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Returning Lists

To return result as a list of objects of some kind, use `(:result (list-of pet))` form:

```
(openrpc-server:define-rpc-method list-pets ()
  (:param limit integer)
  (:param page-key integer)
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

Framework is based on Clack. Use [`make-clack-app`][3311] to create an application suitable for serving with `CLACK:CLACKUP`.

Then just start the web application as usual.

```
(clack:clackup (make-clack-app)
               :address interface
               :port port)
```
Also, you might use any Lack middlewares. For example, here is how "mount" middleware can be used
to make `API` work on `/api/` `URL` path, while the main application is working on other `URL` paths:

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

When you have your `API` up and running, spec will be available on `/openrpc.json` path.
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

<a id="x-28OPENRPC-SERVER-2FMETHOD-3ADEFINE-RPC-METHOD-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](7ea9) `openrpc-server/method:define-rpc-method` name args &body body

Macro to define `RPC` method.

All arguments should have corresponding (:param arg type) form in the `BODY`.

Also, there should be one (:result type) form in the `BODY`.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3ATYPE-TO-SCHEMA-20GENERIC-FUNCTION-29"></a>

#### [generic-function](52a9) `openrpc-server/interface:type-to-schema` type

This method is called for all types for which [`primitive-type-p`][edf5] generic-function
returns `NIL`.

It should return as hash-table with `JSON-SCHEMA` corresponding to type. Keys of the dictionary should
be strings. It is convenient to use `SERAPEUM:DICT` for building the result.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3ATRANSFORM-RESULT-20GENERIC-FUNCTION-29"></a>

#### [generic-function](9066) `openrpc-server/interface:transform-result` object

Prepares object for serialization before responding to `RPC` call.

Result should be list, hash-map or a value of primitive type.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3APRIMITIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>

#### [generic-function](07fc) `openrpc-server/interface:primitive-type-p` type

Should return t for type if it's name matched to simple types supported by [JSON-SCHEMA][686b].

Argument `TYPE` is a symbol.

<a id="x-28OPENRPC-SERVER-2FINTERFACE-3AMAKE-INFO-20GENERIC-FUNCTION-29"></a>

#### [generic-function](54da) `openrpc-server/interface:make-info` server

Returns a basic information about `API` for [info section][5f59] of Open`RPC` spec.

<a id="x-28OPENRPC-SERVER-2FERRORS-3ARETURN-ERROR-20FUNCTION-29"></a>

#### [function](3fed) `openrpc-server/errors:return-error` message &key (code -1)

Raises an error to interrupt processing and return status to the caller.

<a id="x-28OPENRPC-SERVER-2FCLACK-3AMAKE-CLACK-APP-20FUNCTION-29"></a>

#### [function](a0ce) `openrpc-server/clack:make-clack-app` &key (http t) (websocket t)

Returns an Clack application to serve `JSON-RPC` `API`.

<a id="x-28OPENRPC-CLIENT-2FDOCS-3A-3A-40CLIENT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Client

`OPENRPC-CLIENT` `ASDF` system provides a way to build `CL` classes and methods for working with `JSON-RPC` `API`.
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


[3311]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FCLACK-3AMAKE-CLACK-APP-20FUNCTION-29
[5c31]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FDOCS-3A-3A-40PAGINATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[edf5]: https://40ants.com/openrpc/#x-28OPENRPC-SERVER-2FINTERFACE-3APRIMITIVE-TYPE-P-20GENERIC-FUNCTION-29
[4bbd]: https://github.com/40ants/openrpc/actions
[a0ce]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/clack.lisp#L60
[3fed]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/errors.lisp#L9
[9066]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/interface.lisp#L15
[07fc]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/interface.lisp#L23
[52a9]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/interface.lisp#L35
[54da]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/interface.lisp#L80
[7ea9]: https://github.com/40ants/openrpc/blob/785be0102bcdd3b3d73c5d0d60e757f78f4e1644/server/method.lisp#L228
[c597]: https://github.com/cxxxr/jsonrpc
[75f7]: https://github.com/fukamachi/clack
[686b]: https://json-schema.org/
[3160]: https://spec.open-rpc.org/
[5f59]: https://spec.open-rpc.org/#info-object
[7ba8]: https://use-the-index-luke.com/no-offset

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
