(uiop:define-package #:openrpc-server/docs
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:openrpc-server
                #:define-rpc-method
                #:return-error
                #:make-info
                #:type-to-schema
                #:transform-result
                #:primitive-type-p
                #:make-clack-app))
(in-package #:openrpc-server/docs)


(defsection @server (:title "Server")
  (@defining-methods section)
  (@starting-server section)
  (@spec section)
  (@api section))


(defsection @defining-methods (:title "Defining Methods")
  "
Here is an example of openrpc-server ASDF system allows to define
JSON-RPC methods and data-structures they return.

Let's see how we can define an API for usual PetShop example.
"
  (@simple section)
  (@lists section)
  (@pagination section))


(defsection @simple (:title "Simple Example")
  "First, we will operate on usual Common Lisp class:

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

Now we can define an RPC method to create a new pet:

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
First one will tell framework to which JSON-SCHEMA should be mapped objects of this type:

```
(defmethod type-to-schema ((type (eql 'pet)))
  (dict \"type\" \"object\"
        \"properties\" (dict \"id\" (type-to-schema 'integer)
                           \"name\" (type-to-schema 'string)
                           \"tag\" (type-to-schema 'string))
        \"required\" (list \"id\" \"name\" \"tag\")
        \"x-cl-class\" (symbol-name type)
        \"x-cl-package\" (package-name (symbol-package type))))
```

And second method should transform pet instance into simple datastructures
according to scheme. Later result of this transformation will be serialized
to JSON:

```
(defmethod transform-result ((obj pet))
  (dict \"id\" (pet-id obj)
        \"name\" (pet-name obj)
        \"tag\" (pet-tag obj)))
```

Probably, someday framework will generate these methods automatically, using
types from DEFCLASS form.
")


(defsection @lists (:title "Returning Lists")
  "To return result as a list of objects of some kind, use `(:result (list-of pet))` form:

```
(openrpc-server:define-rpc-method list-pets ()
  (:param limit integer)
  (:param page-key integer)
  (:result (list-of pet))
  (retrieve-all-pets))
```
")


(defsection @starting-server (:title "Using Clack to Start Server")
  "Framework is based on Clack. Use MAKE-CLACK-APP to create an application suitable for serving with CLACK:CLACKUP.

Then just start the web application as usual.

```
(clack:clackup (make-clack-app)
               :address interface
               :port port)
```

Also, you might use any Lack middlewares. For example, here is how \"mount\" middleware can be used
to make API work on `/api/` URL path, while the main application is working on other URL paths:


```
(defparameter *app*
  (lambda (env)
    '(200 (:content-type \"text/plain\") (\"Hello, World!\"))))

(clack:clackup
 (lambda (app)
   (funcall (lack.util:find-middleware :mount)
            app
            \"/api\"
            (make-clack-app)))
 *app*)
```
")


(defsection @pagination (:title "Paginated Results")
  "Sometimes your system might operate on a lot of objects and you don't want to return all of them at once.
For this case, framework supports a [keyset pagination](https://use-the-index-luke.com/no-offset). To use
it, your method should accept LIMIT argument and PAGE-KEY argument. And if there are more results, than
method should return as a second value the page key for retrieving the next page.

In this simplified example, we'll return `(list 1 2 3)` for the first page, `(list 4 5 6)` for the second and
`(list 7 8)` for the third. Pay attention how VALUES form is used for first two pages but omitted for the third:

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

Of cause, in the real world application, you should use PAGE-KEY and LIMIT arguments in the WHERE SQL clause.
")


(defsection @spec (:title "OpenRPC Spec"
                   :ignore-words ("OpenRPC spec"))
  "The key feature of the framework, is an automatic [OpenRPC spec][spec] generation.

When you have your API up and running, spec will be available on `/openrpc.json` path.
For our example project it will looks like:

```json
{
  \"methods\": [
    {
      \"name\": \"rpc.discover\",
      \"params\": [],
      \"result\": {
        \"name\": \"OpenRPC Schema\",
        \"schema\": {
          \"$ref\": \"https://raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json\"
        }
      }
    },
    {
      \"name\": \"list-pets\",
      \"params\": [
        {
          \"name\": \"page-key\",
          \"schema\": {
            \"type\": \"integer\"
          }
        },
        {
          \"name\": \"limit\",
          \"schema\": {
            \"type\": \"integer\"
          }
        }
      ],
      \"result\": {
        \"name\": \"list-pets-result\",
        \"schema\": {
          \"type\": \"object\",
          \"properties\": {
            \"items\": {
              \"type\": \"array\",
...
```


")



(defsection @api (:title "API"
                  :ignore-words ("SERAPEUM:DICT")
                  ;; TODO: investigate why
                  ;; this does not work for docstring of transform-result generic-function
                  :external-links (("SERAPEUM:DICT" . "https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#dict-rest-keys-and-values")))
  (define-rpc-method macro)
  (type-to-schema generic-function)
  (transform-result generic-function)
  (primitive-type-p generic-function)
  (make-info generic-function)
  (return-error function)
  (make-clack-app function))
