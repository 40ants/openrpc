(uiop:define-package #:openrpc-client/docs
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:openrpc-client
                #:generate-client))
(in-package #:openrpc-client/docs)


(defsection @client (:title "Client")
  (openrpc-client system)
  "OPENRPC-CLIENT ASDF system provides a way to build CL classes and methods for working with JSON-RPC API.
All you need is to give it an URL and all code will be created in compile-time as a result of macro-expansion.

## Generating

For example, this macro call:

```

(generate-client petshop
                 \"http://localhost:8000/openrpc.json\")

```

Will generate the whole bunch of classes and methods:


```
(defclass petshop (jsonrpc/class:client) nil)

(defun make-petshop () (make-instance 'petshop))

(defmethod describe-object ((openrpc-client/core::client petshop) stream)
  (format stream \"Supported RPC methods:~2%\")
  (format stream \"- ~S~%\" '(rpc-discover))
  (format stream \"- ~S~%\"
          '(list-pets &key (page-key nil page-key-given-p)
            (limit nil limit-given-p)))
  (format stream \"- ~S~%\" '(create-pet (name string) (tag string)))
  (format stream \"- ~S~%\" '(get-pet (id integer))))

(defclass pet nil
  ((id :initform nil :initarg :id :reader pet-id)
   (name :initform nil :initarg :name :reader pet-name)
   (tag :initform nil :initarg :tag :reader pet-tag)))

(defmethod print-object ((openrpc-client/core::obj pet) stream)
  (print-unreadable-object (openrpc-client/core::obj stream :type t)
    (format stream \" ~A=~S\" 'id (pet-id openrpc-client/core::obj))
    (format stream \" ~A=~S\" 'name (pet-name openrpc-client/core::obj))
    (format stream \" ~A=~S\" 'tag (pet-tag openrpc-client/core::obj))))

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

## Using

When client is generated, you need to make an instance of it and to connect
it to the server:

```
(let ((cl (make-petshop)))
    (jsonrpc:client-connect cl :url \"http://localhost:8000/\" :mode :http)
    cl)
```

You can use any transport, supported by [JSONRPC][jsonrpc] library.

DESCRIBE-OBJECT method is defined for a client, so you might see which methods are supported right in the REPL:


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
the library returns not JSON dictionaries, but ready to use CL class instances:

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (create-pet *client* \"Bobik\" \"the dog\")
#<PET  ID=1 NAME=\"Bobik\" TAG=\"the dog\">

OPENRPC-EXAMPLE/CLIENT> (create-pet *client* \"Murzik\" \"the cat\")
#<PET  ID=2 NAME=\"Murzik\" TAG=\"the cat\">

OPENRPC-EXAMPLE/CLIENT> (create-pet *client* \"Homa\" \"the hamster\")
#<PET  ID=3 NAME=\"Homa\" TAG=\"the hamster\">
```

Now, pay attention how pagination does work.

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (list-pets *client* :limit 2)
(#<PET  ID=1 NAME=\"Bobik\" TAG=\"the dog\">
 #<PET  ID=2 NAME=\"Murzik\" TAG=\"the cat\">)
#<FUNCTION (FLET OPENRPC-CLIENT/CORE::RETRIEVE-NEXT-PAGE :IN LIST-PETS) {1006D1F3CB}>
```

This call has returned a list of objects as the first value and a closure, which can
be called to retrive the next page. Let's retrieve it now!

```common-lisp-repl
OPENRPC-EXAMPLE/CLIENT> (funcall #v167:1)
(#<PET  ID=3 NAME=\"Homa\" TAG=\"the hamster\">)
```

Now this is the last page and there is now a closure to retrieve the next page. Learn more how
to implement pagination on server-side in the OPENRPC-SERVER/DOCS::@PAGINATION section.
"
  (generate-client macro))
