(uiop:define-package #:openrpc-example/server
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values)
  (:import-from #:clack
                #:clackup)
  (:import-from #:openrpc-server
                #:return-error
                #:transform-result
                #:type-to-schema)
  (:import-from #:clack.handler.hunchentoot)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:openrpc-server/clack
                #:make-clack-app)
  (:export
   #:start
   #:stop))
(in-package #:openrpc-example/server)

(defvar *server* nil)
;; (defvar *pets* (make-hash-table :test 'equal))
(defvar *pets*)


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


(defmethod type-to-schema ((type (eql 'pet)))
  (dict "type" "object"
        "properties" (dict "id" (type-to-schema 'integer)
                           "name" (type-to-schema 'string)
                           "tag" (type-to-schema 'string))
        "required" (list "id" "name" "tag")
        "x-cl-class" (symbol-name type)
        "x-cl-package" (package-name (symbol-package type))))


(defmethod transform-result ((obj pet))
  (dict "id" (pet-id obj)
        "name" (pet-name obj)
        "tag" (pet-tag obj)))


(defun get-new-id ()
  (1+
   (if (zerop (hash-table-count *pets*))
       0
       (apply #'max (hash-table-keys *pets*)))))


(openrpc-server:define-rpc-method list-pets (&key (limit 10) page-key)
  (:param limit integer)
  (:param page-key integer)
  (:result (paginated-list-of pet))
  (let* ((pets (hash-table-values *pets*))
         (sorted-pets (progn ;; (break)
                             (sort pets #'< :key #'pet-id)))
         (page-key (or page-key
                       -1)))
    (loop with num-found = 0
          for pet in sorted-pets
          while (< num-found limit)
          when (> (pet-id pet)
                  page-key)
          collect pet into results
          and do (incf num-found)
                 ;; We need this complexity to tell framework if there is next page
                 ;; exists. In this case we have to return two values. Second value
                 ;; is next-page-key. Here we are using pet id as a page key.
                 ;; If it is given, then method will return pets with ids higher than
                 ;; one in the key. Pay attention that we are returning pets sorted by id.
                 ;; This makes pagination more efficient in traditional relational databases,
                 ;; compared to SKIP/LIMIT approach.
          finally (return
                    (let* ((last-pet (car (last results)))
                           (last-pet-id (when last-pet
                                          (pet-id last-pet)))
                           (more-pets-exists-p (when last-pet-id
                                                 (loop for pet in sorted-pets
                                                       thereis (> (pet-id pet)
                                                                  last-pet-id)))))
                      (if more-pets-exists-p
                          (values results
                                  (pet-id last-pet))
                          results))))))


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


(openrpc-server:define-rpc-method get-pet (id)
  (:param id integer)
  (:result pet)
  (let ((pet (gethash id *pets*)))
    (unless pet
      (return-error "Pet with id ~A was not found."))
    pet))


(defun start (&key (port 8000) (interface "localhost"))
  (when *server*
    (error "Server is already running. Please, stop it first."))
  (clackup (make-clack-app)
           :address interface
           :port port)
  (values))


(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil))
  (values))
