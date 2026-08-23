(uiop:define-package #:openrpc-tests/client/regression
  (:use #:cl)
  (:import-from #:rove
                #:deftest)
  (:import-from #:diff)
  (:import-from #:openrpc-client/core
                #:%generate-client
		#:generate-client
                #:retrieve-spec)
  (:import-from #:alexandria
                #:last-elt
                #:write-string-into-file
                #:read-file-into-string))
(in-package #:openrpc-tests/client/regression)

(declaim (optimize (debug 3) (safety 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun relative-path (test-name file-name &key (type "lisp"))
    (asdf:system-relative-pathname
     :openrpc-tests
     (make-pathname :directory (list :relative
				     "t"
				     "client"
				     "regress-data"
				     test-name)
		    :name file-name
		    :type type))))


(defun regression-test-names ()
  (let* ()
    (loop with path = (asdf:system-relative-pathname
                       :openrpc-tests
                       (make-pathname :directory (list :relative
                                                       "t"
                                                       "client"
                                                       "regress-data"
                                                       uiop:*wild*)))
          for subdir in (directory path)
          for dir-name = (pathname-directory subdir)
          for last-component = (last-elt dir-name)
          collect last-component)))


(defun compare (lisp-code
                test-name
                file-name)
  "Compares LISP-CODE with content of the etalon saved in the t/client/regress-data/<test-name>"
  (let* ((lisp-code-as-string
           (let ((*print-case* :downcase)
                 (*print-pretty* t))
             (format nil "~S~%" lisp-code)))
         (path (relative-path test-name file-name))
         (etalon (let ((*print-case* :downcase)
                       (*print-pretty* t))
                   (format nil "~S~%"
                           (car (uiop:read-file-forms path)))))
         (diff (diff:generate-seq-diff 'diff:unified-diff
                                       (str:split #\Newline etalon)
                                       (str:split #\Newline lisp-code-as-string)))
         (diff-is-empty (null (diff:diff-windows diff))))
    (restart-case
        (rove:ok diff-is-empty
                 (cond
                   ((diff:diff-windows diff)
                    (cond
                      (rove:*debug-on-error*
                       (error
                        (with-output-to-string (s)
                          (format s "File ~S has this difference:~2%" path)
                          (diff:render-diff diff s))))
                      (t
                       (with-output-to-string (s)
                         (format s "File ~S has this difference:~2%" path)
                         (diff:render-diff diff s)
                         (format s "~2&Set rove:*debug-on-error* to T and select UPDATE-REGRESSION-FILE-CONTENT restart to update the file.")))))
                   (t
                    (format nil "Content of ~S is the same as ~S"
                            file-name
                            path))))
      (update-regression-file-content ()
        :report (lambda (stream)
                  (format stream "Update file ~S with new lisp code."
                          path))
        (write-string-into-file lisp-code-as-string
                                path
                                :if-exists :supersede)))))


(deftest client-regression ()
  (loop for test-name in (regression-test-names)
        do (rove:testing test-name
             (let ((spec
                     (let ((spec
                             (rove:testing "Reading spec"
                               (retrieve-spec
                                (asdf:system-relative-pathname
                                 :openrpc-tests
                                 (relative-path test-name
                                                "spec"
                                                :type "json"))))))
                       (rove:ok spec))))

               (rove:testing "Generating client"
                 (multiple-value-bind (client-class class-definitions methods)
                     (let ((*gensym-counter* 1))
                       (%generate-client 'the-class spec :export-symbols nil))

                   (compare client-class
                            test-name
                            "client-class")
                   (compare class-definitions
                            test-name
                            "class-definitions")
                   (compare methods
                            test-name
                            "methods")))))))

(generate-client
 test-client
 (asdf:system-relative-pathname
  :openrpc-tests
  (relative-path "multiple-types"
		 "spec"
		 :type "json")))

(deftest describe-object
  (rove:testing "describe-object test-client"
    (let ((result
            (let ((s (make-string-output-stream)))
	      (describe-object (make-test-client) s)
              (get-output-stream-string s)))
          (expected-string
            "Supported RPC methods:

- (EXAMPLE ((NAME NULL)))
- (EXAMPLE ((NAME STRING)))
- (EXAMPLE2 ((PARAM1 (EQL T)) &KEY (PARAM-X NIL PARAM-X-GIVEN-P)))
- (EXAMPLE2 ((PARAM1 (EQL YASON:FALSE)) &KEY (PARAM-X NIL PARAM-X-GIVEN-P)))
- (EXAMPLE3 ((PARAM3 LIST)))
"))
      (rove:ok (string-equal
                result
                expected-string)))))
