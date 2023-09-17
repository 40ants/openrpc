(uiop:define-package #:openrpc-tests/client-generation
  (:use #:cl)
  (:import-from #:openrpc-client/core
                #:expand-list)
  (:import-from #:rove
                #:ok
                #:deftest))
(in-package #:openrpc-tests/client-generation)

(deftest test-expand-list ()
  (ok (equal (expand-list '((first integer)
                            (second string)
                            (alpha string)))
             '(((first integer) (second string) (alpha string)))))
  (ok (equal (expand-list '((a list)
                            (b integer string)
                            (c string)))
             '(((a list) (b integer) (c string))
               ((a list) (b string) (c string)))))
  (ok (equal (expand-list
              '((beta integer)
                (delta number string)
                (alpha string)
                (epsylon number string)))
             '(((beta integer) (delta number) (alpha string) (epsylon number))
               ((beta integer) (delta string) (alpha string) (epsylon number))
               ((beta integer) (delta number) (alpha string) (epsylon string))
               ((beta integer) (delta string) (alpha string) (epsylon string)))))
  (ok (equal (expand-list
              '((beta integer)
                (delta number string integer)
                (alpha string)
                (epsylon number string)))
             '(((beta integer) (delta number)  (alpha string) (epsylon number))
               ((beta integer) (delta string)  (alpha string) (epsylon number))
               ((beta integer) (delta integer) (alpha string) (epsylon number))
               ((beta integer) (delta number)  (alpha string) (epsylon string))
               ((beta integer) (delta string)  (alpha string) (epsylon string))
               ((beta integer) (delta integer) (alpha string) (epsylon string))))))
