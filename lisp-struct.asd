(defsystem "lisp-struct"
  :description "A library for packing/unpacking structured data."
  :version "0.1.0"
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:parseq)
  :serial t
  :components ((:file "package")
               (:file "lisp-struct"))
  :in-order-to ((test-op (test-op :lisp-struct/test))))

(defsystem "lisp-struct/test"
  :description "Unit testing for lisp-struct."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:lisp-struct :fiveam)
  :serial t
  :components ((:file "test/test")))

(defmethod perform ((operation test-op) (system (eql (find-system :lisp-struct/test))))
  (funcall (intern "LISP-STRUCT-TEST" :lisp-struct)))
