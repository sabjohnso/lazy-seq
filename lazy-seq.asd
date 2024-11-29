(in-package :cl-user)

(defpackage :lazy-seq-system
  (:use :cl :asdf :uiop))

(in-package :lazy-seq-system)

(defsystem :lazy-seq
  :description "Lazy sequences"
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "lazy-seq")
     (:file "constants"))))
  :in-order-to ((test-op
                 (load-op :lazy-seq)
                 (test-op :lazy-seq/test))))

(defsystem :lazy-seq/test
  :description "Tests for the `LAZY-SEQ' system"
  :depends-on (:fiveam :lazy-seq)
  :components
  ((:module "test"
    :serial nil
    :components
    ((:file "lazy-seq-test"))))
  :perform (test-op (op sys)
            (symbol-call :lazy-seq-test :run-all-tests)))
