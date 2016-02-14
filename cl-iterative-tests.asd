(asdf:defsystem :cl-iterative-tests
  :description "Test suite for CL-ITERATIVE generic iteration algorithm suite"
  :author "Alexey Cherkaev (mobiuseng)"
  :licence "GPLv3"
  :version "0.1.0"
  :depends-on (:cl-iterative :fiveam)
  :components ((:file "cl-iterative-tests.lisp")))
