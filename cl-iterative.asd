(asdf:defsystem #:cl-iterative
  :description "Generic iterative algorithm with multiple controls"
  :license "LGPLv3"
  :version "0.1.0"
  :depends-on (:alexandria :optima)
  :serial t
  :components ((:file "package")
               (:file "iterator")
               (:file "cl-iterative")
               (:file "iteratorex")
               (:file "cl-iterative-ex")))
