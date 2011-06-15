;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem clutter
  :serial t
  :depends-on (#:llvm #:alexandria #:anaphora #:split-sequence)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "cl-package")
             (:file "utilities")
             (:file "reader")
             (:file "environments")
             (:file "combiners")
             (:file "eval")
             (:file "primitives")
             (:file "main")))))
