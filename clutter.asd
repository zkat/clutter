;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem clutter
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "cl-package")
             (:file "utilities")
             (:file "environments")
             (:file "reader")
             (:file "functions")
             (:file "eval")
             (:file "repl")
             (:file "primitives")))))
