;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem clutter
  :depends-on (#:cllvm)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "cl-package")
             (:file "utilities")
             (:file "compile")))))
