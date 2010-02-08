;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem clutter
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "cl-package")
             (:file "environments")
             (:file "primitives")
             (:file "eval")
             (:file "repl")))))

