;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem clutter
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "symbols")
             (:file "conses")
             (:file "reader")))))
