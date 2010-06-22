;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem clutter
  :components
  ((:module "src"
            :components
            ((:file "cl-package")
             (:file "utilities")
             (:file "reader")))))
