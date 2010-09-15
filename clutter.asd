;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem clutter
  :depends-on (#:cllvm #:cffi #:alexandria)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "cl-package")
             (:file "utilities")
             (:file "boehm-gc")
             (:file "type")
             (:file "compile")))))
