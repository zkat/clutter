(asdf:defsystem clutter.test
  :version "0"
  :description "Test cases for Clutter"
  :maintainer "Kat Marchán <zkat@Dagon>"
  :author "Kat Marchán <zkat@Dagon>"
  :licence "MIT"
  :depends-on (clutter eos)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "test")
             (:file "eval")
             (:file "reader")))))
