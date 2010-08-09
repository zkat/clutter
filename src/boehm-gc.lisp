(in-package #:clutter)

(cffi:define-foreign-library boehm-gc
  (t (:default "libgc")))

(cffi:use-foreign-library boehm-gc)

(cffi:defcfun (gc-init "GC_init") :void)
