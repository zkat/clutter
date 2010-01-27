;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl)

(defpackage #:clutter
  (:use "CL")
  (:documentation "Clutter interpreter"))

(in-package #:clutter)

(defvar *source-directory*
  (pathname-directory (load-time-value (or #.*compile-file-truename* *load-truename*)))
  "The top directory of the Clutter source tree")

(load (make-pathname :directory *source-directory*
                     :name "load-clutter"
                     :type "lisp"))
