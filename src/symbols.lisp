;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defstruct clutter-symbol
  "A Clutter symbol in the interpreted lisp"
  name value)

(defvar *clutter-symbols* (make-hash-table :test #'equal)
  "Flat namespace of all clutter symbols")

(defun ensure-clutter-symbol (name)
  (check-type name string)
  (or (gethash name *clutter-symbols*)
      (setf (gethash name *clutter-symbols*)
            (make-clutter-symbol :name name))))