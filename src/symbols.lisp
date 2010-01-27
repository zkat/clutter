;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defstruct clutter-symbol
  "A Clutter symbol in the interpreted lisp"
  name)

(defvar *clutter-symbols* (make-hash-table :test #'equal)
  "Flat namespace of all clutter symbols")

(defun clutter-intern (name)
  (check-type name string)
  (setf (gethash name *clutter-symbols*)
        (make-clutter-symbol :name name)))

(defun ensure-clutter-symbol (name)
  (check-type name string)
  (or (gethash name *clutter-symbols*)
      (clutter-intern name)))
