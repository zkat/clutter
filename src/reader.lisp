;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defvar *clutter-readtable*
  (let ((table (copy-readtable)))
    (setf (readtable-case table) :preserve)
    table))

(defun clutter.symbols::read (stream)
  (let ((*package* (find-package :clutter.symbols))
        (*readtable* *clutter-readtable*))
    (read stream)))
