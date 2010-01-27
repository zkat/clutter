;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(cl:in-package :clutter)

;;; Load the Clutter source files, one by one.

(defvar *source-files*
  ()
  "List of source files to load")

(let ((*load-print*   t)
      (*load-verbose* t))
  (dolist (file *source-files*)
    (load file)))
