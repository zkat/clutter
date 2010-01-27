;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(macrolet ((import-functions (&rest names)
             `(progn ,@(mapcar (lambda (n) `(import-cl-function ,n)) names))))
  (import-functions "cons" "car" "cdr" "list"))

(import-cl-function "cons?" "consp")
(import-cl-function "atom?" "atom")
(import-cl-function "null?" "null")
(import-cl-function "list?" "listp")
