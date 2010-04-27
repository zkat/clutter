;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; REPL
;;;

(defun repl ()
  (loop
     (format t "~&> ")
     (with-simple-restart (abort "Return to Clutter's toplevel")
       (format t "~&=> ~S" (evaluate (clutter-read) *lexical-bindings* *function-bindings*)))))
