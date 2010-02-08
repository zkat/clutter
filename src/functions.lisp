;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Functions
;;;

(defun invoke (function args)
  (if (functionp function)
      (funcall function args)
      (error "Not a function: ~S" function)))

(defun make-function (variables body env fenv)
  (lambda (values)
    (eval-do body (extend env variables values) fenv)))
