;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Functions
;;;

(defstruct (clutter-function (:conc-name #:%function-)) function)

(defun make-function (variables body env fenv)
  (make-clutter-function
   :function (lambda (values)
               (eval-do body (extend env variables values) fenv))))

(defun invoke (function args)
  (if (clutter-function-p function)
      (funcall (%function-function function) args)
      (error "Not a function: ~S" function)))
