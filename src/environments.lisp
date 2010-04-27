;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(defparameter *global-env* '())
(defparameter *global-fenv* '())

(defvar *function-bindings* (make-hash-table :test 'eq))
(defvar *lexical-bindings* (make-hash-table :test 'eq))
(defvar *dynamic-bindings* (make-hash-table :test 'eq))

(defun push-initial-binding (name value)
  (setf (lookup name *lexical-bindings*) value))

(defun push-initial-function-binding (name value)
  (setf (lookup name *function-bindings*) value))

(defun lookup (symbol env)
  (or (gethash symbol env)
      (error "No such binding: ~S" symbol)))

(defun (setf lookup) (new-value symbol env)
  (setf (gethash symbol env) new-value))

(defun extend (env variables values)
  (cond ((consp variables)
         (if values
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "Not enough values")))
        ((null variables)
         (if values
             (error "Too many values")
             env))
        ((clutter-symbol-p variables) (cons (cons variables values) env))))

;;;
;;; Primitive Constants
;;;

(defparameter *true* (clutter-intern "t"))
(defparameter *false* (clutter-intern "f"))

(defun define-initially (clutter-symbol-name value)
  (push-initial-binding (clutter-intern clutter-symbol-name) value))
(define-initially "t" *true*)
(define-initially "f" *false*)
(define-initially "nil" nil)
