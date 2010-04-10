;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(defparameter *global-env* '())
(defparameter *global-fenv* '())

(defun push-initial-binding (name value)
  (push (cons name value) *global-env*))

(defun push-initial-function-binding (name value)
  (push (cons name value) *global-fenv*))

(defun find-binding (id env)
  (or (assoc id env)
      (error "No such binding: ~S" id)))

(defun lookup (id env)
  (cdr (find-binding id env)))

(defun (setf lookup) (new-value id env)
  (setf (cdr (find-binding id env)) new-value))

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
