;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(defparameter *initial-env* '())
(defparameter *global-env* *initial-env*)
(defparameter *global-fenv* '())

(defun push-initial-binding (name value)
  (push (cons name value) *global-env*))

(defmacro definitial (name value)
  `(progn (push-initial-binding ',name ,value)
          ',name))

(defun push-initial-function-binding (name value)
  (push (cons name value) *global-fenv*))

(defmacro definitial-fun (name value)
  `(progn (push-initial-function-binding ',name ,value)
          ',name))

(defmacro defprimitive (name value arity)
  `(definitial-fun ,name (lambda (values)
                           (if (= ,arity (length values))
                               (apply ,value values)
                               (error "Incorrect arity.")))))

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
        ((symbolp variables) (cons (cons variables values) env))))

;;;
;;; Primitive Constants
;;;

(defparameter *true* '|t|)
(defparameter *false* '|f|)

(definitial |t| *true*)
(definitial |f| *false*)
(definitial |nil| '())
