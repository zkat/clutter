;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(declaim (optimize debug))

(defstruct (env (:constructor make-env (parent)))
  parent
  (bindings (make-hash-table :test 'eq)))

(defmethod print-object ((o env) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A binding~:P" (hash-table-count (env-bindings o)))))

(defvar *global-env*
  (make-env nil))

(defvar *denv* nil)

(defun get-current-env () *denv*)

(defun lookup (symbol &optional (env *global-env*))
  (if env
      (multiple-value-bind (value exists)
          (gethash symbol (env-bindings env))
        (if exists
            value
            (lookup symbol (env-parent env))))
      (error "No binding for ~A in or above ~A" symbol env)))

(defun (setf lookup) (new-value symbol &optional (env *global-env*))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          (setf (gethash symbol (env-bindings env)) new-value)
          (setf (lookup symbol (env-parent env)) new-value))
      (error "No binding for ~A in or above ~A" symbol env)))

(defun clutter-bound? (symbol &optional (env *global-env*))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          t
          (clutter-bound? symbol (env-parent env)))
      nil))

(defun extend (env symbol &optional value)
  (if (nth-value 1 (gethash symbol (env-bindings env)))
      (warn "Redefinition of ~A." symbol))
  (setf (gethash symbol (env-bindings env)) value))
