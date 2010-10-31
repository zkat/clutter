;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(declaim (optimize debug))

(defstruct (env (:constructor make-env (&rest parents)))
  parents
  (bindings (make-hash-table :test 'eq)))

(defmethod print-object ((o env) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A binding~:P" (hash-table-count (env-bindings o)))))

(defvar *global-env*
  (make-env))

(defvar *denv* nil)

(defun get-current-env () *denv*)

(defun lookup (symbol &optional (env *global-env*))
  (if env
      (multiple-value-bind (value exists)
          (gethash symbol (env-bindings env))
        (if exists
            value
            (aif (find-if (curry #'clutter-bound? symbol) (env-parents env))
                 (lookup symbol it)
                 (error "No binding for ~A in or above ~A" symbol env))))
      (error "No binding for ~A in or above ~A" symbol env)))

(defun (setf lookup) (new-value symbol &optional (env *global-env*))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          (setf (gethash symbol (env-bindings env)) new-value)
          (aif (find-if (curry #'clutter-bound? symbol) (env-parents env))
               (setf (lookup symbol it) new-value)
               (error "No binding for ~A in or above ~A" symbol env)))
      (error "No binding for ~A in or above ~A" symbol env)))

(defun clutter-bound? (symbol &optional (env *global-env*))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          t
          (when (find-if (curry #'clutter-bound? symbol) (env-parents env))
            t))
      nil))

(defun extend (env symbol &optional value)
  (if (nth-value 1 (gethash symbol (env-bindings env)))
      (warn "Redefinition of ~A." symbol))
  (setf (gethash symbol (env-bindings env)) value))
