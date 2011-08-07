;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(declaim (optimize debug))

(defstruct (env (:constructor make-env (&rest parents)))
  parents
  (bindings (make-hash-table :test 'eq))
  (recompile-triggers (make-hash-table :test 'eq)))

(defmethod print-object ((o env) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A binding~:P" (hash-table-count (env-bindings o)))))

(defvar *global-env*
  (make-env))

(defvar *denv* nil)

(defvar *ignore* (clutter-symbol "#ignore"))

(defun get-current-env () *denv*)

(defun lookup (symbol &optional (env *global-env*))
  "Returns the value of the symbol's binding, whether recompilation must occur if the binding is modified, and the environment in which it is bound"
  (when (eq symbol *ignore*)
    (error "Attempted to lookup #ignore in ~A" env))
  (if env
      (multiple-value-bind (value exists)
          (gethash symbol (env-bindings env))
        (if exists
            (values value (gethash symbol (env-recompile-triggers env)) env)
            (aif (find-if (curry #'clutter-bound? symbol) (env-parents env))
                 (lookup symbol it)
                 (error "No binding for ~A in or above ~A" symbol env))))
      (error "No binding for ~A in or above ~A" symbol env)))

(defun triggers-recompile? (symbol &optional (env *global-env*))
  (gethash symbol env))

(defun (setf triggers-recompile?) (value symbol &optional (env *global-env*))
  (setf (gethash symbol (env-recompile-triggers env))
        value))

(defun binding-env (symbol &optional (lookup-env *global-env*))
  (when (eq symbol *ignore*)
    (error "Attempted to find binding env of #ignore in ~A" lookup-env))
  (if lookup-env
      (if (nth-value 1 (gethash symbol (env-bindings lookup-env)))
          lookup-env
          (loop for parent in (env-parents lookup-env)
                for result = (binding-env symbol parent)
                when result
                  return result))
      nil))

(defun (setf lookup) (new-value symbol &optional (env *global-env*))
  (when (eq symbol *ignore*)
    (error "Attempted to set #ignore in ~A" env))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          (if-let (triggers (gethash symbol (env-recompile-triggers env)))
            (error "Tried to assign to recompile-triggering binding ~A in ~A.  Recompilation is unimplemented." symbol env)
            (setf (gethash symbol (env-bindings env)) new-value))
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

(defun extend (env symbol value &optional (recompile-triggers nil recompile-triggers-p))
  (assert (clutter-symbol-p symbol))
  (unless (eq symbol *ignore*)
    (when (nth-value 1 (gethash symbol (env-bindings env)))
      (warn "Redefinition of ~A." symbol))
    (setf (gethash symbol (env-bindings env)) value)
    (when recompile-triggers-p
      (setf (gethash symbol (env-recompile-triggers env)) recompile-triggers))))

(defun mapenv (func env)
  (maphash func (env-bindings env)))

(defun env-child? (a b &aux (parents (env-parents b)))
  (cond
    ((null parents) nil)
    ((some (curry #'eq a) parents) t)
    (t (some (curry #'env-child? a) parents))))
