;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(declaim (optimize debug))

(defclass env ()
  ((parents :accessor env-parents :initarg :parents)
   (bindings :accessor env-bindings :initarg :bindings :initform (make-hash-table :test 'eq))))

(defgeneric lookup (symbol env))
(defgeneric (setf lookup) (new-value symbol env))
(defgeneric extend (symbol env &optional value))

(defun make-env (&rest parents)
  (make-instance 'env :parents parents))

(defmethod print-object ((o env) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A binding~:P" (hash-table-count (env-bindings o)))))

(defvar *global-env*
  (make-env))

(defvar *denv* nil)

(defvar *ignore* (clutter-symbol "#ignore"))

(defun get-current-env () *denv*)

(defmethod lookup (symbol (env env))
  (when (eq symbol *ignore*)
    (error "Attempted to lookup #ignore in ~A" env))
  (if env
      (multiple-value-bind (value exists)
          (gethash symbol (env-bindings env))
        (if exists
            value
            (aif (find-if (curry #'clutter-bound? symbol) (env-parents env))
                 (lookup symbol it)
                 (error "No binding for ~A in or above ~A" symbol env))))
      (error "No binding for ~A in or above ~A" symbol env)))

(defmethod (setf lookup) (new-value symbol (env env))
  (when (eq symbol *ignore*)
    (error "Attempted to set #ignore in ~A" env))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          (setf (gethash symbol (env-bindings env)) new-value)
          (aif (find-if (curry #'clutter-bound? symbol) (env-parents env))
               (setf (lookup symbol it) new-value)
               (error "No binding for ~A in or above ~A" symbol env)))
      (error "No binding for ~A in or above ~A" symbol env)))

(defmethod clutter-bound? (symbol (env env))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
          t
          (when (find-if (curry #'clutter-bound? symbol) (env-parents env))
            t))
      nil))

(defmethod extend ((env env) symbol &optional value)
  (assert (clutter-symbol-p symbol))
  (unless (eq symbol *ignore*)
    (when (nth-value 1 (gethash symbol (env-bindings env)))
      (warn "Redefinition of ~A." symbol))
    (setf (gethash symbol (env-bindings env)) value)))

(defun mapenv (func env)
  (maphash func (env-bindings env)))
