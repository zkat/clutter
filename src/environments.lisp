;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(declaim (optimize debug))

(defvar *mandatory-subenvs* (list (cs "var") (cs "fun")))

(defun mandatory-subenv-table ()
  (alist-hash-table
   (mapcar (lambda (subenv) (cons subenv (make-hash-table :test 'eq)))
           *mandatory-subenvs*)
   :test 'eq))

(defstruct (env (:constructor %make-env))
  parent
  (subenvs (mandatory-subenv-table)))

(defvar *global-env*
  (%make-env :parent nil))

(defun make-env (&optional (parent *global-env*))
  "Creates a child environment to PARENT.  Child environments have at least the subenvs of their parents."
  (%make-env :parent parent
             :subenvs (aprog1 (mandatory-subenv-table)
                        (maphash (lambda (name bindings)
                                   (declare (ignore bindings))
                                   (setf (gethash name it)
                                         (make-hash-table :test 'eq)))
                                 it))))

(defvar *denv* nil)

(defun get-current-env () *denv*)

(defun lookup (subenv symbol &optional (env *global-env*))
  (if (and env (nth-value 1 (gethash subenv (env-subenvs env))))
      (multiple-value-bind (value exists)
          (gethash symbol (gethash subenv (env-subenvs env)))
        (if exists
            value
            (lookup subenv symbol (env-parent env))))
      (error "No binding for ~A ~A." subenv symbol)))

(defun (setf lookup) (new-value subenv symbol &optional (env *global-env*))
  (if (and env (nth-value 1 (gethash subenv (env-subenvs env))))
      (if (nth-value 1 (gethash symbol (gethash subenv (env-subenvs env))))
          (setf (gethash symbol (gethash subenv (env-subenvs env))) new-value)
          (setf (lookup subenv symbol (env-parent env)) new-value))
      (error "No binding for ~A ~A." subenv symbol)))

(defun clutter-bound? (subenv symbol &optional (env *global-env*))
  (if (and env (nth-value 1 (gethash subenv (env-subenvs env))))
      (if (nth-value 1 (gethash symbol (gethash subenv (env-subenvs env))))
          t
          (clutter-bound? symbol (env-parent env)))
      nil))

(defun extend (env subenv symbol value)
  (if (nth-value 1 (gethash subenv (env-subenvs env)))
      (progn
       (if (nth-value 1 (gethash symbol (gethash subenv (env-subenvs env))))
           (warn "Redefinition of ~A." symbol))
       (setf (gethash symbol (gethash subenv (env-subenvs env))) value))
      (error "Subenv ~A does not exist." subenv)))

(defun add-subenv (env subenv)
  (if (nth-value 1 (gethash subenv (env-subenvs env)))
      (warn "Redefinition of subenv ~A in ~A" subenv env)
      (setf (gethash subenv (env-subenvs env)) (make-hash-table :test 'eq))))
