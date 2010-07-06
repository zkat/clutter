;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Environments
;;;

(defstruct (stack-frame (:constructor make-stack-frame (name &optional scope)))
  (name "anonymous")
  (scope '())
  (functions (make-hash-table :test 'eq))
  (lexicals (make-hash-table :test 'eq))
  (dynamics (make-hash-table :test 'eq))
  (namespaces (make-hash-table :test 'eq)))

(defmethod print-object ((o stack-frame) s)
  (print-unreadable-object (o s :type t :identity t)
    (princ (stack-frame-name o) s)))

(defvar *stack* (list (make-stack-frame "global")))

(defun bind (symbol value env &key global (overwritep t)
             &aux (table (if global
                             (car (last (current-env env)))
                             (car (current-env env)))))
  (multiple-value-bind (old-value exists)
      (gethash symbol table)
    (declare (ignore old-value))
    (when (and exists (not overwritep))
      (error "~A is already bound." symbol)))
  (setf (gethash symbol table) value))

(defun unbind (symbol env &key global)
  (remhash symbol (if global
                      (car (last (current-env env)))
                      (car (current-env env)))))

(defmacro with-frame (frame &body body)
  `(progn
     (unwind-protect
          (progn
            (push ,frame *stack*)
            ,@body)
       (pop *stack*))))

(defun push-initial-binding (name value)
  (bind name value :lexical))

(defun push-initial-function-binding (name value)
  (bind name value :function))

(defun current-scope ()
  (list* (car *stack*) (append (stack-frame-scope (car *stack*))
                               (when (cdr *stack*)
                                 (list (car (last *stack*)))))))

(defun current-env (env)
  (mapcar (case env
            (:function 'stack-frame-functions)
            (:lexical 'stack-frame-lexicals)
            (:dynamic 'stack-frame-dynamics)
            (:namespace 'stack-frame-namespaces)
            (t (error "Not an environment: ~S" env)))
          (current-scope)))

(defun lookup (symbol env)
  (or (some (lambda (table) (gethash symbol table)) (current-env env))
      (error "No such ~A binding: ~S" (string-downcase (symbol-name env)) symbol)))

(defun (setf lookup) (new-value symbol env &aux (tables (current-env env)))
  (some (lambda (table)
          (multiple-value-bind (old-value exists)
              (gethash symbol table)
            (declare (ignore old-value))
            (if exists
                (setf (gethash symbol table) new-value)
                (error "~A is unbound." symbol))))
        tables))
