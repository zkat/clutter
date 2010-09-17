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
  (namespaces (make-hash-table :test 'eq))
  (blocks (make-hash-table :test 'eq)))

(defmethod print-object ((o stack-frame) s)
  (print-unreadable-object (o s :type t :identity t)
    (princ (stack-frame-name o) s)))

(defvar *stack* (list (make-stack-frame "global")))

(defun bind (symbol value env &key global (overwritep t)
             &aux (table (if global
                             (car (last (current-env env)))
                             (car (current-env env)))))
  (when (and (nth-value 1 (gethash symbol table))
             (not overwritep))
    (error "~A is already bound." symbol))
  (setf (gethash symbol table) value))

(defun unbind (symbol env)
  (remhash symbol (lookup-binding-table symbol env)))

(defun clutter-boundp (symbol env &aux (table (lookup-binding-table symbol env)))
  (when table
    (gethash symbol table)))

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
            (:block 'stack-frame-blocks)
            (t (error "Not an environment: ~S" env)))
          (current-scope)))

(defun lookup (symbol env)
  (or (some (lambda (table) (gethash symbol table)) (current-env env))
      (error "No such ~A binding: ~S" (string-downcase (symbol-name env)) symbol)))

(defun lookup-binding-table (symbol env)
  "Find the binding table in which the given symbol is defined, if any."
  (some (lambda (table)
          (when (nth-value 1 (gethash symbol table))
            table))
        (current-env env)))

(defun (setf lookup) (new-value symbol env &aux (tables (current-env env)))
  (or (when (some (lambda (table)
                    (when (nth-value 1 (gethash symbol table))
                      (setf (gethash symbol table) new-value)
                      t))
                  tables)
        new-value)
      (error "~A is unbound." symbol)))
