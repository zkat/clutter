;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Combiners
;;;

(defstruct clutter-operative function name args body denv-var (env *global-env*) (pure t))
(defun make-operative (variables body env &optional name)
  (make-clutter-operative
   :function
   (lambda (*denv* &rest values)
     (let ((env (make-env env)))
       (loop for var in variables
             for value in values
             do (extend env var value))
       (loop for sexp in body
          for last-value = (clutter-eval sexp env)
          finally (return last-value))))
   :name name
   :args variables
   :env env
   :body body))

(defstruct clutter-function operative)
(defun make-function (operative)
  (make-clutter-function :operative operative))

(defstruct clutter-symbol-operative operative)
(defun make-symbol-operative (operative)
  (make-clutter-symbol-operative :operative operative))

(defun combiner-name (combiner)
  (cond
    ((clutter-operative-p combiner) (clutter-operative-name combiner))
    ((clutter-function-p combiner) (combiner-name (clutter-function-operative combiner)))
    ((clutter-symbol-operative-p combiner) (combiner-name (clutter-symbol-operative-operative combiner)))))

(defmethod print-object ((o clutter-operative) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defmethod print-object ((o clutter-function) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defmethod print-object ((o clutter-symbol-operative) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defun invoke (operative env args)
  (if (clutter-operative-p operative)
      (apply (clutter-operative-function operative) env args)
      (error "Not a function: ~A." operative)))
