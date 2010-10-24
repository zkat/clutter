;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Combiners
;;;

(defstruct clutter-operator function name)
(defun make-operator (variables body env)
  (make-clutter-operator 
   :function
   (lambda (*denv* &rest values)
     (let ((env (make-child-env env variables values)))
       (loop for sexp in body
          for last-value = (clutter-eval sexp env)
          finally (return last-value))))))

(defstruct clutter-function operator)
(defun make-function (operator)
  (make-clutter-function :operator operator))

(defstruct clutter-symbol-operator operator)
(defun make-symbol-operator (operator)
  (make-clutter-symbol-operator :operator operator))

(defun combiner-name (combiner)
  (cond
    ((clutter-operator-p combiner) (clutter-operator-name combiner))
    ((clutter-function-p combiner) (combiner-name (clutter-function-operator combiner)))
    ((clutter-symbol-operator-p combiner) (combiner-name (clutter-symbol-operator-operator combiner)))))

(defmethod print-object ((o clutter-operator) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defmethod print-object ((o clutter-function) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defmethod print-object ((o clutter-symbol-operator) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defun invoke (operator env args)
  (if (clutter-operator-p operator)
      (apply (clutter-operator-function operator) env args)
      (error "Not a function: ~A." operator)))


