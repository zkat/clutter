;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:fexpr-clutter (:use :cl :alexandria))
(in-package #:fexpr-clutter)

(defun clutter-eval (expression environment)
  (cond ((symbolp expression) (eval/symbol expression environment))
        ((consp expression) (eval/combiner expression environment))
        (t expression)))

(defun eval/symbol (symbol env)
  (lookup symbol env))

(defun eval/combiner (expression env)
  (let ((f (clutter-eval (car expression) env)))
    (if (clutter-operator-p f)
        (invoke f env (cdr expression))
        (error "Not an operator: ~A." f))))

(defun lookup (symbol env)
  (if (consp env)
      (if (eq (caar env) symbol)
          (cdar env)
          (lookup symbol (cdr env)))
      (error "No binding for ~A." symbol)))

(defun (setf lookup) (new-value symbol env)
  (if (consp env)
      (if (eq (caar env) symbol)
          (setf (cdr (car env)) new-value)
          (setf (lookup symbol (cdr env)) new-value))
      (error "No binding for ~A." symbol)))

(defun extend (env variables values)
  (cond ((consp variables)
         (if (consp values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "Not enough values.")))
        ((null variables)
         (if (null values)
             env
             (error "Too many values.")))
        ((symbolp variables)
         (cons (cons variables values) env))))

(defvar *denv* nil)
(defstruct clutter-operator function)
(defun make-operator (variables body env)
  (make-clutter-operator 
   :function
   (lambda (*denv* values)
     (let ((env (extend env variables values)))
       (loop for sexp in body
          for last-value = (clutter-eval sexp env)
          finally (return last-value))))))

(defun get-current-env () *denv*)

(defun invoke (operator env args)
  (if (clutter-operator-p operator)
      (funcall (clutter-operator-function operator) env args)
      (error "Not a function: ~A." operator)))

(defparameter *initial-env*
  (list (cons '+ (make-clutter-operator
                  :function (lambda (*denv* values)
                              (reduce #'+ (mapcar (rcurry #'clutter-eval *denv*)
                                                  values)))))
        (cons 'car (make-clutter-operator
                    :function (lambda (*denv* values)
                                (car (clutter-eval (car values) *denv*)))))
        (cons 'eval (make-clutter-operator
                     :function (lambda (*denv* values)
                                 (let ((values (mapcar (rcurry #'clutter-eval *denv*)
                                                       values)))
                                   (clutter-eval (car values) (cadr values))))))
        (cons 'get-current-env
              (make-clutter-operator
               :function (lambda (*denv* values)
                           (declare (ignore values))
                           *denv*)))
        (cons 'vau
              (make-clutter-operator
               :function (lambda (static-env values)
                           (destructuring-bind (env-var lambda-var &rest body)
                               values
                             (make-clutter-operator
                              :function
                              (lambda (*denv* values)
                                (let ((env (extend static-env
                                                   (list env-var lambda-var)
                                                   (list *denv* values))))
                                  (loop for sexp in body
                                     for last-value = (clutter-eval sexp env)
                                     finally (return last-value)))))))))))
