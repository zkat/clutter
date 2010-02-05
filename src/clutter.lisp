(cl:defpackage #:clutter (:use :cl))
(cl:in-package :clutter)

(defparameter *false* 'f)
(defparameter *initial-env* ())

(defun eprogn (exps env)
  (when exps
    (evaluate (car exps) env)
    (when (cdr exps)
      (eprogn (cdr exps) env))))

(defun eval-list (list env)
  (mapcar (lambda (exp) (evaluate exp env)) list))

(defun find-binding (id env)
  (or (assoc id env)
      (error "No such binding: ~S" id)))

(defun lookup (id env)
  (cdr (find-binding id env)))

(defun (setf lookup) (new-value id env)
  (setf (cdr (find-binding id env)) new-value))

(defun extend (env variables values)
  (cond ((consp variables)
         (if values
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (error "Not enough values")))
        ((null variables)
         (if values
             (error "Too many values")
             env))
        ((symbolp variables) (cons (cons variables values) env))))

(defun invoke (function args)
  (if (functionp function)
      (funcall function args)
      (error "Not a function: ~S" function)))

(defun make-function (variables body env)
  (lambda (values)
    (eprogn body (extend *initial-env* variables values))))

(defun evaluate (exp env)
  (if (atom exp)
      (typecase exp
        ((or number string character boolean vector) exp)
        (symbol (lookup exp env))
        (otherwise (error "Cannot evaluate: ~S" exp)))
      (case (car exp)
        (quote (cadr exp))
        (if (if (not (eq (evaluate (cadr exp) env) *false*))
                (evaluate (caddr exp) env)
                (evaluate (cadddr exp) env)))
        (do (eprogn (cdr exp) env))
        (set! (setf (lookup (cadr exp) env)
                    (evaluate (caddr exp) env)))
        (lambda (make-function (cadr exp) (cddr exp) env))
        (otherwise (invoke (evaluate (car exp) env)
                           (eval-list (cdr exp) env))))))
