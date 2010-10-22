;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:fexpr-clutter (:use :cl :alexandria :anaphora))
(in-package #:fexpr-clutter)

(defstruct env
  parent bindings)

(defun clutter-eval (expression &optional (environment *global-env*))
  (cond ((symbolp expression) (eval/symbol expression environment))
        ((consp expression) (eval/combiner expression environment))
        (t expression)))

(defun eval/symbol (symbol env)
  (lookup symbol env))

(defun eval/combiner (expression env)
  (let ((f (clutter-eval (car expression) env)))
    (cond ((clutter-operator-p f)
           (invoke f env (cdr expression)))
          ((clutter-function-p f)
           (let ((op (clutter-function-operator f))
                 (values (mapcar (rcurry #'clutter-eval env) (cdr expression))))
             (clutter-eval (cons op values) env)))
          (t
           (error "Not an operator: ~A." f)))))

(defun lookup (symbol &optional (env *global-env*))
  (if env
      (or (gethash symbol (env-bindings env))
          (lookup symbol (env-parent env)))
      (error "No binding for ~A." symbol)))

(defun (setf lookup) (new-value symbol env)
  (if env
      (if (gethash symbol (env-bindings env))
          (setf (gethash symbol (env-bindings env)) new-value)
          (setf (lookup symbol (env-parent env)) new-value))
      (error "No binding for ~A." symbol)))

(defun extend (env symbol value)
  (if (nth-value 1 (gethash symbol (env-bindings env)))
      (warn "Redefinition of ~A." symbol))
  (setf (gethash symbol (env-bindings env)) value))

(defun make-child-env (env variables values)
  (make-env :parent env
            :bindings (aprog1 (make-hash-table :test 'eq)
                        (mapc (lambda (name value)
                                (setf (gethash name it) value))
                              variables values))))

(defvar *denv* nil)
(defstruct clutter-operator function)
(defun make-operator (variables body env)
  (make-clutter-operator 
   :function
   (lambda (*denv* values)
     (let ((env (make-child-env env variables values)))
       (loop for sexp in body
          for last-value = (clutter-eval sexp env)
          finally (return last-value))))))

(defstruct clutter-function operator)
(defun make-function (operator)
  (make-clutter-function :operator operator))

(defun get-current-env () *denv*)

(defun invoke (operator env args)
  (if (clutter-operator-p operator)
      (funcall (clutter-operator-function operator) env args)
      (error "Not a function: ~A." operator)))

(defparameter *global-env*
  (make-env
   :parent nil
   :bindings
   (alist-hash-table 
    (list (cons '+
                (make-function
                 (make-clutter-operator
                  :function (lambda (*denv* values)
                              (reduce #'+ values)))))
          (cons 'car
                (make-function
                 (make-clutter-operator
                  :function (lambda (*denv* values)
                              (caar values)))))
          (cons 'cdr
                (make-function
                 (make-clutter-operator
                  :function (lambda (*denv* values)
                              (cadr values)))))
          (cons 'list (make-function
                       (make-clutter-operator
                        :function (lambda (*denv* values)
                                    values))))
          (cons 'list* (make-function
                        (make-clutter-operator
                         :function (lambda (*denv* values)
                                     (apply #'list* values)))))
          (cons 'eval (make-clutter-operator
                       :function (lambda (*denv* values)
                                   (let ((values (mapcar (rcurry #'clutter-eval *denv*)
                                                         values)))
                                     (clutter-eval (first values) (second values))))))
          (cons 'vau
                (make-clutter-operator
                 :function (lambda (static-env values)
                             (destructuring-bind (env-var lambda-var &rest body)
                                 values
                               (make-clutter-operator
                                :function
                                (lambda (*denv* values)
                                  (let ((env (make-child-env static-env
                                                     (list env-var lambda-var)
                                                     (list *denv* values))))
                                    (loop for sexp in body
                                          for last-value = (clutter-eval sexp env)
                                          finally (return last-value)))))))))
          (cons 'wrap
                (make-function
                 (make-clutter-operator
                  :function (lambda (*denv* values)
                              (make-function (car values))))))
          (cons 'unwrap
                (make-function
                 (make-clutter-operator
                  :function (lambda (*denv* values)
                              (clutter-function-operator (car values))))))
          (cons 'def!
                (make-clutter-operator
                 :function (lambda (*denv* values)
                             (destructuring-bind (var value)
                                 values
                               (extend *denv* var (clutter-eval value *denv*))
                               var))))
          (cons 'set!
                (make-clutter-operator
                 :function (lambda (*denv* values)
                             (destructuring-bind (var value)
                                 values
                               (setf (lookup var *denv*) value)))))
          (cons 'if
                (make-clutter-operator
                 :function (lambda (*denv* values)
                             (destructuring-bind (test if-true if-false)
                                 values
                               (if (clutter-true-p (clutter-eval test *denv*))
                                   (clutter-eval if-true *denv*)
                                   (clutter-eval if-false *denv*)))))))
    :test 'eq)))

(defun clutter-true-p (exp)
  (if exp t nil))