;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:fexpr-clutter (:use :cl :alexandria :anaphora))
(in-package #:fexpr-clutter)

(declaim (optimize debug))

(defstruct env
  parent
  (bindings (make-hash-table :test 'eq)))

(defvar *global-env*
  (make-env :parent nil))

(defun clutter-eval (expression &optional (environment *global-env*))
  (cond ((symbolp expression) (eval/symbol expression environment))
        ((consp expression) (eval/combiner expression environment))
        (t expression)))

(defun eval/symbol (symbol env)
  (if (keywordp symbol)
      symbol
      (let ((val (lookup symbol env)))
        (if (clutter-symbol-operator-p val)
            (invoke (clutter-symbol-operator-operator val) env nil)
            val))))

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
      (multiple-value-bind (value exists)
          (gethash symbol (env-bindings env))
        (if exists
            value
            (lookup symbol (env-parent env))))
      (error "No binding for ~A." symbol)))

(defun (setf lookup) (new-value symbol &optional (env *global-env*))
  (if env
      (if (nth-value 1 (gethash symbol (env-bindings env)))
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

(defun get-current-env () *denv*)

(defun invoke (operator env args)
  (if (clutter-operator-p operator)
      (apply (clutter-operator-function operator) env args)
      (error "Not a function: ~A." operator)))

(defmacro defprimitive (name value)
  `(extend *global-env* ',name ,value))

(defmacro defprimop (name lambda-list &body body)
  `(defprimitive ,name
       (make-clutter-operator
        :name ',name
        :function (lambda ,lambda-list ,@body))))

(defmacro defprimfun (name lambda-list &body body)
  `(defprimitive ,name
       (make-function
        (make-clutter-operator
         :name ',name
         :function (lambda ,lambda-list ,@body)))))

(defprimop vau (static-env lambda-list env-var &rest body)
  (make-clutter-operator
   :function
   (lambda (*denv* &rest values)
     (multiple-value-bind (required optional rest)
         (parse-ordinary-lambda-list lambda-list)
       (declare (ignore optional))
       (unless (or (= (length values) (length lambda-list))
                   (and rest (>= (length values) (1- (length lambda-list))))
                   (error "Wrong number of arguments")))
       (let ((env (make-child-env static-env
                                  (list* env-var rest required)
                                  (list* *denv*
                                         (nthcdr (length required) values)
                                         (subseq values 0 (length required))))))
         (loop for sexp in body
               for last-value = (clutter-eval sexp env)
               finally (return last-value)))))))

(defprimfun wrap (*denv* operative)
  (make-function operative))

(defprimfun unwrap (*denv* function)
  (clutter-function-operator function))

(defprimfun eval (*denv* expression environment)
  (clutter-eval expression environment))

(defprimfun + (*denv* &rest values)
  (reduce #'+ values))

(defprimfun cons (*denv* car cdr)
  (cons car cdr))

(defprimfun car (*denv* cons)
  (car cons))

(defprimfun cdr (*denv* cons)
  (cdr cons))

(defprimfun list (*denv* &rest values)
  values)

(defprimfun list* (*denv* &rest values)
  (apply #'list* values))

(defprimfun random (*denv* max)
  (random max))

(defprimop def! (*denv* var value)
  (extend *denv* var (clutter-eval value *denv*))
  var)

(defprimop set-var! (*denv* var value)
  (setf (lookup var *denv*) (clutter-eval value *denv*)))

(defprimop if (*denv* test if-true if-false)
  (if (clutter-true-p (clutter-eval test *denv*))
      (clutter-eval if-true *denv*)
      (clutter-eval if-false *denv*)))

(defprimop symbolize! (*denv* var value)
  (let ((val (clutter-eval value *denv*)))
    (assert (clutter-operator-p val))
    (extend *denv* var (make-symbol-operator val))))

(defprimfun symbolize (*denv* &rest values)
  (assert (clutter-operator-p (car values)))
  (make-symbol-operator (car values)))

(defprimfun eq (*denv* obj1 obj2)
    (if (eq obj1 obj2)
        :t
        :f))

(defprimfun print (*denv* obj)
  (print obj))

(defun clutter-true-p (exp)
  (if (not (eq exp :f)) t nil))
