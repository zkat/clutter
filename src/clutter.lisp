(cl:defpackage #:clutter (:use :cl))
(cl:in-package :clutter)

(defparameter *secret-unbound-value* (gensym "SECRETLY-UNBOUND-"))
(defparameter *true* '|t|)
(defparameter *false* '|f|)
(defparameter *initial-env* ())
(defparameter *global-env* *initial-env*)

(defun push-initial-binding (name &optional (value *secret-unbound-value*))
  (push (cons name value) *global-env*))
(defmacro definitial (name &optional (value nil valuep))
  `(progn (push-initial-binding ',name ,@(when valuep `(,value)))
          ',name))
(defmacro defprimitive (name value arity)
  `(definitial ,name (lambda (values)
                       (if (= ,arity (length values))
                           (apply ,value values)
                           (error "Incorrect arity.")))))

(definitial |t| *true*)
(definitial |f| *false*)
(definitial |nil| '())
(defprimitive |cons| #'cons 2)
(defprimitive |car| #'car 1)
(defprimitive |cdr| #'cdr 1)
(defprimitive |rplaca| #'rplaca 2)
(defprimitive |rplacd| #'rplacd 2)
(defprimitive |eq?| (lambda (x y) (if (eq x y) *true* *false*)) 2)
(defprimitive |eql?| (lambda (x y) (if (eql x y) *true* *false*)) 2)
(defprimitive |<| (lambda (x y) (if (< x y) *true* *false*)) 2)
(defprimitive |>| (lambda (x y) (if (> x y) *true* *false*)) 2)
(defprimitive |=| (lambda (x y) (if (= x y) *true* *false*)) 2)
(defprimitive |apply| #'invoke 2)

(defun repl ()
  (let ((*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (loop
      (princ "> ")
      (with-simple-restart (abort "Return to Clutter's toplevel")
        (princ (evaluate (read) *global-env*)))
      (fresh-line))))

(defun eprogn (exps env)
  (when exps
    (if (cdr exps)
        (progn (evaluate (car exps) env)
               (eprogn (cdr exps) env))
        (evaluate (car exps) env))))

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
    (eprogn body (extend env variables values))))

(defun evaluate (exp env)
  (if (atom exp)
      (typecase exp
        ((or number string character boolean vector) exp)
        (symbol (lookup exp env))
        (otherwise (error "Cannot evaluate: ~S" exp)))
      (case (car exp)
        (|quote| (cadr exp))
        (|if| (if (not (eq (evaluate (cadr exp) env) *false*))
                  (evaluate (caddr exp) env)
                  (evaluate (cadddr exp) env)))
        (|do| (eprogn (cdr exp) env))
        (|set!| (setf (lookup (cadr exp) env)
                      (evaluate (caddr exp) env)))
        (|lambda| (make-function (cadr exp) (cddr exp) env))
        (otherwise (invoke (evaluate (car exp) env)
                           (eval-list (cdr exp) env))))))
