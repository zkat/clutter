;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(declaim (optimize debug))

(defun clutter-load (filespec)
  (with-open-file (stream filespec)
    (loop for expr = (clutter-read stream nil)
          while expr
          do (clutter-eval expr)))
  t)

(defun clutter-eval (expression &optional (environment *global-env*) (subenv (cs "var")))
  (cond ((clutter-symbol-p expression) (eval/symbol subenv expression environment))
        ((consp expression) (eval/combiner expression environment))
        (t expression)))

(defun eval/symbol (subenv symbol env)
  (if (or (eq symbol (cs "#ignore"))
          (eq symbol *true*)
          (eq symbol *false*))
      symbol
      (let ((val (lookup subenv symbol env)))
        (if (clutter-symbol-operator-p val)
            (invoke (clutter-symbol-operator-operator val) env nil)
            val))))

(defun eval/combiner (expression env)
  (let ((f (clutter-eval (car expression) env (cs "fun"))))
    (cond ((clutter-operator-p f)
           (invoke f env (cdr expression)))
          ((clutter-function-p f)
           (let ((op (clutter-function-operator f))
                 (values (mapcar (rcurry #'clutter-eval env) (cdr expression))))
             (clutter-eval (cons op values) env)))
          (t
           (error "Not an operator: ~A." f)))))
