;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(declaim (optimize debug))

;;;
;;; Primitive Constants
;;;

(defparameter *true* (cs "#t"))
(defparameter *false* (cs "#f"))


(defun clutter-load (filespec)
  (with-open-file (stream filespec)
    (loop for expr = (clutter-read stream nil)
          while expr
          do (clutter-eval expr)))
  t)

(defun clutter-eval (expression &optional (environment *global-env*))
  (cond ((clutter-symbol-p expression) (eval/symbol expression environment))
        ((consp expression) (eval/combiner expression environment))
        (t expression)))

(defun eval/symbol (symbol env)
  (if (or (eq symbol *ignore*)
          (eq symbol *true*)
          (eq symbol *false*))
      symbol
      (let ((val (lookup symbol env)))
        (if (clutter-symbol-operative-p val)
            (invoke (clutter-symbol-operative-operative val) env nil)
            val))))

(defun eval/combiner (expression env)
  (let ((f (clutter-eval (car expression) env)))
    (cond ((clutter-operative-p f)
           (invoke f env (cdr expression)))
          ((clutter-function-p f)
           (let ((op (clutter-function-operative f))
                 (values (mapcar (rcurry #'clutter-eval env) (cdr expression))))
             (clutter-eval (cons op values) env)))
          (t
           (error "Not an operative: ~A." f)))))
