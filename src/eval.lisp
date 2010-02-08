;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Evaluator
;;;

(defun eprogn (exps env fenv)
  (when exps
    (if (cdr exps)
        (progn (evaluate (car exps) env fenv)
               (eprogn (cdr exps) env fenv))
        (evaluate (car exps) env fenv))))

(defun eval-list (list env fenv)
  (mapcar (lambda (exp) (evaluate exp env fenv)) list))

(defun invoke (function args)
  (if (functionp function)
      (funcall function args)
      (error "Not a function: ~S" function)))

(defun make-function (variables body env fenv)
  (lambda (values)
    (eprogn body (extend env variables values) fenv)))

(defun evaluate-application (fn args fenv)
  (assert (symbolp fn))
  (funcall (lookup fn fenv) args))

(defun evaluate (exp env fenv)
  (if (atom exp)
      (typecase exp
        ((or number string character boolean vector) exp)
        (symbol (lookup exp env))
        (otherwise (error "Cannot evaluate: ~S" exp)))
      (case (car exp)
        (|quote| (cadr exp))
        (|if| (if (not (eq (evaluate (cadr exp) env fenv) *false*))
                  (evaluate (caddr exp) env fenv)
                  (evaluate (cadddr exp) env fenv)))
        (|do| (eprogn (cdr exp) env fenv))
        (|set!| (setf (lookup (cadr exp) env)
                      (evaluate (caddr exp) env fenv)))
        (|lambda| (make-function (cadr exp) (cddr exp) env fenv))
        (|function| (if (symbolp (cadr exp))
                        (lookup (cadr exp) fenv)
                        (error "No such function: ~A" (cadr exp))))
        (|flet| (eprogn (cddr exp) env (extend fenv (mapcar #'car (cadr exp))
                                               (mapcar (lambda (def)
                                                         (make-function (cadr def)
                                                                        (cddr def)
                                                                        env fenv))
                                                       (cadr exp)))))
          (otherwise (evaluate-application (car exp)
                                         (eval-list (cdr exp) env fenv)
                                         fenv)))))

(defun repl ()
  (let ((*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (loop
      (princ "> ")
      (with-simple-restart (abort "Return to Clutter's toplevel")
        (prin1 (evaluate (read) *global-env* *global-fenv*)))
      (fresh-line))))
