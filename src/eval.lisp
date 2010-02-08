;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Evaluator
;;;

(defun eval-do (forms env fenv)
  "`Evaluate' each of FORMS in the environments ENV and FENV"
  (dolist (form forms)
    (evaluate form env fenv)))

(defun eval-list (list env fenv)
  (mapcar (lambda (exp) (evaluate exp env fenv)) list))

(defun invoke (function args)
  (if (functionp function)
      (funcall function args)
      (error "Not a function: ~S" function)))

(defun make-function (variables body env fenv)
  (lambda (values)
    (eval-do body (extend env variables values) fenv)))

(defun evaluate-application (fn args fenv)
  (assert (symbolp fn))
  (funcall (lookup fn fenv) args))

(defun evaluate (form env fenv)
  (typecase form

    ;; Variable lookup or symbol macro (the latter is unimplemented)
    (symbol (lookup form env))

    ;; Compound form
    (cons (case (car form)
            (|quote| (cadr form))
            (|if| (if (not (eq (evaluate (cadr form) env fenv) *false*))
                      (evaluate (caddr form) env fenv)
                      (evaluate (cadddr form) env fenv)))
            (|do| (eval-do (cdr form) env fenv))
            (|set!| (setf (lookup (cadr form) env)
                          (evaluate (caddr form) env fenv)))
            (|lambda| (make-function (cadr form) (cddr form) env fenv))
            (|function| (if (symbolp (cadr form))
                            (lookup (cadr form) fenv)
                            (error "No such function: ~A" (cadr form))))
            (|flet| (eval-do (cddr form) env (extend fenv (mapcar #'car (cadr form))
                                                     (mapcar (lambda (def)
                                                               (make-function (cadr def)
                                                                              (cddr def)
                                                                              env fenv))
                                                             (cadr form)))))
            (otherwise (evaluate-application (car form)
                                             (eval-list (cdr form) env fenv)
                                             fenv))))

    ;; Self-evaluating object
    (t form)))

(defun repl ()
  (let ((*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (loop
      (princ "> ")
      (with-simple-restart (abort "Return to Clutter's toplevel")
        (prin1 (evaluate (read) *global-env* *global-fenv*)))
      (fresh-line))))
