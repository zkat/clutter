;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Evaluator
;;;

(defun eval-do (forms env fenv)
  "`Evaluate' each of FORMS in the environments ENV and FENV"
  (when forms
    (loop until (null (rest forms))
          do (evaluate (pop forms) env fenv)
          finally (return (evaluate (first forms) env fenv)))))

(defun invoke (function args)
  (if (functionp function)
      (funcall function args)
      (error "Not a function: ~S" function)))

(defun make-function (variables body env fenv)
  (lambda (values)
    (eval-do body (extend env variables values) fenv)))

(defun evaluate (form env fenv)
  (typecase form

    ;; Variable lookup or symbol macro (the latter is unimplemented)
    (symbol (lookup form env))

    ;; Compound form
    (cons
     (destructuring-bind (operator &rest argument-forms) form

       ;; Sanity check -- this should happen at "compile time"
       (unless (symbolp operator)
         (error "~A is not a valid operator name" (car form)))

       ;; Special operators
       (case operator
         (|quote|
          (destructuring-bind (object)
              argument-forms
            object))
         (|if|
          (destructuring-bind (test then else)
              argument-forms
            (if (not (eq (evaluate test env fenv) *false*))
                (evaluate then env fenv)
                (evaluate else env fenv))))
         (|do| (eval-do (cdr form) env fenv))
         (|set-lexical-variable|
          (destructuring-bind (variable value)
              argument-forms
            ;; Sanity checks -- these should happen at "compile time"
            (unless (symbolp variable)
              (error "~A is not a valid variable name" variable))
            (unless (find-binding variable env)
              (error "~A is not a lexically visible variable" variable))
            (setf (lookup variable env)
                  (evaluate value env fenv))))
         (|bind-lexical-variable|
          (destructuring-bind (variable value &body body)
              argument-forms
            (eval-do body
                     (extend env
                             (list variable)
                             (list (evaluate value env fenv)))
                     fenv)))
         (|lambda|
          (destructuring-bind ((&rest args) &body body)
              argument-forms
            (make-function args body env fenv)))
         (|function|
          (destructuring-bind (name)
              argument-forms
            ;; Sanity checks -- these should happen at "compile time"
            (unless (symbolp name)
              (error "~A is not a valid function name" name))
            (unless (find-binding name fenv)
              (error "~A is not a lexically visible function" name))
            (lookup name fenv)))
         (|bind-lexical-function|
          (destructuring-bind (name value &body body)
              argument-forms
            (let ((new-function (evaluate value env fenv)))
              (unless (functionp new-function)
                (error "~A is not a function" new-function))
              (eval-do body env (extend fenv (list name) (list new-function))))))

         (t

          ;; FIXME: Macros

          ;; Function call
          (funcall (lookup operator fenv)
                   (mapcar (lambda (form) (evaluate form env fenv))
                           argument-forms))))))

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
