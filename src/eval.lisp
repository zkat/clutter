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

(defun evaluate (form env fenv)
  (typecase form

    ;; Variable lookup or symbol macro (the latter is unimplemented)
    (clutter-symbol (lookup form env))

    ;; Compound form
    (cons
     (destructuring-bind (operator &rest argument-forms) form

       ;; Sanity check -- this should happen at "compile time"
       (unless (or (clutter-symbol-p operator) (listp operator))
         (error "~A is not a valid operator." (car form)))

       ;; Special operators
       (cond ((eq operator (clutter-intern "quote"))
              (destructuring-bind (object) argument-forms
                object))
             ((eq operator (clutter-intern "if"))
              (destructuring-bind (test then else)
                  argument-forms
                (if (not (eq (evaluate test env fenv) *false*))
                    (evaluate then env fenv)
                    (evaluate else env fenv))))
             ((eq operator (clutter-intern "do"))
              (eval-do (cdr form) env fenv))
             ((eq operator (clutter-intern "set-lexical-variable"))
              (destructuring-bind (variable value)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (symbolp variable)
                  (error "~A is not a valid variable name" variable))
                (unless (lookup variable env)
                  (error "~A is not a lexically visible variable" variable))
                (setf (lookup variable env)
                      (evaluate value env fenv))))
             ((eq operator (clutter-intern "bind-lexical-variables"))
              (destructuring-bind (vars-and-values &body body)
                  argument-forms
                (eval-do body
                         (extend env
                                 (loop for var in vars-and-values by #'cddr collect var)
                                 (loop for val in (cdr vars-and-values) by #'cddr
                                    collect (evaluate val env fenv)))
                         fenv)))
             ((eq operator (clutter-intern "lambda"))
              (destructuring-bind ((&rest args) &body body)
                  argument-forms
                (make-function args body env fenv)))
             ((eq operator (clutter-intern "fun"))
              (destructuring-bind (name)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (symbolp name)
                  (error "~A is not a valid function name" name))
                (unless (lookup name fenv)
                  (error "~A is not a lexically visible function" name))
                (lookup name fenv)))
             ((eq operator (clutter-intern "var"))
              (destructuring-bind (name)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (symbolp name)
                  (error "~A is not a valid variable name." name))
                (unless (lookup name env)
                  (error "~A is not a visible variable." name))
                (lookup name env))              )
             ((eq operator (clutter-intern "set-lexical-function"))
              (destructuring-bind (variable value)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (symbolp variable)
                  (error "~A is not a valid function name." variable))
                (unless (lookup variable env)
                  (error "~A is not a lexically visible function." variable))
                (setf (lookup variable fenv)
                      (evaluate value env fenv))))
             ((eq operator (clutter-intern "bind-lexical-functions"))
              (destructuring-bind (vars-and-funs &body body)
                  argument-forms
                (eval-do body
                         env
                         (extend fenv
                                 (loop for var in vars-and-funs by #'cddr collect var)
                                 (loop for val in (cdr vars-and-funs) by #'cddr
                                    collect
                                    (let ((new-function (evaluate val env fenv)))
                                      (if (clutter-function-p new-function) new-function
                                          (error "~A is not a function." new-function))))))))
             ((eq operator (clutter-intern "define-global-variable"))
              (destructuring-bind (name value)
                  argument-forms
                (unless (symbolp name)
                  (error "~A is not a valid variable name." name))
                (setf (lookup name *lexical-bindings*) (evaluate value env fenv))
                name))
             ((eq operator (clutter-intern "define-global-function"))
              (destructuring-bind (name value)
                  argument-forms
                (unless (symbolp name)
                  (error "~A is not a valid variable name." name))
                (let ((fn (evaluate value env fenv)))
                  (unless (clutter-function-p fn)
                    (error "~A is not a function." fn))
                  (setf (lookup name *function-bindings*) (evaluate value env fenv)))
                name))
             (t
              ;; TODO: Macros

              ;; Function call
              (invoke (if (listp operator)
                          (evaluate operator env fenv)
                          (lookup operator fenv))
                      (mapcar (lambda (form) (evaluate form env fenv))
                              argument-forms))))))
    ;; Self-evaluating object
    (t form)))

