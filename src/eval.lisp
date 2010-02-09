;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Evaluator
;;;

(defstruct (clutter-block (:constructor make-clutter-block (exit))) exit)

(defun eval-do (forms env fenv)
  "`Evaluate' each of FORMS in the environments ENV and FENV"
  (when forms
    (loop until (null (rest forms))
          do (evaluate (pop forms) env fenv)
          finally (return (evaluate (first forms) env fenv)))))

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
         (|bind-lexical-variables|
          (destructuring-bind (vars-and-values &body body)
              argument-forms
            (eval-do body
                     (extend env
                             (loop for var in vars-and-values by #'cddr collect var)
                             (loop for val in (cdr vars-and-values) by #'cddr
                                collect (evaluate val env fenv)))
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
         (|bind-lexical-functions|
          (destructuring-bind (vars-and-funs &body body)
              argument-forms
            (eval-do body
                     env
                     (extend fenv
                             (loop for var in vars-and-funs by #'cddr collect var)
                             (loop for val in (cdr vars-and-funs) by #'cddr
                                collect
                                (let ((new-function (evaluate val env fenv)))
                                  (if (functionp new-function) new-function
                                      (error "~A is not a function." new-function))))))))
         (|define-global-variable|
          (destructuring-bind (name value)
              argument-forms
            (unless (symbolp name)
              (error "~A is not a valid variable name." name))
            (let ((existing-binding (assoc name *global-env*)))
              (if existing-binding
                  (setf (cdr existing-binding)
                        (evaluate value env fenv))
                  (let ((relevant-cons (last *global-env*)))
                    (setf (cdr relevant-cons) (cons (cons name (evaluate value env fenv)) nil)))))
            name))
         (|define-global-function|
          (destructuring-bind (name value)
              argument-forms
            (unless (symbolp name)
              (error "~A is not a valid variable name." name))
            (let ((fn (evaluate value env fenv)))
              (unless (functionp fn)
                (error "~A is not a function." fn))
              (let ((existing-binding (assoc name *global-fenv*)))
                (if existing-binding
                    (setf (cdr existing-binding)
                          (evaluate value env fenv))
                    (let ((relevant-cons (last *global-fenv*)))
                      (setf (cdr relevant-cons) (cons (cons name (evaluate value env fenv)) nil))))))
            name))
         (|block|
          (destructuring-bind (name &body body)
              argument-forms
            (unless (symbolp name)
              (error "~A is not a valid block name." name))
            (block nil
              (eval-do body env
                       (extend fenv (list name)
                               (list (make-clutter-block (lambda (x) (return x)))))))))
         (|return-from|
          (destructuring-bind (name &optional (form nil formp))
              argument-forms
            (unless (symbolp name)
              (error "~A is not a valid block name." name))
            (let ((clutter-block (lookup name fenv)))
              (unless (clutter-block-p clutter-block)
                (error "~A is not a valid block." name))
              (funcall (clutter-block-exit clutter-block)
                       (when formp (evaluate form env fenv))))))
         (t

          ;; FIXME: Macros

          ;; Function call
          (invoke (lookup operator fenv)
                  (mapcar (lambda (form) (evaluate form env fenv))
                          argument-forms))))))

    ;; Self-evaluating object
    (t form)))
