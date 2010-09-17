;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Evaluator
;;;

(defun pretreat/literal (expression env)
  (declare (ignore env))
  (lambda () expression))

(defun pretreat/quote (expression env)
  (assert (= 2 (length expression)))
  (pretreat/literal (cadr expression) env))

(defun pretreat/if (expression env)
  (assert (= 4 (length expression)))
  (let ((test (pretreat (second expression) env))
        (true (pretreat (third expression) env))
        (false (pretreat (fourth expression) env)))
    (lambda ()
      (if (funcall test)
          (funcall true)
          (funcall false)))))

(defun pretreat/do (expression env)
  (if (null (cdr expression))
      (error "Illegal syntax: (do)")
      (if (null (cddr expression))
          (pretreat/single-do (second expression) env)
          (pretreat/multi-do (cdr expression) env))))

(defun pretreat/single-do (expression env)
  (pretreat expression env))

(defun pretreat/multi-do (expressions env)
  (let ((pretreated-expressions (mapcar (rcurry #'pretreat env) expressions)))
    (lambda ()
      (loop for exp in pretreated-expressions
         for val = (funcall exp)
         finally (return val)))))

(defun pretreat/application (expression env)
  (if (atom (car expression))
      (let ((pretreated-func (pretreat/function-ref (car expression) env))
            (pretreated-args (mapcar (rcurry #'pretreat env) (cdr expression))))
        (lambda ()
          (invoke (funcall pretreated-func) (mapcar #'funcall pretreated-args))))
      (let ((func-expression (pretreat (car expression) env))
            (pretreated-args (mapcar (rcurry #'pretreat env) (cdr expression))))
        (lambda ()
          (let ((func (funcall func-expression)))
            (if (clutter-function-p func)
                (invoke func (mapcar #'funcall pretreated-args))
                (error "Not a function: " (car expression))))))))

(defun pretreat/symbol (expression env)
  (declare (ignore env))
  (lambda ()
    (lookup expression :lexical)))

(defun pretreat/function-ref (name env)
  (declare (ignore env))
  (lambda ()
    (lookup name :function)))

(defun pretreat/fun (expression env)
  (let ((var (cadr expression)))
    (pretreat/function-ref var env)))

(defun pretreat/bind-lexical-variables (expression env)
  (destructuring-bind (vars-and-values &body body)
      (cdr expression)
    (let ((pre-vars (loop for var in vars-and-values by #'cddr collect var))
          (pre-vals (loop for val in (cdr vars-and-values) by #'cddr
                       collect (pretreat val env)))
          (pre-body (pretreat/multi-do body env)))
      (lambda ()
        (let ((new-frame (make-stack-frame "lexical binding block" (current-scope))))
          (with-frame new-frame
            (loop for var in pre-vars
               for val in pre-vals
               do (bind var (funcall val) :lexical))
            (funcall pre-body)))))))

(defun pretreat/set-lexical-variables (expression env
                                       &aux (vars-and-values (cdr expression)))
  (let ((pre-vars (loop for var in vars-and-values by #'cddr
                     collect (prog1 var
                               (unless (clutter-symbol-p var)
                                 (error "~A is not a valid variable name." var)))))
        (pre-vals (loop for val in (cdr vars-and-values) by #'cddr
                     collect (pretreat val env))))
    (lambda ()
      (loop for var in pre-vars
         for val in pre-vals
         for last-value = (progn
                            (unless (lookup var :lexical)
                              (error "~A is not a lexically visible variable." var))
                            (setf (lookup var :lexical) (funcall val)))
         finally (return last-value)))))

(defun pretreat/var (expression env)
  ;; TODO
  )

(defun pretreat/bind-lexical-functions (expression env)
  ;; TODO
  )

(defun pretreat/set-lexical-functions (expression env)
  ;; TODO
  )

(defun pretreat/lambda (expression env)
  ;; TODO
  )

(defparameter *pretreaters*
  `(("quote" . ,#'pretreat/quote)
    ("if" . ,#'pretreat/if)
    ("do" . ,#'pretreat/do)
    ("bind-lexical-variables" . ,#'pretreat/bind-lexical-variables)
    ("set-lexical-variables" . ,#'pretreat/set-lexical-variables)
    ("var" . ,#'pretreat/var)
    ("bind-lexical-functions" . ,#'pretreat/bind-lexical-functions)
    ("set-lexical-functions" . ,#'pretreat/set-lexical-functions)
    ("fun" . ,#'pretreat/fun)
    ("lambda" . ,#'pretreat/lambda)))

(defun find-pretreater (operator)
  (assert (clutter-symbol-p operator))
  (or (cdr (assoc (clutter-symbol-name operator) *pretreaters* :test #'equal))
      #'pretreat/application))

(defun pretreat (expression env)
  (if (atom expression)
      (if (clutter-symbol-p expression)
          (pretreat/symbol expression env)
          (pretreat/literal expression env))
      (let ((operator (car expression)))
        (unless (or (clutter-symbol-p operator) (listp operator))
          (error "~A is not a valid operator." operator))
        (if (listp operator)
            (pretreat operator env)
            (funcall (find-pretreater operator) expression env)))))

(defun eval-do (forms)
  "`Evaluate' each of FORMS in the environments ENV and FENV"
  (when forms
    (loop until (null (rest forms))
          do (evaluate (pop forms))
          finally (return (evaluate (first forms))))))

(defmacro evaluator-switch (op-var &body body)
  `(cond ,@(loop for (symbol-name . condition-body) in body
              if (stringp symbol-name)
              collect `((eq ,op-var (clutter-intern ,symbol-name))
                        ,@condition-body)
              else
              collect `(,symbol-name ,@condition-body))))

(defun evaluate (form)
  (typecase form

    ;; Variable lookup or symbol macro (the latter is unimplemented)
    (clutter-symbol (lookup form :lexical))

    ;; Compound form
    (cons
     (destructuring-bind (operator &rest argument-forms) form

       ;; Sanity check -- this should happen at "compile time"
       (unless (or (clutter-symbol-p operator) (listp operator))
         (error "~A is not a valid operator." (car form)))

       ;; Special operators
       (evaluator-switch operator
         ("quote"
          (destructuring-bind (object) argument-forms
            object))
         ("if"
          (destructuring-bind (test then else)
              argument-forms
            (if (not (eq (evaluate test) *false*))
                (evaluate then)
                (evaluate else))))
         ("do"
          (eval-do argument-forms))
         ("set-lexical-variables"
          (loop for (var form) on argument-forms by #'cddr
               for last-value = (progn
                                  (unless (clutter-symbol-p var)
                                    (error "~A is not a valid variable name." var))
                                  (unless (lookup var :lexical)
                                    (error "~A is not a lexically visible variable." var))
                                  (setf (lookup var :lexical) (evaluate form)))
               finally (return last-value)))
         ("bind-lexical-variables"
          (destructuring-bind (vars-and-values &body body)
              argument-forms
            (let ((new-frame (make-stack-frame "lexical binding block" (current-scope))))
              (with-frame new-frame
                (let ((vars (loop for var in vars-and-values by #'cddr collect var))
                      (vals (loop for val in (cdr vars-and-values) by #'cddr collect (evaluate val))))
                  (loop for var in vars
                     for val in vals
                     do (bind var val :lexical)))
                (eval-do body)))))
         ("bind-lexical-functions"
          (destructuring-bind (vars-and-funs &body body)
              argument-forms
            (let ((new-frame (make-stack-frame "function binding block" (current-scope))))
              (with-frame new-frame
                (let ((vars (loop for var in vars-and-funs by #'cddr collect var))
                      (vals (loop for val in (cdr vars-and-funs) by #'cddr
                               collect
                                 (let ((new-function (evaluate val)))
                                   (if (clutter-function-p new-function)
                                       new-function
                                       (error "~A is not a function." new-function))))))
                  (loop for var in vars
                     for val in vals
                     do (bind var val :function)))
                (eval-do body)))))
         ("lambda"
          (destructuring-bind ((&rest args) &body body)
              argument-forms
            (make-function args body)))
         ("fun"
          (destructuring-bind (name)
              argument-forms
            ;; Sanity checks -- these should happen at "compile time"
            (unless (clutter-symbol-p name)
              (error "~A is not a valid function name" name))
            (unless (lookup name :function)
              (error "~A is not a lexically visible function" name))
            (lookup name :function)))
         ("var"
          (destructuring-bind (name)
              argument-forms
            ;; Sanity checks -- these should happen at "compile time"
            (unless (clutter-symbol-p name)
              (error "~A is not a valid variable name." name))
            (unless (lookup name :lexical)
              (error "~A is not a visible variable." name))
            (lookup name :lexical)))
         ("set-lexical-functions"
          ;; The sanity checks should happen at 'compile time'
          (loop for (var form) on argument-forms by #'cddr
             for last-value = (progn
                                (unless (clutter-symbol-p var)
                                  (error "~A is not a valid function name." var))
                                (unless (lookup var :function)
                                  (error "~A is not a lexically visible function." var))
                                (let ((func (evaluate form)))
                                  (if (clutter-function-p func)
                                      (setf (lookup var :function) func)
                                      (error "~A is not a function." func))))
                      finally (return last-value)))
         ("define-variable"
          (destructuring-bind (name value)
              argument-forms
            (unless (clutter-symbol-p name)
              (error "~A is not a valid variable name." name))
            (bind name (evaluate value) :lexical)
            name))
         ("define-function"
          (destructuring-bind (name value)
              argument-forms
            (unless (clutter-symbol-p name)
              (error "~A is not a valid variable name." name))
            (let ((fn (evaluate value)))
              (unless (clutter-function-p fn)
                (error "~A is not a function." fn))
              (bind name fn :function))
            name))
         ("define-namespace"
          (destructuring-bind (name)
              argument-forms
            (unless (clutter-symbol-p name)
              (error "~A is not a valid namespace name." name))
            (bind name (make-namespace) :namespace)
            name))
         (t
          (if (listp operator)
              (invoke (evaluate operator) (mapcar #'evaluate argument-forms))
              (let ((operation (lookup operator :function)))
                (cond ((clutter-macro-p operation)
                       (evaluate (invoke operation argument-forms)))
                      ((clutter-function-p operation)
                       (invoke operation (mapcar #'evaluate argument-forms)))
                      (t
                       (error "this should not happen! only forms, spec-ops, macros, and functions allowed!")))))))))
    ;; Self-evaluating object
    (t form)))
