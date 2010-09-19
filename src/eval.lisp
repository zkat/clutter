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
      (if (not (eq (funcall test) *false*))
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

(defun pretreat/function-application (expression env)
  (let ((pretreated-func (pretreat/function-ref (car expression) env))
        (pretreated-args (mapcar (rcurry #'pretreat env) (cdr expression))))
    (lambda ()
      (invoke (funcall pretreated-func) (mapcar #'funcall pretreated-args)))))

(defun pretreat/application (expression env)
  (if (atom (car expression))
      (pretreat/function-application expression env)
      (let ((func-expression (pretreat (car expression) env))
            (pretreated-args (mapcar (rcurry #'pretreat env) (cdr expression))))
        (lambda ()
          (let ((func (funcall func-expression)))
            (if (clutter-function-p func)
                (invoke func (mapcar #'funcall pretreated-args))
                (error "Not a function: ~A" (car expression))))))))

(defun pretreat/symbol (expression env)
  (declare (ignore env))
  (lambda ()
    (lookup expression :lexical)))

(defun pretreat/var (expression env)
  (let ((var (cadr expression)))
    (pretreat/symbol var env)))

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
               for val in (mapcar #'funcall pre-vals)
               do (bind var val :lexical))
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

(defun pretreat/bind-lexical-functions (expression env)
  (destructuring-bind (vars-and-values &body body)
      (cdr expression)
    (let ((pre-vars (loop for var in vars-and-values by #'cddr collect var))
          (pre-vals (loop for val in (cdr vars-and-values) by #'cddr
                       collect (pretreat val env)))
          (pre-body (pretreat/multi-do body env)))
      (lambda ()
        (let ((new-frame (make-stack-frame "lexical function binding block" (current-scope))))
          (with-frame new-frame
            (loop for var in pre-vars
               for val in (mapcar #'funcall pre-vals)
               do (bind var (if (clutter-function-p val)
                                val
                                (error "~A is not a function." val))
                        :function))
            (funcall pre-body)))))))

(defun pretreat/set-lexical-functions (expression env
                                       &aux (vars-and-values (cdr expression)))
  (let ((pre-vars (loop for var in vars-and-values by #'cddr
                     collect (prog1 var
                               (unless (clutter-symbol-p var)
                                 (error "~A is not a valid function name." var)))))
        (pre-vals (loop for val in (cdr vars-and-values) by #'cddr
                     collect (pretreat val env))))
    (lambda ()
      (loop for var in pre-vars
         for val in pre-vals
         for last-value = (progn
                            (unless (lookup var :function)
                              (error "~A is not a lexically visible function." var))
                            (let ((function (funcall val)))
                              (if (clutter-function-p function)
                                  (setf (lookup var :function) function)
                                  (error "~A is not a function." function))))
         finally (return last-value)))))

(defun pretreat/lambda (expression env)
  (destructuring-bind ((&rest args) &body body)
      (cdr expression)
    (assert (every #'clutter-symbol-p args))
    (let ((pre-body (pretreat/multi-do body env)))
      (lambda ()
        (make-function args pre-body)))))

(defun pretreat/define-variable (expression env)
  (destructuring-bind (name value)
      (cdr expression)
    (assert (clutter-symbol-p name) () "~A is not a valid variable name." name)
    (let ((pre-value (pretreat value env)))
      (lambda ()
        (bind name (funcall pre-value) :lexical)
        name))))

(defun pretreat/define-function (expression env)
  (destructuring-bind (name value)
      (cdr expression)
    (assert (clutter-symbol-p name) () "~A is not a valid function name." name)
    (let ((pre-value (pretreat value env)))
      (lambda ()
        (let ((function (funcall pre-value)))
          (assert (clutter-function-p function) () "~A is not a valid function." function)
          (bind name function :function)
          name)))))

(defstruct (clutter-block (:constructor make-clutter-block (exit))) exit)
(defun pretreat/block (expression env)
  (destructuring-bind (name &rest body)
      (cdr expression)
    (assert (clutter-symbol-p name) () "~A is not a valid block name." name)
    (let ((pre-body (pretreat/multi-do body env)))
      (lambda ()
        (block nil
          (let ((new-frame (make-stack-frame "lexical block" (current-scope)))
                (block (make-clutter-block (lambda (x) (return x)))))
            (with-frame new-frame
              (bind name block :block)
              (funcall pre-body))))))))

(defun pretreat/return-from (expression env)
  (destructuring-bind (name &optional (form nil formp))
      (cdr expression)
    (assert (clutter-symbol-p name) () "~A is not a valid block name." name)
    (let ((pre-form (when formp (pretreat form env))))
      (lambda ()
        (funcall (clutter-block-exit (lookup name :block))
                 (when pre-form (funcall pre-form)))))))


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
    ("lambda" . ,#'pretreat/lambda)
    ("define-variable" . ,#'pretreat/define-variable)
    ("define-function" . ,#'pretreat/define-function)
    ("block" . ,#'pretreat/block)
    ("return-from" . ,#'pretreat/return-from)))

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
            (pretreat/application expression env)
            (funcall (find-pretreater operator) expression env)))))

(defun evaluate (form)
  (let ((pre-treated (pretreat form nil)))
    (funcall pre-treated)))
