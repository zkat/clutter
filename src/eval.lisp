;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Evaluator
;;;

(defun eval-do (forms)
  "`Evaluate' each of FORMS in the environments ENV and FENV"
  (when forms
    (loop until (null (rest forms))
          do (evaluate (pop forms))
          finally (return (evaluate (first forms))))))

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
       (cond ((eq operator (clutter-intern "quote"))
              (destructuring-bind (object) argument-forms
                object))
             ((eq operator (clutter-intern "if"))
              (destructuring-bind (test then else)
                  argument-forms
                (if (not (eq (evaluate test) *false*))
                    (evaluate then)
                    (evaluate else))))
             ((eq operator (clutter-intern "do"))
              (eval-do (cdr form)))
             ((eq operator (clutter-intern "set-lexical-variable"))
              (destructuring-bind (variable value)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (clutter-symbol-p variable)
                  (error "~A is not a valid variable name" variable))
                (setf (lookup variable :lexical)
                      (evaluate value))))
             ((eq operator (clutter-intern "bind-lexical-variables"))
              (destructuring-bind (vars-and-values &body body)
                  argument-forms
                (let ((new-frame (make-stack-frame "lexical binding block" (current-scope))))
                  (push new-frame *stack*)
                  (loop for var in vars-and-values by #'cddr
                       for val in (cdr vars-and-values) by #'cddr
                       do (bind var val :lexical))
                  (eval-do body)
                  (pop *stack*))))
             ((eq operator (clutter-intern "lambda"))
              (destructuring-bind ((&rest args) &body body)
                  argument-forms
                (make-function args body)))
             ((eq operator (clutter-intern "fun"))
              (destructuring-bind (name)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (clutter-symbol-p name)
                  (error "~A is not a valid function name" name))
                (unless (lookup name :function)
                  (error "~A is not a lexically visible function" name))
                (lookup name :function)))
             ((eq operator (clutter-intern "var"))
              (destructuring-bind (name)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (clutter-symbol-p name)
                  (error "~A is not a valid variable name." name))
                (unless (lookup name :lexical)
                  (error "~A is not a visible variable." name))
                (lookup name :lexical)))
             ((eq operator (clutter-intern "set-lexical-function"))
              (destructuring-bind (variable value)
                  argument-forms
                ;; Sanity checks -- these should happen at "compile time"
                (unless (clutter-symbol-p variable)
                  (error "~A is not a valid function name." variable))
                (unless (lookup variable :function)
                  (error "~A is not a lexically visible function." variable))
                (setf (lookup variable :function)
                      (evaluate value))))
             ((eq operator (clutter-intern "bind-lexical-functions"))
              (destructuring-bind (vars-and-funs &body body)
                  argument-forms
                (let ((new-frame (make-stack-frame "function binding block" (current-scope))))
                  (with-frame new-frame
                    (loop for var in vars-and-funs by #'cddr
                          for val in (cdr vars-and-funs) by #'cddr
                          do (let ((new-function (evaluate val)))
                               (if (clutter-function-p new-function)
                                   (bind var new-function :function)
                                   (error "~A is not a function." new-function))))
                   (eval-do body)))))
             ((eq operator (clutter-intern "define-global-variable"))
              (destructuring-bind (name value)
                  argument-forms
                (unless (clutter-symbol-p name)
                  (error "~A is not a valid variable name." name))
                (bind name (evaluate value) :lexical :global t)
                name))
             ((eq operator (clutter-intern "define-global-function"))
              (destructuring-bind (name value)
                  argument-forms
                (unless (clutter-symbol-p name)
                  (error "~A is not a valid variable name." name))
                (let ((fn (evaluate value)))
                  (unless (clutter-function-p fn)
                    (error "~A is not a function." fn))
                  (bind name (evaluate value) :function :global t))
                name))
             ((eq operator (clutter-intern "define-global-namespace"))
              (destructuring-bind (name)
                  argument-forms
                (unless (clutter-symbol-p name)
                  (error "~A is not a valid namespace name." name))
                (bind name (make-namespace) :namespace :global t)
                name))
             (t
              ;; TODO: Macros

              ;; Function call
              (invoke (if (listp operator)
                          (evaluate operator)
                          (lookup operator :function))
                      (mapcar (lambda (form) (evaluate form))
                              argument-forms))))))
    ;; Self-evaluating object
    (t form)))

