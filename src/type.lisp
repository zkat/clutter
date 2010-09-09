;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defstruct basic-type name)

(defun atom-type (atom env)
  (cond
    ((or (eq atom 't) (eq atom 'f)) (make-basic-type :name "bool"))
    ((numberp atom) (make-basic-type :name "number"))
    (t
     (get-variable-type atom env))))

(defun variable-env (env)
  (car env))

(defun function-env (env)
  (cdr env))

(defun extend-variable-env (env extension)
  (cons (cons extension (car env)) (cdr env)))

(defun extend-function-env (env extension)
  (cons (car env) (cons extension (cdr env))))

(defun get-variable-type (var env)
  (cdr (assoc var (variable-env env))))

(defun get-function-type (fn env)
  (cdr (assoc fn (function-env env))))

(defstruct (type-constructor (:conc-name tc-))
  name
  types)

(defun tc-length (tc)
  (length (tc-types tc)))

(defun make-fn-type (from to)
  (make-type-constructor :name "function" :types (list from to)))

(defun make-multiargument-fn-type (list)
  (if (null (cdr list))
      (car list)
      (make-fn-type (car list) (make-multiargument-fn-type (cdr list)))))

(defun fn-type-p (fn-type)
  (and (type-constructor-p fn-type) (eq (tc-name fn-type) "function")))

(defun fn-type-arg (fn-type)
  (car (tc-types fn-type)))

(defun fn-type-return (fn-type)
  (cadr (tc-types fn-type)))

(defun fn-type-all-args (fn-type)
  (butlast (fn-type-all-types fn-type)))

(defun fn-type-final-return (fn-type)
  (car (last (fn-type-all-types fn-type))))

(defun fn-type-all-types (fn-type)
  (if (eq (tc-name fn-type) "function")
      (let ((arg (fn-type-arg fn-type))
            (return (fn-type-return fn-type)))
        (cons arg
              (if (fn-type-p return)
                  (fn-type-all-types return)
                  (cons return '()))))
      (error "~S is not a function types" fn-type)))

(defun make-type-variable ()
  (gensym "TYPE-VARIABLE"))

(defun make-type-variable-list (length)
  (make-gensym-list length "TYPE-VARIABLE"))

(defun derive-types (form env)
  (let ((marked-form (mark-type-variables form)))
    (substitute-type-variables marked-form (unify (unify (make-constraints marked-form env)))))) ;; TODO: Get rid of double unify - bug is somewhere

(defun mark-type-variables (form)
  (if (not (consp form))
      (cons form (make-type-variable))
      (cons (mapcar #'mark-type-variables form) (make-type-variable))))

(defun substitute-type-variables (form substs)
  (let ((subst (cdr (assoc (cdr form) substs))))
    (if subst
        (if (not (consp (car form)))
            (cons (car form) subst)
            (cons (mapcar (lambda (item) (substitute-type-variables item substs)) (car form)) subst))
        (if (not (consp (car form)))
            (car form)
            (mapcar (lambda (item) (substitute-type-variables item substs)) (car form))))))

(defun make-constraints (form env &optional function)
  (let ((actual-form (car form))
        (var (cdr form)))
    (if (not (consp actual-form))
        (make-atom-constraints var actual-form env function)
        (case (caar actual-form)
          (do
           (make-do-constraints var actual-form env))
          (if
           (make-if-constraints var actual-form env))
          (lambda
              (make-lambda-constraints var actual-form env))
          (fun
           (make-env-constraints var actual-form env 't))
          (var
           (make-env-constraints var actual-form env))
          (bind-lexical-variables
           (make-bind-constraints var actual-form env))
          (bind-lexical-functions
           (make-bind-constraints var actual-form env 't))
          (otherwise
           (make-fapp-constraints var actual-form env))))))

(defun make-atom-constraints (var atom env &optional function)
  (list (cons var (if function
                      (get-function-type atom env)
                      (atom-type atom env)))))

(defun make-do-constraints (var form env)
  (let* ((return-form (car (last (mapcar #'cdr (cdr form))))) ;; Last do clause's type variable
         (form-constraints (mappend #'(lambda (item) (make-constraints item env)) (cdr form))))
    (append
     (list
      (cons var return-form))
     form-constraints)))

(defun make-if-constraints (var form env)
  (let* ((if-clause-var (cdr (second form)))
         (then-clause-var (cdr (third form)))
         (else-clause-var (cdr (fourth form)))
         (arg-constraints (mappend #'(lambda (arg) (make-constraints arg env)) (cdr form))))
    (append
     (list
      (cons var then-clause-var)
      (cons var else-clause-var)
      (cons if-clause-var (make-basic-type :name "bool")))
     arg-constraints)))

(defun make-lambda-constraints (var form env)
  (let* ((arg-vars (mapcar #'cdr (car (second form))))
         (new-env (reduce #'(lambda (env new) (extend-variable-env env new)) (car (second form)) :initial-value env))
         (body-var (cdr (third form)))
         (body-constraints (make-constraints (third form) new-env)))
    (append
     (list
      (cons var (make-multiargument-fn-type (append arg-vars (list body-var)))))
     body-constraints)))

(defun make-env-constraints (var form env &optional function)
  (let* ((inner-var (cdr (second form)))
         (inner-constraint (make-constraints (second form) env function)))
    (append
     (list
      (cons var inner-var))
     inner-constraint)))

(defun make-bind-constraints (var form env &optional function)
  (let* ((bindings (mapcar #'car (car (second form))))
         (name-constraints (mapcar #'car bindings))
         (names (mapcar #'car name-constraints))
         (name-vars (mapcar #'cdr name-constraints))
         (value-constraints (mapcar #'cadr bindings))
         (value-vars (mapcar #'cdr value-constraints))
         (name-value-constraints (mapcar #'cons name-vars value-vars))
         (value-computed-constraints (mappend #'(lambda (item) (make-constraints item env)) value-constraints))
         (envs (mapcar #'(lambda (name constraint) (cons name (cdr constraint))) names
                       (remove-if-not #'(lambda (item) (member (car item) value-vars)) value-computed-constraints)))
         (new-env (reduce #'(lambda (env new) (if function
                                                  (extend-function-env env new)
                                                  (extend-variable-env env new)))
                          envs :initial-value env))
         (body-var (make-type-variable))
         (body (cddr form))
         (body-with-do (cons (cons (cons 'do (make-type-variable)) body) body-var))
         (body-constraints (make-constraints body-with-do new-env)))
    (append
     (list
      (cons var body-var))
     name-constraints
     name-value-constraints
     value-computed-constraints
     body-constraints)))

(defun make-fapp-constraints (var form env)
  (let* ((arg-vars (mapcar #'cdr (cdr form)))
         (arg-constraints (mappend #'(lambda (arg) (make-constraints arg env)) (cdr form)))
         (function-var (cdr (first form)))
         (function-constraints (make-constraints (first form) env 't))
         (function-type (cdr (assoc function-var function-constraints))))
    (append
     (list
      (cons function-var (make-multiargument-fn-type (append arg-vars (list (make-type-variable)))))
      (cons var (fn-type-final-return function-type)))
     function-constraints
     arg-constraints)))

(defun unify (constraints)
  (labels ((unify-h (stack substs)
             (if (null stack)
                 substs
                 (let* ((pop (car stack))
                        (left (car pop))
                        (right (cdr pop)))
                   (cond
                     ((equalp left right)
                      (unify-h (cdr stack) substs))
                     ((symbolp left)
                      (unify-h
                       (substitute-type-variable left right (cdr stack))
                       (cons pop (substitute-type-variable left right substs))))
                     ((symbolp right)
                      (unify-h (cons (cons right left) (cdr stack)) substs))
                     ((and (type-constructor-p left)
                           (type-constructor-p right)
                           (equal (tc-name left) (tc-name right))
                           (= (tc-length left) (tc-length right)))
                      (unify-h
                       (append (cdr stack) (unify-tc left right))
                       substs))
                     (t (error "Can't unify ~S ~S" left right)))))))
    (unify-h constraints '())))

(defun substitute-type-variable (var subst constraints)
  (mapcar #'(lambda (constraint)
              (cond
                ((eq var (car constraint)) (cons subst (cdr constraint)))
                ((eq var (cdr constraint)) (cons (car constraint) subst))
                ((fn-type-p (cdr constraint))
                 (cons (car constraint) (substitute-function-type-variables var subst (cdr constraint))))
                ((fn-type-p (car constraint))
                 (cons (substitute-function-type-variables var subst (car constraint)) (cdr constraint)))
                (t constraint)))
          constraints))

(defun substitute-function-type-variables (var subst function)
  (let ((args (fn-type-all-types function)))
    (make-multiargument-fn-type
     (mapcar #'(lambda (arg) (if (eql var arg)
                                 subst
                                 arg))
             args))))

(defun unify-tc (left right)
  (let ((left-t (tc-types left))
        (right-t (tc-types right)))
    (mapcar #'cons left-t right-t)))