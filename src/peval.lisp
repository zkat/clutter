(in-package :clutter)

(declaim (optimize (debug 3)))

(defvar *peval-prims* (make-hash-table :test 'eq))

(defstruct (dynamic (:constructor make-dynamic (form)))
  form)

(defmethod print-object ((o dynamic) s)
  (print-unreadable-object (o s :type t)
    (format s "~A" (dynamic-form o))))

(defun dynamic? (value)
  (typep value 'dynamic))

(defun staticify (value)
  (if (dynamic? value)
      (dynamic-form value)
      (list (lookup (cs "quote")) value)))

(defun peval-prim? (combiner)
  (nth-value 1 (gethash combiner *peval-prims*)))

(defun pevaluator (combiner)
  (multiple-value-bind (value exists) (gethash combiner *peval-prims*)
    (if exists
        value
        (error "~A is not a peval primitive!" combiner))))

(defun (setf pevaluator) (value combiner)
  (setf (gethash combiner *peval-prims*) value))

(defmacro def-peval-prim (name args &body body)
  `(setf (pevaluator (lookup (cs ,name) *global-env*)) (lambda ,args ,@body)))

(defmacro def-peval-prim-op (name env-var args &body body)
  `(setf (pevaluator (lookup (cs ,name) *global-env*)) (lambda ,(cons env-var args) ,@body)))

(def-peval-prim "eval" (form env)
  (peval form env))

(def-peval-prim-op "nlambda" denv (name args &rest body &aux (fake-env (make-env denv)))
  (mapc (lambda (arg)
          (extend fake-env arg (make-dynamic arg)))
        args)
  (subst (list (lookup (cs "get-current-env")))
         fake-env
         (clutter-eval (list (lookup (cs "nlambda")) name args
                             (mapcar (compose #'staticify (rcurry #'peval fake-env)) body))
                       denv)))

(defun peval (form &optional (env *global-env*))
  (typecase form
    (list (peval-combiner form env))
    (clutter-symbol (peval-symbol form env))
    (t form)))

(defun peval-symbol (symbol env)
  ;; TODO: Only deref if the binding is constant; otherwise make a dynamic value.
  (clutter-eval symbol env))

(defun peval-combiner (form env)
  (destructuring-bind (combiner-form &rest arg-forms) form
    (let* ((combiner (peval combiner-form env))
           (primitive (peval-prim? combiner)))
     (typecase combiner
       (clutter-operative
        (if primitive
            (apply (pevaluator combiner) env arg-forms)
            (inline-op combiner arg-forms env)))
       (clutter-function
        (let ((args (mapcar (rcurry #'peval env) arg-forms)))
         (cond
           (primitive
            (apply (pevaluator combiner) args))
           ((and (clutter-operative-pure (clutter-function-operative combiner))
                 (every (complement #'dynamic?) args))
            (clutter-eval (cons (clutter-function-operative combiner) args)))
           (t
            (make-dynamic form)))))
       (t (error "Tried to invoke ~A, which is not a combiner" combiner))))))

(defun inline-op (operative args env &aux (inline-env (make-env (clutter-operative-env operative))))
  ;; TODO: Mark all these bindings as constant.
  (mapc (curry #'extend inline-env)
        (list* (clutter-operative-denv-var operative) (clutter-operative-args operative))
        (list* env args))
  (list* (lookup (cs "do"))
         (mapcar (rcurry #'peval inline-env) (clutter-operative-body operative))))
