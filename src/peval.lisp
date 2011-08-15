(in-package :clutter)

(declaim (optimize (debug 3)))

(defvar *peval-prims* #+sbcl (make-hash-table :test 'eq :weakness :key)
                      #+ccl  (make-hash-table :test 'eq :weak t)
  "Mapping from interpreter Clutter functions to primitive partial evaluation functions.")

(defstruct (dynamic (:constructor make-dynamic (form)))
  form)

(defmethod print-object ((o dynamic) s)
  (print-unreadable-object (o s :type t)
    (format s "~A" (dynamic-form o))))

(defun dynamic? (value)
  (typep value 'dynamic))

(defun static? (value)
  (not (dynamic? value)))

(defun staticify (value)
  "Prepares peval'd values to be conventionally evaluated."
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
  (if (and (static? form) (static? env))
      (peval form env)
      (make-dynamic (list (lookup (cs "eval")) (staticify form) (staticify env)))))

(def-peval-prim "rem" (number divisor)
  (if (and (static? number) (static? divisor))
      (rem number divisor)
      (make-dynamic (list (lookup (cs "rem")) (staticify number) (staticify divisor)))))

(def-peval-prim-op "nvau" denv (name denv-var args &rest body &aux (fake-env (make-env denv)))
  (mapc (lambda (arg)
          (unless (eq arg (cs "&rest"))
            (extend fake-env arg (make-dynamic arg))))
        args)
  (unless (eq denv-var *ignore*)
    (extend fake-env denv-var denv))
  (clutter-eval
   (list*
    (lookup (cs "nvau"))
    name denv-var args
    ;; TODO: Handle trivial child environments.
    (nsubst (list (lookup (cs "get-current-env")))
            (list (lookup (cs "quote")) fake-env)
            (mapcar (compose #'staticify (rcurry #'peval fake-env)) body)
            :test #'equal))
   denv))

(def-peval-prim-op "if" denv (condition-form then-form else-form)
  (let ((condition (peval condition-form denv))
        (then (peval then-form denv))
        (else (peval else-form denv)))
    (if (static? condition)
        (if (eq condition *false*)
            else
            then)
        (make-dynamic (list (lookup (cs "if")) (staticify condition)
                            (staticify then)
                            (staticify else))))))

(def-peval-prim-op "set-in!" denv (target-env-form var value-form)
  (make-dynamic (list (lookup (cs "set-in!")) (staticify (peval target-env-form denv)) var (staticify (peval value-form denv)))))

(def-peval-prim-op "def-in!" denv (target-env-form var value-form)
  (make-dynamic (list (lookup (cs "def-in!")) (staticify (peval target-env-form denv)) var (staticify (peval value-form denv)))))

(defun peval (form &optional (env *global-env*))
  (typecase form
    (list (peval-combiner form env))
    (clutter-symbol (peval-symbol form env))
    (t form)))

(defun peval-symbol (symbol env)
  (if (eq symbol *ignore*)
      *ignore*
      (multiple-value-bind (value triggers binding-env) (lookup symbol env)
        ;; Values which already require a recompile on change can be inlined.
        ;; Fexprs (usually) must be inlined and are forced to do so.
        (if (or triggers
                (and (typep value 'clutter-operative)
                     (setf (triggers-recompile? symbol binding-env) t)))
            value
            (make-dynamic symbol)))))

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
                 (every #'static? args))
            (clutter-eval (cons (clutter-function-operative combiner) args)))
           (t
            (make-dynamic (list* combiner (mapcar #'staticify args)))))))
       (dynamic
        ;; HACK: This check should use type inference, but currently just hopes nobody sticks an operative where a function used to be.
        (if (and (clutter-symbol-p combiner-form)
                 (clutter-bound? combiner-form)
                 (clutter-function-p (lookup combiner-form env)))
            (make-dynamic (list* (staticify combiner)
                                 (mapcar (compose #'staticify (rcurry #'peval env))
                                         arg-forms)))
            (make-dynamic (list* combiner-form arg-forms))))
       (t (error "Tried to invoke ~A, which is not a combiner" combiner))))))

;;; TODO: Handle unwrapped primitive functions somehow.
(defun inline-op (operative args env &aux (inline-env (make-env (clutter-operative-env operative))))
  (assert (clutter-operative-pure operative))
  ;; NOTE: Strictly recompile-trigger=t here is nonsensical; we use it to ensure eval goes through, because these are constant values.
  (extend inline-env (clutter-operative-denv-var operative) env t)
  (loop with vau-list = (clutter-operative-args operative)
        while args
        do (if (eq (first vau-list) (cs "&rest"))
               (progn (extend inline-env (second vau-list) args t)
                      (return))
               (extend inline-env (pop vau-list) (pop args) t)))
  (let ((body (nsubst (clutter-operative-env operative) inline-env
                      (mapcar (rcurry #'peval inline-env) (clutter-operative-body operative)))))
    (if (> (length body) 1)
        (list* (lookup (cs "do")) body)
        (first body))))
