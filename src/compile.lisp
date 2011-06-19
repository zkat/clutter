(in-package #:clutter)

(declaim (optimize (debug 3)))

(defstruct (compiler-env (:constructor make-compiler-env (&rest parents)))
  parents
  (bindings (make-hash-table :test 'eq)))

(defun compiler-lookup (symbol env)
  (multiple-value-bind (value exists)
      (gethash symbol (compiler-env-bindings env))
    (if exists
        value
        (loop for parent in (compiler-env-parents env)
              for result = (compiler-lookup symbol parent)
              when result
                return result))))

(defvar *root-compiler-env* (make-compiler-env)
  "Globals, including most primitives.")

(defvar *module*)

(defstruct (primitive-func (:constructor make-primitive-func (compiler)))
  compiler)
(defstruct (primitive-fexpr (:constructor make-primitive-fexpr (compiler)))
  compiler)

(defvar *compiled-combs* (make-hash-table :test 'eq :weakness :key)
  "Mapping from interpreter Clutter functions to compiled versions thereof.")
(defvar *compiled-envs* (aprog1 (make-hash-table :test 'eq :weakness :key)
                          (setf (gethash *global-env* it)
                                *root-compiler-env*))
  "Mapping from interpreter environments to compiler environments.")

(defmacro def-compiler-primfun (name args &body body)
  (with-gensyms (primfunc symbol)
    `(let ((,primfunc (make-primitive-func (lambda ,args ,@body)))
           (,symbol (cs ,name)))
      (setf (gethash ,symbol (compiler-env-bindings *root-compiler-env*)) ,primfunc
            (gethash (lookup ,symbol *global-env*) *compiled-combs*) ,primfunc))))

(defmacro def-compiler-primfexpr (name args &body body)
  (with-gensyms (primfunc symbol)
    `(let ((,primfunc (make-primitive-fexpr (lambda ,args ,@body)))
           (,symbol (cs ,name)))
      (setf (gethash ,symbol (compiler-env-bindings *root-compiler-env*)) ,primfunc
            (gethash (lookup ,symbol *global-env*) *compiled-combs*) ,primfunc))))

(defun compiled-comb (builder clutter-comb)
  (multiple-value-bind (value exists) (gethash clutter-comb *compiled-combs*)
    (cond
      ;; Allow for functions and primitive fexprs only
      (exists value)
      ((clutter-operative-p clutter-comb)
       (error "Can't compile fexprs!"))
      (t
       (setf (gethash clutter-comb *compiled-combs*)
             (let ((op (clutter-function-operative clutter-comb)))
               (compile-form builder
                             (list* (cs "nlambda")
                                    (clutter-operative-name op)
                                    (clutter-operative-args op)
                                    (clutter-operative-body op))
                             (compiled-env (clutter-operative-env op)))))))))

(defun compiled-env (clutter-env)
  (multiple-value-bind (value exists) (gethash clutter-env *compiled-envs*)
    (if exists
        value
        (setf (gethash clutter-env *compiled-envs*)
              ;; TODO: This.  Should reference, creating if necessary,
              ;; the parallels to all of clutter-env's parents as
              ;; well.  Thought: Do we need to do anything more than
              ;; create an empty env here?
              (error "Constant environment compilation unimplemented!")))))

(defun compile-symbol (builder symbol env &aux (value (compiler-lookup symbol env)))
  (if value
      (typecase value
        (primitive-func
           value)
        (primitive-fexpr
           value)
        (sb-sys:system-area-pointer
           (llvm:build-load builder value (clutter-symbol-name symbol))))
      (error "Undefined binding: ~A" symbol)))

(defun compile-invocation (builder invocation env)
  (destructuring-bind (combiner-code . args) invocation
    (let ((combiner (compile-form builder combiner-code env)))
      (typecase combiner
        (primitive-func (apply (primitive-func-compiler combiner) builder
                               (mapcar (rcurry (curry #'compile-form builder) env)
                                       args)))
        (primitive-fexpr (apply (primitive-fexpr-compiler combiner) builder env
                                args))
        (sb-sys:system-area-pointer     ; Assume it's an LLVM pointer.
           (llvm:build-call builder combiner
                            (map 'vector
                                 (rcurry (curry #'compile-form builder) env)
                                 args)
                            "result"))
        (t (error "Attempted to invoke something other than a combiner!"))))))

(defun compile-constant (builder value)
  (typecase value
    ;; Literals
    (integer (llvm:const-int (llvm:int32-type) value nil))
    (single-float (llvm:const-real (llvm:float-type) value))
    (double-float (llvm:const-real (llvm:double-type) value))
    (string (llvm:const-string value nil))
    ;; peval results
    (clutter-function (compiled-comb builder value))
    (env (compiled-env value))
    (clutter-operative (compiled-comb builder value))
    (t (error "Unsupported compiletime constant!"))))

(defun compile-form (builder form env)
  (typecase form
    (clutter-symbol (compile-symbol     builder form env))
    (list           (compile-invocation builder form env))
    (t              (compile-constant   builder form))))

(def-compiler-primfun "+" (builder x y)
  (llvm:build-add builder x y "sum"))
(def-compiler-primfun "-" (builder x y)
  (llvm:build-sub builder x y "difference"))
(def-compiler-primfun "*" (builder x y)
  (llvm:build-mul builder x y "product"))
(def-compiler-primfun "/" (builder x y)
  (llvm:build-s-div builder x y "quotient"))

(def-compiler-primfun ">?" (builder x y)
  (llvm:build-i-cmp builder :> x y "greater"))
(def-compiler-primfun "<?" (builder x y)
  (llvm:build-i-cmp builder :< x y "lesser"))
(def-compiler-primfun "=?" (builder x y)
  (llvm:build-i-cmp builder := x y "equal"))

(def-compiler-primfexpr "quote" (builder denv value)
  (declare (ignore denv))
  (compile-constant builder value))

(def-compiler-primfexpr "def-in!" (builder denv target-env name value)
  (let* ((target-compiler-env
          (cond
            ((env-p target-env)
             (compiled-env target-env))
            ((equal target-env (list (lookup (cs "get-current-env") *global-env*)))
             denv)
            (t (error "Binding values in non-constant environments is unimplemented!"))))
         (compiled-value (compile-form builder value denv))
         (type (llvm:type-of compiled-value)))
    (setf (gethash name (compiler-env-bindings target-compiler-env))
          (if (eq target-compiler-env *root-compiler-env*)
              (aprog1 (llvm:add-global *module* type (clutter-symbol-name name))
                ;; TODO: Evaluate compiled-value first. (JIT? Interpret?)
                (llvm:set-initializer it compiled-value))
              (aprog1 (llvm:build-alloca builder type
                                         (clutter-symbol-name name))
                (llvm:build-store builder compiled-value it))))))

;;; FIXME: This will error if the stdlib hasn't been loaded yet due to nlambda being defined in-language.
(def-compiler-primfexpr "nlambda" (builder env name args &rest body &aux
                                           ret (new-builder (llvm:make-builder)))
  (declare (ignore builder))
  (unwind-protect
       (let* ((func (llvm:add-function *module* (clutter-symbol-name name) (llvm:function-type (llvm:int32-type) (make-array (length args) :initial-element (llvm:int32-type)))))
              (entry (llvm:append-basic-block func "entry"))
              (inner-env (make-compiler-env env)))
         (setf ret func)
         (llvm:position-builder-at-end new-builder entry)
         ;; Name and allocate mutable space for arguments
         (map nil
              (lambda (argument name &aux (name-string (clutter-symbol-name name)))
                (setf (llvm:value-name argument) name-string
                      (gethash name (compiler-env-bindings inner-env))
                      (aprog1 (llvm:build-alloca new-builder (llvm:int32-type)
                                                 (concatenate 'string name-string "-ptr"))
                        (llvm:build-store new-builder argument it ))))
              (llvm:params func)
              args)
         ;; Compile body and return the value of the last form
         (loop for (form . remaining) on body
               for result = (compile-form new-builder form inner-env)
               unless remaining do
                 (llvm:build-ret new-builder result)))
    (llvm:dispose-builder new-builder))
  ret)

(def-compiler-primfexpr "if" (builder env condition then else)
  (let* ((cond-result (compile-form builder condition env))
         (func (llvm:basic-block-parent (llvm:insertion-block builder)))
         (then-block (llvm:append-basic-block func "then"))
         (else-block (llvm:append-basic-block func "else"))
         (done-block (llvm:append-basic-block func "endif"))
         then-result else-result)
    (llvm:build-cond-br builder cond-result then-block else-block)

    (llvm:position-builder builder then-block)
    (setf then-result (compile-form builder then env))
    (setf then-block (llvm:insertion-block builder))
    (llvm:build-br builder done-block)

    (llvm:position-builder builder else-block)
    (setf else-result (compile-form builder else env))
    (setf else-block (llvm:insertion-block builder))
    (llvm:build-br builder done-block)

    (llvm:position-builder builder done-block)
    (aprog1 (llvm:build-phi builder (llvm:int32-type) "result")
      (llvm:add-incoming it
                         (list then-result else-result)
                         (list then-block else-block)))))

(defun cltr-compile (expr &aux builder pm)
  (unwind-protect
       (progn
         (setf *module* (llvm:make-module "clutter")
               builder (llvm:make-builder)
               pm (llvm:create-pass-manager))
         
         (compile-form builder expr *root-compiler-env*)

         (llvm:add-promote-memory-to-register-pass pm)
         (llvm:run-pass-manager pm *module*)

         (llvm:dump-module *module*)
         (llvm:verify-module *module*))
    
    (llvm:dispose-builder builder)
    (llvm:dispose-pass-manager pm)
    (when *module*
      (llvm:dispose-module *module*)
      (setf *module* nil))))
