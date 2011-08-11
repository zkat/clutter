(in-package #:clutter)

(declaim (optimize (debug 3)))

(defstruct compiler-env
  parents
  (bindings (make-hash-table :test 'eq))
  func)

(defun compiler-env-toplevel (instance)
  (null (compiler-env-func instance)))

(defun compiler-lookup (symbol env)
  "Returns value, existence, and binding environment"
  (multiple-value-bind (value exists)
      (gethash symbol (compiler-env-bindings env))
    (if exists
        (values value t env)
        (loop for parent in (compiler-env-parents env)
              for result = (compiler-lookup symbol parent)
              when result
                return result))))

(defvar *root-compiler-env* (make-compiler-env :func nil)
  "Globals, including most primitives.")

(defvar *module*)
(defvar *alloc*)

(defstruct (primitive-func (:constructor make-primitive-func (compiler)))
  compiler)
(defstruct (primitive-fexpr (:constructor make-primitive-fexpr (compiler)))
  compiler)
(defstruct (unwrapped-func (:constructor make-unwrapped-func (func)))
  func)

(defvar *compiled-combs* #+sbcl (make-hash-table :test 'eq :weakness :key)
                         #+ccl  (make-hash-table :test 'eq :weak t)
  "Mapping from interpreter Clutter functions to compiled versions thereof.")
(defvar *compiled-envs* (aprog1
                            #+sbcl (make-hash-table :test 'eq :weakness :key)
                            #+ccl  (make-hash-table :test 'eq :weak t)
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
        (unwrapped-func
         value)
        (#+sbcl sb-sys:system-area-pointer
         #+ccl  ccl:macptr
         (llvm:build-load builder value (clutter-symbol-name symbol))))
      (error "Undefined binding: ~A" symbol)))

(defun build-closure-call (builder closure args &aux function context)
  (if (llvm:constantp closure)
      (setf context (llvm:const-extract-value closure (vector 0))
            function (llvm:const-extract-value closure (vector 1)))
      (setf context (llvm:build-load builder (llvm:build-struct-gep builder closure 0 "context-addr") "context")
            function (llvm:build-load builder (llvm:build-struct-gep builder closure 1 "function-addr") "function")))
  (llvm:build-call builder function
                   (coerce (cons context args) 'vector)
                   "result"))

(defun compile-invocation (builder invocation env)
  (destructuring-bind (combiner-form . arg-forms) invocation
    (let ((combiner (compile-form builder combiner-form env)))
      (typecase combiner
        (primitive-func (apply (primitive-func-compiler combiner) builder
                               (mapcar (rcurry (curry #'compile-form builder) env)
                                       arg-forms)))
        (primitive-fexpr (apply (primitive-fexpr-compiler combiner) builder env
                                arg-forms))
        (unwrapped-func
         (let ((inner-comb (unwrapped-func-func combiner))
               (args (mapcar (curry #'compile-constant builder) arg-forms)))
           (cond
             ((clutter-function-p inner-comb)
              (build-closure-call builder (compiled-comb builder inner-comb) args))
             ((primitive-func-p inner-comb)
              (apply (primitive-func-compiler inner-comb) builder args))
             (t (error "Internal error: invalid unwrapped function")))))
        (#+sbcl sb-sys:system-area-pointer     ; Assume it's an LLVM pointer.
         #+ccl  ccl:macptr
         ;; Closures are { i8*, function }
         (build-closure-call builder combiner (mapcar (rcurry (curry #'compile-form builder) env) arg-forms)))
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
    (t (error "Unsupported compiletime constant: ~A" value))))

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

(def-compiler-primfun "wrap" (builder value)
  (unless (clutter-operative-p value)
    (error "Dynamic fexprs wrapping unimplemented."))
  (compile-form builder
                (list* (cs "nlambda")
                       (clutter-operative-name value)
                       (clutter-operative-args value)
                       (clutter-operative-body value))
                (compiled-env (clutter-operative-env value))))

(def-compiler-primfun "unwrap" (builder value)
  (declare (ignore builder))
  (unless (or (clutter-function-p value) (primitive-func-p value))
    (error "Dynamic function unwrapping unimplemented."))
  (make-unwrapped-func value))

(def-compiler-primfexpr "do" (builder denv &rest body)
  ;; Compile body and return the value of the last form
  (loop for (form . remaining) on body
        for result = (compile-form builder form denv)
        unless remaining
          return result))

(defun add-entry-alloca (function type name &aux builder)
  (unwind-protect
       (progn
         (setf builder (llvm:make-builder))
         (llvm:position-builder-at-end builder (llvm:entry-basic-block function))
         (llvm:build-alloca builder type name))
    (llvm:dispose-builder builder)))

(def-compiler-primfexpr "def-in!" (builder denv target-env name value)
  (let* ((target-compiler-env
           (if (equal target-env (list (lookup (cs "get-current-env"))))
               denv
               (compile-form builder target-env denv)))
         (compiled-value (compile-form builder value denv))
         (type (llvm:type-of compiled-value)))
    (unless (typep target-compiler-env 'compiler-env)
      (error "Binding values in non-constant environments is unimplemented!"))
    (setf (gethash name (compiler-env-bindings target-compiler-env))
          (cond
            ((and (compiler-env-toplevel target-compiler-env)
                  (compiler-env-toplevel denv))
             (aprog1 (llvm:add-global *module* type (clutter-symbol-name name))
               ;; TODO: Evaluate compiled-value first. (JIT? Interpret?)
               (llvm:set-initializer it compiled-value)))
            ((compiler-env-toplevel target-compiler-env)
             (error "Dynamic bindings are unimplemented! (tried to add global binding from a function)"))
            ((compiler-env-toplevel denv)
             (error "Tried to modify a dynamic environment from the toplevel; WTF?"))
            ((eq (compiler-env-func target-compiler-env)
                 (compiler-env-func denv))
             (aprog1 (add-entry-alloca (compiler-env-func target-compiler-env)
                                       type (clutter-symbol-name name))
               (llvm:build-store builder compiled-value it)))
            (t (error "wat"))))
    compiled-value))

(def-compiler-primfexpr "set-in!" (builder denv target-env-form name value-form)
  (let ((target-env (if (equal target-env-form (list (lookup (cs "get-current-env"))))
                        denv
                        (compile-form builder target-env-form denv))))
    (unless (typep target-env 'compiler-env)
      (error "Modifying bindings in non-constant environments is unimplemented!"))
    (aprog1 (compile-form builder value-form denv)
      (llvm:build-store builder it (compiler-lookup name target-env)))))

(defun free-vars (locals form)
  (typecase form
    (clutter-symbol
     (if (member form locals)
         (values nil locals)
         (values (list form) locals)))
    (list
     (destructuring-bind (combiner . args) form
       (cond
         ;; Handle unresolved combiner reference
         ((clutter-symbol-p combiner)
          (let ((arg-refs
                  (loop for form in args
                        for result = (multiple-value-list (free-vars locals form))
                        appending (first result)
                        do (setf locals (append (second result) locals)))))
            (if (member combiner locals)
              (values arg-refs locals)
              (values (cons combiner arg-refs) locals))))
         ((listp combiner)
          (let ((arg-refs
                  (loop for form in args
                        for result = (multiple-value-list (free-vars locals form))
                        appending (first result)
                        do (setf locals (append (second result) locals)))))
            (values (cons combiner arg-refs) locals)
            (multiple-value-bind (crefs more-locals) (free-vars locals combiner)
              (values (append crefs arg-refs) (append more-locals locals)))))
         ;; Handle each possible post-peval binding-introduction form.
         ((eq combiner (lookup (cs "def-in!")))
          (destructuring-bind (env symbol value) args
            (if (equal env (list (lookup (cs "get-current-env"))))
                (multiple-value-bind (refs new-locals) (free-vars locals value)
                  (values refs (cons symbol new-locals)))
                (error "Nontrivial function environments in closures are unimplemented!"))))
         ((eq combiner (lookup (cs "set-in!")))
          (destructuring-bind (env symbol value) args
            (if (equal env (list (lookup (cs "get-current-env"))))
                (multiple-value-bind (refs new-locals) (free-vars locals value)
                  (if (member symbol locals)
                      (values refs new-locals)
                      (values (cons symbol refs) new-locals)))
                (error "Nontrivial function environments in closures are unimplemented!"))))
         ((eq combiner (lookup (cs "nlambda")))
          (destructuring-bind (name args &rest body) args
            (declare (ignore name))
            (values
             (let ((inner-locals (append args (copy-list locals))))
               (loop for form in body
                     for result = (multiple-value-list (free-vars inner-locals form))
                     appending (first result)
                     do (setf inner-locals (second result))))
             locals)))
         ;; Handle resolved normal combiners
         ((clutter-function-p combiner)
          (values
           (loop for form in args
                 for result = (multiple-value-list (free-vars locals form))
                 appending (first result)
                 do (setf locals (second result)))
           locals))
         ((clutter-operative-p combiner)
          (if (eq combiner (lookup (cs "quote")))
              (values nil locals)
              ;; Assumes that all args of the remaining primitive combiners may be evaluated (simply), and that no non-primitive combiners have been passed in
              (values
               (loop for form in args
                     for result = (multiple-value-list (free-vars locals form))
                     appending (first result)
                     do (setf locals (second result)))
               locals))))))
    (t (values nil locals))))

;;; FIXME: This will error if the stdlib hasn't been loaded yet due to nlambda being defined in-language.
(def-compiler-primfexpr "nlambda" (builder env name args &rest body &aux
                                   closing-over (new-builder (llvm:make-builder)))
  ;; Determine what, if anything, we're closing over (removing stuff not on the stack)
  (setf closing-over (remove-if (lambda (symbol)
                                  (multiple-value-bind (value exists binding-env)
                                      (compiler-lookup symbol env)
                                    (declare (ignore value))
                                    (unless exists
                                      (error "Symbol ~A has no binding in or above ~A"
                                             symbol env))
                                   (compiler-env-toplevel binding-env)))
                                (loop with locals = args
                                      for form in body
                                      for result = (multiple-value-list (free-vars locals form))
                                      appending (first result)
                                      do (setf locals (second result)))))
  (unwind-protect
       (let* ((context-type (and closing-over
                                 (llvm:struct-type (coerce (loop for symbol in closing-over
                                                                 collecting (llvm:type-of (compiler-lookup symbol env))) 'vector) nil)))
              (ftype (llvm:function-type (llvm:int32-type)
                                         (aprog1 (make-array (+ 1 (length args)) :initial-element (llvm:int32-type))
                                           (setf (aref it 0) (llvm:pointer-type (llvm:int8-type))))))
              (value-type (llvm:struct-type (vector (llvm:pointer-type (llvm:int8-type))
                                                    (llvm:pointer-type ftype)) nil))
              (func (llvm:add-function *module* (concatenate 'string (clutter-symbol-name name) "-fn")
                                       ftype))
              (entry (llvm:append-basic-block func "entry"))
              (begin (llvm:append-basic-block func "begin"))
              (inner-env (make-compiler-env :func func :parents (list env))))
         (llvm:add-type-name *module* (concatenate 'string (clutter-symbol-name name) "-closure")
                             value-type)
         (when closing-over
           (llvm:add-type-name *module* (concatenate 'string (clutter-symbol-name name) "-context")
                               context-type))
         ;; Name and allocate mutable space for arguments
         (llvm:position-builder-at-end new-builder entry)
         (let ((params (llvm:params func)))
           (when closing-over
             (setf (llvm:value-name (first params)) "outer-context-ptr")
             (let ((context (llvm:build-pointer-cast new-builder (first params)
                                                     (llvm:pointer-type context-type)
                                                     "outer-context")))
               (loop for symbol in closing-over
                     for index from 0
                     for name-string = (clutter-symbol-name symbol)
                     do (setf (gethash symbol (compiler-env-bindings inner-env))
                              (llvm:build-load new-builder
                                               (llvm:build-struct-gep new-builder context index
                                                                      (concatenate 'string
                                                                                   name-string
                                                                                   "-addr"))
                                               name-string)))))
           (map nil
                (lambda (argument name &aux (name-string (clutter-symbol-name name)))
                  (setf (llvm:value-name argument) (concatenate 'string name-string "-arg")
                        (gethash name (compiler-env-bindings inner-env))
                        (aprog1 (llvm:build-alloca new-builder (llvm:int32-type)
                                                   name-string)
                          (llvm:build-store new-builder argument it)))))
           (rest params)
           args)
         ;; Compile body and return the value of the last form
         (llvm:position-builder-at-end new-builder begin)
         (loop for (form . remaining) on body
               for result = (compile-form new-builder form inner-env)
               unless remaining do
                 (llvm:build-ret new-builder result))
         ;; Complete entry block
         (llvm:position-builder-at-end new-builder entry)
         (llvm:build-br new-builder begin)
         ;; Construct closure struct in the caller (context pointer + function pointer) as the return value
         ;; TODO: Heap-allocate closure and referenced variables when necessary
         (if closing-over
             (aprog1 (add-entry-alloca (compiler-env-func env) value-type "closure")
               (let ((context (add-entry-alloca (compiler-env-func env) context-type "local-context")))
                 (loop for var in closing-over
                       for index from 0
                       do (llvm:build-store builder (compiler-lookup var env)
                                            (llvm:build-struct-gep builder context index (concatenate 'string (clutter-symbol-name var) "-addr"))))
                 (llvm:build-store builder
                                   (llvm:build-pointer-cast builder context
                                                            (llvm:pointer-type (llvm:int8-type))
                                                            "pointer")
                                   (llvm:build-struct-gep builder it 0 "local-context-addr"))
                 (llvm:build-store builder
                                   func
                                   (llvm:build-struct-gep builder it 1 "function-addr"))))
             (llvm:const-struct (vector (llvm:undef (llvm:pointer-type (llvm:int8-type)))
                                        func) nil)))
    (llvm:dispose-builder new-builder)))

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

(defun emit-externs ()
  (setf *alloc* (llvm:add-function *module* "GC_malloc" (llvm:function-type (llvm:pointer-type (llvm:int8-type)) (vector (llvm:int32-type))))))

(defun emit-main (builder closure)
  (let* ((func (llvm:add-function *module* "main" (llvm:function-type (llvm:int32-type)
                                                                      (vector (llvm:int32-type)
                                                                              (llvm:pointer-type
                                                                               (llvm:pointer-type
                                                                                (llvm:int8-type)))))))
         (entry (llvm:append-basic-block func "entry"))
         (params (llvm:params func))
         (argc (first params))
         (argv (second params)))
    (llvm:position-builder-at-end builder entry)
    (setf (llvm:value-name argc) "argc"
          (llvm:value-name argv) "argv")
    (llvm:build-ret builder (build-closure-call builder closure (list argc)))))

(defun cltr-compile (expr &aux builder pm)
  (unwind-protect
       (progn
         (setf *module* (llvm:make-module "clutter")
               builder (llvm:make-builder)
               pm (llvm:create-pass-manager))

         (emit-externs)

         (emit-main builder (compile-form builder expr *root-compiler-env*))

         (llvm:add-promote-memory-to-register-pass pm)
         (llvm:run-pass-manager pm *module*)

         (llvm:dump-module *module*)
         (llvm:verify-module *module*))
    
    (llvm:dispose-builder builder)
    (llvm:dispose-pass-manager pm)
    (when *module*
      (llvm:dispose-module *module*)
      (setf *module* nil))))
