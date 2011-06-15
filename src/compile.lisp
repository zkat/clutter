(in-package #:clutter)

(declaim (optimize (debug 3)))

(defvar *module*)

(defvar *compiler-prim-funcs* (make-hash-table :test 'equal))
(defvar *compiler-prim-fexprs* (make-hash-table :test 'equal))
(defvar *compiled-funcs* (make-hash-table :test 'eq))

(defmacro def-compiler-primfun (name args &body body)
  `(setf (gethash ,name *compiler-prim-funcs*) (lambda ,args ,@body)))

(defmacro def-compiler-primfexpr (name args &body body)
  `(setf (gethash ,name *compiler-prim-fexprs*) (lambda ,args ,@body)))

(defun compiled-func (clutter-function)
  (let ((result (gethash clutter-function *compiled-funcs*)))
    (if result
        result
        (setf (gethash clutter-function *compiled-funcs*)
              (error "Constant function compilation unimplemented!")))))

(defun compile-symbol (builder symbol env)
  (error "PLACEHOLDER"))

(defun compile-invocation (builder invocation env)
  (destructuring-bind (combiner . args) invocation
    (typecase combiner
      (clutter-symbol (if env
                          (error "Environments not implemented!")
                          (acond
                            ((gethash (clutter-symbol-name combiner)
                                      *compiler-prim-funcs*)
                             (apply it builder
                                    (mapcar (rcurry (curry #'compile-form builder) env)
                                            args)))
                            ((gethash (clutter-symbol-name combiner)
                                      *compiler-prim-fexprs*)
                             (print (apply it builder env args)))
                            (t
                             (error "Undefined combiner ~A" combiner)))))
      (clutter-function (apply (compiled-func combiner) builder
                               (mapcar (rcurry (curry #'compile-form builder) env)
                                       args)))
      (clutter-operative (error "Tried to compile an operative: ~A" combiner)))))

(defun compile-constant (builder value env)
  (declare (ignore env))
  (typecase value
    (integer (llvm:const-int (llvm:int32-type) value nil))
    (t (error "Unsupported compiletime constant!"))))

(defun compile-form (builder form env)
  (typecase form
    (symbol (compile-symbol     builder form env))
    (list   (compile-invocation builder form env))
    (t      (compile-constant   builder form env))))

(def-compiler-primfun "+" (builder x y)
  (llvm:build-add builder x y "sum"))
(def-compiler-primfun "-" (builder x y)
  (llvm:build-sub builder x y "difference"))
(def-compiler-primfun "*" (builder x y)
  (llvm:build-mul builder x y "product"))
(def-compiler-primfun "/" (builder x y)
  (llvm:build-s-div builder x y "quotient"))

(def-compiler-primfexpr "nlambda" (builder env name args &rest body &aux (new-builder (llvm:make-builder)))
  (declare (ignore builder))
  (unwind-protect
       (let* ((func (llvm:add-function *module* name (llvm:function-type (llvm:int32-type) (vector (llvm:int32-type)))))
              (entry (llvm:append-basic-block func "entry")))
         ;; TODO: New environment
         (llvm:position-builder-at-end new-builder entry)
         (loop for (form . remaining) on body
               for result = (compile-form new-builder form nil)
               unless remaining do
               (llvm:build-ret new-builder result)))
    (llvm:dispose-builder new-builder)))

(defun cltr-compile (expr &aux builder)
  (unwind-protect
       (progn
         (setf *module* (llvm:make-module "clutter"))
         (setf builder (llvm:make-builder))
         
         (compile-form builder expr nil)

         (llvm:verify-module *module*)
         (llvm:dump-module *module*))
    
    (llvm:dispose-builder builder)
    (when *module*
      (llvm:dispose-module *module*)
      (setf *module* nil))))