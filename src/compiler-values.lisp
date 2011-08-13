(in-package :clutter)

(defclass invokable () ())

(defgeneric build-invocation (builder invokable arg-forms env))

(defclass compiled-value ()
  ((llvm-handle :accessor llvm-handle
                :initarg :llvm-handle
                :initform (error "Compiled values must have LLVM handles"))
   (interpreter-value :accessor interpreter-value
                      :initarg :interpreter-value)))

(defun make-value (llvm-handle &optional (interpreter-value nil ivalp))
  (if ivalp
      (make-instance 'compiled-value
                     :interpreter-value interpreter-value
                     :llvm-handle llvm-handle)
      (make-instance 'compiled-value
                     :llvm-handle llvm-handle)))

(defun llvm-type (compiled-value)
  (llvm:type-of (llvm-handle compiled-value)))

(defclass compiled-func (invokable compiled-value) ())

(defclass compiled-fexpr (invokable compiled-value) ())

(defun build-closure-call (builder closure args &aux function context)
  (setf closure (llvm-handle closure))
  (if (llvm:constantp closure)
      (setf context (llvm:const-extract-value closure (vector 0))
            function (llvm:const-extract-value closure (vector 1)))
      (setf context (llvm:build-load builder (llvm:build-struct-gep builder closure 0 "context-addr") "context")
            function (llvm:build-load builder (llvm:build-struct-gep builder closure 1 "function-addr") "function")))
  (llvm:build-call builder function
                   (coerce (cons context (mapcar #'llvm-handle args)) 'vector)
                   "result"))

(defmethod build-invocation (builder (invokable compiled-func) arg-forms env)
  (make-value
   (build-closure-call builder (llvm-handle invokable)
                       (mapcar (rcurry (curry #'compile-form builder) env)
                               arg-forms))))

(defmethod build-invocation (builder (invokable compiled-fexpr) arg-forms env)
  ;; TODO: Pass dynamic environment
  (declare (ignore env))
  (make-value
   (build-closure-call builder (llvm-handle invokable)
                       (mapcar (curry #'compile-constant builder)
                               arg-forms))))

(defclass primitive (invokable)
  ((compiler :accessor compiler
             :initarg :compiler
             :initform (error "Primitives must have a compiler!"))))

(defclass primitive-func (primitive) ())

(defclass primitive-fexpr (primitive) ())

(defmethod build-invocation (builder (invokable primitive-func) arg-forms env)
  (apply (compiler invokable) builder
         (mapcar (rcurry (curry #'compile-form builder) env)
                 arg-forms)))

(defmethod build-invocation (builder (invokable primitive-fexpr) arg-forms env)
  (apply (compiler invokable) builder env arg-forms))
