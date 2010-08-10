(in-package :clutter)
(declaim (optimize (debug 3)))

(defvar *ir-builder* (%llvm:create-builder))

(defvar *module* (%llvm:module-create-with-name "root"))

(defvar *functions* (make-hash-table :test 'eq))

(defstruct env
  (heap-p nil)
  (closure-p nil)
  (function)
  (table (make-hash-table :test 'eq)))

(defun env-heapify (env)
  (unless (env-heap-p env)
    (let ((builder (%llvm:create-builder))
          (hash-table (env-table env))
          (values (list)))
      (maphash
       (lambda (key value)
         (push (cons key value) values))
       hash-table)
      (let ((bb (%llvm:get-first-basic-block (env-function env))))
        (%llvm:position-builder builder bb (%llvm:get-first-instruction bb)))
      ;; Place env struct allocation
      (let ((new-frame (%llvm:build-malloc builder
                                           (llvm:struct-type (mapcar #'%llvm:get-element-type
                                                                     (mapcar #'%llvm:type-of
                                                                             (mapcar #'cdr values))))
                                           "heap-frame")))
        ;; Replace stack vars with struct members
        (loop with index = -1
              for (key . value) in values
              for new-value = (llvm:build-gep builder new-frame (list (%llvm:const-int (%llvm:int32-type) 0 0)
                                                                      (%llvm:const-int (%llvm:int32-type) (incf index) 0))
                                                                (%llvm:get-value-name value))
              do (%llvm:replace-all-uses-with value new-value)
                 (print (%llvm:get-value-name value))
                 (%llvm:delete-instruction value)
                 (setf (gethash key hash-table) new-value))))
    (setf (env-heap-p env) t)))

(defvar *global-env* (make-env :heap-p t))

(defvar *scope* (list *global-env*)
  "Keeps track of the environments that will be visible when execution is at the insertion point compiled.")

(defun toplevelp ()
  (cdr *scope*))

(defun current-func (&aux (insert-block (%llvm:get-insert-block *ir-builder*)))
  (if (cffi:null-pointer-p insert-block)
      nil
      (let ((func (%llvm:get-basic-block-parent insert-block)))
        (if (cffi:null-pointer-p func)
            nil
            func))))

(defun insert-bb-after (bb name &aux (new-bb (%llvm:append-basic-block (%llvm:get-basic-block-parent bb) name)))
  (%llvm:move-basic-block-after new-bb bb)
  new-bb)

(defun compile-if (condition true-code false-code &aux return-value)
  (let* ((true-block (insert-bb-after (%llvm:get-insert-block *ir-builder*) "if-true"))
         (false-block (insert-bb-after true-block "if-false"))
         (continue-block (insert-bb-after false-block "if-continue")))
    (%llvm:build-cond-br *ir-builder*
                          (%llvm:build-trunc *ir-builder* (compile-sexp condition) (%llvm:int1-type) "boolean")
                          true-block
                          false-block)
    (%llvm:position-builder-at-end *ir-builder* continue-block)
    (setf return-value (llvm:build-phi *ir-builder* (%llvm:int32-type) "if-result"))

    (%llvm:position-builder-at-end *ir-builder* true-block)
    ;; Get insert block in case true-code contains other blocks
    (llvm:add-incoming return-value (compile-sexp true-code) (%llvm:get-insert-block *ir-builder*))
    (%llvm:build-br *ir-builder* continue-block)

    (%llvm:position-builder-at-end *ir-builder* false-block)
    ;; Get insert block in case false-code contains other blocks
    (llvm:add-incoming return-value (compile-sexp false-code) (%llvm:get-insert-block *ir-builder*))
    (%llvm:build-br *ir-builder* continue-block)
      
    (%llvm:position-builder-at-end *ir-builder* continue-block))
  return-value)

(defun lookup-var (name)
  (loop for env in *scope*
        for value = (gethash name (env-table env))
        when value
          do (return (%llvm:build-load *ir-builder* value (symbol-name name)))))

(defun compile-function (name args &rest body &aux func (env (make-env)))
  ;; Get our function handle
  (let* ((fname (symbol-name name)))
    (unless (cffi:null-pointer-p (%llvm:get-named-function *module* fname))
      (error "Redefining function ~A" name))
    (setf func (%llvm:add-function *module* fname
                                   (llvm:function-type (%llvm:int32-type) (loop repeat (length args) collecting (%llvm:int32-type)))))
    (setf (gethash name *functions*) func))
  ;; Initialization
  (%llvm:set-function-call-conv func :c)
  (setf (env-function env) func)
  (let ((entry (%llvm:append-basic-block func "entry"))
        (*scope* (cons env *scope*)))
    (%llvm:position-builder-at-end *ir-builder* entry)
    ;; Allocate argument stack space and initialize env
    (loop for index from 0
          for param = (%llvm:get-param func index)
          for arg in args
          for name = (symbol-name arg)
          for alloca = (%llvm:build-alloca *ir-builder* (%llvm:int32-type) name)
          do (%llvm:set-value-name param name)
             (%llvm:build-store *ir-builder* param alloca)
             (setf (gethash arg (env-table env)) alloca))
    ;; Compile function body and return instruction
    (%llvm:build-ret *ir-builder* (car (last (mapcar #'compile-sexp body))))
    ;; Check for errors (TODO: Informative error message)
    (when (%llvm:verify-function func :print-message)
      (error "Invalid function")))
  func)

(defun get-llvm-type (clutter-type)
  (ecase clutter-type
    (i8* (%llvm:pointer-type (%llvm:int8-type) 0))
    (i8 (%llvm:int8-type))
    (i16 (%llvm:int16-type))
    (i32 (%llvm:int32-type))
    (i64 (%llvm:int64-type))))

(defun compile-c-binding (name return-type &rest args &aux (fname (symbol-name name)))
  (let ((old-func (%llvm:get-named-function *module* fname)))
    (unless (cffi:null-pointer-p old-func)
      (error "Trying to bind existing function ~A" name)))
  (let ((func (%llvm:add-function *module* (symbol-name name)
                                  (llvm:function-type (get-llvm-type return-type)
                                                      (mapcar #'get-llvm-type (mapcar #'second args))))))
    (%llvm:set-linkage func :external)
    (setf (gethash name *functions*) func)))

(defun compile-definer (subenv &rest body)
  (case subenv
    (fun (apply #'compile-function body))
    (cfun (apply #'compile-c-binding body))
    (var (destructuring-bind (name &optional initializer) body
           (when (gethash name (env-table (car *scope*)))
             (error "Attempting to define existing var ~A" name))
           (let ((value)
                 (name-str (symbol-name name)))
             (if (toplevelp)
                 (progn (setf value (%llvm:build-alloca *ir-builder* (%llvm:int32-type) name-str))
                        (when initializer
                          (%llvm:build-store *ir-builder* (compile-sexp initializer) value)))
                 (progn (setf value (%llvm:add-global *module* (%llvm:int32-type) name-str))
                        (%llvm:set-linkage value :common)
                        (%llvm:set-initializer value (%llvm:const-int (%llvm:int32-type) 0 0))))
             (setf (gethash name (env-table (first *scope*))) value))))))

(defun compile-sexp (code)
  (cond
    ((listp code)
     (case (first code)
       (def (apply #'compile-definer (rest code)))
       (if (apply #'compile-if (rest code)))
       (= (destructuring-bind (a b) (rest code)
            (%llvm:build-icmp *ir-builder* :eq (compile-sexp a) (compile-sexp b) "equality")))
       (* (destructuring-bind (a b) (rest code)
            (%llvm:build-mul *ir-builder* (compile-sexp a) (compile-sexp b) "product")))
       (/ (destructuring-bind (a b) (rest code)
            (%llvm:build-sdiv *ir-builder* (compile-sexp a) (compile-sexp b) "quotient")))
       (- (destructuring-bind (a b) (rest code)
            (%llvm:build-sub *ir-builder* (compile-sexp a) (compile-sexp b) "difference")))
       (+ (destructuring-bind (a b) (rest code)
            (%llvm:build-add *ir-builder* (compile-sexp a) (compile-sexp b) "sum")))
       (t (llvm:build-call *ir-builder* (gethash (first code) *functions*) (mapcar #'compile-sexp (rest code)) "result"))))
    ((symbolp code)
     (or (lookup-var code)
         (error "Undefined variable: ~A" code)))
    ((integerp code)
     (%llvm:const-int (%llvm:int32-type) code nil))))
