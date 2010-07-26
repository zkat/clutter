(in-package :clutter)
(declaim (optimize (debug 3)))

(defvar *ir-builder* (%llvm:create-builder))

(defvar *module* (%llvm:module-create-with-name "root"))

(defvar *compiler* (progn
                     (%llvm:initialize-native-target)
                     (llvm:create-jit-compiler-for-module *module* 2)))

(defvar *functions* (make-hash-table :test 'eq))

(defstruct env
  (heap-p nil)
  (closure-p nil)
  (function)
  (table (make-hash-table :test 'eq)))

(defvar *global-env* (make-env :heap-p t))

(defvar *environments* (make-hash-table :hash-function #'cffi:pointer-address
                                        :test #'cffi:pointer-eq)
  "Maps functions to their environments")

(defvar *scope* (list *global-env*)
  "Keeps track of the environments that will be visible when execution is at the insertion point compiled.")

(defun insert-func (&aux (insert-block (%llvm:get-insert-block *ir-builder*)))
  (if (cffi:null-pointer-p insert-block)
      nil
      (let ((func (%llvm:get-basic-block-parent insert-block)))
        (if (cffi:null-pointer-p func)
            nil
            func))))

(defun compile-and-eval (expr)
  (if (and (listp expr) (eq (first expr) 'def))
      (apply #'compile-definer (rest expr))
      (let ((func (compile-sexp `(def fun nil () ,expr))))
        (llvm:verify-module *module*)
        (%llvm:generic-value-to-int (%llvm:run-function *compiler* func 0 (cffi:null-pointer)) nil))))

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
  (let* ((fname (symbol-name name))
         (old-func (%llvm:get-named-function *module* fname)))
    (if (cffi:null-pointer-p old-func)
        (progn (setf func (%llvm:add-function *module* fname
                                              (llvm:function-type (%llvm:int32-type) (loop repeat (length args) collecting (%llvm:int32-type)))))
                 (setf (gethash name *functions*) func))
        (progn (unless (eq name nil)
                 (format t "Overriding ~A~%" name))
               (setf func old-func)
               (loop for block = (%llvm:get-last-basic-block func)
                     until (cffi:null-pointer-p block)
                     do (%llvm:delete-basic-block block)))))
  (%llvm:set-function-call-conv func :c)
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
             (setf (gethash arg (env-table env)) alloca)
          finally (setf (gethash func *environments*) env))
    ;; Compile function body and return instruction
    (%llvm:build-ret *ir-builder* (car (last (mapcar #'compile-sexp body))))
    ;; Check for errors (TODO: Informative error message)
    (when (%llvm:verify-function func :print-message)
      (error "Invalid function")))
  (%llvm:recompile-and-relink-function *compiler* func)
  func)

(defun compile-definer (subenv &rest body)
  (case subenv
    (fun (apply #'compile-function body))
    (var (destructuring-bind (name &optional initializer) body

           (let ((value)
                 (name-str (symbol-name name)))
             (if (insert-func)
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
