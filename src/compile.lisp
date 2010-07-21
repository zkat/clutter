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
  (table (make-hash-table :test 'eq)))

(defvar *environments* (make-hash-table :hash-function #'cffi:pointer-address
                                        :test #'sb-sys:sap=)
  "Maps functions to their environments")

(defvar *scope* ()
  "Keeps track of the environments that will be visible when execution is at the insertion point compiled.")

(defun insert-func ()
  (%llvm:get-basic-block-parent (%llvm:get-insert-block *ir-builder*)))

(defun compile-and-eval (expr)
  (if (and (listp expr) (eq (first expr) 'def))
      (apply #'compile-definer (rest expr))
      (let ((func (compile-sexp `(def fun nil () ,expr))))
        (llvm:verify-module *module*)
        (%llvm:generic-value-to-int (%llvm:run-function *compiler* func 0 (cffi:null-pointer)) nil))))

(defun init-llvm ()
  (%llvm:link-in-jit)
  (%llvm:initialize-native-target))

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
  (%llvm:get-param (insert-func) (gethash name (env-table (gethash (insert-func) *environments*)))))

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
  (loop for index from 0
        for arg in args
        do (setf (gethash arg (env-table env)) index)
           (%llvm:set-value-name (%llvm:get-param func index) (symbol-name arg))
        finally (setf (gethash func *environments*) (env-table env)))
  (%llvm:set-function-call-conv func :c)
  (let ((entry (%llvm:append-basic-block func "entry")))
    (%llvm:position-builder-at-end *ir-builder* entry)
    (let ((*scope* (cons env *scope*)))
      (%llvm:build-ret *ir-builder* (car (last (mapcar #'compile-sexp body)))))
    (when (%llvm:verify-function func :print-message)
      (error "Invalid function")))
  (%llvm:recompile-and-relink-function *compiler* func)
  func)

(defun compile-definer (subenv name args &rest body)
  (case subenv
    (fun (apply #'compile-function name args body))))

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
     (lookup-var code))
    ((integerp code)
     (%llvm:const-int (%llvm:int32-type) code nil))))
