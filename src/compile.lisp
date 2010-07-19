(in-package :clutter)
(declaim (optimize (debug 3)))

(defvar *ir-builder* (%llvm:create-builder))

(defvar *module* (%llvm:module-create-with-name "root"))

(defvar *compiler* (progn
                     (%llvm:initialize-native-target)
                     (llvm:create-jit-compiler-for-module *module* 2)))

(defvar *functions* (make-hash-table :test 'eq))

;;; TODO: This shouldn't be O(N) lookup.
(defvar *function-params* ())

(defun compile-and-eval (expr)
  (if (and (listp expr) (eq (first expr) 'def))
      (apply #'compile-definer (rest expr))
      (let ((func (compile-sexp `(def fun nil () ,expr))))
        (llvm:verify-module *module*)
        (%llvm:generic-value-to-int (%llvm:run-function *compiler* func 0 (cffi:null-pointer)) nil))))

(defun init-llvm ()
  (%llvm:link-in-jit)
  (%llvm:initialize-native-target))

(defun insert-bb-after (bb name)
  (%llvm:move-basic-block-after (%llvm:append-basic-block (%llvm:get-basic-block-parent bb) name) bb))

(defun compile-if (function condition true-code false-code &aux return-value)
  (let* ((true-block (insert-bb-after (%llvm:get-insert-block *ir-builder*) "if-true"))
         (false-block (insert-bb-after true-block "if-false"))
         (continue-block (insert-bb-after false-block "if-continue")))
    (%llvm:build-cond-br *ir-builder*
                          (%llvm:build-trunc *ir-builder* (compile-sexp condition function) (%llvm:int1-type) "boolean")
                          true-block
                          false-block)
    (%llvm:position-builder-at-end *ir-builder* continue-block)
    (setf return-value (llvm:build-phi *ir-builder* (%llvm:int32-type) "if-result"))

    (%llvm:position-builder-at-end *ir-builder* true-block)
    ;; Get insert block in case true-code contains other blocks
    (llvm:add-incoming return-value (compile-sexp true-code function) (%llvm:get-insert-block *ir-builder*))
    (%llvm:build-br *ir-builder* continue-block)

    (%llvm:position-builder-at-end *ir-builder* false-block)
    ;; Get insert block in case false-code contains other blocks
    (llvm:add-incoming return-value (compile-sexp false-code function) (%llvm:get-insert-block *ir-builder*))
    (%llvm:build-br *ir-builder* continue-block)
      
    (%llvm:position-builder-at-end *ir-builder* continue-block))
  return-value)

(defun compile-function (name args &rest body &aux func)
  (let* ((fname (symbol-name name))
         (old-func (%llvm:get-named-function *module* fname)))
    (if (cffi:null-pointer-p old-func)
        (progn (setf func (%llvm:add-function *module* fname
                                              (llvm:function-type (%llvm:int32-type) (loop repeat (length args) collecting (%llvm:int32-type)))))
                 (setf (gethash name *functions*) func))
        (progn (unless (eq name nil)
                 (format t "Overriding ~A~%" name))
               (setf func old-func)
               (loop for block = (%llvm:get-first-basic-block func)
                     until (cffi:null-pointer-p block)
                     do (%llvm:delete-basic-block block)))))
  (loop with arg-table = (make-hash-table :test 'eq)
        for index from 0
        for arg in args
        do (setf (gethash arg arg-table) index)
           (%llvm:set-value-name (%llvm:get-param func index) (symbol-name arg))
        finally (push (cons func arg-table) *function-params*))
  (%llvm:set-function-call-conv func :c)
  (let ((entry (%llvm:append-basic-block func "entry")))
    (%llvm:position-builder-at-end *ir-builder* entry)
    (let ((last-val))
      (mapc #'(lambda (sexp) (setf last-val (compile-sexp sexp func))) body)
      (%llvm:build-ret *ir-builder* last-val)
      (when (%llvm:verify-function func :print-message)
        (error "Invalid function"))))
  (%llvm:recompile-and-relink-function *compiler* func)
  func)

(defun compile-definer (subenv name args &rest body)
  (case subenv
    (fun (apply #'compile-function name args body))))

(defun compile-sexp (code &optional function)
  (cond
    ((listp code)
     (case (first code)
       (def (apply #'compile-definer (rest code)))
       (if (apply #'compile-if function (rest code)))
       (= (destructuring-bind (a b) (rest code)
            (%llvm:build-icmp *ir-builder* :eq (compile-sexp a function) (compile-sexp b function) "equality")))
       (* (destructuring-bind (a b) (rest code)
            (%llvm:build-mul *ir-builder* (compile-sexp a function) (compile-sexp b function) "product")))
       (/ (destructuring-bind (a b) (rest code)
            (%llvm:build-sdiv *ir-builder* (compile-sexp a function) (compile-sexp b function) "quotient")))
       (- (destructuring-bind (a b) (rest code)
            (%llvm:build-sub *ir-builder* (compile-sexp a function) (compile-sexp b function) "difference")))
       (+ (destructuring-bind (a b) (rest code)
            (%llvm:build-add *ir-builder* (compile-sexp a function) (compile-sexp b function) "sum")))
       (t (llvm:build-call *ir-builder* (gethash (first code) *functions*) (mapcar (lambda (sexp) (compile-sexp sexp function)) (rest code)) "result"))))
    ((symbolp code)
     (let ((function (%llvm:get-basic-block-parent (%llvm:get-insert-block *ir-builder*))))
       (%llvm:get-param function (gethash code (cdr (assoc function *function-params* :test #'sb-sys:sap=))))))
    ((integerp code)
     (%llvm:const-int (%llvm:int32-type) code nil))))
