(in-package #:clutter)

(declaim (optimize debug))

(defvar *context* (llvm:global-context))
(defvar *module*)
(defvar *builder*)

(let ((llvm-inited nil))
 (defun init-llvm ()
   (unless llvm-inited
     (llvm:initialize-native-target)
     (setf llvm-inited t))))

(defun compile-form (form env)
  (typecase form
    (integer (llvm:const-int (llvm:int32-type) form))
    (clutter-symbol (llvm:build-load *builder* (lookup form env) (clutter-symbol-name form)))
    ;; TODO: Recursively compile funcs
    (list (llvm:build-call *builder* (lookup (car form) env)
                           (make-array (length (rest form))
                                       :initial-contents (mapcar (rcurry #'compile-form env)
                                                                 (rest form)))
                           ""))))

(defun compile-func (func env)
  (let* ((op (clutter-function-operator func))
         (args (clutter-operator-args op))
         (argtypes (make-array (length args) :initial-element (llvm:int32-type)))
         (ftype (llvm:function-type (llvm:int32-type) argtypes))
         (fobj (llvm:add-function *module* (clutter-symbol-name (clutter-operator-name op)) ftype))
         (fenv (make-env env)))
    (llvm:position-builder-at-end *builder*
                                  (llvm:append-basic-block fobj "entry"))
    (mapc (lambda (arg name)
            (setf (llvm:value-name arg) (clutter-symbol-name name))
            (extend fenv name
                    (llvm:build-alloca *builder* (llvm:type-of arg) (clutter-symbol-name name)))
            (llvm:build-store *builder* arg (lookup name fenv)))
          (llvm:params fobj)
          args)
    (let ((ret))
      (mapc (compose (lambda (x) (setf ret x))
                     (rcurry #'compile-form fenv))
            (clutter-operator-body op))
      (llvm:build-ret *builder* ret))
    (unless (llvm:verify-function fobj)
      (error "Invalid function ~A" func))))

(defun clutter-compile (main &optional (output "binary"))
  "Write a binary to OUTPUT which invokes clutter function MAIN on execution."
  (init-llvm)
  (setf *module* (llvm:make-module output))
  (setf *builder* (llvm:make-builder))
  (unwind-protect
       (progn
         (compile-func main (make-env))
         (llvm:dump-module *module*))
    (llvm:dispose-module *module*)
    (llvm:dispose-builder *builder*)))
