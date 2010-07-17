(in-package :clutter)

(defvar *ir-builder* (llvm:llvmcreatebuilder))

(defvar *module* (llvm:llvmmodulecreatewithname "root"))

(defvar *functions* (make-hash-table :test 'eq))

(defun verify ()
  (cffi:with-foreign-objects ((error '(:pointer :char)) (error-addr :pointer))
    (setf (cffi:mem-aref error-addr :pointer) error)
    (when (llvm:llvmverifymodule *module* :llvmprintmessageaction error-addr)
        (error "Module has errors"))
    ;(llvm:llvmdisposemessage error-addr) ;segfaults
))

(defun reset ()
  (llvm:llvmdisposemodule *module*)
  (setf *module* (llvm:llvmmodulecreatewithname "root")))

(defun compile-if (function condition true-code false-code &aux return-value)
  (let ((true-block (llvm:llvmappendbasicblock function "if-true"))
        (false-block (llvm:llvmappendbasicblock function "if-false"))
        (continue-block (llvm:llvmappendbasicblock function "if-continue")))
    (llvm:llvmpositionbuilderatend *ir-builder* (llvm:llvmgetpreviousbasicblock true-block))
    (llvm:llvmbuildcondbr *ir-builder*
                          (llvm:llvmbuildicmp *ir-builder* :llvmintne
                                              (llvm:llvmconstint (llvm:llvmint32type) 0)
                                              (compile-sexp condition function))
                          true-block
                          false-block)
    (llvm:llvmpositionbuilderatend *ir-builder* continue-block)
    (setf return-value (llvm:llvmbuildphi *ir-builder* (llvm:llvmint32type) "if-result"))
    (llvm:llvmpositionbuilderatend *ir-builder* true-block)
    (llvm:llvmaddincoming return-value (compile-sexp true-code function) true-block)
    (llvm:llvmbuildbr *ir-builder* continue-block)

    (llvm:llvmpositionbuilderatend *ir-builder* false-block)
    (llvm:llvmaddincoming return-value (compile-sexp false-code function) false-block)
    (llvm:llvmbuildbr *ir-builder* continue-block)
      
    (llvm:llvmpositionbuilderatend *ir-builder* continue-block))
  return-value)

(defun compile-sexp (code &optional function)
  (cond
    ((listp code)
     (case (first code)
       (def (apply #'compile-definer (rest code)))
       (if (apply #'compile-if function (rest code)))
       (t (llvm:llvmbuildcall *ir-builder* (gethash (first code) *functions*) nil "result"))))
    ((integerp code)
     (llvm:llvmconstint (llvm:llvmint32type) code))))

(defun compile-definer (subenv &rest args)
  (case subenv
    (fun (apply #'compile-function args))))

(defun compile-function (name args &rest body &aux (func (llvm:llvmaddfunction *module* (symbol-name name) (llvm:llvmfunctiontype (llvm:llvmint32type) nil nil))))
  (declare (ignore args))
  (setf (gethash name *functions*) func)
  (llvm:llvmsetfunctioncallconv func :llvmccallconv)
  (let ((entry (llvm:llvmappendbasicblock func "entry")))
    (llvm:llvmpositionbuilderatend *ir-builder* entry)
    (let ((last-val))
      (mapc #'(lambda (sexp) (setf last-val (compile-sexp sexp func))) body)
      (llvm:llvmbuildret *ir-builder* last-val))))
