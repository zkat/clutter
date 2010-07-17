(in-package :clutter)

(defvar *ir-builder* (llvm:llvmcreatebuilder))

(defvar *module* (llvm:llvmmodulecreatewithname "root"))

(defvar *functions* (make-hash-table :test 'eq))

(defun verify ()
  (cffi:with-foreign-objects ((error '(:pointer :char)) (error-addr :pointer))
    (setf (cffi:mem-aref error-addr :pointer) error)
    (when (llvm:llvmverifymodule *module* :llvmprintmessageaction error-addr)
      ;; TODO: Why does llvm kill the lisp here?
        (error "Module has errors"))
    ;(llvm:llvmdisposemessage error-addr) ;segfaults
))

(defun reset ()
  (llvm:llvmdisposemodule *module*)
  (setf *module* (llvm:llvmmodulecreatewithname "root")))

(defun insert-bb-after (bb name &aux (next (llvm:llvmgetnextbasicblock bb)))
  ;; TODO: Why is this necessary?
  (if (cffi:null-pointer-p next)
      (llvm:llvmappendbasicblock (llvm:llvmgetbasicblockparent bb) name)
      (llvm:llvminsertbasicblock next name)))

(defun compile-if (function condition true-code false-code &aux return-value)
  (let* ((current (llvm:llvmgetinsertblock *ir-builder*))
         (true-block (insert-bb-after current "if-true"))
         (false-block (insert-bb-after true-block "if-false"))
         (continue-block (insert-bb-after false-block "if-continue")))
    (llvm:llvmbuildcondbr *ir-builder*
                          (llvm:llvmbuildtrunc *ir-builder* (compile-sexp condition function) (llvm:llvmint1type) "boolean")
                          true-block
                          false-block)
    (llvm:llvmpositionbuilderatend *ir-builder* continue-block)
    (setf return-value (llvm:llvmbuildphi *ir-builder* (llvm:llvmint32type) "if-result"))

    (llvm:llvmpositionbuilderatend *ir-builder* true-block)
    ;; Get insert block in case true-code contains other blocks
    (llvm:llvmaddincoming return-value (compile-sexp true-code function) (llvm:llvmgetinsertblock *ir-builder*))
    (llvm:llvmbuildbr *ir-builder* continue-block)

    (llvm:llvmpositionbuilderatend *ir-builder* false-block)
    ;; Get insert block in case false-code contains other blocks
    (llvm:llvmaddincoming return-value (compile-sexp false-code function) (llvm:llvmgetinsertblock *ir-builder*))
    (llvm:llvmbuildbr *ir-builder* continue-block)
      
    (llvm:llvmpositionbuilderatend *ir-builder* continue-block))
  return-value)

(defun compile-function (name args &rest body &aux (func (llvm:llvmaddfunction *module* (symbol-name name) (llvm:llvmfunctiontype (llvm:llvmint32type) nil nil))))
  (declare (ignore args))
  (setf (gethash name *functions*) func)
  (llvm:llvmsetfunctioncallconv func :llvmccallconv)
  (let ((entry (llvm:llvmappendbasicblock func "entry")))
    (llvm:llvmpositionbuilderatend *ir-builder* entry)
    (let ((last-val))
      (mapc #'(lambda (sexp) (setf last-val (compile-sexp sexp func))) body)
      (llvm:llvmbuildret *ir-builder* last-val))))

(defun compile-definer (subenv &rest args)
  (case subenv
    (fun (apply #'compile-function args))))

(defun compile-sexp (code &optional function)
  (cond
    ((listp code)
     (case (first code)
       (def (apply #'compile-definer (rest code)))
       (if (apply #'compile-if function (rest code)))
       (t (llvm:llvmbuildcall *ir-builder* (gethash (first code) *functions*) nil "result"))))
    ((integerp code)
     (llvm:llvmconstint (llvm:llvmint32type) code))))
