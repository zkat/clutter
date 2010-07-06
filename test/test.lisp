(in-package :clutter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(Eos:def-suite Eos:run! Eos:is Eos:in-suite Eos:signals))
  (export 'run-all-tests))

(defmacro test (name &body body)
  `(Eos:test ,name ,@body))

(macrolet ((import-Eos-test-macros (&rest names)
             `(progn
                ,@(mapcar #'(lambda (name)
                              `(defmacro ,name (&rest message-args)
                                 `(,(intern (symbol-name ',name) :Eos)
                                    ,@message-args)))
                          names))))
  (import-Eos-test-macros pass fail skip))

(defun eval-clutter-code (code)
  (with-input-from-string (*standard-input* code)
    (evaluate (clutter-read))))

(def-suite clutter)

(defun run-all-tests ()
  (run! 'clutter))

