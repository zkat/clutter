;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* *clutter-readtable*))

(IN-PACKAGE #:CLUTTER.SYMBOLS)

;; (def λ repl (&key stream)
;;   (loop (print (eval (read stream)))))

(CL:DEFUN CLUTTER.SYMBOLS:repl (CL:&KEY stream)
  (CL:LOOP (print (eval (read stream)))))
