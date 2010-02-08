;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; REPL
;;;

(defun repl ()
  (let ((*readtable* (copy-readtable *readtable*))
        (*global-fenv* (extend *global-fenv*
                               (list '|die|)
                               (list (lambda (args)
                                       (return-from repl args))))))
    (setf (readtable-case *readtable*) :preserve)
    (loop
      (princ "> ")
      (with-simple-restart (abort "Return to Clutter's toplevel")
        (prin1 (evaluate (read) *global-env* *global-fenv*)))
      (fresh-line))))
