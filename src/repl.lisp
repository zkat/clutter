;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; REPL
;;;

(defun repl ()
  (let ((*global-fenv* (extend *global-fenv*
                               (list '|die|)
                               (list (make-clutter-function
                                      :function (lambda (args)
                                                  (return-from repl args)))))))
    (loop
       (princ "> ")
       (with-simple-restart (abort "Return to Clutter's toplevel")
         (format t "~%=> ~S" (evaluate (clutter-read) *global-env* *global-fenv*)))
       (fresh-line))))
