;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; REPL
;;;

(defun repl ()
  (loop
     (format t "~&> ")
     (with-simple-restart (abort "Return to Clutter's toplevel")
       (format t "~&=> ~S" (evaluate (clutter-read))))))

#+sbcl
(defun main ()
  (defprimitive "quit" ()
    (return-from main))
  (if (cdr sb-ext:*posix-argv*)
      (with-open-file (s (cadr sb-ext:*posix-argv*))
        (loop for exp = (handler-case (clutter-read s) (end-of-file () nil))
           while exp do (evaluate exp)))
      (repl)))
