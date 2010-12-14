;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; REPL
;;;

(defun repl ()
  (handler-case 
      (loop
        (format t "~&> ")
        (with-simple-restart (abort "Return to Clutter's toplevel")
          (format t "~S" (clutter-eval (clutter-read)))))
    (quit () (values))))

(defun clutter-read-str (str)
  (with-input-from-string (stream str)
    (clutter-read stream nil)))

;; #+sbcl
;; (defun main ()
;;   (defprimitive "http-get" (url)
;;     (funcall (fdefinition (intern "HTTP-REQUEST" (find-package 'drakma)))
;;              url))
;;   (defprimitive "quit" ()
;;     (return-from main))
;;   (if (cdr sb-ext:*posix-argv*)
;;       (with-open-file (s (cadr sb-ext:*posix-argv*))
;;         (loop for exp = (handler-case (clutter-read s) (end-of-file () nil))
;;            while exp do (evaluate exp)))
;;       (repl)))
