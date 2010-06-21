;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Functions
;;;

(defstruct (clutter-function (:conc-name #:%function-)) function)
(defmethod print-object ((o clutter-function) s)
  (print-unreadable-object (o s :identity t)
    (format s "closure")))

(defun make-function (variables body &aux
                      (new-frame (make-stack-frame "lambda" (current-scope))))
  (make-clutter-function
   :function (lambda (values)
               (with-frame new-frame
                 (loop for var in variables for value in values do
                   (bind var value :lexical))
                 (eval-do body)))))

(defun invoke (function args)
  (if (clutter-function-p function)
      (funcall (%function-function function) args)
      (error "Not a function: ~S" function)))
