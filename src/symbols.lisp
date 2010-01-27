;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defun clutter-symbol? (x)
  (and (symbolp x)
       (eq (symbol-package x) (find-package :clutter.symbols))))

(deftype clutter-symbol ()
  '(satisfies clutter-symbol?))

(defun clutter-intern (name)
  (intern name :clutter.symbols))

(defmacro import-cl-function (name &optional (cl-name-string name))
  (let ((clutter-name (clutter-intern name))
        (cl-name (find-symbol (string-upcase cl-name-string) :cl)))
    `(progn
       (setf (fdefinition ',clutter-name) #',cl-name)
       (define-compiler-macro ,clutter-name (&whole form)
         (cons ',cl-name (cdr form))))))
