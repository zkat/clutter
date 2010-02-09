;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Primitives
;;;

(defmacro definitial-fun (name value)
  `(progn (push-initial-function-binding ',name ,value)
          ',name))

(defmacro defprimitive (name value arity)
  `(definitial-fun ,name (lambda (values)
                           (if (= ,arity (length values))
                               (apply ,value values)
                               (error "Incorrect arity.")))))

(defprimitive |not| (lambda (x) (if (eq x *false*) *true* *false*)) 1)
(defprimitive |cons| #'cons 2)
(defprimitive |car| #'car 1)
(defprimitive |cdr| #'cdr 1)
(defprimitive |rplaca| #'rplaca 2)
(defprimitive |rplacd| #'rplacd 2)
(defprimitive |eq?| (lambda (x y) (if (eq x y) *true* *false*)) 2)
(defprimitive |eql?| (lambda (x y) (if (eql x y) *true* *false*)) 2)
(defprimitive |symbol?| (lambda (x) (if (symbolp x) *true* *false*)) 1)
(defprimitive |<| (lambda (x y) (if (< x y) *true* *false*)) 2)
(defprimitive |>| (lambda (x y) (if (> x y) *true* *false*)) 2)
(defprimitive |=| (lambda (x y) (if (= x y) *true* *false*)) 2)
(defprimitive |apply| #'invoke 2)
(definitial-fun |call| (lambda (args) (invoke (car args) (cdr args))))