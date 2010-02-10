;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Primitives
;;;

(defmacro definitial-fun (name value)
  `(progn (push-initial-function-binding ',name ,value)
          ',name))

(defmacro defprimitive (name arglist &body body)
  (let ((argv (gensym)))
    `(definitial-fun ,name
         (make-clutter-function
          :function (lambda (,argv)
                      (apply (lambda ,arglist ,@body) ,argv))))))

(defprimitive |not| (x)
  (if (eq x *false*) *true* *false*))

(defprimitive |cons| (x y)
  (cons x y))
(defprimitive |car| (cons)
  (car cons))
(defprimitive |cdr| (cons)
  (cdr cons))

(defprimitive |rplaca| (cons new-car)
  (rplaca cons new-car))
(defprimitive |rplacd| (cons new-cdr)
  (rplacd cons new-cdr))

(defprimitive |eq?| (x y)
  (if (eq x y) *true* *false*))
(defprimitive |eql?| (x y)
  (if (eql x y) *true* *false*))

(defprimitive |symbol?| (x)
  (if (symbolp x) *true* *false*))

(defprimitive |<| (x y)
  (if (< x y) *true* *false*))
(defprimitive |>| (x y)
  (if (> x y) *true* *false*))
(defprimitive |=| (x y)
  (if (= x y) *true* *false*))

(defprimitive |apply| (function args)
  (invoke function args))
(defprimitive |call| (function &rest args)
  (invoke function args))
