;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Primitive Constants
;;;

(defparameter *true* (clutter-intern "t"))
(defparameter *false* (clutter-intern "f"))

(defun define-initially (clutter-symbol-name value)
  (push-initial-binding (clutter-read-from-string clutter-symbol-name) value))
(define-initially "t" *true*)
(define-initially "f" *false*)
(define-initially "nil" nil)

;;;
;;; Primitive functions
;;;

(defun define-function-initially (function-name value)
  (push-initial-function-binding (clutter-intern function-name) value))

(defmacro defprimitive (name arglist &body body)
  (let ((argv (gensym)))
    `(define-function-initially ,name
         (make-clutter-function
          :function (lambda (,argv)
                      (apply (lambda ,arglist ,@body) ,argv))))))

(defprimitive "not" (x)
  (if (eq x *false*) *true* *false*))
(defprimitive "null?" (x)
  (if (eq x nil) *true* *false*))

(defprimitive "cons" (x y)
  (cons x y))
(defprimitive "head" (cons)
  (car cons))
(defprimitive "tail" (cons)
  (cdr cons))
(defprimitive "length" (seq)
  (length seq))

(defprimitive "set-head" (cons new-car)
  (rplaca cons new-car))
(defprimitive "set-tail" (cons new-cdr)
  (rplacd cons new-cdr))

(defprimitive "eql?" (x y)
  (if (eql x y) *true* *false*))

(defprimitive "symbol?" (x)
  (if (clutter-symbol-p x) *true* *false*))
(defprimitive "number?" (x)
  (numberp x))

(defprimitive "<?" (x y)
  (if (< x y) *true* *false*))
(defprimitive ">?" (x y)
  (if (> x y) *true* *false*))
(defprimitive "<=?" (x y)
  (if (<= x y) *true* *false*))
(defprimitive ">=?" (x y)
  (if (>= x y) *true* *false*))
(defprimitive "=?" (x y)
  (if (= x y) *true* *false*))
(defprimitive "+" (x y)
  (+ x y))
(defprimitive "-" (x y)
  (- x y))
(defprimitive "*" (x y)
  (* x y))
(defprimitive "/" (x y)
  (/ x y))

(defprimitive "apply" (function args)
  (invoke function args))
(defprimitive "call" (function &rest args)
  (invoke function args))

(defprimitive "print" (obj)
  (print obj))
