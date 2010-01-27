;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defun clutter-symbol? (x)
  (and (symbolp x)
       (eq (symbol-package x) (find-package :clutter.symbols))))

(deftype clutter-symbol ()
  '(satisfies clutter-symbol?))

(defun clutter-intern (name)
  (intern name :clutter.symbols))
