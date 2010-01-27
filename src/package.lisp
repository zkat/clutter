;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl)

(defpackage #:clutter
  (:use #:cl)
  (:documentation "Cluttered interpreter"))

(defpackage #:clutter.symbols
  (:use)
  (:export "repl")
  (:documentation "Clutter symbols"))