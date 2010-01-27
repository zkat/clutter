;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defvar *symbol-terminators*
  '(#\Space #\( #\) #\Newline))

(defun read-clutter-symbol (stream)
  (do ((name (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))
       (char (read-char stream) (read-char stream)))
      ((find char *symbol-terminators*)
       (ensure-clutter-symbol (coerce name 'simple-string)))
    (vector-push-extend char name)))
