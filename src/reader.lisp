;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defun read-clutter-expr (stream)
  (labels ((rec (tree)
             (typecase tree
               (symbol (clutter-intern (string tree)))
               (atom tree)
               (t (cons (rec (car tree)) (rec (cdr tree)))))))
    (rec (read stream))))

(defparameter *whitespace*
  '(#\Backspace #\Tab #\Newline #\Linefeed #\Page #\Return #\Space))

(defparameter *symbol-terminators*
  '(#\( #\)))

(defun symbol-terminator? (char)
  (or (member char *whitespace*)
      (member char *symbol-terminators*)))

(defparameter *symbol-single-escape* #\\)

(defun read-clutter-symbol (stream)
  (do ((name (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))
       (char (read-char stream) (read-char stream)))
      ((symbol-terminator? char)
       (unread-char char stream)
       (clutter-intern (coerce name 'simple-string)))
    (when (eq char *symbol-single-escape*)
      (setf char (read-char stream)))
    (vector-push-extend char name)))
