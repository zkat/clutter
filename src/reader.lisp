;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

;;;
;;; Symbols
;;;
(defstruct clutter-symbol name)
(defvar *symbols* (make-hash-table :test #'equal))
(defun clutter-intern (name)
  (or (gethash name *symbols*)
      (values (setf (gethash name *symbols*)
                    (make-clutter-symbol :name name))
              nil)))

;;;
;;; Reader
;;;
(defparameter *clutter-read-base* 10)
(defparameter *whitespace-chars* '(#\Space #\Return #\Tab #\Newline #\Page #\Linefeed))
(defun whitespacep (char)
  (if (member char *whitespace-chars*) t nil))

(defun read-token (stream)
  (loop with token = (make-array 8 :adjustable t :fill-pointer 0 :element-type 'character)
     for char = (read-char stream nil nil t)
     while (and char (not (whitespacep char)))
     do (vector-push-extend char token)
     finally (return token)))

(defun parse-token (token)
  (or (parse-integer-token token)
      (parse-symbol-token token)
      (error "Can't parse yet: ~A" token)))

(defun parse-integer-token (token)
  (loop with mantissa = 0
     for char across token
     do (if (digit-char-p char *clutter-read-base*)
            (setf mantissa (+ (* *clutter-read-base* mantissa)
                              (digit-char-p char *clutter-read-base*)))
            (return nil))
     finally (return mantissa)))

(defun parse-symbol-token (token)
  (clutter-intern token))

(defun clutter-read (&optional (stream *standard-input*))
  (parse-token (read-token stream)))
