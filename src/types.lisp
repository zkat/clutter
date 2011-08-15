;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;; Clutter types

(in-package :clutter)

(defstruct (clutter-form (:constructor make-form (code type)))
  code
  type)

(defmethod print-object ((object clutter-form) stream)
  (format stream "~S" (clutter-form-code object)))

(defclass clutter-type () ())

(defclass basic-type (clutter-type) ())

(defclass constant-type (basic-type)
  ((value
    :initarg :value
    :accessor constant-type-value)))

(defun make-constant-type (value)
  (make-instance 'constant-type :value value))

(defmethod print-object ((object constant-type) stream)
  (format stream "~a:CONSTANT" (constant-type-value object)))

(defclass primitive-type (basic-type)
  ((name
    :initarg :name
    :accessor primitive-type-name)
   (allocation
    :initarg :allocation
    :accessor primitive-type-allocation)))

(defun make-primitive-type (name &key (allocation :cl))
  (make-instance 'primitive-type
                 :name name
                 :allocation allocation))

(defmethod print-object ((object primitive-type) stream)
  (format stream "~a" (primitive-type-name object)))

(defclass compound-type (clutter-type) ())

(defclass labeled-type (compound-type)
  ((types
    :initarg :types
    :accessor labeled-type-pairs)))

(defgeneric labeled-type-keys (labeled-type)
  (:method ((labeled-type labeled-type))
    (mapcar #'car (labeled-type-pairs labeled-type))))

(defgeneric labeled-type-values (labeled-type)
  (:method ((labeled-type labeled-type))
    (mapcar #'cdr (labeled-type-pairs labeled-type))))

(defgeneric labeled-type-get (labeled-type label)
  (:method ((labeled-type labeled-type) label)
    (assoc label (labeled-type-pairs labeled-type))))

(defclass record-type (labeled-type) ())

(defun make-record-type (&rest types-plist)
  (make-instance 'record-type :types (plist-alist types-plist)))

(defmethod print-object ((object record-type) stream)
  (format stream "{ ~{~a~^, ~} }" (labeled-type-pairs object)))

(defclass variant-type (labeled-type) ())

(defmethod print-object ((object variant-type) stream)
  (format stream "~{~a~^ | ~} " (labeled-type-pairs object)))

(defun make-variant-type (&rest types-plist)
  (make-instance 'variant-type :types (plist-alist types-plist)))

(defclass arrow-type (compound-type)
  ((from
    :initarg :from
    :accessor arrow-type-from)
   (to
    :initarg :to
    :accessor arrow-type-to)))

(defclass operative-type (arrow-type) ())

(defun make-operative-type (from to)
  (make-instance 'operative-type :from from :to to))

(defmethod print-object ((object operative-type) stream)
  (format stream "~a ↝ ~a"
          (arrow-type-from object)
          (arrow-type-to object)))

(defclass function-type (arrow-type) ())

(defun make-function-type (from to)
  (make-instance 'function-type :from from :to to))

(defmethod print-object ((object function-type) stream)
  (format stream "~a → ~a"
          (arrow-type-from object)
          (arrow-type-to object)))

(defclass set-type (clutter-type)
  ((types
    :initarg :types
    :accessor set-type-types)))

(defclass union-type (set-type) ())

(defun make-union-type (&rest types)
  (make-instance 'union-type :types types))

(defmethod print-object ((object union-type) stream)
  (format stream "~{~a~^ ∪ ~}" (set-type-types object)))

(defclass intersection-type (set-type) ())

(defun make-intersection-type (&rest types)
  (make-instance 'intersection-type :types types))

(defmethod print-object ((object intersection-type) stream)
  (format stream "~{~a~^ ∩ ~}" (set-type-types object)))