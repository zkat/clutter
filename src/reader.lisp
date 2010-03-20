;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defvar *default-namespace*)
(defvar *namespace*)
(defvar *namespace-root* nil)

;;;
;;; Symbols
;;;
(defstruct clutter-symbol name namespace)
(defmethod print-object ((o clutter-symbol) s)
  (if (eq *namespace* (clutter-symbol-namespace o))
      (princ (clutter-symbol-name o) s)
      (format s "~A:~A"
              (namespace-name (clutter-symbol-namespace o))
              (clutter-symbol-name o))))

;;;
;;; Namespaces
;;;
(defstruct (namespace (:predicate namespacep)
                      (:constructor make-namespace (name)))
  name (symbols (make-hash-table :test #'equal)))

(defmethod print-object ((o namespace) s)
  (print-unreadable-object (o s :type t :identity t)
    (princ (namespace-name o) s)))

(defun find-clutter-symbol (name &optional (namespace *namespace*))
  (gethash name (namespace-symbols namespace)))

(defun add-clutter-symbol (symbol &optional (namespace *namespace*))
  (check-type symbol clutter-symbol)
  (check-type namespace namespace)
  (setf (gethash (clutter-symbol-name symbol)
                 (namespace-symbols namespace))
        symbol))

(defun remove-clutter-symbol (symbol &optional (namespace *namespace*))
  (check-type symbol clutter-symbol)
  (check-type namespace namespace)
  (remhash (clutter-symbol-name symbol)
           (namespace-symbols namespace))
  symbol)

(defun find-namespace (name)
  (find name *namespace-root* :test #'string= :key #'namespace-name))

(defun ensure-namespace (name)
  (or (find-namespace name)
      (let ((namespace (make-namespace name)))
        (push namespace *namespace-root*)
        namespace)))

(setf *default-namespace* (ensure-namespace "clutter")
      *namespace* *default-namespace*
      *namespace-root* (list *default-namespace*))

(defun clutter-intern (name &optional (namespace *namespace*))
  (or (find-clutter-symbol name namespace)
      (add-clutter-symbol (make-clutter-symbol :name name :namespace namespace)
                          namespace)))

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
      (parse-float-token token)
      (parse-rational-token token)
      (parse-symbol-token token)))

(defun parse-integer-token (token)
  (let ((minusp nil)
        (first-char (char token 0)))
    (cond ((char= #\- first-char)
           (setf minusp t
                 token (subseq token 1)))
          ((char= #\+ first-char)
           (setf token (subseq token 1))))
    (loop with mantissa = 0
      for char across token
      do (if (digit-char-p char *clutter-read-base*)
             (setf mantissa (+ (* *clutter-read-base* mantissa)
                               (digit-char-p char *clutter-read-base*)))
             (return nil))
      finally (return (if minusp (- mantissa) mantissa)))))

(defun parse-rational-token (token)
  (let ((/-position (position #\/ token)))
    (when /-position
      (let ((num-str (subseq token 0 /-position))
            (den-str (subseq token (1+ /-position))))
        (let ((numerator (or (parse-integer-token num-str)
                             (parse-float-token num-str)))
              (denominator (or (parse-integer-token den-str)
                               (parse-float-token den-str))))
          (when (and numerator denominator)
            (/ numerator denominator)))))))

;; todo: rewrite this, a lot of it was taken from
;; ftp://ftp.cs.cmu.edu/user/ai/lang/lisp/code/math/atof/atof.cl
(defun parse-float-token (token)
  (let ((minusp nil)
        (found-point-p nil)
        (found-digit-p nil)
        (before-decimal 0)
        (after-decimal 0)
        (decimal-counter 0)
        (exponent 0)
        (result nil)
        (index 0))
    (let ((first-char (char token 0)))
      (cond ((char= first-char #\-)
             (setf minusp t)
             (incf index))
            ((char= first-char #\+)
             (incf index))))
    (loop with token-length = (length token)
       until (>= index token-length) do
         (let* ((char (char token index))
                (weight (digit-char-p char *clutter-read-base*)))
           (cond ((and weight (not found-point-p))
                  (setf before-decimal (+ weight (* before-decimal *clutter-read-base*))
                        found-digit-p t))
                 ((and weight found-point-p)
                  (setf after-decimal (+ weight (* after-decimal *clutter-read-base*))
                        found-digit-p t)
                  (incf decimal-counter))
                 ((and (char= char #\.) (not found-point-p))
                  (setf found-point-p t))
                 ((and (char-equal char #\e) (= *clutter-read-base* 10))
                  (unless (ignore-errors
                            (multiple-value-bind (num idx)
                                (parse-integer token :start (1+ index)
                                               :radix *clutter-read-base*)
                              (setf exponent (or num 0)
                                    index idx)))
                    (return-from parse-float-token nil)))
                 (t (return-from parse-float-token nil))))
       (incf index))
    (setf result (float (* (+ before-decimal
                              (* after-decimal
                                 (expt *clutter-read-base* (- decimal-counter))))
                           (expt *clutter-read-base* exponent))))
    (if found-digit-p
        (if minusp (- result) result)
        nil)))

(defun parse-symbol-token (token)
  (clutter-intern token))

(defun clutter-read (&optional (stream *standard-input*))
  (parse-token (read-token stream)))
