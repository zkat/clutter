;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defvar *namespace-marker* #\:)
(defvar *subnamespace-marker* #\:)
(defvar *keyword-marker* #\:)
(defvar *keyword-marker-in-front* 't)

;;;
;;; Symbols
;;;
(defstruct (clutter-symbol (:constructor %make-clutter-symbol (name)))
  (interned t)
  name)

(defvar *symbol-table* (make-hash-table :test 'equal))

(defun clutter-symbol (name &optional (intern t))
  "Return the symbol corresponding to string NAME"
  (if intern
      (or (gethash name *symbol-table*)
          (setf (gethash name *symbol-table*) (%make-clutter-symbol name)))
      (%make-clutter-symbol name)))

(defun cs (name)
  "Shorthand for clutter-symbol"
  (clutter-symbol name))

(defmethod print-object ((o clutter-symbol) s)
  (princ (clutter-symbol-name o) s))

;;;
;;; Reader
;;;
(defparameter *clutter-read-base* 10)
(defparameter *whitespace-chars* '(#\Space #\Return #\Tab #\Newline #\Page #\Linefeed))
(defparameter *single-escape* #\\)
(defun whitespace? (char)
  (if (member char *whitespace-chars*) t nil))

;; (defun invalid-char-p (char)
;;   (declare (ignore char))
;;   nil)

;; (defparameter *multiple-escape-chars* '())
;; (defun multiple-escape-char-p (char)
;;   (when (find char *multiple-escape-chars*) t))

;; (defparameter *single-escape-chars* '())
;; (defun single-escape-char-p (char)
;;   (when (find char *single-escape-chars*) t))

;; (defparameter *macro-characters* '())
;; (defun macro-character-p (char)
;;   (when (find char *macro-characters*) t))

;; (defun constituent-char-p (char)
;;   (and (not (invalid-char-p char))
;;        (whitespacep char)))

(defparameter *terminating-macro-characters* '())
(defun terminating-macro-char-p (char)
  (when (find char *terminating-macro-characters*) t))

(defparameter *reader-macro-functions* (make-hash-table))
(defun reader-macro-function (char)
  (gethash char *reader-macro-functions*))

(defun set-clutter-reader-macro-function (char function)
  (check-type char character)
  (check-type function function)
  (unset-clutter-reader-macro char)
  (pushnew char *terminating-macro-characters*)
  (setf (gethash char *reader-macro-functions*) function))

(defun unset-clutter-reader-macro (char)
  (setf *terminating-macro-characters* (remove char *terminating-macro-characters*))
  (remhash char *reader-macro-functions*)
  char)

(defun clutter-read-delimited-list (end-char stream)
  (loop ;; with list = ()
     for char = (peek-char t stream)
     if (char= end-char char)
     do (progn (read-char stream nil nil)
               (return-from clutter-read-delimited-list list))
     else unless (whitespace? char)
     collect (clutter-read stream) into list))

;;; Taken from SICL
(defun clutter-read-string (end-char stream)
  (loop
     with result = (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
     for new-char = (read-char stream nil nil)
     until (eql new-char end-char)
     do (cond ((null new-char)
               (error 'end-of-file :stream stream))
              ((eql new-char *single-escape*)
               (let ((next-char (read-char stream nil nil)))
                 (when (null next-char)
                   (error 'end-of-file :stream stream))
                 (vector-push-extend next-char result)))
              (t (vector-push-extend new-char result)))
     finally (return (copy-seq result))))

(set-clutter-reader-macro-function #\( (lambda (stream char)
                                         (declare (ignore char))
                                         (clutter-read-delimited-list #\) stream)))
(set-clutter-reader-macro-function #\' (lambda (stream char)
                                         (declare (ignore char))
                                         `(,(clutter-symbol "quote")
                                            ,(clutter-read stream))))
(set-clutter-reader-macro-function #\) (lambda (stream char)
                                         (declare (ignore stream char))
                                         (error "Unmatched #\).")))
(set-clutter-reader-macro-function #\" (lambda (stream char)
                                         (declare (ignore char))
                                         (clutter-read-string #\" stream)))

(defun read-token (stream)
  (loop with token = (make-array 8 :adjustable t :fill-pointer 0 :element-type 'character)
        with collecting-token = nil
        with eof-error = t
        for char = (read-char stream (prog1 eof-error
                                       (setf eof-error nil))
                        nil t)
        while char
        do (if (terminating-macro-char-p char)
               (if collecting-token
                   (progn (unread-char char stream)
                          (return token))
                   (let ((result (multiple-value-list
                                     (funcall (reader-macro-function char) stream char))))
                     (when result
                       (return-from read-token (values (car result) t)))))
               (cond ((and collecting-token (whitespace? char))
                      (return-from read-token token))
                     ((and (whitespace? char) (not collecting-token))
                      (values))
                     (t
                      (vector-push-extend char token)
                      (setf collecting-token t))))
        finally (return token)))

(defun parse-token (token)
  (or (parse-integer-token token)
      (parse-float-token token)
      (parse-rational-token token)
      (parse-symbol-token token)
      (error "How the hell did I ever get here?")))

(defun parse-integer-token (token)
  (let ((minusp nil)
        (first-char (char token 0)))
    (cond ((char= #\- first-char)
           (let ((maybe-token (subseq token 1)))
             (unless (zerop (length maybe-token))
               (setf minusp t
                     token maybe-token))))
          ((char= #\+ first-char)
           (let ((maybe-token (subseq token 1)))
             (unless (zerop (length maybe-token))
               (setf token maybe-token)))))
    (loop with mantissa = 0
      for char across token
      do (if (digit-char-p char *clutter-read-base*)
             (setf mantissa (+ (* *clutter-read-base* mantissa)
                               (digit-char-p char *clutter-read-base*)))
             (return nil))
      finally (return (if minusp (- mantissa) mantissa)))))

(defun parse-rational-token (token)
  (let ((/-position (position #\/ token)))
    (when (and /-position (plusp /-position))
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

(defun symbol-illegal-characters-p (symbol)
  (find *keyword-marker* symbol))

;; (defun parse-keyword-token (token)
;;   (let ((symbol-name (subseq token 1)))
;;     (if *keyword-marker-in-front*
;;         (when (char= (char symbol-name 0) *keyword-marker*)
;;           (setf symbol-name (subseq symbol-name 1)))
;;         (when (char= (char symbol-name (1- (length symbol-name))) *keyword-marker*)
;;           (setf symbol-name (subseq symbol-name (1- (length symbol-name))))))
;;     (if (not (symbol-illegal-characters-p symbol-name))
;;         (let* ((symbol (clutter-intern symbol-name *keyword-env*)))
;;           (unless (clutter-bound? symbol :lexical)
;;             (bind symbol symbol :lexical))
;;           symbol)
;;         (error "Illegal characters in symbol name"))))

(defun build-namespace-lookup (token)
  (reduce (lambda (accum arg) (list (clutter-symbol "lookup") arg accum))
          (split-sequence *namespace-marker* token)
          :key #'clutter-symbol))

;; (defun parse-qualified-symbol-token (token)
;;   (multiple-value-bind (symbol-name symbol-end)
;;       (split-sequence *namespace-marker* token :from-end t :count 1)
;;     (let ((namespace-identifier (subseq token 0 symbol-end)))
;;       (when (char= (char namespace-identifier (1- symbol-end)) *namespace-marker*)
;;         ;; Should handle internal symbols here
;;         (setf namespace-identifier (subseq namespace-identifier 0 (1- symbol-end))))
;;       (if (not (symbol-illegal-characters-p symbol-name))
;;           `(,(clutter-symbol "lookup") ,(clutter-symbol (car symbol-name)) ,namespace-identifier)
;;           (error "Illegal characters in symbol name")))))

(defun parse-symbol-token (token)
  (cond
    ;; ((or (and *keyword-marker-in-front*
    ;;           (char= (char token 0) *keyword-marker*))
    ;;      (and (not *keyword-marker-in-front*)
    ;;           (char= (char token (- (length token) 1)) *keyword-marker*)))
    ;;  (parse-keyword-token token))
    ((find *namespace-marker* token :from-end t)
     (build-namespace-lookup token))
    ;; Normal, unqualified symbol
    (t (if (symbol-illegal-characters-p token)
           (error "Illegal characters in symbol name")
           (clutter-symbol token)))))

(defun clutter-read (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  (handler-case
      (multiple-value-bind (token donep)
          (read-token stream)
        (if donep
            token
            (parse-token token)))
    (end-of-file (e) (if eof-error-p (error e) eof-value))))

(defun clutter-read-from-string (string)
  (with-input-from-string (s string)
    (clutter-read s)))
