;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(defvar *default-namespace*)
(defvar *namespace*)
(defvar *namespace-root* nil)

(defvar *namespace-marker* #\:)
(defvar *subnamespace-marker* #\:)
(defvar *keyword-marker* #\:)
(defvar *keyword-marker-in-front* 't)
(defvar *keyword-namespace-name* "keyword")

;;;
;;; Symbols
;;;
(defstruct clutter-symbol name namespace)
(defmethod print-object ((o clutter-symbol) s)
  (cond ((eq *namespace* (clutter-symbol-namespace o))
         (princ (clutter-symbol-name o) s))
        ((eq (find-namespace "keyword") (clutter-symbol-namespace o))
         (format s ":~A" (clutter-symbol-name o)))
        (t
         (format s "~A:~A"
                 (clutter-symbol-namespace o)
                 (clutter-symbol-name o)))))

;;;
;;; Namespaces
;;;
(defstruct (namespace (:predicate namespacep)
                      (:constructor make-namespace (name)))
  name (symbols (make-hash-table :test #'equal)))

(defmethod print-object ((o namespace) s)
  (princ (namespace-name o) s))

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
  (pushnew char *terminating-macro-characters*)
  (setf (gethash char *reader-macro-functions*) function))

(defun clutter-read-delimited-list (end-char stream)
  (loop with list = ()
     for char = (peek-char t stream)
     do (if (char= end-char char)
            (progn (read-char stream nil nil)
                   (return-from clutter-read-delimited-list (nreverse list)))
            (unless (whitespacep char)
              (push (clutter-read stream) list)))))

(set-clutter-reader-macro-function #\( (lambda (stream char)
                                         (declare (ignore char))
                                         (clutter-read-delimited-list #\) stream)))
(set-clutter-reader-macro-function #\' (lambda (stream char)
                                         (declare (ignore char))
                                         `(,(clutter-intern "quote")
                                            ,(clutter-read stream))))
(set-clutter-reader-macro-function #\) (lambda (stream char)
                                         (declare (ignore stream char))
                                         (error "Unmatched #\).")))


(defun read-token (stream)
  (loop with token = (make-array 8 :adjustable t :fill-pointer 0 :element-type 'character)
     with collecting-token = nil
     for char = (read-char stream nil nil t)
     while char
     do (if (terminating-macro-char-p char)
            (if collecting-token
                (progn (unread-char char)
                       (return token))
                (let ((result (multiple-value-list (funcall (reader-macro-function char) stream char))))
                  (when result
                    (return-from read-token (values (car result) t)))))
            (cond ((and collecting-token (whitespacep char))
                   (return-from read-token token))
                  ((and (whitespacep char) (not collecting-token))
                   (values))
                  (t
                   (vector-push-extend char token)
                   (setf collecting-token t))))
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
  (or (find *namespace-marker* symbol) (find *subnamespace-marker* symbol) (find *keyword-marker* symbol)))

(defun parse-symbol-token (token)
  (cond
    ;; Keyword
    ((or (and *keyword-marker-in-front*
              (char= (char token 0) *keyword-marker*))
         (and (not *keyword-marker-in-front*)
              (char= (char token (- (length token) 1)) *keyword-marker*)))
     (let ((symbol-name (subseq token 1)))
       (if *keyword-marker-in-front*
           (when (char= (char symbol-name 0) *keyword-marker*)
             (setf symbol-name (subseq symbol-name 1)))
           (when (char= (char symbol-name (1- (length symbol-name))) *keyword-marker*)
             (setf symbol-name (subseq symbol-name (1- (length symbol-name))))))
       (if (not (symbol-illegal-characters-p symbol-name))
           (let* ((symbol (clutter-intern symbol-name (ensure-namespace *keyword-namespace-name*)))
                  (existing-binding (assoc symbol *global-env*)))
             (unless existing-binding
               (let ((relevant-cons (last *global-env*)))
                 (setf (cdr relevant-cons) (cons (cons symbol symbol) nil))
                 symbol)))
           (error "Illegal characters in symbol name")))) ; Should find keyword namespace, not create
    ;; Namespaced symbol
    ((find *namespace-marker* token :from-end 't)
     (multiple-value-bind (symbol-name symbol-end)
         (split-sequence *namespace-marker* token :from-end 't :count 1)
       (let ((namespace-identifier (subseq token 0 symbol-end)))
         (when (char= (char namespace-identifier (1- symbol-end)) *namespace-marker*)
           ;; Should handle internal symbols here
           (setf namespace-identifier (subseq namespace-identifier 0 (1- symbol-end))))
         ;; For now I just use namespace-identifier string to make namespace
         ;; Should split-sequence on *subnamespace-marker* and then find correct namespace through hierarchy
         (if (not (symbol-illegal-characters-p symbol-name))
             (clutter-intern (car symbol-name) (ensure-namespace namespace-identifier))
             (error "Illegal characters in symbol name")))))
    ;; Normal symbol
    (t (if (not (symbol-illegal-characters-p token))
           (clutter-intern token)
           (error "Illegal characters in symbol name")))))

(defun clutter-read (&optional (stream *standard-input*))
  (multiple-value-bind (token donep)
      (read-token stream)
    (if donep
        token
        (parse-token token))))
