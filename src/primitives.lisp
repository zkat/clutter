;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(declaim (optimize debug))

;;;
;;; Primitive Constants
;;;

(defparameter *true* (cs "#t"))
(defparameter *false* (cs "#f"))

;;;
;;; Primitive functions
;;;

(defvar *primitives* (make-env))

(defun primitive? (x)
  (clutter-bound? x *primitives*))

(defmacro defprimitive (name value)
  (once-only (name value)
   `(progn
      (extend *primitives* (clutter-symbol ,name) ,value)
      (extend *global-env* (clutter-symbol ,name) ,value))))

(defmacro defprimop (name vau-list &body body)
  `(defprimitive ,name
       (make-clutter-operative
        :name (cs ,name)
        :function (lambda ,vau-list ,@body)
        :args ',(rest vau-list))))

(defmacro defprimfun (purity name vau-list &body body)
  `(defprimitive ,name
       (make-function
        (make-clutter-operative
         :name (cs ,name)
         :function (lambda (*denv* ,@vau-list)
                     ,@body)
         :args ',vau-list
         :pure ,purity))))

(defun make-vau (static-env env-var vau-list body &optional (name "anonymous fexpr"))
  ;; TODO: Determine purity
  (make-clutter-operative
   :function
   (lambda (*denv* &rest values)
     (multiple-value-bind (required optional rest)
         (parse-vau-list vau-list)
       (declare (ignore optional))
       (unless (or (= (length values) (length required))
                   (and rest (>= (length values) (length required))))
         (error "Wrong number of arguments to ~A (expected at least ~A)" name (length required)))
       (let ((env (make-env static-env)))
         (loop for var in (list* env-var rest required)
               for value in (list* *denv*
                                   (nthcdr (length required) values)
                                   (subseq values 0 (length required)))
               when var                 ; Ignore nil binding elements
                 do (extend env var value))
         (loop for sexp in body
               for last-value = (clutter-eval sexp env)
               finally (return last-value)))))
   :args vau-list
   :env static-env
   :denv-var env-var
   :body body))

(defprimop "vau" (static-env env-var vau-list &rest body)
  (make-vau static-env env-var vau-list body))

(defprimop "nvau" (static-env name env-var vau-list &rest body)
  (let ((value (make-vau static-env env-var vau-list body (clutter-symbol-name name))))
    (setf (clutter-operative-name value) name)
    value))

(defprimfun t "wrap" (operative)
  (make-function operative))

(defprimfun t "unwrap" (function)
  (clutter-function-operative function))

(defprimfun nil "eval" (expression environment)
  (clutter-eval expression environment))

(defprimfun t "make-env" (&rest parents)
  (apply #'make-env parents))

(defprimfun t "env-parents" (env)
  (env-parents env))

(defprimfun t "binding-env" (symbol env)
  (or (binding-env symbol env) *false*))

(defprimfun t "env-child?" (a b)
  (if (env-child? a b)
      *true*
      *false*))

(defprimfun t "bound?" (symbol &optional (env (get-current-env)))
  (if (clutter-bound? symbol env)
      *true*
      *false*))

(defprimop "set-in!" (*denv* env var value)
  (setf (lookup var (clutter-eval env *denv*)) (clutter-eval value *denv*)))

(defprimop "def-in!" (*denv* env var value)
  (extend (clutter-eval env *denv*) var (clutter-eval value *denv*))
  var)

(defprimop "forget-in!" (*denv* env var)
  (forget var (clutter-eval env *denv*))
  *true*)

(defun clutter-true-p (exp)
  (not (eq exp *false*)))

(defprimop "if" (*denv* test if-true if-false)
  (if (clutter-true-p (clutter-eval test *denv*))
      (clutter-eval if-true *denv*)
      (clutter-eval if-false *denv*)))

(defprimop "symbolize!" (*denv* var value)
  (let ((val (clutter-eval value *denv*)))
    (assert (clutter-operative-p val))
    (extend *denv* var (make-symbol-operative val))))

(defprimfun nil "symbolize" (&rest values)
  (assert (clutter-operative-p (car values)))
  (make-symbol-operative (car values)))

(defprimfun t "symbol-name" (symbol)
  (clutter-symbol-name symbol))
(defprimfun t "keyword-name" (kw)
  (clutter-keyword-name kw))

(defprimfun t "cons" (x y)
  (cons x y))
(defprimfun t "cons?" (x)
  (if (consp x) *true* *false*))
(defprimfun t "list?" (x)
  (if (listp x) *true* *false*))
(defprimfun t "car" (cons)
  (car cons))
(defprimfun t "cdr" (cons)
  (cdr cons))
(defprimfun t "list" (&rest values)
  values)
(defprimfun t "list*" (&rest values)
  (apply #'list* values))
(defprimfun t "length" (seq)
  (length seq))
(defprimfun t "append" (&rest lists)
  (apply #'append lists))

(defprimfun t "eql?" (x y)
  (if (eql (type-of x) (type-of y))
      (cond ((numberp x) (if (= x y) *true* *false*))
            ((or (listp x) (stringp x)) (if (equal x y) *true* *false*))
            (t (if (eq x y) *true* *false*)))
      *false*))

(defprimfun t "intern" (string)
  (clutter-symbol string))
(defprimfun t "make-keyword" (string)
  (clutter-keyword string))

(defprimfun t "boolean?" (x)
  (if (or (eql x *true*) (eql x *false*)) *true* *false*))
(defprimfun t "symbol?" (x)
  (if (clutter-symbol-p x) *true* *false*))
(defprimfun t "keyword?" (x)
  (if (clutter-keyword-p x) *true* *false*))
(defprimfun t "number?" (x)
  (if (numberp x) *true* *false*))
(defprimfun t "string?" (x)
  (if (stringp x) *true* *false*))
(defprimfun t "function?" (x)
  (if (clutter-function-p x) *true* *false*))
(defprimfun t "operative?" (x)
  (if (clutter-operative-p x) *true* *false*))
(defprimfun t "primitive?" (x)
  (if (primitive? x) *true* *false*))
(defprimfun t "pure?" (x)
  (if (clutter-operative-pure (clutter-function-operative x))
      *true*
      *false*))
;;; HACK FOR TESTING PURPOSES ONLY.
;;; TODO: Scrap this once we autodetect purity.
(defprimfun nil "declare-pure!" (x purity)
  (setf (clutter-operative-pure x)
        (if (eq purity *false*) nil t)))

(defprimfun t "vau-name" (v)
  (clutter-operative-name v))
(defprimfun t "vau-denv-var" (v)
  (clutter-operative-denv-var v))
(defprimfun t "vau-args" (v)
  (clutter-operative-args v))
(defprimfun t "vau-body" (v)
  (clutter-operative-body v))
(defprimfun t "vau-env" (v)
  (clutter-operative-env v))

(defprimfun t "<?" (x y)
  (if (< x y) *true* *false*))
(defprimfun t ">?" (x y)
  (if (> x y) *true* *false*))
(defprimfun t "<=?" (x y)
  (if (<= x y) *true* *false*))
(defprimfun t ">=?" (x y)
  (if (>= x y) *true* *false*))
(defprimfun t "=?" (x y)
  (if (= x y) *true* *false*))
(defprimfun t "+" (&rest values)
  (apply #'+ values))
(defprimfun t "-" (number &rest more-numbers)
  (apply #'- number more-numbers))
(defprimfun t "*" (&rest values)
  (apply #'* values))
(defprimfun t "/" (number &rest more-numbers)
  (apply #'/ number more-numbers))
(defprimfun t "div" (number divisor)
  (round number divisor))
(defprimfun t "rem" (number divisor)
  (rem number divisor))

(defprimfun t "string<?" (x y)
  (if (string< x y) *true* *false*))
(defprimfun t "string>?" (x y)
  (if (string> x y) *true* *false*))

(defprimfun t "make-string" (&rest values)
  (apply (curry #'concatenate 'string)
         (mapcar #'(lambda (el)
                     (if (stringp el)
                         el
                         (format nil "~S" el)))
                 values)))

(defprimfun nil "print" (obj)
  (format t "~S~%" obj)
  obj)

(defprimfun nil "format" (destination str &rest args)
  (apply #'format (if (eq destination (clutter-symbol "#t"))
              t
              destination)
          str args))

(defprimfun nil "load" (path)
  (clutter-load path))

(defprimfun nil "forget-all!" ()
  (setf *global-env* (make-env))
  (mapenv (lambda (symbol value)
            (extend *global-env* symbol value))
          *primitives*))

;;; For escaping the REPL cleanly.
(define-condition quit () ())
(defprimfun nil "quit" ()
  (signal (make-condition 'quit)))
