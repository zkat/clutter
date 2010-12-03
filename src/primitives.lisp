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

(defmacro defprimitive (name value)
  `(extend *global-env* (clutter-symbol ,name) ,value))

(defmacro defprimop (name vau-list &body body)
  `(defprimitive ,name
       (make-clutter-operator
        :name ,name
        :function (lambda ,vau-list ,@body)
        :args ',vau-list
        :body ',body)))

(defmacro defprimfun (name vau-list &body body)
  `(defprimitive ,name
       (make-function
        (make-clutter-operator
         :name ,name
         :function (lambda (*denv* ,@vau-list)
                     ,@body)
         :args ',vau-list
         :body ',body))))

(defprimop "vau" (static-env env-var vau-list &rest body)
  (make-clutter-operator
   :function
   (lambda (*denv* &rest values)
     (multiple-value-bind (required optional rest)
         (parse-vau-list vau-list)
       (declare (ignore optional))
       (unless (or (= (length values) (length required))
                   (and rest (>= (length values) (length required))))
         (error "Wrong number of arguments"))
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
   :body body))

(defprimfun "wrap" (operative)
  (make-function operative))

(defprimfun "unwrap" (function)
  (clutter-function-operator function))

(defprimfun "name-vau!" (v name)
  (setf (clutter-operator-name v) name)
  v)

(defprimfun "eval" (expression environment)
  (clutter-eval expression environment))

(defprimfun "make-env" (&rest parents)
  (apply #'make-env parents))

(defprimfun "env-parents" (env)
  (env-parents env))

(defprimfun "bound?" (symbol &optional (env (get-current-env)))
  (if (clutter-bound? symbol env)
      *true*
      *false*))

(defprimop "set-in!" (*denv* env var value)
  (setf (lookup var (clutter-eval env *denv*)) (clutter-eval value *denv*)))

(defprimop "def-in!" (*denv* env var value)
  (extend (clutter-eval env *denv*) var (clutter-eval value *denv*))
  var)

(defun clutter-true-p (exp)
  (not (eq exp *false*)))

(defprimop "if" (*denv* test if-true if-false)
  (if (clutter-true-p (clutter-eval test *denv*))
      (clutter-eval if-true *denv*)
      (clutter-eval if-false *denv*)))

(defprimop "symbolize!" (*denv* var value)
  (let ((val (clutter-eval value *denv*)))
    (assert (clutter-operator-p val))
    (extend *denv* var (make-symbol-operator val))))

(defprimfun "symbolize" (&rest values)
  (assert (clutter-operator-p (car values)))
  (make-symbol-operator (car values)))

(defprimfun "symbol-name" (symbol)
  (clutter-symbol-name symbol))

(defprimfun "cons" (x y)
  (cons x y))
(defprimfun "cons?" (x)
  (if (consp x) *true* *false*))
(defprimfun "list?" (x)
  (if (listp x) *true* *false*))
(defprimfun "car" (cons)
  (car cons))
(defprimfun "cdr" (cons)
  (cdr cons))
(defprimfun "list" (&rest values)
  values)
(defprimfun "list*" (&rest values)
  (apply #'list* values))
(defprimfun "length" (seq)
  (length seq))
(defprimfun "append" (&rest lists)
  (apply #'append lists))

(defprimfun "eql?" (x y)
  (if (eql (type-of x) (type-of y))
      (cond ((numberp x) (if (= x y) *true* *false*))
            ((or (listp x) (stringp x)) (if (equal x y) *true* *false*))
            (t (if (eq x y) *true* *false*)))
      *false*))

(defprimfun "symbol?" (x)
  (if (clutter-symbol-p x) *true* *false*))
(defprimfun "keyword?" (x)
  (if (clutter-keyword-p x) *true* *false*))
(defprimfun "number?" (x)
  (if (numberp x) *true* *false*))
(defprimfun "function?" (x)
  (if (clutter-function-p x) *true* *false*))
(defprimfun "vau?" (x)
  (if (clutter-operator-p x) *true* *false*))

(defprimfun "vau-name" (v)
  (clutter-operator-name v))
(defprimfun "vau-args" (v)
  (clutter-operator-args v))
(defprimfun "vau-body" (v)
  (clutter-operator-body v))
(defprimfun "vau-env" (v)
  (clutter-operator-env v))

(defprimfun "<?" (x y)
  (if (< x y) *true* *false*))
(defprimfun ">?" (x y)
  (if (> x y) *true* *false*))
(defprimfun "<=?" (x y)
  (if (<= x y) *true* *false*))
(defprimfun ">=?" (x y)
  (if (>= x y) *true* *false*))
(defprimfun "=?" (x y)
  (if (= x y) *true* *false*))
(defprimfun "+" (&rest values)
  (apply #'+ values))
(defprimfun "-" (number &rest more-numbers)
  (apply #'- number more-numbers))
(defprimfun "*" (&rest values)
  (apply #'* values))
(defprimfun "/" (number &rest more-numbers)
  (apply #'/ number more-numbers))
(defprimfun "rem" (number divisor)
  (rem number divisor))

(defprimfun "print" (obj)
  (print obj))
(defprimfun "load" (path)
  (clutter-load path))

;;; For escaping the REPL cleanly.
(define-condition quit () ())
(defprimfun "quit" ()
  (signal (make-condition 'quit)))
