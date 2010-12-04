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

(defvar *primitives* (list))

(defmacro defprimitive (name value)
  (once-only (name value)
   `(progn
      (push ,value *primitives*)
      (extend *global-env* (clutter-symbol ,name) ,value))))

(defmacro defprimop (name vau-list &body body)
  `(defprimitive ,name
       (make-clutter-operator
        :name ,name
        :function (lambda ,vau-list ,@body)
        :args ',vau-list
        :body ',body)))

(defmacro defprimfun (purity name vau-list &body body)
  `(defprimitive ,name
       (make-function
        (make-clutter-operator
         :name ,name
         :function (lambda (*denv* ,@vau-list)
                     ,@body)
         :args ',vau-list
         :pure ,purity
         :body ',body))))

(defprimop "vau" (static-env env-var vau-list &rest body)
  ;; TODO: Determine purity
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
   :denv-var env-var
   :body body))

(defprimfun t "wrap" (operative)
  (make-function operative))

(defprimfun t "unwrap" (function)
  (clutter-function-operator function))

(defprimfun nil "name-vau!" (v name)
  (setf (clutter-operator-name v) name)
  v)

(defprimfun nil "eval" (expression environment)
  (clutter-eval expression environment))

(defprimfun t "make-env" (&rest parents)
  (apply #'make-env parents))

(defprimfun t "env-parents" (env)
  (env-parents env))

(defprimfun t "bound?" (symbol &optional (env (get-current-env)))
  (if (clutter-bound? symbol env)
      *true*
      *false*))

(defprimop "set-in!" (*denv* env var value)
  (setf (lookup var (clutter-eval env *denv*)) (clutter-eval value *denv*)))

(defprimop "def-in!" (*denv* env var value)
  (extend (clutter-eval env *denv*) var (clutter-eval value *denv*))
  var)

(defprimfun nil "dyn-def-in!" (env var value)
  (extend env var value)
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

(defprimfun nil "symbolize" (&rest values)
  (assert (clutter-operator-p (car values)))
  (make-symbol-operator (car values)))

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
  (if (clutter-operator-p x) *true* *false*))
(defprimfun t "primitive?" (x)
  (if (find x *primitives*) *true* *false*))
(defprimfun t "pure?" (x)
  (if (clutter-operator-pure (clutter-function-operator x))
      *true*
      *false*))
;;; HACK FOR TESTING PURPOSES ONLY.
;;; TODO: Scrap this once we autodetect purity.
(defprimfun nil "declare-pure!" (x purity)
  (setf (clutter-operator-pure (clutter-function-operator x))
        (if (eq purity *false*) nil t)))

(defprimfun t "vau-name" (v)
  (clutter-operator-name v))
(defprimfun t "vau-denv-var" (v)
  (clutter-operator-denv-var v))
(defprimfun t "vau-args" (v)
  (clutter-operator-args v))
(defprimfun t "vau-body" (v)
  (clutter-operator-body v))
(defprimfun t "vau-env" (v)
  (clutter-operator-env v))

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
(defprimfun t "rem" (number divisor)
  (rem number divisor))

(defprimfun nil "print" (obj)
  (print obj))
(defprimfun nil "load" (path)
  (clutter-load path))

;;; For escaping the REPL cleanly.
(define-condition quit () ())
(defprimfun nil "quit" ()
  (signal (make-condition 'quit)))
