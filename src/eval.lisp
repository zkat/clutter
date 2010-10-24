;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:clutter)

(declaim (optimize debug))

(defun clutter-load (filespec)
  (with-open-file (stream filespec)
    (loop for expr = (read stream nil)
          while expr
          do (clutter-eval expr))))

(defun clutter-eval (expression &optional (environment *global-env*))
  (cond ((symbolp expression) (eval/symbol expression environment))
        ((consp expression) (eval/combiner expression environment))
        (t expression)))

(defun eval/symbol (symbol env)
  (if (keywordp symbol)
      symbol
      (let ((val (lookup symbol env)))
        (if (clutter-symbol-operator-p val)
            (invoke (clutter-symbol-operator-operator val) env nil)
            val))))

(defun eval/combiner (expression env)
  (let ((f (clutter-eval (car expression) env)))
    (cond ((clutter-operator-p f)
           (invoke f env (cdr expression)))
          ((clutter-function-p f)
           (let ((op (clutter-function-operator f))
                 (values (mapcar (rcurry #'clutter-eval env) (cdr expression))))
             (clutter-eval (cons op values) env)))
          (t
           (error "Not an operator: ~A." f)))))

(defstruct clutter-operator function name)
(defun make-operator (variables body env)
  (make-clutter-operator 
   :function
   (lambda (*denv* &rest values)
     (let ((env (make-child-env env variables values)))
       (loop for sexp in body
          for last-value = (clutter-eval sexp env)
          finally (return last-value))))))

(defstruct clutter-function operator)
(defun make-function (operator)
  (make-clutter-function :operator operator))

(defstruct clutter-symbol-operator operator)
(defun make-symbol-operator (operator)
  (make-clutter-symbol-operator :operator operator))

(defun combiner-name (combiner)
  (cond
    ((clutter-operator-p combiner) (clutter-operator-name combiner))
    ((clutter-function-p combiner) (combiner-name (clutter-function-operator combiner)))
    ((clutter-symbol-operator-p combiner) (combiner-name (clutter-symbol-operator-operator combiner)))))

(defmethod print-object ((o clutter-operator) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defmethod print-object ((o clutter-function) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defmethod print-object ((o clutter-symbol-operator) s)
  (let ((name (combiner-name o)))
    (print-unreadable-object (o s :type t :identity (null name))
      (princ (combiner-name o) s))))

(defun invoke (operator env args)
  (if (clutter-operator-p operator)
      (apply (clutter-operator-function operator) env args)
      (error "Not a function: ~A." operator)))

(defmacro defprimitive (name value)
  `(extend *global-env* ',name ,value))

(defmacro defprimop (name vau-list &body body)
  `(defprimitive ,name
       (make-clutter-operator
        :name ',name
        :function (lambda ,vau-list ,@body))))

(defmacro defprimfun (name vau-list &body body)
  `(defprimitive ,name
       (make-function
        (make-clutter-operator
         :name ',name
         :function (lambda (*denv* ,@vau-list)
                     ,@body)))))

(defprimop vau (static-env env-var vau-list &rest body)
  (make-clutter-operator
   :function
   (lambda (*denv* &rest values)
     (multiple-value-bind (required optional rest)
         (parse-ordinary-lambda-list vau-list)
       (declare (ignore optional))
       (unless (or (= (length values) (length vau-list))
                   (and rest (>= (length values) (1- (length vau-list))))
                   (error "Wrong number of arguments")))
       (let ((env (make-child-env static-env
                                  (list* env-var rest required)
                                  (list* *denv*
                                         (nthcdr (length required) values)
                                         (subseq values 0 (length required))))))
         (loop for sexp in body
               for last-value = (clutter-eval sexp env)
               finally (return last-value)))))))

(defprimfun wrap (operative)
  (make-function operative))

(defprimfun unwrap (function)
  (clutter-function-operator function))

(defprimfun eval (expression environment)
  (clutter-eval expression environment))

(defprimfun + (&rest values)
  (reduce #'+ values))

(defprimfun cons (car cdr)
  (cons car cdr))

(defprimfun car (cons)
  (car cons))

(defprimfun cdr (cons)
  (cdr cons))

(defprimfun list (&rest values)
  values)

(defprimfun list* (&rest values)
  (apply #'list* values))

(defprimfun random (max)
  (random max))

(defprimop def! (*denv* var value)
  (extend *denv* var (clutter-eval value *denv*))
  var)

(defprimop set-var! (*denv* var value)
  (setf (lookup var *denv*) (clutter-eval value *denv*)))

(defprimop if (*denv* test if-true if-false)
  (if (clutter-true-p (clutter-eval test *denv*))
      (clutter-eval if-true *denv*)
      (clutter-eval if-false *denv*)))

(defprimop symbolize! (*denv* var value)
  (let ((val (clutter-eval value *denv*)))
    (assert (clutter-operator-p val))
    (extend *denv* var (make-symbol-operator val))))

(defprimfun symbolize (&rest values)
  (assert (clutter-operator-p (car values)))
  (make-symbol-operator (car values)))

(defprimfun eq (obj1 obj2)
    (if (eq obj1 obj2)
        :t
        :f))

(defprimfun print (obj)
  (print obj))

(defun clutter-true-p (exp)
  (if (not (eq exp :f)) t nil))
