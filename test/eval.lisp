(in-package :clutter)

(def-suite eval :in clutter)

(in-suite eval)

(test evaluate/symbol
  (let ((frame (make-stack-frame "lexical binding block" (current-scope))))
    (with-frame frame
      (signals error (evaluate (clutter-intern "x")))
      (bind (clutter-intern "x") 1 :lexical)
      (is (= 1 (evaluate (clutter-intern "x"))))
      (bind (clutter-intern "y") 2 :function) ; hack hack hack
      (signals error (evaluate (clutter-intern "y"))))))

;; TODO - Actually write this macro shit!
(test evaluate/macro)

(test evaluate/literal
  ;; TODO - there's not really many self-evaluating things yet.
  (is (= 1 (eval-clutter-code "1"))))

(test evaluate/quote
  (is (eq (clutter-intern "foo") (eval-clutter-code "(quote foo)")))
  (is (equal '(1 2 3) (eval-clutter-code "(quote (1 2 3))")))
  (is (= 1 (eval-clutter-code "(quote 1)"))))

(test evaluate/if
  ;; XXX - booleans may be changed.
  (is (= 1 (eval-clutter-code "(if t 1 2)")))
  (is (= 2 (eval-clutter-code "(if f 1 2)"))))

(test evaluate/do
  ;; TODO - do some side-effects and make sure all forms are actually being evaluated.
  (is (= 3 (eval-clutter-code "(do 1 2 3)"))))

(test evaluate/bind-lexical-variables
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x 1) x)"))))

(test evaluate/bind-lexical-functions
  (signals error (eval-clutter-code "(bind-lexical-functions (x 1) x)"))
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (fun x))")))
  (is (= 5 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (x 5))")))
  (is (= 25 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) (* x x))) (x 5))")))
  (is (= 25 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) (* x x))) ((fun x) 5))"))))

(test evaluate/lambda
  (is (clutter-function-p (eval-clutter-code "(lambda (x) x)")))
  (is (= 5 (eval-clutter-code "((lambda (x) x) 5)")))
  (is (= 25 (eval-clutter-code "((lambda (x) (* x x)) 5)"))))

(test evaluate/fun
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (fun x))")))
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (bind-lexical-variables (x 1) (fun x)))")))
  (is (= 1 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (bind-lexical-variables (x 1) ((fun x) x)))"))))

(test evaluate/var
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x 1) (var x))")))
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-variables (x (lambda (x) x)) (var x))")))
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x (lambda (x) x)) ((var x) 1))")))
  (is (= 1 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (bind-lexical-variables (x 1) (x (var x))))"))))

;; TODO
(test evaluate/set-lexical-variables)
(test evaluate/set-lexical-functions)

;; TODO ... sigh
(test evaluate/define-global-variable)
(test evaluate/define-global-function)
(test evaluate/define-global-namespace)

(test evaluate/function-call
  (is (= 3 (eval-clutter-code "(+ 1 2)"))))
