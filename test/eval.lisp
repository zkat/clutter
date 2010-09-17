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
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x 1) x)")))
  (signals error (eval-clutter-code "(bind-lexical-variables (x 1 y x) y)"))
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x 1) (bind-lexical-variables (y x) y))")))
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x 1) (bind-lexical-functions (x (lambda (x) (* x x))) x))"))))

(test evaluate/bind-lexical-functions
  (signals error (eval-clutter-code "(bind-lexical-functions (x 1) x)"))
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (fun x))")))
  (is (= 5 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (x 5))")))
  (is (= 5 (eval-clutter-code "(bind-lexical-functions (x (lambda (x) x)) (bind-lexical-variables (x 5) (x x)))")))
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

(test evaluate/set-lexical-variables
  (is (= 2 (eval-clutter-code "(bind-lexical-variables (x 1) (set-lexical-variables x 2))")))
  (is (= 2 (eval-clutter-code "(bind-lexical-variables (x 1) (set-lexical-variables x 2) x)")))
  ;; Sequential setting
  (is (= 3 (eval-clutter-code "(bind-lexical-variables (x 1) (set-lexical-variables x 2 x 3) x)")))
  (is (not (clutter-function-p (eval-clutter-code "(bind-lexical-variables (x 1) (bind-lexical-functions (x (lambda (x) (* x x))) (set-lexical-variables x 2) x))"))))
  (is (= 1 (eval-clutter-code "(bind-lexical-variables (x 1) (bind-lexical-variables (x 2) (set-lexical-variables x 3)) x)"))))

(test evaluate/set-lexical-functions
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-functions (x (lambda () 1)) (set-lexical-functions x (lambda () 2)))")))
  (is (= 2 (eval-clutter-code "(bind-lexical-functions (x (lambda () 1)) (set-lexical-functions x (lambda () 2)) (x))")))
  (is (= 2 (eval-clutter-code "(bind-lexical-functions (x (lambda () 1)) ((set-lexical-functions x (lambda () 2))))")))
  ;; Sequential setting
  (is (eq (clutter-intern "yes")
         (eval-clutter-code "(bind-lexical-functions (x (lambda () 'no)) (set-lexical-functions x (lambda () 'no) x (lambda () 'yes)) (x))")))
  (is (clutter-function-p (eval-clutter-code "(bind-lexical-functions (x (lambda () 'test)) (bind-lexical-variables (x 1) (set-lexical-functions x (lambda () 'test-success)) (fun x)))")))
  (is (= 1 (eval-clutter-code "(bind-lexical-functions (x (lambda () 1)) (bind-lexical-functions (x (lambda () 2)) (set-lexical-functions x (lambda () 3))) (x))"))))

;; TODO ... sigh
(test evaluate/define-variable)
(test evaluate/define-function)

(test evaluate/block/return-from
  (is (= 2 (eval-clutter-code "(block x (return-from x 2) 1)")))
  (is (= 2 (eval-clutter-code "(block x (+ 1 (block x (return-from x 1))))")))
  (is (= 1 (eval-clutter-code "(block x (+ 1 (block y (return-from x 1))))"))))

(test evaluate/function-call
  (is (= 3 (eval-clutter-code "(+ 1 2)"))))

(test evaluate/recursion
  (eval-clutter-code "(define-function factorial (lambda (n) (if (=? n 0) 1 (* n (factorial (- n 1))))))")
  (is (= 120 (eval-clutter-code "(factorial 5)")))
  (eval-clutter-code "(define-function fib (lambda (n) (if (<? n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
  (is (= 55 (eval-clutter-code "(fib 10)"))))
