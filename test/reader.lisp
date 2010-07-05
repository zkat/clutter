(in-package :clutter)

(def-suite reader :in clutter)

(in-suite reader)

;;;
;;; Symbols
;;;
(test find-clutter-symbol
  (let* ((ns (make-namespace))
         (symbol (add-clutter-symbol (make-clutter-symbol :name "test" :namespace ns))))
    (is (eq symbol (find-clutter-symbol "test" ns)))))

(test add-clutter-symbol
  (let* ((ns (make-namespace))
         (symbol (add-clutter-symbol (make-clutter-symbol :name "test" :namespace ns))))
    (is (clutter-symbol-p symbol))
    (is (eq symbol (find-clutter-symbol "test" ns)))))

(test remove-clutter-symbol
  (let* ((ns (make-namespace))
         (symbol (add-clutter-symbol (make-clutter-symbol :name "test" :namespace ns))))
    (is (clutter-symbol-p symbol))
    (is (clutter-symbol-p (remove-clutter-symbol symbol ns)))
    (is (null (find-clutter-symbol "test" ns)))))

(test clutter-intern
  (let* ((ns (make-namespace))
         (symbol (clutter-intern "test" ns)))
    (is (clutter-symbol-p symbol))
    (is (eq symbol (clutter-intern "test" ns)))
    (is (not (eq symbol (clutter-intern "TEST" ns))))))

;;;
;;; Namespaces
;;;
(test *namespace*
  (is (namespacep *namespace*))
  (is (eq (lookup nil :namespace) *namespace*)))

;; TODO - These need a lot more testing, specially for hierarchical stuff.
(test find-namespace
  (is (namespacep (find-namespace nil)))
  (let ((ns (ensure-namespace "test")))
    (is (eq ns (find-namespace "test")))
    (is (namespacep (find-namespace "test")))))

(test ensure-namespace
  (let ((ns (ensure-namespace "test")))
    (is (namespacep ns))
    (is (eq (find-namespace "test") ns))))

;;;
;;; Reader
;;;
(test terminating-macro-char-p)
(test reader-macro-function)
(test set-clutter-reader-macro-function)
(test clutter-read-delimited-list)
(test reader-macro-function/open-paren)
(test reader-macro-function/quote)
(test reader-macro-function/unmatched-closed-paren)
(test read-token)
(test parse-token)
(test parse-integer-token)
(test parse-rational-token)
(test parse-float-token)
(test parse-complex-token)
(test symbol-illegal-characters-p)
(test parse-symbol-token/basic)
(test parse-symbol-token/keywords)
(test parse-symbol-token/qualified-symbol)
(test clutter-read)
