(in-package :clutter)

(def-suite reader :in clutter)

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

;; TODO
(test find-namespace)
(test ensure-namespace)

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
