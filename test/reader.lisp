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
(test terminating-macro-char-p
  (set-clutter-reader-macro-function #\! (lambda (stream char) (declare (ignore stream char)) 'test))
  (is (terminating-macro-char-p #\!))
  (is (not (terminating-macro-char-p #\?)))
  (unset-clutter-reader-macro #\!))

(test reader-macro-function
  (is (null (reader-macro-function #\!)))
  (set-clutter-reader-macro-function #\! (lambda (stream char) stream char))
  (is (eq 'test (funcall (reader-macro-function #\!) 'stream 'char)))
  (unset-clutter-reader-macro #\!)
  (is (null (reader-macro-function #\!))))

(test unset-clutter-reader-macro
  (is (null (unset-clutter-reader-macro #\!)))
  (set-clutter-reader-macro-function #\! (lambda (stream char) stream char))
  (is (eql #\! (unset-clutter-reader-macro #\!))))

(test set-clutter-reader-macro-function)
(test clutter-read-delimited-list
  (with-input-from-string (s "1 2 3 4]")
    (is (equal '(1 2 3 4) (clutter-read-delimited-list #\] s))))
  (with-input-from-string (s "1 2 3 4)")
    (is (equal '(1 2 3 4) (clutter-read-delimited-list #\) s)))))

(test reader-macro-function/open-paren
  (with-input-from-string (s "(1 2 3)")
    (is (equal '(1 2 3) (clutter-read s)))))

(test reader-macro-function/quote
  (with-input-from-string (s "'foo")
    (is (equal (list (clutter-intern "quote") (clutter-intern "foo"))
               (clutter-read s))))
  (with-input-from-string (s "''foo")
    (is (equal (list (clutter-intern "quote")
                     (list (clutter-intern "quote") (clutter-intern "foo")))
               (clutter-read s)))))

(test reader-macro-function/unmatched-closed-paren
  (with-input-from-string (s ")")
    (signals error (clutter-read s)))
  (with-input-from-string (s "(1 2 3)")
    (eos:finishes (clutter-read s))))

(test read-token
  (with-input-from-string (s "123 456")
    (is (string= "123" (read-token s))))
  (with-input-from-string (s "(1 2 3)")
    (is (equal '(1 2 3) (read-token s)))))

(test parse-token
  (is (= 1 (parse-integer-token "1")))
  (is (= 1.2 (parse-float-token "1.2")))
  (is (eq (clutter-intern "hello") (parse-symbol-token "hello")))
  (is (= 1/2 (parse-rational-token "1/2")))
  #+nil(is (= #c(1.4 123) (parse-complex-token "1.4+123i"))))

(test parse-integer-token
  (is (= 1 (parse-integer-token "1")))
  (is (null (parse-integer-token "1.2")))
  (is (null (parse-integer-token "hello")))
  (is (null (parse-integer-token "1/2")))
  (is (null (parse-integer-token "1.4+123i"))))

(test parse-rational-token
  (is (null (parse-rational-token "1")))
  (is (null (parse-rational-token "1.2")))
  (is (null (parse-rational-token "hello")))
  (is (= 1/2 (parse-rational-token "1/2")))
  (is (null (parse-rational-token "1.4+123i"))))

(test parse-float-token
  (is (null (parse-float-token "1")))
  (is (= 1.2 (parse-float-token "1.2")))
  (is (= 1.3e-17 (parse-float-token "1.3e-17")))
  (is (null (parse-float-token "hello")))
  (is (null (parse-float-token "1/2")))
  (is (null (parse-float-token "1.4+123i"))))

(test parse-complex-token)

(test symbol-illegal-characters-p
  (is (null (symbol-illegal-characters-p "Hello")))
  (is (symbol-illegal-characters-p (format nil "Hello~AWorld" *keyword-marker*)))
  (is (symbol-illegal-characters-p (format nil "Hello~AWorld" *namespace-marker*)))
  (is (symbol-illegal-characters-p (format nil "Hello~AWorld" *subnamespace-marker*))))

(test parse-symbol-token/basic
  (is (eq (clutter-intern "foo") (parse-symbol-token "foo")))
  (is (eq (clutter-intern "Foo") (parse-symbol-token "Foo")))
  (is (not (eq (clutter-intern "Foo") (parse-symbol-token "foo"))))
  (is (eq *namespace* (clutter-symbol-namespace (parse-symbol-token "foo"))))
  (let ((*namespace* (make-namespace)))
    (is (eq *namespace* (clutter-symbol-namespace (parse-symbol-token "foo"))))))

(test parse-symbol-token/keywords
  (is (clutter-keyword-p (parse-symbol-token ":foo")))
  (is (string= "foo" (clutter-symbol-name (parse-symbol-token ":foo"))))
  (is (eq (parse-symbol-token ":foo") (evaluate (parse-symbol-token ":foo")))))

(test parse-symbol-token/qualified-symbol
  (signals error (parse-symbol-token "foo:bar")) ; it should be an error if namespace foo doesn't exist
  (ensure-namespace "foo")
  (let ((symbol (parse-symbol-token "foo:bar")))
    (is (clutter-symbol-p symbol))
    (is (eq (find-namespace "foo") (clutter-symbol-namespace symbol)))))

(test clutter-read
  (with-input-from-string (*standard-input* "foo")
    (is (clutter-symbol-p (clutter-read))))
  (with-input-from-string (s "foo")
    (is (clutter-symbol-p (clutter-read s)))))
