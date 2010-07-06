(in-package :clutter)

(def-suite environments :in clutter)

(in-suite environments)

(test stack-frame)
(test *stack*)
(test binding
  (let* ((ns (make-namespace))
         (symbol (make-clutter-symbol :name "test" :namespace ns))) ;TODO: clutter-gensym to ensure no collision
    (is (not (clutter-boundp symbol :lexical)))
    (is (bind symbol "value" :lexical))
    (is (clutter-boundp symbol :lexical))
    (is-true (unbind symbol :lexical) "Failed to unbind the symbol.")))
(test with-frame)
(test push-initial-binding)
(test push-initial-function-binding)
(test current-scope)
(test current-env)
(test lookup)
(test setf-lookup)
