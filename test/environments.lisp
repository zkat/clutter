(in-package :clutter)

(def-suite environments :in clutter)

(in-suite environments)

(test stack-frame)
(test *stack*)
(test binding
  (mapc #'(lambda (env)
            (let* ((ns (make-namespace))
                   (symbol (make-clutter-symbol :name "test" :namespace ns))) ;TODO: clutter-gensym to ensure no collision
              (is (not (clutter-boundp symbol env)))
              (is (bind symbol "value" env))
              (is (clutter-boundp symbol env))
              (is (unbind symbol env) "Failed to unbind ~A from ~A." symbol env)))
        '(:function :lexical :dynamic :namespace)))
(test with-frame)
(test push-initial-binding)
(test push-initial-function-binding)
(test current-scope)
(test current-env)
(test lookup)
(test setf-lookup)
