(in-package :clutter)

(def-suite environments :in clutter)

(in-suite environments)

(test stack-frame)
(test *stack*)
(test bind)
(test with-frame)
(test push-initial-binding)
(test push-initial-function-binding)
(test current-scope)
(test current-env)
(test lookup)
(test setf-lookup)
