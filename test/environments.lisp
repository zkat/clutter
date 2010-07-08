(in-package :clutter)

(def-suite environments :in clutter)

(in-suite environments)

(test stack-frame
  (let ((frame (make-stack-frame "test")))
    (is (typep frame 'stack-frame))
    (mapc #'(lambda (accessor)
              (is (typep (funcall accessor frame) 'hash-table)))
          (list #'stack-frame-lexicals
                #'stack-frame-dynamics
                #'stack-frame-functions
                #'stack-frame-namespaces))
    (is (listp (stack-frame-scope frame)))))
(test *stack*
  (is (listp *stack*))
  (is (string= (stack-frame-name (car (last *stack*))) "global")))
(test binding
  (mapc #'(lambda (env)
            (let* ((ns (make-namespace))
                   (symbol (make-clutter-symbol :name "test" :namespace ns))) ;TODO: clutter-gensym to ensure no collision
              (is (not (clutter-boundp symbol env)))
              (is (bind symbol "value" env))
              (is (clutter-boundp symbol env))
              (is (unbind symbol env) "Failed to unbind ~A from ~A." symbol env)))
        '(:function :lexical :dynamic :namespace)))
(test with-frame
  (let ((old-head (first *stack*))
        (new-frame (make-stack-frame "test")))
    (with-frame new-frame
      (is (eq old-head (second *stack*)))
      (is (eq new-frame (first *stack*))))
    (is (eq old-head (first *stack*)))))
(test push-initial-binding)
(test push-initial-function-binding)
(test current-scope
  (let* ((frame1 (make-stack-frame "test1"))
         (frame2 (make-stack-frame "test2" (list frame1))))
    (with-frame frame1
      (with-frame frame2
       (let ((scope (current-scope)))
         (is (eq (first scope) frame2))
         (is (eq (second scope) frame1))
         (is (eq (car (last scope)) (car (last *stack*)))))))))
(test current-env)
(test lookup
  (mapc #'(lambda (env)
            (let* ((ns (make-namespace))
                   (symbol (make-clutter-symbol :name "test" :namespace ns)) ;TODO: clutter-gensym to ensure no collision
                   (value "value"))
              (is (bind symbol value env))
              (is (eq (lookup symbol env) value))
              (is (unbind symbol env) "Failed to unbind ~A from ~A." symbol env)))
        '(:function :lexical :dynamic :namespace)))
(test setf-lookup
    (mapc #'(lambda (env)
            (let* ((ns (make-namespace))
                   (symbol (make-clutter-symbol :name "test" :namespace ns)) ;TODO: clutter-gensym to ensure no collision
                   (value "value")
                   (other-value "other-value"))
              (is (bind symbol value env))
              (is (eq (setf (lookup symbol env) other-value) other-value))
              (is (eq (lookup symbol env) other-value))
              (is (unbind symbol env) "Failed to unbind ~A from ~A." symbol env)))
        '(:function :lexical :dynamic :namespace)))
