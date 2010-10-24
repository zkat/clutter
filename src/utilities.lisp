;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :clutter)
;;; Utilities for clutter

;;; Derived from Alexandria

(defun parse-vau-list (lambda-list &key (normalize t)
                                  allow-specializers
                                  (normalize-optional normalize)
                                  (normalize-keyword normalize)
                                  (normalize-auxilary normalize)
                       &aux
                       (coptional (cs "&optional"))
                       (crest (cs "&rest"))
                       (ckey (cs "&key"))
                       (callow-other-keys (cs "&allow-other-keys"))
                       (caux (cs "&aux")))
  "Parses a vau-list, returning:

\(values requireds optionals rest keywords allow-other-keys? auxiliaries)

 1. Required parameters.
 2. Optional parameter specifications, normalized into form (NAME INIT SUPPLIEDP)
    where SUPPLIEDP is NIL if not present.
 3. Name of the rest parameter, or NIL.
 4. Keyword parameter specifications, normalized into form ((KEYWORD-NAME NAME) INIT SUPPLIEDP)
    where SUPPLIEDP is NIL if not present.
 5. Boolean indicating &ALLOW-OTHER-KEYS presence.
 6. &AUX parameter specifications, normalized into form (NAME INIT).

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (or (clutter-symbol-p elt)
                           (symbolp elt)
                           (and allow-specializers
                                (consp elt) (= 2 (length elt)) (clutter-symbol-p (first elt))))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil))))
      (dolist (elt lambda-list)
        (cond
          ((eq elt coptional)
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          ((eq elt crest)
           (if (member state '(:required coptional))
               (setf state elt)
               (fail elt)))
          ((eq elt ckey)
           (if (member state '(:required coptional :after-rest))
               (setf state elt)
               (fail elt)))
          ((eq elt callow-other-keys)
           (if (eq state ckey)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          ((eq elt caux)
           (cond ((eq state crest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (t
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (cond
             ((eq state :required)
              (check-variable elt "required parameter")
              (push elt required))
             ((eq state coptional)
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             (normalize-optional
                              (setf elt (append elt '(nil)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil nil))))))
              (push (ensure-list elt) optional))
             ((eq state crest)
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             ((eq state ckey)
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (clutter-symbol-p keyword)
                                  (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                        keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (when normalize-keyword
                                (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv)))))
                       (if (cdr tail)
                           (check-spec tail "keyword-supplied-p parameter")
                           (when normalize-keyword
                             (setf tail (append tail '(nil)))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (make-keyword elt) elt) nil nil)
                                   elt))))
              (push elt keys))
             ((eq state caux)
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (progn
                    (check-variable elt "&aux parameter")
                    (setf elt (list* elt (when normalize-auxilary
                                           '(nil))))))
              (push elt aux))
             (t
              (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux))))