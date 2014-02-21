(module veritas-verifiers
  *
  (import chicken scheme data-structures extras ports srfi-69)
  (use veritas srfi-1 matchable)

(define (eval-expr complement? expr)
  ((if complement? not identity) expr))

(define (message-from-predicate-form form)
  (if (list? form)
      (let* ((rest (third form))
             (name (car rest))
             (args (flatten (cdr rest))))
        (with-output-to-string
          (lambda ()
            (for-each (cut printf "~A " <>) args))))
      form))

;; (define-syntax is
;;   (syntax-rules (a an true false)
;;     ((_ true)
;;      (is #t))
;;     ((_ false)
;;      (is #f))
;;     ((_ a type)
;;      (verify-type type))
;;     ((_ an type)
;;      (verify-type type))
;;     ((_ pred-or-value)
;;      (is-verifier pred-or-value))
;;     ((_ pred value more-values ...)
;;      (is-verifier/predicate pred (list value more-values ...)))))


(define-syntax is
  (syntax-rules (a an true false)
    ((_ true)
     (is #t))
    ((_ false)
     (is #f))
    ((_ pred-or-value)
     (if (procedure? pred-or-value)
         (is-verifier pred-or-value)
         (is-verifier (make-equal-predicate? pred-or-value))))
    ((_ (predicate-ctor body0 ...))
     (is-verifier (predicate-ctor body0 ...)))
    ((_ pred value0 value1 ...)
     (is-verifier/curried-predicate pred (list value0 value1 ...)))))

(define ((make-equal-predicate? a) subject complement?)
  (let* ((b (force (verification-subject-expression-promise subject)))
         (res (or (equal? a b)
                 (and (number? a)
                      (inexact? a)
                      (inexact? b)
                      (approx-equal? a b)))))
    (values res (sprintf "Expected ~a to be equal to ~a" b a))))

(define current-equality-epsilon (make-parameter 1e-5))

(define (approx-equal? a b  #!optional (epsilon (current-equality-epsilon)))
  (cond
   ((> (abs a) (abs b))
    (approx-equal? b a epsilon))
   ((zero? b)
    (< (abs a) epsilon))
   (else
    (< (abs (/ (- a b) b)) epsilon))))

;;(verify subj (is 3))
;;(verify subj (is predicate?))
;;(verify subj (is (predicate-constructor args)))
;;(verify subj (is curried-pred args)) ;; (is > 3)
(define ((is-verifier pred) subject complement?)
  (let* ((quoted-expr (verification-subject-quoted-expression  subject))
         (expr        (verification-subject-expression-promise subject))
         (value       (force expr)))
    (receive (result message) (pred subject complement?)
        (if result
            (pass subject)
            (fail subject message)))))

(define ((is-verifier/curried-predicate pred values) subject complement?)
  (let* ((quoted-expr (verification-subject-quoted-expression subject))
         (expr        (verification-subject-expression-promise subject))
         (value       (force expr))
         (result      (eval-expr complement? (apply pred value values))))
    (if result
        (pass subject)
        (fail subject
              (if complement?
                  (sprintf "Expected ~S not to be ~S" value quoted-expr)
                  (sprintf "Expected ~S to be ~A"     value (message-from-predicate-form quoted-expr)))))))

(define-syntax verify-type
  (lambda (form rename env)
    (let* ((type (cadr form))
           (type-pred (string->symbol (conc (symbol->string type) "?")))
           (%type-verifier (rename 'type-verifier)))
      `(,%type-verifier ,type-pred (quote ,type)))))

(define ((type-verifier type-pred type) subject complement?)
  (let* ((quoted-expr (verification-subject-quoted-expression))
         (expr        (verification-subject-expression-promise subject))
         (value (force expr))
         (result (eval-expr complement? (type-pred value))))
    (if result
        (pass subject)
        (fail subject (sprintf "Expected ~S ~A be a ~A" value (if complement? "not to" "to") type)))))

(define ((close-to what #!key (delta 0.3)) actual complement?)
  (values (approx-equal? what actual delta)
          (sprintf "Expected ~a to be roughly equal to ~a (epsilon ~a)" actual what delta)))

(define roughly close-to)

(define ((any-of item . more-items) subject complement?)
  (let ((value (force (verification-subject-expression-promise subject))))
    (values (member value (cons item more-items))
            (sprintf "Expected ~a to be a member of ~a" value (cons item more-items)))))

(define ((none-of item . more-items) subject complement?)
  (let ((value (force (verification-subject-expression-promise subject))))
    (values (not (member value (cons item more-items)))
            (sprintf "Expected ~a not to be a member of ~a" value (cons item more-items)))))


(define ((list-including item . more-items) subject complement?)
  (let ((value (force (verification-subject-expression-promise subject))))
    (values  (and (list? value)
                  (every (cut member <> value) (cons item more-items)))
             (sprintf "Expected ~a to be a list that includes ~a" value (cons item more-items)))))

(define ((vector-including . args) subject complement?)
  (let ((value (force (verification-subject-expression-promise subject))))
    (values (and (vector? value)
                 (let ((value (vector->list value)))
                   (every (cut member <> value) args)))
            (sprintf "Expected ~a to be vector that includes ~a" value args))))

(define ((hash-table-including . args) subject complement?)
  (let ((value (force (verification-subject-expression-promise subject))))
    (values (and (hash-table? value)
                 (every (lambda (pair)
                          (equal? (cdr pair) (hash-table-ref value (car pair))))
                        args))
            (sprintf "Expected ~a to be a hash-table that includes ~a" value args))))


)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Have/Has
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-syntax has
;;   (syntax-rules ()
;;     ((_ amount procedure-or-sugar)
;;      (if (procedure? (quote procedure-or-sugar))
;;          (have-matcher amount procedure-or-sugar (quote procedure-or-sugar))
;;          (have-matcher amount (quote procedure-or-sugar) (quote procedure-or-sugar))))))



;; (define (have-matcher expected-amount procedure-or-sugar procedure-or-sugar-name #!key (compare =))
;;   (let ((actual-amount #f))
;;     (matcher
;;      (check (subject)
;;             (let* ((collection (if (procedure? procedure-or-sugar) (procedure-or-sugar (force subject)) (force subject)))
;;                    (item-amount (size collection)))
;;               (set! actual-amount item-amount)
;;               (compare item-amount expected-amount)))

;;      (message (form subject negate)
;;               (if negate
;;                   (sprintf "Didn't expect ~A ~A" expected-amount procedure-or-sugar-name)
;;                   (sprintf "Expected ~A ~A but found ~A"  expected-amount procedure-or-sugar-name actual-amount))))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; raise
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (define-syntax raise
;;   (syntax-rules (error errors with)
;;     ((_ error)
;;      (make-error-matcher))
;;     ((_ errors)
;;      (make-error-matcher))
;;     ((_ (kind more-kinds ...))
;;      (make-error-matcher kinds: '(kind more-kinds ...)))))

;; (define (make-error-matcher #!key (kinds #f) (properties #f))
;;   (let ((message "")
;;         (negative-message ""))
;;     (make-matcher
;;      (lambda (code)
;;        (handle-exceptions exn
;;                           (let* ((condition (condition->list exn))
;;                                  (exn-kinds (map car condition)))
;;                             (cond
;;                              ((and kinds properties) #t)
;;                              (kinds
;;                               (if (every (cut member <> exn-kinds) kinds)
;;                                   #t
;;                                   (begin
;;                                     (set! message (sprintf "Expected exn of kinds ~A but got ~A" kinds exn-kinds))
;;                                         ;FIXME find proper wording
;;                                     (set! negative-message (sprintf "Expected exn not of kinds ~A but got ~A" kinds exn-kinds))
;;                                     #f)))
;;                              (properties #t)
;;                              (else
;;                               (set! message            (sprintf "Expecte errors but didn't get one"))
;;                               (set! negative-message   (sprintf "Expected no errors but got one"))
;;                               #t)))
;;                           (force code)
;;                           #f))
;;      (lambda (form subject negate)
;;        (if negate
;;            negative-message
;;            message)))))
