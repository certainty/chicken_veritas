(module veritas-verifiers
  *
  (import chicken scheme data-structures extras ports)
  (use veritas srfi-1)


(define (eval-expr complement? expr)
  ((if complement? not identity) expr))

(define (message-from-predicate-form form)
  (if (list? form)
      (let ((name (symbol->string (car form))))
        (with-output-to-string
          (lambda ()
            (display (string-translate name "-" " "))
            (display " ")
            (for-each (cut printf "~A " <>) (cdr form)))))
      form))

(define-syntax is
  (syntax-rules (a an true false)
    ((_ true)
     (is #t))
    ((_ false)
     (is #f))
    ((_ a type)
     (verify-type type))
    ((_ an type)
     (verify-type type))
    ((_ pred-or-value)
     (is-verifier pred-or-value))
    ((_ pred value more-values ...)
     (is-verifier/predicate pred (list value more-values ...)))))


(define ((is-verifier pred-or-value) complement? quoted-expr expr)
  (let* ((value (force expr))
         (result
          (eval-expr
           complement?
           (if (procedure? pred-or-value)
               (pred-or-value value)
               (equal? pred-or-value value)))))
    (if result
        (pass quoted-expr)
        (cond
         ((procedure? pred-or-value)
          (fail quoted-expr (sprintf "Expected ~S ~A be ~A" value (if complement? "not to" "to") (message-from-predicate-form quoted-expr))))
         (else
          (fail quoted-expr (sprintf "Expected ~S ~A be ~S" value (if complement? "not to" "to") pred-or-value)))))))

(define ((is-verifier/predicate pred values) complement? quoted-expr expr)
  (let* ((value (force expr))
         (result (eval-expr complement? (apply pred value values))))
    (if result
        (pass quoted-expr)
        (fail quoted-expr
              (if complement?
                  (sprintf "Expected ~S not to be ~S" value quoted-expr)
                  (sprintf "Expected ~S to be ~S" value quoted-expr))))))

(define-syntax verify-type
  (lambda (form rename env)
    (let* ((type (cadr form))
           (type-pred (string->symbol (conc (symbol->string type) "?")))
           (%type-verifier (rename 'type-verifier)))
      `(,%type-verifier ,type-pred (quote ,type)))))

(define ((type-verifier type-pred type) complement? quoted-expr expr)
  (let* ((value (force expr))
         (result (eval-expr complement? (type-pred value))))
    (if result
        (pass quoted-expr)
        (fail quoted-expr (sprintf "Expected ~S ~A be a ~A" value (if complement? "not to" "to") type)))))

(define ((close-to what #!key (delta 0.3)) actual)
  (define (approx-equal? a b delta)
    (cond
     ((> (abs a) (abs b))
      (approx-equal? b a delta))
     ((zero? b)
      (< (abs a) delta))
     (else
      (< (abs (/ (- a b) b)) delta))))
  (approx-equal? what actual delta))

(define roughly close-to)

(define ((any-of item . more-items) subject)
  (member subject (cons item more-items)))

(define ((none-of item . more-items) subject)
  (not (member subject (cons item more-items))))

(define ((list-including item . more-items) subject)
  (and (list? subject)
       (every (cut member <> subject) (cons item more-items))))

(define ((vector-including . args) subject)
  (and (vector? subject)
       (let ((subject (vector->list subject)))
         (every (cut member <> subject) args))))

(define ((hash-table-including item . more-items) subject) #t)

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
