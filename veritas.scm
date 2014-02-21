(module veritas
  *
  (import chicken scheme data-structures extras srfi-1 kvlists)
  (require-library matchable kvlists)
  (import-for-syntax matchable)

(define current-meta-data (make-parameter '()))

(define-record verification-subject quoted-expression expression-promise meta-data)
(define-record verification-result id subject message status)

(define (fail    subject message) (make-verification-result 'nil subject message 'fail))
(define (pass    subject)         (make-verification-result 'nil subject "" 'pass))
(define (pending subject)         (make-verification-result 'nil subject "" 'pending))

(define (verification-failure? result)
  (and (verification-result? result)
       (eq? 'fail (verification-result-status result))))

(define (verification-success? result)
  (and (verification-result? result)
       (eq? 'pass (verification-result-status result))))

(define (verification-pending? result)
  (and (verification-result? result)
       (eq? 'pending (verification-result-status result))))


;; The base library assumes nothing about outputting/handling  failed or succeeded verifications.
;; All it does is provide a protocoll that other parts can hook into to actually do something useful with this information
(define current-failure-notification-receiver (make-parameter (constantly #t)))
(define current-success-notification-receiver (make-parameter (constantly #t)))
(define current-pending-notification-receiver (make-parameter (constantly #t)))

(define (notify-failure result)
  ((current-failure-notification-receiver) result))

(define (notify-success result)
  ((current-success-notification-receiver) result))

(define (notify-pending result)
  ((current-pending-notification-receiver) result))

(define (notify result)
  (cond
   ((verification-failure? result)
    (notify-failure result))
   ((verification-success? result)
    (notify-success result))
   ((verification-pending? result)
    (notify-pending result))
   (else (error "invalid result" result)))
  result)

;; META-DATA
(define (merge-alists lhs rhs)
  (fold (lambda (elt ls) (alist-update (car elt) (cdr elt) ls)) lhs rhs))

(define (merge-meta-data meta-data)
  (merge-alists (current-meta-data) (kvlist->alist meta-data)))

(define (meta-data-get subject key)
  (alist-ref key (verification-subject-meta-data subject)))

(define (collect-verification-results thunk)
  (let* ((results '())
         (handler (lambda (result) (set! results (cons result results)))))
    (parameterize ((current-failure-notification-receiver handler)
                   (current-success-notification-receiver handler)
                   (current-pending-notification-receiver handler))
      (thunk)
      (reverse results))))

;; this little indirection is here to have control over
;; how/if tests are run.
;; for example one might want to run them in a sandbox
;; or in its own thread
(define (run-verification id subject verifier complement?)
  (if (meta-data-get subject 'pending)
      (set-id! id (pending subject))
      (set-id! id (apply-verifier subject verifier complement?))))

(define (set-id! id result)
  (verification-result-id-set! result id)
  result)

(define (apply-verifier subject verifier complement?)
  ;; add timing and error handling
  (condition-case (verifier subject complement?)
    (e () (condition->verification-failure e))))

(define (condition->verification-failure condition) #t)

;; the verifier protocol is simple
;; a verifier is a procedure that returns a procedure of two arguments
;; 1) subject - the verification subject
;; 2) complement? - is that in complement context
(define ((boolean-verifier . args) subject complement?)
  (let* ((expr        (verification-subject-expression-promise subject))
         (quoted-expr (verification-subject-quoted-expression subject))
         (result      (if complement? (not (force expr)) (force expr))))
    (if result
        (pass subject)
        (fail subject
              (if complement?
                  (sprintf "Expected ~S not to hold" (cadr quoted-expr))
                  (sprintf "Expected ~S to hold" (cadr quoted-expr)))))))


;; SYNTAX
;; TODO: find a way to generate both verify and falsify with only one syntax

(define-syntax define-verify
  (syntax-rules ()
    ((_ name complement?)
     (define-syntax name
       (ir-macro-transformer
        (lambda (expr inject compare)
          (match expr
            ((_ expr)
             `(name ,expr (boolean-verifier)))
            ((_ expr (? string? description))
             `(name ,expr (boolean-verifier) ,description))
            ((_ expr verifier)
             (let ((id (gensym 'veritas)))
               `(let ((subject (make-verification-subject
                                (quote (verify ,expr ,verifier))
                                (delay ,expr)
                                (current-meta-data))))
                  (notify (run-verification (quote ,id) subject ,verifier complement?)))))
            ((_ expr verifier (? string? description))
             `(meta (description: ,description)
                (name ,expr ,verifier)))
            (else (syntax-error (quote name) "Invalid syntax")))))))))

(define-verify verify #f)
(define-verify falsify #t)

;; (define-syntax verify
;;   (ir-macro-transformer
;;    (lambda (expr inject compare)
;;      (match expr
;;        ((_ expr)
;;         `(verify ,expr (boolean-verifier)))
;;        ((_ expr (? string? description))
;;         `(verify ,expr (boolean-verifier) ,description))
;;        ((_ expr verifier)
;;         (let ((id (gensym 'veritas)))
;;           `(let ((subject (make-verification-subject
;;                            (quote (verify ,expr ,verifier))
;;                            (delay ,expr)
;;                            (current-meta-data))))
;;              (notify (run-verification (quote ,id) subject ,verifier #f)))))
;;        ((_ expr verifier (? string? description))
;;         `(meta (description: ,description)
;;            (verify ,expr ,verifier)))
;;        (else (syntax-error 'verify "Invalid syntax"))))))


;; (define-syntax falsify
;;   (ir-macro-transformer
;;    (lambda (expr inject compare)
;;      (match expr
;;        ((_ expr)
;;         `(falsify ,expr (boolean-verifier)))
;;        ((_ expr (? string? description))
;;         `(falsify ,expr (boolean-verifier) ,description))
;;        ((_ expr verifier)
;;         (let ((id (gensym 'veritas)))
;;           `(let ((subject (make-verification-subject
;;                            (quote (falsify ,expr ,verifier))
;;                            (delay ,expr)
;;                            (current-meta-data))))
;;              (notify (run-verification (quote ,id) subject ,verifier #t)))))
;;        ((_ expr verifier (? string? description))
;;         `(meta (description: ,description)
;;            (falsify ,expr ,verifier)))
;;        (else (syntax-error 'falsify "Invalid syntax"))))))


(define-syntax verify-every
  (syntax-rules ()
    ((_ expr e e+ ...)
     (list
       (verify expr e)
       (verify expr e+) ...))))

(define-syntax falsify-every
  (syntax-rules ()
    ((_ expr e e+ ...)
     (list
      (falsify expr e)
      (falsify expr e+) ...))))

(define-syntax pending
  (syntax-rules ()
    ((_ reason (body0 ...))
     (meta (pending: reason)
       (body0 ...)))
    ((_ body0 ...)
     (meta (pending: #t)
       body0 ...))))

(define-syntax describe
  (syntax-rules ()
    ((_ description body0 ...)
     (meta (description: description)
         body0 ...))))

(define-syntax meta
  (syntax-rules ()
    ((_ (k v ...) body0 ...)
     (parameterize ((current-meta-data (merge-meta-data (quote (k v ...)))))
       body0 ...))))



)
