(module veritas
  *
  (import chicken scheme data-structures extras srfi-1 kvlists)
  (require-library matchable kvlists)
  (import-for-syntax matchable)

(define-record verification-subject quoted-expression expression-promise meta-data)

(define-record verification-failure subject message)
(define fail make-verification-failure)

(define-record verification-success subject)
(define pass make-verification-success)


(define pending? (make-parameter #f))
(define current-description (make-parameter #f))
(define current-meta-data (make-parameter '()))

;; The base library assumes nothing about outputting/handling  failed or succeeded verifications.
;; All it does is provide a protocoll that other parts can hook into to actually do something useful with this information
(define current-failure-notification-receiver (make-parameter (constantly #t)))
(define current-success-notification-receiver (make-parameter (constantly #t)))
(define current-pending-notification-receiver (make-parameter (constantly #t)))

;; support procedures for reporter
(define reporter-use-colors?
  (make-parameter
   (cond
    ((get-environment-variable "VERITAS_USE_COLORS")
     => (lambda (s) (not (equal? s "0"))))
    (else
     (and (##sys#tty-port? (current-output-port))
          (member (get-environment-variable "TERM")
                  '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                    "linux" "screen" "screen-256color" "vt100")))))))

(define (notify-failure . args)
  (apply (current-failure-notification-receiver) args))

(define (notify-success . args)
  (apply (current-success-notification-receiver) args))

(define (notify-pending . args)
  (apply (current-pending-notification-receiver) args))

(define (merge-alists lhs rhs)
  (fold (lambda (elt ls) (alist-update (car elt) (cdr elt) ls)) lhs rhs))

(define (merge-meta-data meta-data)
  (merge-alists (current-meta-data) (kvlist->alist meta-data)))

(define (meta-data-get subject key)
  (alist-ref key (verification-subject-meta-data subject)))

(define-syntax meta
  (syntax-rules ()
    ((_ (k v ...) body0 ...)
     (parameterize ((current-meta-data (merge-meta-data (quote (k v ...)))))
       body0 ...))))


;; TODO: find a way to generate both verify and falsify with only one syntax
(define-syntax verify
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match expr
       ((_ expr)
        `(verify ,expr (boolean-verifier)))
       ((_ expr (? string? description))
        `(verify ,expr (boolean-verifier) ,description))
       ((_ expr verifier)
        `(run-verification
          (make-verification-subject
           (quote (verify ,expr ,verifier))
           (delay ,expr)
           (current-meta-data))
          ,verifier
          #f))
       ((_ expr verifier (? string? description))
        `(meta (description: ,description)
           (verify ,expr ,verifier)))
       (else (syntax-error 'verify "Invalid syntax"))))))


(define-syntax falsify
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match expr
       ((_ expr)
        `(falsify ,expr (boolean-verifier)))
       ((_ expr (? string? description))
        `(falsify ,expr (boolean-verifier) ,description))
       ((_ expr verifier)
        `(run-verification
          (make-verification-subject
           (quote (falsify ,expr ,verifier))
           (delay ,expr)
           (current-meta-data))
          ,verifier
          #t))
       ((_ expr verifier (? string? description))
        `(meta (description: ,description)
           (falsify ,expr ,verifier)))
       (else (syntax-error 'falsify "Invalid syntax"))))))


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
    ((_ body0 ...)
     (meta (pending: #t)
       body0 ...))))

(define-syntax describe
  (syntax-rules ()
    ((_ description body0 ...)
     (meta (description: description)
         body0 ...))))

;; this little indirection is here to have control over
;; how/if tests are run.
;; for example one might want to run them in a sandbox
;; or in its own thread
(define (run-verification subject verifier complement?)
  (if (meta-data-get subject 'pending)
      (notify-pending subject)
      (let ((result (apply-verifier subject verifier complement?)))
        (if (verification-failure? result)
            (notify-failure result)
            (notify-success result))
        result)))

(define (apply-verifier subject verifier complement?)
  ;; add timing and error handling
  (verifier subject complement?))

;; the verifier protocol is simple
;; a verifier is a procedure that returns a procedure of two arguments
;; 1) subject - the verification subject
;; 2) complement? - is that in complement context
(define ((boolean-verifier . args) subject complement?)
  (let* ((expr (verification-subject-expression-promise subject))
         (quoted-expr (verification-subject-quoted-expression subject))
         (result (if complement? (not (force expr)) (force expr))))
    (if result
        (pass subject)
        (fail subject
              (if complement?
                  (sprintf "Expected ~S not to hold" (cadr quoted-expr))
                  (sprintf "Expected ~S to hold" (cadr quoted-expr)))))))

)
