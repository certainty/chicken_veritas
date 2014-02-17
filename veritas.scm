(module veritas
  *
  (import chicken scheme data-structures extras)

(define-record verification-failure expression message)
(define fail make-verification-failure)

(define-record verification-success expression)
(define pass make-verification-success)


(define pending? (make-parameter #f))
(define current-description (make-parameter #f))

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

;; these are the core macros that do the basic dispatch.
;; They are really just a thin wrapper to call the verifier
;; all the "heavy lifting" is done inside the verifier procedure
(define-syntax verify
  (syntax-rules ()
    ((_ expr)
     (verify expr (boolean-verifier)))
    ((_ expr (verifier-name verifier-args+ ...))
     (let ((verifier (verifier-name  verifier-args+ ...)))
       (run-verifier (quote (verify expr (verifier-name verifier-args+ ...))) (delay expr) #f verifier)))
    ((_ expr verifier-name verifier-args+ ...)
     (verify  expr (verifier-name verifier-args+ ...)))))

(define-syntax verify-every
  (syntax-rules ()
    ((_ expr e e+ ...)
     (list
       (verify expr e)
       (verify expr e+)...))))

(define-syntax falsify
  (syntax-rules ()
    ((_ expr)
     (falsify expr (boolean-verifier)))
    ((_ expr (verifier-name verifier-args+ ...))
     (let ((verifier (verifier-name verifier-args+ ...)))
       (run-verifier (quote (falsify expr (verifier-name verifier-args+ ...))) (delay expr) #t verifier)))
    ((_ expr verifier-name verifier-args+ ...)
     (falsify expr (verifier-name verifier-args+ ...)))))

(define-syntax falsify-every
  (syntax-rules ()
    ((_ expr e e+ ...)
     (list
      (falsify expr e)
      (falsify expr e+) ...))))

(define-syntax pending
  (syntax-rules ()
    ((_ e e+ ...)
     (parameterize ((pending? #t))
       e e+ ...))))

(define-syntax describe
  (syntax-rules ()
    ((_ description e e+ ...)
     (parameterize ((current-description description))
       e e+ ...))))

;; this little indirection is here to have control over
;; how/if tests are run.
;; for example one might want to run them in a sandbox
;; or in its own thread

(define (run-verifier quoted-expr expr complement? verifier)
  (if (pending?)
      (notify-pending quoted-expr)
      (let ((result (verifier complement? quoted-expr expr)))
        (if (verification-failure? result)
            (notify-failure result)
            (notify-success result))
        result)))

;; ;; the verifier protocol is simple
;; ;; a verifier is a procedure that returns a procedure of three arguments
;; ;; 1) complement? - is that in complement context
;; ;; 2) quoted-expr - the quoted-expr that shall be checked
;; ;; 3) expr        - a promise fore the expression

;; ;; this is the builtin verifier that allows us to run the simplified form
;; ;; for verify
(define ((boolean-verifier . args) complement? quoted-expr expr)
  (let ((result (if complement? (not (force expr)) (force expr))))
    (if result
        (pass quoted-expr)
        (fail quoted-expr
              (if complement? (sprintf "Expected ~S not to hold" (cadr quoted-expr)) (sprintf "Expected ~S to hold" (cadr quoted-expr)))))))

)
