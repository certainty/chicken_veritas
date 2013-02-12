;; This is the new implementation of missbehave. It is a heavily simplified version of the original code.
;; For the reasons on why i implemented it please refer to:
;; {{link_to_blogpost}


(define pending? (make-parameter #f))
(define current-description (make-parameter #f))

;; The base library assumes nothing about outputting/handling  failed or succeeded verifications.
;; All it does is provide a protocoll that other parts can hook into to actually do something useful with this information

(define current-failure-notification-receiver (make-parameter (lambda _ #t)))
(define current-success-notification-receiver (make-parameter (lambda _ #t)))

(define (notify-failure  . args)
  (apply (current-failure-notification-receiver) args))

(define (notify-success . args)
  (apply (current-success-notification-receiver) args))

;; these are the core macros that do the basic dispatch.
;; They are really just a thin wrapper to call the verifier
;; all the "heavy lifting" is done inside the verifier procedure
(define-syntax verify
  (syntax-rules ()
    ((_ expr)
     (verify expr (boolean-verifier)))
    ((_ expr (verifier-name verifier-args+ ...))
     (let ((matcher (verifier-name  verifier-args+ ...)))
       (run-verifier (quote expr) (delay expr) #f verifier)))
    ((_ expr verifier-name verifier-args+ ...)
     (verify expr (verifier-name verifier-args+ ...)))))

(define-syntax verify-every
  (syntax-rules ()
    ((_ expr e e+ ...)
     (begin
       (verify expr e)
       (verify expr e+)...))))

(define-syntax falsify
  (syntax-rules ()
    ((_ expr)
     (falsify expr (boolean-verifier)))
    ((_ expr (verifier-name verifier-args+ ...))
     (let ((matcher (verifier-name verifier-args+ ...)))
       (run-verifier  (quote expr) (delay expr) #t verifier)))
    ((_ expr verifier-name verifier-args+ ...)
     (falsify expr (verifier-name verifier-args+ ...)))))

(define-syntax falsify-every
  (syntax-rules ()
    ((_ expr e e+ ...)
     (begin
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

;; i added that little indirection to have control on
;; how/if tests are run.
;; for example one might to run them in a sandbox
;; or in its own thread

(define (run-verifier quoted-expr expr complement? verifier)
  (let ((result (verifier complement? quoted-expr expr))
        (failure-message (if complement? cadr caddr)))
    (if (not (car result))
        (notify-failure (failure-message result))
        (notify-success quoted-expr))))

;; the verifier protocoll is simple
;; a verifier i expected to return a procedure that receives three arguments
;; 1) complement? - is that in complement context
;; 2) quoted-expr - the quoted-expr that shall be checked
;; 3) expr        - a promise fore the expression

;; this is the buildin verifier that allows us to run the simplified form
;; for verify
(define ((boolean-verifier . args) complement? quoted-expr expr)
  (let ((result (if complement? (not (force expr)) (force expr))))
    (if result
        (list #t)
        (list #f (if complement? (sprintf "Expected ~S not to hold" quoted-expr) (sprintf "Expected ~S to hold" quoted-expr))))))
