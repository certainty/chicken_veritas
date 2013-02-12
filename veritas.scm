;; This is the new implementation of missbehave. It is a heavily simplified version of the original code.
;; For the reasons on why i implemented it please refer to:
;; {{link_to_blogpost}


(define pending? (make-parameter #f))
(define current-description (make-parameter #f))

;; The base library assumes nothing about outputting/handling  failured or succeeded verifications.
;; All it does is provide a protocoll that other parts can hook into to actually do something useful with this information

(define current-failure-notification-receiver (make-parameter (lambda _ #t)))
(define current-success-notification-receiver (make-parameter (lambda _ #t)))

(define (notify-failure  . args)
  (apply (current-failure-notification-receiver) args))

(define (notify-success . args)
  (apply (current-success-notification-receiver) args))

;; these are the core macros that do the basic dispatch.
;; They are really just a thin wrapper to call the matcher.
;; all the "heavy lifting" is done inside the matcher procedure
(define-syntax verify
  (syntax-rules ()
    ((_ expr)
     (verify expr (boolean-matcher)))
    ((_ expr (matcher-name matcher-args+ ...))
     (let ((matcher (matcher-name  matcher-args+ ...)))
       (run-matcher (quote expr) (delay expr) #f matcher)))
    ((_ expr matcher-name matcher-args+ ...)
     (verify expr (matcher-name matcher-args+ ...)))))

(define-syntax verify-every
  (syntax-rules ()
    ((_ expr verification more-verifications ...)
     (begin
       (verify expr verification)
       (verify expr more-verifications)) ...)))

(define-syntax falsify
  (syntax-rules ()
    ((_ expr)
     (falsify expr (boolean-matcher)))
    ((_ expr (matcher-name matcher-args+ ...))
     (let ((matcher (matcher-name matcher-args+ ...)))
       (run-matcher  (quote expr) (delay expr) #t matcher)))
    ((_ expr matcher-name matcher-args+ ...)
     (falsify expr (matcher-name matcher-args+ ...)))))

(define-syntax falsify-every
  (syntax-rules ()
    ((_ expr falsification more-falsifications ...)
     (begin
       (verify expr falsification)
       (verify expr more-falsifications)) ...)))

(define-syntax pending
  (syntax-rules ()
    ((_ expr more-exprs)
     (parameterize ((pending? #t))
       expr more-exprs ...))))

(define-syntax describe
  (syntax-rules ()
    ((_ description e e+ ...)
     (parameterize ((current-description description))
       e e+ ...))))

;; i added that little indirection to have control on
;; how/if tests are run.
;; for example one might to run them in a sandbox
;; or in its own thread

(define (run-matcher quoted-expr expr complement? matcher)
  (let* ((result (matcher complement? quoted-expr expr))
         (failure-message (if complement? cadr caddr)))
    (if (not (car result))
        (notify-failure (failure-message result))
        (notify-success quoted-expr))))

;; the matcher protocoll is simple
;; a matcher i expected to return a procedure that receives three arguments
;; 1) complement? - is that in complement context
;; 2) quoted-expr - the quoted-expr that shall be checked
;; 3) expr        - a promise fore the expression

;; this is the buildin matcher that allows us to run the simplified form
;; for verify
(define ((boolean-matcher . args) complement? quoted-expr expr)
  (let ((result (if complement? (not (force expr)) (force expr))))
    (if result
        (list #t)
        (list #f (if complement? (sprintf "Expected ~S not to hold" quoted-expr) (sprintf "Expected ~S to hold" quoted-expr))))))
