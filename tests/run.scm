(use test veritas veritas-verifiers veritas-memory-reporter)

(define *protocol* (list))

(define (jot-invokation result)
  (set! *protocol* (cons result *protocol*)))

(define-syntax with-protocol
  (syntax-rules ()
    ((_ code ...)
     (parameterize ((current-success-notification-receiver jot-invokation)
                    (current-failure-notification-receiver jot-invokation)
                    (current-pending-notification-receiver jot-invokation))
       (set! *protocol* (list))
       code ...))))

(define-syntax with-protocol*
  (syntax-rules ()
    ((_ code ...)
     (begin
       (reset-memory-reporter!)
       code ...))))

(define-syntax extract-subject
  (syntax-rules ()
    ((_ (subject-name verify-exp) body0 ...)
     (with-protocol* verify-exp
      ((lambda (subject-name) body0 ...) (verification-success-subject (car  *success-notifications*)))))))


(test-group "verify syntax"
  (test-group "(verify exp)"
     (test "creates subject with correct qouted expression"
        '(verify #t (boolean-verifier))
         (extract-subject (subj (verify #t))
           (verification-subject-quoted-expression subj))))

  (test-group "(verify exp \"description\") "
     (test "creates subject with correct quoted expression"
           '(verify #t (boolean-verifier))
           (extract-subject (subj (verify #t))
             (verification-subject-quoted-expression subj)))
     (test "creates subject with description meta data"
           "test-description"
           (extract-subject (subj (verify #t "test-description"))
             (meta-data-get subj 'description))))

  (test-group "(verify exp verifier) "
     (test "creates subject with correct quoted expression"
           '(verify #t (is #t))
           (extract-subject (subj (verify #t (is #t)))
             (verification-subject-quoted-expression subj))))

    (test-group "(verify exp verifier \"description\") "
     (test "creates subject with correct quoted expression"
           '(verify #t (is #t))
           (extract-subject (subj (verify #t (is #t)))
             (verification-subject-quoted-expression subj)))
     (test "creates subject with description meta data"
           "test-description"
           (extract-subject (subj (verify #t (is #t) "test-description"))
             (meta-data-get subj 'description)))))

(test-group "reporter protocol"
            (test "invokation of success notifier"
                  #t
                  (with-protocol*
                   (verify #t)
                   (verification-success? (car *success-notifications*))))

            (test "invokation of failure notifier"
                  #t
                  (with-protocol*
                   (verify #f)
                   (verification-failure? (car *failure-notifications*))))
            (test "invokation of pending notifier"
                  #t
                  (with-protocol*
                   (pending
                    (verify #f))
                   (verification-subject? (car *pending-notifications*))))
)

;; (test-group "verify-every")

;; (test-group "falsify-every")

;; (test-group "pending"
;; ;            (test "invokation of pending notifier"
;; ;                  pending:
;; ;                  (with-protocol*
;; ;                   (pending
;; ;                    (verify pending:)
;; ;                    (car *pending-notifications*))))
;;             (test "it doesn't run the contained tests"
;;                   '()
;;                   (with-protocol*
;;                    (pending
;;                     (verify (error "test"))
;;                     *success-notifications*))))

;; (test-group "describe")

;; (test-group "tag")

;; (test-group "verifiers"
;;   (test-group "is"
;;               (test "with values"
;;                     #t
;;                     (verification-success? (verify 3 is 3)))
;;               (test "with predicate"
;;                     #t
;;                     (verification-success? (verify 3 is > 2)))
;;               (test "with true"
;;                     #t
;;                     (verification-success? (verify #t is true)))
;;               (test "with false"
;;                     #t
;;                     (verification-success? (verify #f is false)))
;;               (test "list-including"
;;                     #t
;;                     (verification-success? (verify (list 1 2) is (list-including 2))))
;;               (test "list-including negative"
;;                     #t
;;                     (verification-failure? (verify (list 1 2) is (list-including 0))))
;;               (test "list-including negative (no list)"
;;                     #t
;;                     (verification-failure? (verify 2 is (list-including 2))))

;;               (test "vector-including"
;;                     #t
;;                     (verification-success? (verify (vector 1 2) is (vector-including 2))))
;;               (test "vector-including negative"
;;                     #t
;;                     (verification-failure? (verify (vector 1 2) is (vector-including 0))))
;;               (test "vector-including negative (no vector)"
;;                     #t
;;                     (verification-failure? (verify 2 is (vector-including 2))))

;;               (test "none-of"
;;                     #t
;;                     (verification-success? (verify 1 is (none-of 4 5 6))))
;;               (test "none-of negative"
;;                     #t
;;                     (verification-failure? (verify 1 is (none-of 1 5 6))))
;;               (test "any-of"
;;                     #t
;;                     (verification-success? (verify 1 is (any-of 1 2 3))))
;;               (test "any-of negative"
;;                     #t
;;                     (verification-failure? (verify 1 is (any-of 0))))))
