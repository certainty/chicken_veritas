(use test)
(load "../veritas")
(load "../veritas-verifiers")

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

(test-group "verify"
  (test "invokation of success notifier" #t
    (with-protocol
      (verify #t)
      (verification-success? (car *protocol*))))
  (test "invokation of failure notifier" #t
    (with-protocol
      (verify #f)
      (verification-failure? (car *protocol*)))))

(test-group "falsify"
  (test "invokation of failure notifier" #t
    (with-protocol
      (falsify #t)
      (verification-failure? (car *protocol*))))
  (test "invokation of success notifier" #t
    (with-protocol
      (falsify #f)
      (verification-success? (car *protocol*)))))

(test-group "verify-every")

(test-group "falsify-every")

(test-group "pending"
            (test "invokation of pending notifier"
                  pending:
                  (with-protocol
                   (pending
                    (verify pending:)
                    (car *protocol*))))
            (test "it doesn't run the contained tests"
                  '(#f)
                  (with-protocol
                   (pending
                    (verify #f)
                    *protocol*))))

(test-group "describe")

(test-group "tag")

(test-group "verifiers"
  (test-group "is"
              (test "with values"
                    #t
                    (verification-success? (verify 3 is 3)))
              (test "with predicate"
                    #t
                    (verification-success? (verify 3 is > 2)))
              (test "with true"
                    #t
                    (verification-success? (verify #t is true)))
              (test "with false"
                    #t
                    (verification-success? (verify #f is false)))))
