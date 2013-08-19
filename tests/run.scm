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


(test-group "verify"
  (test "invokation of success notifier"
        #t
        (with-protocol*
          (verify #t)
          (verification-success? (car *success-notifications*))))

  (test "invokation of failure notifier" #t
    (with-protocol*
      (verify #f)
      (verification-failure? (car *failure-notifications*)))))

(test-group "falsify"
  (test "invokation of failure notifier" #t
    (with-protocol*
      (falsify #t)
      (verification-failure? (car *failure-notifications*))))

  (test "invokation of success notifier" #t
    (with-protocol*
      (falsify #f)
      (verification-success? (car *success-notifications*)))))

(test-group "verify-every")

(test-group "falsify-every")

(test-group "pending"
            (test "invokation of pending notifier"
                  pending:
                  (with-protocol*
                   (pending
                    (verify pending:)
                    (car *pending-notifications*))))
            (test "it doesn't run the contained tests"
                  '()
                  (with-protocol*
                   (pending
                    (verify (error "test"))
                    *success-notifications*))))

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
                    (verification-success? (verify #f is false)))
              (test "list-including"
                    #t
                    (verification-success? (verify (list 1 2) is (list-including 2))))
              (test "list-including negative"
                    #t
                    (verification-failure? (verify (list 1 2) is (list-including 0))))
              (test "list-including negative (no list)"
                    #t
                    (verification-failure? (verify 2 is (list-including 2))))

              (test "vector-including"
                    #t
                    (verification-success? (verify (vector 1 2) is (vector-including 2))))
              (test "vector-including negative"
                    #t
                    (verification-failure? (verify (vector 1 2) is (vector-including 0))))
              (test "vector-including negative (no vector)"
                    #t
                    (verification-failure? (verify 2 is (vector-including 2))))

              (test "none-of"
                    #t
                    (verification-success? (verify 1 is (none-of 4 5 6))))
              (test "none-of negative"
                    #t
                    (verification-failure? (verify 1 is (none-of 1 5 6))))
              (test "any-of"
                    #t
                    (verification-success? (verify 1 is (any-of 1 2 3))))
              (test "any-of negative"
                    #t
                    (verification-failure? (verify 1 is (any-of 0))))))
