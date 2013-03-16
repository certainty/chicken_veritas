(use test)
(load "../veritas")

(define *protocol* (list))

(define (jot-invokation result)
  (set! *protocol* (cons result *protocol*)))

(define-syntax with-protocol
  (syntax-rules ()
    ((_ code ...)
     (parameterize ((current-success-notification-receiver jot-invokation)
                    (current-failure-notification-receiver jot-invokation))
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

(test-group "pending")

(test-group "describe")

(test-group "tag")

(test-group "verifiers")
