(module veritas-core-test
  ()
  (import chicken scheme)
  (use test veritas veritas-verifiers veritas-memory-reporter)

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
      ((lambda (subject-name) body0 ...) (verification-result-subject (car success-notifications)))))))



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
                   (verification-success? (car success-notifications))))

            (test "invokation of failure notifier"
                  #t
                  (with-protocol*
                   (verify #f)
                   (verification-failure? (car failure-notifications))))
            (test "invokation of pending notifier"
                  #t
                  (with-protocol*
                   (pending
                    (verify #f))
                   (verification-pending? (car pending-notifications)))))

(test-group "pending"
  (test "it doesn't run the contained tests"
        '()
        (with-protocol*
           (pending
             (verify (error "test"))
        success-notifications))))


(test-group "describe")

(test-group "meta"
  (test "it adds on piece of data"
    'test
    (extract-subject (subj (meta (foo: test) (verify #t)))
       (meta-data-get subj 'foo)))

  (test-group "nested meta"
    (test "last meta wins"
      'test2
      (extract-subject (subj (meta (foo: test)
                               (meta (foo: test2)
                                 (verify #t))))
        (meta-data-get subj 'foo)))
    (test "is only visible in its scope"
      'test
      (extract-subject (subj (meta (foo: test)
                               (meta (foo: test2) #t)
                               (verify #t)))
        (meta-data-get subj 'foo)))
    (test "adds up"
      '(test test2)
      (extract-subject (subj (meta (foo: test)
                               (meta (bar: test2)
                                 (verify #t))))
       (list (meta-data-get subj 'foo) (meta-data-get subj 'bar))))))

(test-group "verify-every"
   (test "it runs every verification"
         2
         (length (verify-every 3
                     (is > 0)
                     (is > 1)))))

(test-group "falsify-every"
   (test "it runs all falsifications"
         2
         (length (falsify-every 42
                   (is < 0)
                   (is < -10)))))

  )
