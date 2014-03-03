(module verifier-is-test
  ()
  (import chicken scheme)
  (use test veritas veritas-verifiers)
  (test-group "verifiers"
       (test-group "is"
              (test "with values"
                    #t
                    (verification-success? (verify 3 (is 3))))
              (test "with curried predicate "
                    #t
                    (verification-success? (verify 3 (is > 2))))

              (test "with curried predicate / failure output"
                    "Expected 3 to be < 2 "
                    (verification-result-message (verify 3 (is < 2))))

              (test "with true"
                    #t
                    (verification-success? (verify #t (is true))))

              (test "with true / failure output"
                    "expected: #t\n     got: #f\n"
                    (verification-result-message (verify #f (is true))))

              (test "with false"
                    #t
                    (verification-success? (verify #f (is false))))
              (test "list-including"
                    #t
                    (verification-success? (verify (list 1 2) (is (list-including 2)))))

              (test "list-including / failure output"
                    "Expected (1 2) to be a list that includes (3)"
                    (verification-result-message (verify (list 1 2) (is (list-including 3)))))

              (test "list-including negative"
                    #t
                    (verification-failure? (verify (list 1 2) (is (list-including 0)))))
              (test "list-including negative (no list)"
                    #t
                    (verification-failure? (verify 2 (is (list-including 2)))))

              (test "vector-including"
                    #t
                    (verification-success? (verify (vector 1 2) (is (vector-including 2)))))
              (test "vector-including negative"
                    #t
                    (verification-failure? (verify (vector 1 2) (is (vector-including 0)))))
              (test "vector-including negative (no vector)"
                    #t
                    (verification-failure? (verify 2 (is (vector-including 2)))))

              (test "none-of"
                    #t
                    (verification-success? (verify 1 (is (none-of 4 5 6)))))
              (test "none-of negative"
                    #t
                    (verification-failure? (verify 1 (is (none-of 1 5 6)))))
              (test "any-of"
                    #t
                    (verification-success? (verify 1 (is (any-of 1 2 3)))))
              (test "any-of negative"
                    #t
                    (verification-failure? (verify 1 (is (any-of 0)))))
              )
  )

  )
