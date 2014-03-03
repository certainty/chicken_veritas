(module veritas-console-reporter-test
  ()
  (import chicken scheme irregex ports data-structures posix)
  (use test veritas veritas-base-reporter veritas-console-reporter advice utils)

  (report-status-on-exit #f)

  (define-syntax test-contains?
    (syntax-rules ()
      ((_ ?args ...)
       (parameterize ((current-test-comparator irregex-search))
         (test ?args ...)))))

  (define-syntax capture
    (syntax-rules ()
      ((_ ?body ...)
       (with-output-to-string
         (lambda () ?body ...)))))

  (define-syntax with-colors
    (syntax-rules ()
      ((_  ?body ...)
       (parameterize ((reporter-use-colors? #t))
         ?body ...))))

  (define-syntax without-colors
    (syntax-rules ()
      ((_ ?body ...)
       (parameterize ((reporter-use-colors? #f))
         ?body ...))))

  (define example-output-with-colors #<<EOF
test
EOF
    )

  (define example-output-with-colors
    "\x1b[32m.\x1b[0m\x1b[32m.\x1b[0m\x1b[31mF\x1b[0m\x1b[31mF\x1b[0m\x1b[33mP\x1b[0m\x1b[33mP\x1b[0m\x1b[32m.\x1b[0m\x1b[31mF\x1b[0m\n\n\x1b[1mPending:\x1b[0m\x1b[0m\n    \x1b[33m(verify #t) is pending\x1b[0m\n    \x1b[33mREASON: I'm pending for a reason\x1b[0m\n\n    \x1b[33mIt's pending is pending\x1b[0m\n\n\n\x1b[1mFailed:\x1b[0m\x1b[0m\n    1) (verify #f)\n\n        \x1b[31mExpected #f to hold\x1b[0m\n\n    2) It fails\n\n        \x1b[31mExpected #f to hold\x1b[0m\n\n    3) This failed\n\n        \x1b[31mExpected #f to hold\x1b[0m\n\n\n\x1b[1mTotal: 8\x1b[0m\x1b[0m \x1b[32m\x1b[1mPassed: 3\x1b[0m\x1b[32m\x1b[0m \x1b[33m\x1b[1mPending: 2\x1b[0m\x1b[33m\x1b[0m \x1b[31m\x1b[1mFailed: 3\x1b[0m\x1b[31m\x1b[0m\n"
    )

  (define example-output-without-colors
    #<<EOF
..FFPP.F

Pending:
    (verify #t) is pending
    REASON: I'm pending for a reason

    It's pending is pending


Failed:
    1) (verify #f)

        Expected #f to hold

    2) It fails

        Expected #f to hold

    3) This failed

        Expected #f to hold


Total: 8 Passed: 3 Pending: 2 Failed: 3

EOF
    )

  (define (run-veritas-file path)
    (receive (i o pid e) (process* "csi" (list "-s" path))
      (read-all i)))

(test-group "console reporter"
            ;; these are our smoke tests for this reporter
            (test "Integration with colors"
                  example-output-with-colors
                  (run-veritas-file "console-integration-colors.scm"))

            (test "Integration without colors"
                  example-output-without-colors
                  (run-veritas-file "console-integration-no-colors.scm"))

            ;; unit tests
            (test-group "short reporter"
                        (use-short-formatter)
                        (test-group "successful tests"
                                    (test-group "with colors"
                                                (test "It prints a green dot"
                                                      "\x1b[32m.\x1b[0m"
                                                      (capture (with-colors  (verify #t "it is true")))))

                                    (test-group "without colors"
                                                (test "It prints a dot"
                                                      "." (capture (without-colors (verify #t "it is true"))))))


                        (test-group "failung tests"
                                    (test-group "with colors"
                                                (test "It prints a red F"
                                                      "\x1b[31mF\x1b[0m"
                                                      (capture (with-colors  (verify #f "it is false")))))
                                    (test-group "without colores"
                                                (test "It prints an F"
                                                      "F"
                                                      (capture (without-colors (verify #f "it is false"))))))

                        (test-group "pending tests"
                                    (test-group "with colors"
                                                (test "It prints a yellow P"
                                                      "\x1b[33mP\x1b[0m"
                                                      (capture (with-colors (pending (verify #t))))))
                                    (test-group "without colors"
                                                (test "It prints a P"
                                                      "P"
                                                      (capture (without-colors (pending (verify #t))))))))


            (test-group "documentation reporter"
                        (use-documentation-formatter)
                        (test-group "successful tests"
                                    (test-group "with colors"
                                                (test "It prints the expression in green"
                                                      "\x1b[32m ✔ (verify #t)\n\x1b[0m"
                                                      (capture (with-colors (verify #t))))
                                                (test "It prints the description in green"
                                                      "\x1b[32m ✔ It succeeds\n\x1b[0m"
                                                      (capture (with-colors (verify #t "It succeeds")))))

                                    (test-group "without colors"
                                                (test "it prints the expression"
                                                      " ✔ (verify #t)\n"
                                                      (capture (without-colors (verify #t))))
                                                (test "it prints the description"
                                                      " ✔ It succeeds\n"
                                                      (capture (without-colors (verify #t "It succeeds"))))))



                        ))

)
