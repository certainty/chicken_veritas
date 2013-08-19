(module veritas-console-reporter
  *
  (import chicken scheme extras)
  (use veritas fmt fmt-color posix)

  (define failure-count 0)
  (define success-count 0)
  (define pending-count 0)
  (define total-count   0)

  (on-exit (lambda ()
             (report-summary)
             (_exit (if (zero? failure-count) 0 1))))

  (define (update-statistics what)
    (case what
      ((success) (set! success-count (add1 success-count)))
      ((failure) (set! failure-count (add1 failure-count)))
      (else (set! pending-count (add1 pending-count))))
    (set! total-count (add1 total-count)))

  (define (report-summary)
    (newline)
    (fmt #t (cat
             (fmt-bold (cat "Total: " total-count))
             " "
             (fmt-green (fmt-bold (cat "Passed: " success-count)))
             " "
             (fmt-yellow (fmt-bold (cat "Pending: " pending-count)))
             " "
             (fmt-red (fmt-bold (cat "Failed: " failure-count)))))
    (newline)
    (flush-output))

  (define (report-success result)
    (update-statistics 'success)
    (if (current-description)
        (fmt #t (fmt-green (cat "✔ " (current-description))))
        (fmt #t (fmt-green (cat "✔ " (verification-success-expression result)))))
    (newline))

  (define (report-failure result)
    (update-statistics 'failure)
    (if (current-description)
        (fmt #t (fmt-red (cat "✘ "  (current-description))))
        (fmt #t (fmt-red (cat "✘ " (verification-failure-expression result)))))
    (newline)
    (fmt #t (cat "  " (fmt-red (verification-failure-message result))))
    (newline))

  (define (report-pending expr)
    (update-statistics 'pending)
    (if (current-description)
        (fmt #t (fmt-yellow (cat "☐ " (current-description))))
        (fmt #t (fmt-yellow (cat "☐ " expr))))
    (newline))

  (current-success-notification-receiver report-success)
  (current-failure-notification-receiver report-failure)
  (current-pending-notification-receiver report-pending)

)
