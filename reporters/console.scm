(module veritas-console-reporter
  ()
  (import chicken scheme extras)
  (use veritas fmt fmt-color posix (only data-structures conc))

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
    (if (reporter-use-colors?)
        (report-summary-with-colors)
        (report-summary-without-colors)))

  (define (report-summary-without-colors)
    (newline)
    (printf "Total: ~a Passed: ~a Pending: ~a Failed: ~a" total-count success-count pending-count failure-count)
    (newline)
    (flush-output))

  (define (report-summary-with-colors)
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
    (if (reporter-use-colors?)
        (report-success/colors result)
        (report-success/nocolors result)))

  (define (report-success/colors result)
    (update-statistics 'success)
    (if (current-description)
        (fmt #t (fmt-green (cat "✔ " (current-description))))
        (fmt #t (fmt-green (cat "✔ " (verification-success-expression result)))))
    (newline))

  (define (report-success/nocolors result)
    (update-statistics 'success)
    (if (current-description)
        (print (conc "✔ " (current-description)))
        (print (conc "✔ " (verification-success-expression result)))))

  (define (report-failure result)
    (if (reporter-use-colors?)
        (report-failure/colors result)
        (report-failure/nocolors result)))

  (define (report-failure/colors result)
    (update-statistics 'failure)
    (if (current-description)
        (fmt #t (fmt-red (cat "✘ "  (current-description))))
        (fmt #t (fmt-red (cat "✘ " (verification-failure-expression result)))))
    (newline)
    (fmt #t (cat "  " (fmt-red (verification-failure-message result))))
    (newline))

  (define (report-failure/nocolors result)
    (update-statistics 'failure)
    (if (current-description)
        (print (conc "✘ "  (current-description)))
        (print (conc "✘ " (verification-failure-expression result))))
    (print (conc "  " (verification-failure-message result))))

  (define (report-pending expr)
    (if (reporter-use-colors?)
        (report-pending/colors expr)
        (report-pending/nocolors expr)))

  (define (report-pending/colors expr)
    (update-statistics 'pending)
    (if (current-description)
        (fmt #t (fmt-yellow (cat "☐ " (current-description))))
        (fmt #t (fmt-yellow (cat "☐ " expr))))
    (newline))

  (define (report-pending/nocolors expr)
    (update-statistics 'pending)
    (if (current-description)
        (print (conc "☐ " (current-description)))
        (print (conc "☐ " expr))))

  (current-success-notification-receiver report-success)
  (current-failure-notification-receiver report-failure)
  (current-pending-notification-receiver report-pending)

)
