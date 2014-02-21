(module veritas-console-reporter
  ()
  (import chicken scheme extras)
  (use veritas veritas-base-reporter fmt fmt-color posix (only data-structures conc))

  (define failure-count 0)
  (define success-count 0)
  (define pending-count 0)
  (define total-count   0)

  (define reporter-summary-on-exit   (make-parameter #t))
  (define reporter-failure-exit-code (make-parameter 1))
  (define reporter-success-exit-code (make-parameter 0))


  (on-exit (lambda ()
             (when (reporter-summary-on-exit)
               (report-summary))
             (_exit (if (zero? failure-count) (reporter-success-exit-code) (reporter-failure-exit-code)))))

  (define (update-statistics what)
    (case what
      ((success) (set! success-count (add1 success-count)))
      ((failure) (set! failure-count (add1 failure-count)))
      (else (set! pending-count (add1 pending-count))))
    (set! total-count (add1 total-count)))

  (define (report-summary)
    (if (reporter-use-colors?)
        (report-summary/colors)
        (report-summary/nocolors)))

  (define (report-summary/colors)
    (newline)
    (printf "Total: ~a Passed: ~a Pending: ~a Failed: ~a" total-count success-count pending-count failure-count)
    (newline)
    (flush-output))

  (define (report-summary/nocolors)
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

  (define (pretty-print-expression expr)
    (if (and (= 3 (length expr)) (equal? '(boolean-verifier) (caddr expr)))
        `(,(car expr) ,(cadr expr))
        expr))

  (define (report-success/colors result)
    (update-statistics 'success)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (fmt-green (cat (current-success-designator) " " description)))
      (newline)))

  (define (report-success/nocolors result)
    (update-statistics 'success)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (print (conc (current-success-designator) " " description))))

  (define (report-failure result)
    (if (reporter-use-colors?)
        (report-failure/colors result)
        (report-failure/nocolors result)))

  (define (report-failure/colors result)
    (update-statistics 'failure)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (fmt-red (cat (current-success-designator) " " description)))
      (newline)
      (fmt #t (cat "  " (fmt-red (verification-result-message result))))
      (newline)))

  (define (report-failure/nocolors result)
    (update-statistics 'failure)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (print (conc (current-failure-designator) " "  description))
      (print (conc "  " (verification-result-message result)))))

  (define (report-pending subj)
    (if (reporter-use-colors?)
        (report-pending/colors subj)
        (report-pending/nocolors subj)))

  (define (report-pending/colors result)
    (update-statistics 'pending)
    (let* ((subj (verification-result-subject result))
           (description (or (meta-data-get subj 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression subj))))
           (reason (meta-data-get subj 'pending))
           (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (fmt #t (fmt-yellow (cat (current-pending-designator) reason-str " " description)))
      (newline)))

  (define (report-pending/nocolors subj)
    (update-statistics 'pending)
    (let* ((description (or (meta-data-get subj 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression subj))))
          (reason (meta-data-get subj 'pending))
          (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (print (conc (current-pending-designator) reason-str " " description))))

  (current-success-notification-receiver report-success)
  (current-failure-notification-receiver report-failure)
  (current-pending-notification-receiver report-pending)

)
