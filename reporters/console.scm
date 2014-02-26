(module veritas-console-reporter
  ()
  (import chicken scheme extras)
  (use veritas veritas-base-reporter fmt fmt-color posix (only data-structures conc))

  (define failure-count 0)
  (define success-count 0)
  (define pending-count 0)
  (define total-count   0)

  (define passed-verifications  '())
  (define failed-verifications  '())
  (define pending-verifications '())

  (define reporter-failure-exit-code (make-parameter 1))
  (define reporter-success-exit-code (make-parameter 0))

  (define current-success-formatter (make-parameter (lambda _ #t)))
  (define current-failure-formatter (make-parameter (lambda _ #t)))
  (define current-pending-formatter (make-parameter (lambda _ #t)))

  (define (report-success result)
    (update-statistics result)
    ((current-success-formatter) result))

  (define (report-failure result)
    (update-statistics result)
    ((current-failure-formatter) result))

  (define (report-pending result)
    (update-statistics result)
    ((current-pending-formatter) result))

  (define (update-statistics result)
    (cond
     ((verification-failure? result)
      (set! failure-count (add1 failure-count))
      (set! failed-verifications (cons result failed-verifications)))
     ((verification-success? result)
      (set! success-cound (add1 success-count))
      (set! passed-verifications (cons result passed-verifications)))
     (else
      (set! pending-cound (add1 pending-count))
      (set! pending-verifications (cons result pending-verifications)))))

  (define (report-details) #t)

  (define (report-summary) #t)

  (on-exit (lambda ()
             (report-details)
             (_exit (if (zero? failure-count) (reporter-success-exit-code) (reporter-failure-exit-code)))))

  (define (report-summary)
    (if (reporter-use-colors?)
        (report-summary/colors)
        (report-summary/nocolors)))

  (define (report-summary/nocolors)
    (newline)
    (printf "Total: ~a Passed: ~a Pending: ~a Failed: ~a" total-count success-count pending-count failure-count)
    (newline)
    (flush-output))

  (define (report-summary/colors)
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

  ;; formatters
  ;; short
  (define (short/success-formatter result)
    (if (reporter-use-colors?)
        (fmt #t (fmt-green "."))
        (display "."))
    (flush-output))

  (define (short/failure-formatter result)
    (if (reporter-use-colors?)
        (fmt #t (fmt-red "F"))
        (display "F"))
    (flush-output))

  (define (short/pending-formatter result)
    (if (reporter-use-colors?)
        (fmt #t (fmt-yellow "P"))
        (display "P"))
    (flush-output))

  (define (use-short-formatter)
    (current-success-formatter short/success-formatter)
    (current-failure-formatter short/failure-formatter)
    (current-pending-formatter short/pending-formatter))

  ;; documentation
  (define (doc/success-formatter result)
    (if (reporter-use-colors?)
        (doc/report-success/colors result)
        (doc/report-success/nocolors result)))

  (define (doc/report-success/colors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (fmt-green (cat (current-success-designator) " " description)))
      (newline)))

  (define (doc/report-success/nocolors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (print (conc (current-success-designator) " " description))))

  (define (doc/failure-formatter result)
    (if (reporter-use-colors?)
        (doc/report-failure/colors result)
        (doc/report-failure/nocolors result)))

  (define (doc/report-failure/colors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (fmt-red (cat (current-failure-designator) " " description)))
      (newline)
      (fmt #t (cat "  " (fmt-red (verification-result-message result))))
      (newline)))

  (define (doc/report-failure/nocolors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (print (conc (current-failure-designator) " "  description))
      (print (conc "  " (verification-result-message result)))))


  (define (doc/pending-formatter result)
    (if (reporter-use-colors?)
        (doc/report-pending/colors result)
        (doc/report-pending/nocolors result)))

  (define (doc/report-pending/colors result)
    (let* ((subj (verification-result-subject result))
           (description (or (meta-data-get subj 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression subj))))
           (reason (meta-data-get subj 'pending))
           (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (fmt #t (fmt-yellow (cat (current-pending-designator) reason-str " " description)))
      (newline)))

  (define (doc/report-pending/nocolors result)
    (let* ((subj (verification-result-subject result))
           ( description (or (meta-data-get subj 'description)
                             (pretty-print-expression
                              (verification-subject-quoted-expression subj))))
           (reason (meta-data-get subj 'pending))
           (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (print (conc (current-pending-designator) reason-str " " description))))

  (define (pretty-print-expression expr)
    (if (and (= 3 (length expr)) (equal? '(boolean-verifier) (caddr expr)))
        `(,(car expr) ,(cadr expr))
        expr))

  (define (use-documentation-formatter)
    (current-success-formatter doc/success-formatter)
    (current-failure-formatter doc/failure-formatter)
    (current-pending-formatter doc/pending-formatter))


  ;;the defaut is the short-formatter
  (use-short-formatter)

  (current-success-notification-receiver report-success)
  (current-failure-notification-receiver report-failure)
  (current-pending-notification-receiver report-pending))
