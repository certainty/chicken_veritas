(module veritas-console-reporter
  ()
  (import chicken scheme extras)
  (use veritas veritas-base-reporter fmt fmt-color posix (only data-structures conc))

  (define failure-count 0)
  (define success-count 0)
  (define pending-count 0)
  (define total-count   0)

  (define passed-verifications '())
  (define failed-verifications '())
  (define pending-verifications '())

  (define reporter-summary-on-exit   (make-parameter #t))
  (define reporter-failure-exit-code (make-parameter 1))
  (define reporter-success-exit-code (make-parameter 0))


  ;; (define (report-success result)
  ;;   (update-statistics result)
  ;;   ((current-success-formatter) result))

  ;; (define (report-failure result)
  ;;   (update-statistics result)
  ;;   ((current-failure-formatter) result))

  ;; (define (report-pending result)
  ;;   (update-statistics result)
  ;;   ((current-pending-formatter) result))

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

  ;; the overall output is splitted into 3 parts
  ;; 1) the progress
  ;; 2) the summary of pending and failed tests
  ;; 3) the bottom line showing statistics

  (on-exit (lambda ()
             (when (reporter-summary-on-exit)
               (report-summary))
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

  (define (report-success result)
    (update-statistics result)
    (if (reporter-use-colors?)
        (report-success/colors result)
        (report-success/nocolors result)))

  (define (pretty-print-expression expr)
    (if (and (= 3 (length expr)) (equal? '(boolean-verifier) (caddr expr)))
        `(,(car expr) ,(cadr expr))
        expr))

  (define (report-success/colors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (fmt-green (cat (current-success-designator) " " description)))
      (newline)))

  (define (report-success/nocolors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (print (conc (current-success-designator) " " description))))

  (define (report-failure result)
    (update-statistics result)
    (if (reporter-use-colors?)
        (report-failure/colors result)
        (report-failure/nocolors result)))

  (define (report-failure/colors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (fmt-red (cat (current-failure-designator) " " description)))
      (newline)
      (fmt #t (cat "  " (fmt-red (verification-result-message result))))
      (newline)))

  (define (report-failure/nocolors result)
    (let ((description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (print (conc (current-failure-designator) " "  description))
      (print (conc "  " (verification-result-message result)))))

  (define (report-pending result)
    (update-statistics result)
    (if (reporter-use-colors?)
        (report-pending/colors result)
        (report-pending/nocolors result)))

  (define (report-pending/colors result)
    (let* ((subj (verification-result-subject result))
           (description (or (meta-data-get subj 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression subj))))
           (reason (meta-data-get subj 'pending))
           (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (fmt #t (fmt-yellow (cat (current-pending-designator) reason-str " " description)))
      (newline)))

  (define (report-pending/nocolors result)
    (let* ((subj (verification-result-subject result))
           ( description (or (meta-data-get subj 'description)
                             (pretty-print-expression
                              (verification-subject-quoted-expression subj))))
           (reason (meta-data-get subj 'pending))
           (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (print (conc (current-pending-designator) reason-str " " description))))

  (current-success-notification-receiver report-success)
  (current-failure-notification-receiver report-failure)
  (current-pending-notification-receiver report-pending)

)
