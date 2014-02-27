(module veritas-console-reporter
  (use-short-formatter use-documentation-formatter current-failure-exit-code current-success-exit-code)
  (import chicken scheme extras)
  (use veritas veritas-base-reporter fmt fmt-color posix (only data-structures conc identity))

  (define current-column 0)
  (define passed-count 0)
  (define failed-verifications  '())
  (define pending-verifications '())

  (define current-failure-exit-code (make-parameter 1))
  (define current-success-exit-code (make-parameter 0))

  (define current-success-formatter (make-parameter (lambda _ #t)))
  (define current-failure-formatter (make-parameter (lambda _ #t)))
  (define current-pending-formatter (make-parameter (lambda _ #t)))

  (define (failures?) (not (null? failed-verifications)))

  (define (report-success result)
    (update-statistics result)
    ((current-success-formatter) result))

  (define report-failure
    (let ((failure-id 0))
      (lambda (result)
        (set! failure-id (add1 failure-id))
        (update-statistics result failure-id)
        ((current-failure-formatter) result failure-id))))

  (define (report-pending result)
    (update-statistics result)
    ((current-pending-formatter) result))

  ;; TODO find a better/adequate name
  (define (update-statistics result #!optional id)
    (cond
     ((verification-failure? result)
      (set! failed-verifications (cons (cons id result) failed-verifications)))
     ((verification-success? result)
      (set! passed-count (add1 passed-count)))
     (else
      (set! pending-verifications (cons result pending-verifications)))))

  (define (report-details)
    (report-pending-verifications)
    (report-failed-verifications))

  (define (report-pending-verifications) #t)

  (define (report-failed-verifications) #t)

  (define (report-summary)
    (if (reporter-use-colors?)
        (report-summary/colors)
        (report-summary/nocolors)))

  (define (report-summary/nocolors)
    (let* ((failed-count (length failed-verifications))
           (pending-count (length pending-verifications))
           (total-count (+ passed-count failed-count pending-count)))
      (newline)
      (printf "Total: ~a Passed: ~a Pending: ~a Failed: ~a" total-count passed-count pending-count failed-count)
      (newline)
      (flush-output)))

  (define (report-summary/colors)
    (let* ((failed-count (length failed-verifications))
           (pending-count (length pending-verifications))
           (total-count (+ passed-count failed-count pending-count)))
      (newline)
      (fmt #t (cat
               (fmt-bold (cat "Total: " total-count))
               " "
               (fmt-green (fmt-bold (cat "Passed: " passed-count)))
               " "
               (fmt-yellow (fmt-bold (cat "Pending: " pending-count)))
               " "
               (fmt-red (fmt-bold (cat "Failed: " failed-count)))))
      (newline)
      (flush-output)))

  ;; formatters
  ;; short
  (define (short/success-formatter result)
    (if (reporter-use-colors?)
        (fmt #t (fmt-green "."))
        (display "."))
    (flush-output))

  (define (short/failure-formatter result _)
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


  (define (doc/failure-formatter result failure-id)
    (let ((colorize (if (reporter-use-colors?) fmt-red identity))
          (description (or (meta-data-get (verification-result-subject result) 'description)
                           (pretty-print-expression
                            (verification-subject-quoted-expression
                             (verification-result-subject result))))))
      (fmt #t (colorize (cat (current-failure-designator) "  " description " [ID: " failure-id "]" nl)))))


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
  (current-pending-notification-receiver report-pending)

  (on-exit (lambda ()
             (newline)
             (report-details)
             (newline)
             (report-summary)
             (_exit (if (failures?) (current-failure-exit-code) (current-success-exit-code)))))

  )
