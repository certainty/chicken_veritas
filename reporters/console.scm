(module veritas-console-reporter
  (use-short-formatter use-documentation-formatter current-failure-exit-code current-success-exit-code)
  (import chicken scheme extras)
  (use veritas veritas-base-reporter fmt fmt-color posix (only data-structures conc identity))

  (define current-column (make-parameter 0))
  (define passed-count 0)
  (define failed-verifications  '())
  (define pending-verifications '())

  (define current-description #f)

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

  (define (colorize comb)
    (if (reporter-use-colors?) comb identity))

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

  (define (report-pending-verifications)
    (fmt #t "Pending:" nl)
    (for-each report-pending-verification pending-verifications))


  (define (report-pending-verification result)
    (fmt #t (space-to 4) ((colorize fmt-yellow) (extract-description result) nl)))


  (define (extract-description result)
    (or (meta-data-get (verification-result-subject result) 'description)
        (pretty-print-expression
         (verification-subject-quoted-expression
          (verification-result-subject result)))))

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
    (fmt #t ((colorize fmt-green) "."))
    (flush-output))

  (define (short/failure-formatter result _)
    (fmt #t ((colorize fmt-red) "F"))
    (flush-output))

  (define (short/pending-formatter result)
    (fmt #t ((colorize fmt-yellow) "P"))
    (flush-output))

  (define (use-short-formatter)
    (current-success-formatter short/success-formatter)
    (current-failure-formatter short/failure-formatter)
    (current-pending-formatter short/pending-formatter))

  ;; documentation
  (define (doc/success-formatter result)
    (fmt #t
         (space-to (current-column))
         ((colorize fmt-green) (cat (current-success-designator) " " (extract-description result) nl))))


  (define (doc/failure-formatter result failure-id)
    (fmt #t
         (space-to (current-column))
         ((colorize fmt-red) (cat (current-failure-designator) "  " (extract-description result) " [ID: " failure-id "]" nl))))


  (define (doc/pending-formatter result)
    (let* ((reason (meta-data-get (verification-result-subject result) 'pending))
           (reason-str (if (string? reason) (conc "[" reason "]: ") "")))
      (fmt #t
           (space-to (current-column))
           ((colorize fmt-yellow) (cat (current-pending-designator) reason-str " " (extract-description result) nl)))))

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
