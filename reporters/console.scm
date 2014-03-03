(module veritas-console-reporter
  (use-short-formatter use-documentation-formatter current-failure-exit-code current-success-exit-code report-status-on-exit)
  (import chicken scheme extras srfi-13 ports srfi-1)
  (use veritas veritas-base-reporter fmt fmt-color posix (only data-structures conc identity string-split))

  (define current-column (make-parameter 0))
  (define passed-count 0)
  (define failed-verifications  '())
  (define pending-verifications '())

  (define current-description #f)

  (define report-status-on-exit (make-parameter #t))

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
    (newline)
    (report-failed-verifications))

  (define (report-pending-verifications)
    (unless (null? pending-verifications)
      (fmt #t ((colorize fmt-bold) "Pending:") nl)
      (for-each report-pending-verification pending-verifications)))

  (define (report-pending-verification result)
    (let* ((reason (meta-data-get (verification-result-subject result) 'pending)))
      (fmt #t (space-to 4) ((colorize fmt-yellow) (cat (extract-description result) " is pending")) nl)
      (when (string? reason)
          (fmt #t (space-to 4) ((colorize fmt-yellow) (cat "REASON: " reason)) nl))
      (fmt #t nl)))

  (define (extract-description result)
    (or (meta-data-get (verification-result-subject result) 'description)
        (pretty-print-expression
         (verification-subject-quoted-expression
          (verification-result-subject result)))))

  (define (report-failed-verifications)
    (unless (null? failed-verifications)
      (fmt #t ((colorize fmt-bold) "Failed:") nl)
      (for-each report-failed-verification (reverse failed-verifications))))

  (define (report-failed-verification entry)
    (let ((id     (car entry))
          (result (cdr entry)))
      (fmt #t (space-to 4) (cat id ") " (extract-description result) nl))
      (newline)
      (report-failure-details result)))

  (define (report-failure-details result)
    (cond
     ((verification-result-condition result)
      (fmt #t (format-failure-lines (format-condition result) 8)))
     (else
      (fmt #t (format-failure-lines (verification-result-message result) 8) nl))))

  (define (format-condition result)
    (with-output-to-string
      (lambda ()
        (let ((condition (verification-result-condition result)))
          (fmt #t (cat (verification-result-message result) " CONDITION") nl)
          (newline)
          (let ((ls (condition->list condition)))
            (for-each
             (lambda (elt)
               (fmt #t (format-condition-kind (car elt) (cdr elt)) nl))
             ls))
          (let ((callchain ((condition-property-accessor 'exn 'call-chain '()) condition)))
            (fmt #t (cleanup-callchain (verification-result-stacktrace result))))))))

  (define (cleanup-callchain string)
    (string-join
     (map (lambda (line) (string-trim line #\tab))
          (string-split string "\n"))
     "\n"))

  (define (format-condition-kind kind properties)
    (with-output-to-string
      (lambda ()
        (let* ((props (remove (lambda (elt) (eq? 'call-chain (car elt))) properties))
               (->string    (lambda (e) (sprintf "~a" e)))
               (prop-keys   (map (o ->string car) props))
               (prop-values (map (o ->string cadr) props)))
          (fmt #t (cat "Kind: " kind) nl)
          (fmt #t "Properties: " nl)
          (fmt #t (tabular " | "
                           (dsp (string-join prop-keys " \n")) " | "
                           (dsp (string-join prop-values " \n")) " | ") nl)))))

  (define (format-failure-lines text spaces)
    (apply cat (map (lambda (text) (cat (space-to spaces) ((colorize fmt-red) text) nl)) (string-split text "\n" #t))))

  (define (report-summary)
    (if (reporter-use-colors?)
        (report-summary/colors)
        (report-summary/nocolors)))

  (define (report-summary/nocolors)
    (let* ((failed-count (length failed-verifications))
           (pending-count (length pending-verifications))
           (total-count (+ passed-count failed-count pending-count)))
      (printf "Total: ~a Passed: ~a Pending: ~a Failed: ~a" total-count passed-count pending-count failed-count)
      (newline)
      (flush-output)))

  (define (report-summary/colors)
    (let* ((failed-count (length failed-verifications))
           (pending-count (length pending-verifications))
           (total-count (+ passed-count failed-count pending-count)))
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
  (define current-formatter (make-parameter #f))

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
    (current-pending-formatter short/pending-formatter)
    (current-formatter 'short))

  ;; documentation
  (define (doc/success-formatter result)
    (fmt #t
         (space-to (current-column))
         ((colorize fmt-green) (cat (current-success-designator) " " (extract-description result) nl))))

  (define (doc/failure-formatter result failure-id)
    (fmt #t
         (space-to (current-column))
         ((colorize fmt-red) (cat (current-failure-designator) " " (extract-description result) " [ID: " failure-id "]" nl))))

  (define (doc/pending-formatter result)
    (let* ((reason (meta-data-get (verification-result-subject result) 'pending))
           (reason-str (if (string? reason) (conc "[" reason "] ") "")))
      (fmt #t
           (space-to (current-column))
           ((colorize fmt-yellow) (cat (current-pending-designator) " " (extract-description result) " " reason-str nl)))))

  (define (group-handler groupname state)
    (when (eq? 'documentation (current-formatter))
      (cond
       ((eq? 'start state)
        (fmt #t (space-to (current-column)) ((colorize fmt-bold) groupname) nl)
        (current-column (+ (current-column) 2)))
       ((eq? 'end state)
        (newline)
        (current-column (- (current-column) 2)))
       (else #t))))

  (define (pretty-print-expression expr)
    (string-trim-right
     (fmt #f
          (pretty
           (if (and (= 3 (length expr)) (equal? '(boolean-verifier) (caddr expr)))
               `(,(car expr) ,(cadr expr))
               expr)))
     #\newline))

  (define (use-documentation-formatter)
    (current-success-formatter doc/success-formatter)
    (current-failure-formatter doc/failure-formatter)
    (current-pending-formatter doc/pending-formatter)
    (current-formatter 'documentation))

  ;;the defaut is the short-formatter
  (use-short-formatter)

  (add-success-listener report-success)
  (add-failure-listener report-failure)
  (add-pending-listener report-pending)
  (add-group-listener    group-handler)

  (on-exit (lambda ()
             (when (report-status-on-exit)
               (newline)
               (newline)
               (report-details)
               (newline)
               (report-summary))
             (_exit (if (failures?) (current-failure-exit-code) (current-success-exit-code)))))

  )
