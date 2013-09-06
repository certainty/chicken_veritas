(module veritas-repl-reporter
  *
  (import chicken scheme data-structures csi)
  (use veritas fmt fmt-color )

  (define +mode-map+
    `((plain  . ("passed" "failed"))
      (default . ("✔" "✘"))
      (hearts . ("♥" "♥"))
      (smileys . ("☺" "☹"))))

  (define current-reporting-designators (make-parameter'default))

  (define (success-designator)
    (car (alist-ref (current-reporting-designators) +mode-map+)))

  (define (failure-designator)
    (cadr (alist-ref (current-reporting-designators) +mode-map+)))

  (define (print-success result out)
    (if (reporter-use-colors?)
        (print-success-with-colors result out)
        (print-success-without-colors result out)))

  (define (print-failure result out)
    (if (reporter-use-colors?)
        (print-failure-with-colors result out)
        (print-failure-without-colors result out)))

  (define (print-success-with-colors result out)
    (fmt out (fmt-green (fmt-bold (cat (success-designator) "  ")))))

  (define (print-success-without-colors result out)
    (display (conc (success-designator) "  ") out))

  (define (print-failure-with-colors result out)
    (fmt out (fmt-red (fmt-bold (cat (failure-designator) "  "))))
    (fmt out (fmt-red (verification-failure-message result))))

  (define (print-failure-without-colors result out)
    (display (conc (failure-designator) "  ") out)
    (display (verification-failure-message result) out))

  (define-record-printer (verification-success result out)
    (print-success result out))

  (define-record-printer (verification-failure result out)
    (print-failure result out))

  (define (verify-toplevel)
    (let ((content (read)))
      (eval `(begin (display (verify ,@content)) (newline)))))

  (toplevel-command 'v verify-toplevel ",v EXP\tVerify expression")


  )
