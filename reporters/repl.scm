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

  (define-record-printer (verification-success result out)
    (fmt out (fmt-green (fmt-bold (cat (success-designator) "  ")))))

  (define-record-printer (verification-failure result out)
    (begin
      (fmt out (fmt-red (fmt-bold (cat (failure-designator) "  "))))
      (fmt out (fmt-red (verification-failure-message result)))))

  (define (verify-toplevel args)
    (print args))

  (toplevel-command 'v verify-toplevel ",v EXP\tVerify expression")


  )
