(module veritas-repl-reporter
  *
  (import chicken scheme data-structures csi)
  (use veritas veritas-base-reporter fmt fmt-color)

  (define (ignored . _) (void))
  (current-failure-notification-receiver ignored)
  (current-success-notification-receiver ignored)
  (current-pending-notification-receiver ignored)

  (define (print-success result out)
    (if (reporter-use-colors?)
        (print-success/colors result out)
        (print-success/nocolors result out)))

  (define (print-failure result out)
    (if (reporter-use-colors?)
        (print-failure/colors result out)
        (print-failure/nocolors result out)))

  (define (print-success/colors result out)
    (fmt out (fmt-green (fmt-bold (cat (current-success-designator) "  ")))))

  (define (print-success/nocolors result out)
    (display (conc (current-success-designator) "  ") out))

  (define (print-failure/colors result out)
    (fmt out (fmt-red (fmt-bold (cat (current-failure-designator) "  "))))
    (fmt out (fmt-red (verification-failure-message result))))

  (define (print-failure/nocolors result out)
    (display (conc (current-failure-designator) "  ") out)
    (display (verification-failure-message result) out))

  (define-record-printer (verification-success result out)
    (print-success result out))

  (define-record-printer (verification-failure result out)
    (print-failure result out)))
