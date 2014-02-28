(module veritas-repl-reporter
  *
  (import chicken scheme data-structures csi)
  (use veritas veritas-base-reporter fmt fmt-color)

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
    (fmt out (fmt-red (fmt-bold (cat (current-failure-designator) "  FAILED"))) nl)
    (fmt out (fmt-red (verification-result-message result))))

  (define (print-failure/nocolors result out)
    (display (conc (current-failure-designator) "  ") out)
    (display (verification-result-message result) out))

  (define-record-printer (verification-result result out)
    (if (verification-failure? result)
        (print-failure result out)
        (print-success result out)))
)
