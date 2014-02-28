(module veritas-repl-reporter
  *
  (import chicken scheme data-structures csi)
  (use veritas veritas-base-reporter fmt fmt-color)

  (define (colorize format)
    (if (reporter-use-colors?) format identity))

  (define (print-success result out)
    (fmt out ((colorize fmt-green) ((colorize fmt-bold) (cat (current-success-designator) "  ")))))

  (define (print-failure result out)
    (fmt out ((colorize fmt-red) ((colorize fmt-bold) (cat (current-failure-designator) "  FAILED"))) nl)
    (fmt out ((colorize fmt-red) (verification-result-message result))))

  (define-record-printer (verification-result result out)
    (if (verification-failure? result)
        (print-failure result out)
        (print-success result out)))
)
