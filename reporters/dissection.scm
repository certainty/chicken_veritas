(module veritas-dissection-reporter
  ()
  (import chicken scheme)
  (use veritas veritas-base-reporter dissector)

  (define dissect-every-failure (make-parameter #f))

  (define (failure-handler result)
    (let ((subject (verification-result-subject result)))
      (when (or (dissect-every-failure) (meta-data-get subject 'dissect))
        (parameterize ((dissection-prompt "Dissector>> "))
          (dissect (force (verification-subject-expression-promise subject)))))))

  (add-failure-listener failure-handler))
