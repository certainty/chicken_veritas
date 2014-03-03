(module veritas-console-reporter-test
  ()
  (import chicken scheme)
  (use test veritas veritas-console-reporter)

  (report-status-on-exit #f)

  (define-syntax test-contains?
    (syntax-rules ()
      ((_ ?args ...)
       (parameterize ((current-test-comparator irregex-search))
         (test ?args ...)))))

  )
