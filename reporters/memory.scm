(module veritas-memory-reporter

  (*success-notifications*
   *failure-notifications*
   *pending-notifications*
   reset-memory-reporter!)


  (import chicken scheme)
  (use veritas)

  (define *success-notifications* (list))
  (define *failure-notifications* (list))
  (define *pending-notifications* (list))

  ;; stores the results in memory
  (define (reset-memory-reporter!)
    (set! *success-notifications* (list))
    (set! *failure-notifications* (list))
    (set! *pending-notifications* (list)))

  (define ((make-appender binding) result)
    (set! binding (cons result binding)))

  (current-success-notification-receiver (make-appender *success-notifications*))
  (current-failure-notification-receiver (make-appender *failure-notifications*))
  (current-pending-notification-receiver (make-appender *pending-notifications*))

)
