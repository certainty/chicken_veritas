(module veritas-memory-reporter

  (success-notifications
   failure-notifications
   pending-notifications
   reset-memory-reporter!)


  (import chicken scheme)
  (use veritas)

  (define success-notifications (list))
  (define failure-notifications (list))
  (define pending-notifications (list))

  ;; stores the results in memory
  (define (reset-memory-reporter!)
    (set! success-notifications (list))
    (set! failure-notifications (list))
    (set! pending-notifications (list)))


  (current-success-notification-receiver
   (lambda (result)
     (set! success-notifications (cons result success-notifications))))

  (current-failure-notification-receiver
   (lambda (result)
     (set! failure-notifications (cons result failure-notifications))))

  (current-pending-notification-receiver
   (lambda (result)
     (set! pending-notifications (cons result pending-notifications))))

)
