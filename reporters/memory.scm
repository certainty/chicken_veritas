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

  (define-syntax collect-in
    (syntax-rules ()
      ((_ bucket)
       (lambda (result) (set! bucket (cons result bucket))))))

  (add-success-listener (collect-in success-notifications))
  (add-failure-listener (collect-in failure-notifications))
  (add-pending-listener (collect-in pending-notifications))

)
