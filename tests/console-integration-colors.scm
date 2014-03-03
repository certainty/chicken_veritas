(use veritas veritas-base-reporter veritas-console-reporter)

(reporter-use-colors? #t)

(verify #t)

(verify #t "It succeeds")

(verify #f)

(verify #f "It fails")

(pending
 (verify #t "It's pending"))

(pending "I'm pending for a reason"
  (verify #t))

(group "Group1"
       (verify #t)
       (group "Group2"
              (verify #f "This failed")))
