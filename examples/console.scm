(use veritas veritas-verifiers veritas-console-reporter fmt fmt-color)

(verify #t is true)
(verify-every 3 
             (is a number)
             (is > 0))

(describe "it is 5"
  (falsify 4 is 5))

(pending
  (verify 3 is 2))
