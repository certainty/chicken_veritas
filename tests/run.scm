(use test)
(load "../veritas")



(test-group "verify"
  (verify (= 3 4)))

(test-group "falsify")

(test-group "verify-every")

(test-group "falsify-every")

(test-group "pending")

(test-group "describe")

(test-group "tag")

(test-group "verifiers")
