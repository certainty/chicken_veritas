(use veritas-console veritas-quickcheck data-generators)

(use-documentation-formatter)
;(use-short-formatter)

;; adding descriptions
(verify #t "This is a test")

;; curried predicate
(verify 3 (is > 3))

;; Diff when strings are compared
(verify "This is my\nfooo" (is "This is my\nbarbaz"))

(verify (cons 1 2) (is '(1 . 2)))

(let ((the-pair (cons 1 2)))
  (verify the-pair (is '(1 . 2)))
  (verify (cdr the-pair) (is 2))
  (verify (car the-pair) (is 1)))

(verify-every 3
  (is > 0)
  (is < 4))

(describe "it is 5"
  (verify 5 (is 5)))

(pending
  (verify 3 (is 2)))

(pending "this is my reason"
  (verify 3 (is 2)))

;; quickcheck functionality
(quickcheck (subj (gen-uint8))
  ;; will probably fail but is reported only once
  (verify subj (is < 100))
  (pending
    (verify subj (is < 100)))
  ;; reported only once
  (verify subj (is > 0)))

;; Grouping
(group "Root"
  (verify 3 (is 3))
  (verify 3 (is 4))
  (group "Level 1"
    (verify 3 (is 3))
    (verify 3 (is 4))
    (group "Level 2"
      (verify 3 (is 3) "With inline description")
      (verify 3 (is 4) "Error inline descritption")
      (describe "With wrapping description"
        (verify 3 (is 3)))
      (describe "Error with wrapping description"
        (verify 3 (is 4))))))
