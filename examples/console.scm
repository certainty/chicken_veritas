(use veritas veritas-verifiers)


(let ((the-pair (cons 1 2)))
  (verify the-pair (is '(1 . 2)))
  (verify (cdr the-pair) is 2)
  (verify (car the-pair) is 1))

(let ((the-list (list 1 2 3)))
  (verify the-list is a list)
  (verify the-list (is list-including 2))
  (verify (equal? (list 3 2 1) (reverse the-list))))


(verify #t is true)

(verify-every 3
  (is a number)
  (is > 0)
  (is < 4))

(describe "it is 5"
  (verify 4 is 5))

(pending
  (verify 3 is 2))
