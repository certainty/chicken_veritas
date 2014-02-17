(use veritas-console data-generators)

(let ((the-pair (cons 1 2)))
  (verify the-pair (is '(1 . 2)))
  (verify (cdr the-pair) is 2)
  (verify (car the-pair) is 1))



(run-sequence (gen->sequence 3 (with-size 5 (gen-list-of gen-fixnum)))
  (lambda (the-list)
    (verify the-list is a list)
    (verify (length the-list) (is 5))
    (verify (reverse (reverse the-list)) (is the-list))))

(verify #t is true)

(verify-every 3
  (is a number)
  (is > 0)
  (is < 4))

(describe "it is 5"
  (verify 5 is 5))

(pending
  (verify 3 is 2))
