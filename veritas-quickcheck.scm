(module veritas-quickcheck
  *
  (import chicken scheme srfi-1)
  (use veritas data-generators)

(define current-sample-size (make-parameter 10))

(define (run-quickcheck-verifications proc)
  (let* ((results (collect-verification-results proc))
         (groups  (group-by verification-result-id results)))
    (for-each report-group groups)))


(define (report-group group)
  (let ((results (cdr group)))
    (cond
     ((find verification-failure? results) => notify)
     ((find verification-pending? results) => notify)
     ((find verification-success? results) => notify)
     (else #t))))

(define (group-by proc ls #!optional (equals equal?))
  (let loop ((ls ls) (acc '()))
    (if (null? ls)
        (reverse! acc)
        (let ((key (proc (car ls))))
          (receive (group rest) (partition (lambda (item) (equals key (proc item))) ls)
            (loop rest (cons (cons key group) acc)))))))

(define-syntax quickcheck
  (syntax-rules ()
    ((_ (subject generator-expr) body0 ...)
     (let ((proc (lambda (subject) body0 ...))
           (seq  (gen->sequence (current-sample-size) generator-expr)))
       (run-quickcheck-verifications (lambda () (run-sequence seq proc)))))))

)
