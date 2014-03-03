(module veritas-base-reporter
  *
  (import chicken scheme data-structures)

  (define report-designator-map
    `((plain  . ("[passed]" "[failed]" "[pending]"))
      (short   . ("[S]" "[F]" "[P]"))
      (fancy . (" ✔" " ✘" " ☐"))
      (smileys . (" ☺" " ☹" " ☐"))))


  (define reporter-use-colors?
    (make-parameter
     (cond
      ((get-environment-variable "VERITAS_USE_COLORS")
       => (lambda (s) (not (equal? s "0"))))
      (else
       (and (##sys#tty-port? (current-output-port))
            (member (get-environment-variable "TERM")
                    '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                      "linux" "screen" "screen-256color" "vt100")))))))

  (define current-reporting-designators (make-parameter 'fancy))

  (define (current-success-designator)
    (car (alist-ref (current-reporting-designators) report-designator-map)))

  (define (current-failure-designator)
    (cadr (alist-ref (current-reporting-designators) report-designator-map)))

  (define (current-pending-designator)
    (caddr (alist-ref (current-reporting-designators) report-designator-map)))

  )
