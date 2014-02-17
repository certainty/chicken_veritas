(module veritas-base-reporter
  *
  (import chicken scheme data-structures csi)
  (use veritas)

  (define report-designator-map
    `((plain  . ("[passed]" "[failed]" "[pending]"))
      (short   . ("[S]" "[F]" "[P]"))
      (fancy . (" ✔" " ✘" " ☐"))
      (smileys . (" ☺" " ☹" " ☐"))))

  (define current-reporting-designators (make-parameter 'fancy))

  (define (current-success-designator)
    (car (alist-ref (current-reporting-designators) report-designator-map)))

  (define (current-failure-designator)
    (cadr (alist-ref (current-reporting-designators) report-designator-map)))

  (define (current-pending-designator)
    (caddr (alist-ref (current-reporting-designators) report-designator-map)))

  )
