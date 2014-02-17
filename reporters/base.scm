(module veritas-base-reporter
  *
  (import chicken scheme data-structures csi)
  (use veritas)

  (define report-designator-map
    `((plain  . ("[passed]" "[failed]"))
      (short   . ("[P]" "[F]"))
      (fancy . (" ✔" " ✘"))
      (smileys . (" ☺" " ☹"))))

  (define current-reporting-designators (make-parameter 'fancy))

  (define (success-designator)
    (car (alist-ref (current-reporting-designators) report-designator-map)))

  (define (failure-designator)
    (cadr (alist-ref (current-reporting-designators) report-designator-map)))
  )
