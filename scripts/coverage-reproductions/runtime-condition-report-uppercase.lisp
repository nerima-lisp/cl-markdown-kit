(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (define-condition runtime-uppercase-report-error (error)
      ()
      (:REPORT (lambda (condition stream)
                 (format stream "~A" (compute-report condition)))))))
