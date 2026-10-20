(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (define-condition runtime-report-error (error)
      ()
      (:report (lambda (condition stream)
                 (format stream "~A" (compute-report condition)))))))
