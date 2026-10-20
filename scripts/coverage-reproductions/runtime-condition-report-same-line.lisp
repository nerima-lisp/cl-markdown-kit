(define-condition runtime-same-line-report-error (error)
  ()
  (:report (lambda (condition stream) (format stream "~A" (compute-report condition)))))
