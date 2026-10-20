(define-condition runtime-multiline-report-error (error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~A" (compute-report condition)))))
