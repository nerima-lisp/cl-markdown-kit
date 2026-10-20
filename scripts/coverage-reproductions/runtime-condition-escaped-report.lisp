(define-condition runtime-escaped-report-error (error)
  ()
  (:|REPORT|
   (lambda (condition stream)
     (format stream "~A" (compute-report condition)))))
