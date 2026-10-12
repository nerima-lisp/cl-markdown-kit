(define-condition coverage-reproduction (error)
  ((message :initarg :message :reader coverage-reproduction-message))
  (:report (lambda (condition stream)
             (format stream "; ~A"
                     (coverage-reproduction-message condition)))))

(define-condition later-coverage-reproduction (error)
  ())
