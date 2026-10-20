(define-condition runtime-default-error (error)
  ((created-at
    :initform (get-universal-time)))
  (:default-initargs
   :created-at (compute-default-initarg)))
