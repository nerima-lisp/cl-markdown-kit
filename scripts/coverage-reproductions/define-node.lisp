(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-node
    (defstruct (coverage-point (:constructor make-coverage-point (value)))
      (value 0 :type (integer 0 *)))
    :kind :coverage-point))
