(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (defun verify-node-dispatches (&key operation (families '(:block :inline)))
      (declare (ignore operation families))
      t)))
