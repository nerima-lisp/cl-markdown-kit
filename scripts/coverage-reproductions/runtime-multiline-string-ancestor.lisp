"decoy
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn"
(defparameter *runtime-after-multiline-string* (if flag :covered :uncovered))
