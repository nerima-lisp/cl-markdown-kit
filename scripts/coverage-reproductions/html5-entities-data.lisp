(defparameter *html5-entities*
  (let ((table (make-hash-table :test (quote equal))))
    (dolist (entry (quote (("amp;" #x26))))
      (setf (gethash (first entry) table) (second entry)))
    table))
