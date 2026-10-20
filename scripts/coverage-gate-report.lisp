(in-package #:coverage-gate)

(defun percentage-text (covered total)
  (if (plusp total)
      (format nil "~,1F%" (float (* 100 (/ covered total)) 1.0d0))
      "n/a"))

(defun aggregate-counts (rows)
  (let ((totals (vector 0 0 0 0)))
    (dolist (row rows totals)
      (let ((counts (getf row :counts)))
        (dotimes (index 4)
          (incf (aref totals index) (aref counts index)))))))

(defun meets-threshold-p (covered total threshold)
  (and (plusp total)
       (>= (* covered 100) (* total threshold))))

(defun print-report (rows output)
  (let ((totals (aggregate-counts rows)))
    (dolist (row rows)
      (let ((counts (getf row :counts)))
        (format output
                "~A: expressions ~D/~D (~A), branches ~D/~D (~A)~%"
                (getf row :file)
                (aref counts 0)
                (aref counts 1)
                (percentage-text (aref counts 0) (aref counts 1))
                (aref counts 2)
                (aref counts 3)
                (percentage-text (aref counts 2) (aref counts 3)))))
    (format output
            "Aggregate expressions: ~D/~D (~A), threshold ~D.0%~%"
            (aref totals 0)
            (aref totals 1)
            (percentage-text (aref totals 0) (aref totals 1))
            *default-expression-minimum*)
    (format output
            "Aggregate branches: ~D/~D (~A), threshold ~D.0%~%"
            (aref totals 2)
            (aref totals 3)
            (percentage-text (aref totals 2) (aref totals 3))
            *default-branch-minimum*)
    (if (and (meets-threshold-p (aref totals 0) (aref totals 1)
                                *default-expression-minimum*)
             (meets-threshold-p (aref totals 2) (aref totals 3)
                                *default-branch-minimum*))
        (progn
          (write-line "coverage gate: PASS" output)
          0)
        (progn
          (write-line "coverage gate: FAIL" output)
          1))))
