(in-package #:coverage-gate)

(defun percentage (covered total)
  (when (plusp total)
    (* 100 (/ covered total))))

(defun percent-text (value)
  (if value
      (format nil "~,1F%" (float value 1.0d0))
      "n/a"))

(defun source-path (source-root file)
  (format nil "~A/~A" (string-right-trim "/" source-root) file))

(defun evaluate-rows (rows rules source-root minimum-line minimum-branch)
  (let ((totals (vector 0 0 0 0))
        (failures nil))
    (dolist (row rows)
      (let* ((source (source-path source-root (getf row :file)))
             (ranges
              (if (probe-file source)
                  (source-exclusion-lines source rules)
                  nil)))
        (multiple-value-bind (branch-covered branch-total)
            (branch-counts-outside-exclusions row ranges)
          (let* ((counts (getf row :counts))
                 (line-value (percentage (aref counts 0) (aref counts 1)))
                 (branch-value (percentage branch-covered branch-total))
                 (reasons nil)
                 (line-failed (and line-value (< line-value minimum-line)))
                 (branch-failed
                  (and branch-value (< branch-value minimum-branch))))
            (when line-failed
              (push
               (format nil
                       "line ~A, required ~A"
                       (percent-text line-value)
                       (percent-text minimum-line))
               reasons))
            (when branch-failed
              (push
               (format nil
                       "branch ~A, required ~A"
                       (percent-text branch-value)
                       (percent-text minimum-branch))
               reasons))
            (setf (getf row :ranges) ranges
                  (getf row :values) (list line-value branch-value))
            (when
                (or branch-failed
                    (and line-failed
                         (not (uncovered-lines-excluded-p row ranges))))
              (push (list :file (getf row :file) :reasons (nreverse reasons))
                    failures))
            (incf (aref totals 0) (aref counts 0))
            (incf (aref totals 1) (aref counts 1))
            (incf (aref totals 2) branch-covered)
            (incf (aref totals 3) branch-total)))))
    (values (nreverse failures) totals)))

(defun print-report (rows failures totals output)
  (format output "| file | line | branch | uncovered lines |~%")
  (format output "|---|---:|---:|---|~%")
  (dolist (row rows)
    (format output
            "| ~A | ~A | ~A | ~A |~%"
            (getf row :file)
            (percent-text (first (getf row :values)))
            (percent-text (second (getf row :values)))
            (getf row :uncovered)))
  (format output
          "| total | ~A | ~A |  |~%"
          (percent-text (percentage (aref totals 0) (aref totals 1)))
          (percent-text (percentage (aref totals 2) (aref totals 3))))
  (if failures
      (progn
        (terpri output)
        (dolist (failure failures)
          (format output
                  "  ~A: ~{~A~^; ~}~%"
                  (getf failure :file)
                  (getf failure :reasons)))
        (dolist (row rows)
          (dolist (location (getf row :branch-locations))
            (unless (= (second location) (third location))
              (format output
                      "  ~A line ~D: ~D/~D branches covered~%"
                      (getf row :file)
                      (first location)
                      (second location)
                      (third location)))))
        (format output
                "coverage gate: FAIL (~D of ~D files below threshold)~%"
                (length failures)
                (length rows))
        1)
      (progn
        (format output
                "coverage gate: PASS (all ~D files at or above threshold)~%"
                (length rows))
        0)))

(defun resolve-input (given)
  (multiple-value-bind (value origin)
      (resolve-setting "input"
                       given
                       "COVERAGE_SUMMARY"
                       (merge-pathnames "cl-markdown-kit-coverage/per-file.tsv"
                                        (uiop:ensure-directory-pathname
                                         (uiop:temporary-directory))))
    (declare (ignore origin))
    value))

(defun execute-gate (arguments output)
  (let* ((given (parse-arguments arguments))
         (input (resolve-input given))
         (source-root (gethash "source-root" given *default-source-root*))
         (exclusions (gethash "exclusions" given *default-exclusions*))
         (minimum-line
          (resolve-percent "min-line"
                           given
                           "CL_MARKDOWN_COVERAGE_MIN_LINE"
                           *default-line-minimum*))
         (minimum-branch
          (resolve-percent "min-branch"
                           given
                           "CL_MARKDOWN_COVERAGE_MIN_BRANCH"
                           *default-branch-minimum*))
         (rows (read-rows input))
         (rules (read-exclusion-rules exclusions)))
    (multiple-value-bind (failures totals)
        (evaluate-rows rows rules source-root minimum-line minimum-branch)
      (print-report rows failures totals output))))

(defun run (arguments &key
                      (output *standard-output*)
                      (error-output *error-output*))
  (handler-case (execute-gate arguments output)
    (coverage-gate-error (condition)
      (format error-output
              "~A: ~A~%"
              *program-name*
              (coverage-gate-error-message condition))
      (when (coverage-gate-error-usage-p condition)
        (usage error-output))
      2)
    (error (condition)
      (format error-output "~A: ~A~%" *program-name* condition)
      2)))
