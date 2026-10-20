(in-package #:coverage-gate)

(defun gate-fail (message &optional usage-p)
  (error 'coverage-gate-error :message message :usage-p usage-p))

(defun usage (stream)
  (format stream
          "usage: sbcl --script scripts/check-coverage.lisp [--input TSV] [--source-root DIRECTORY]~%~%  --input TSV          per-file coverage summary (default: $COVERAGE_SUMMARY,~%                       else cl-markdown-kit-coverage/per-file.tsv in the system temporary directory)~%  --source-root DIRECTORY tree whose .lisp files must match summary rows exactly (default: src)~%~%Aggregate expressions and branches must each reach 95.0%. Per-file percentages~%are informational only. Exit status is 0 on PASS, 1 when either aggregate threshold~%is missed, and 2 for invalid arguments or inputs.~%"))

(defun valid-option-name-p (name)
  (and (plusp (length name))
       (every (lambda (character)
                (or (char<= #\a character #\z)
                    (char= character #\-)))
              name)
       (char<= #\a (char name 0) #\z)))

(defun parse-arguments (arguments)
  (let ((given (make-hash-table :test #'equal)))
    (loop while arguments
          for argument = (pop arguments)
          do (unless (and (> (length argument) 2)
                          (string= argument "--" :end1 2))
               (gate-fail (format nil "unexpected argument '~A'" argument) t))
             (let ((setting (subseq argument 2)))
               (unless (valid-option-name-p setting)
                 (gate-fail (format nil "unexpected argument '~A'" argument) t))
               (unless (member setting '("input" "source-root") :test #'equal)
                 (gate-fail (format nil "unknown option --~A" setting) t))
               (unless arguments
                 (gate-fail (format nil "option --~A requires a value" setting) t))
               (when (nth-value 1 (gethash setting given))
                 (gate-fail (format nil "option --~A was provided more than once" setting) t))
               (setf (gethash setting given) (pop arguments))))
    given))

(defun resolve-input (given)
  (multiple-value-bind (value present-p) (gethash "input" given)
    (if present-p
        value
        (or (uiop:getenv "COVERAGE_SUMMARY")
            (namestring
             (merge-pathnames
              "cl-markdown-kit-coverage/per-file.tsv"
              (uiop:ensure-directory-pathname (uiop:temporary-directory)))))))
)
