(in-package #:coverage-gate)

(defun gate-fail (message &optional usage-p)
  (error 'coverage-gate-error :message message :usage-p usage-p))

(defun usage (stream)
  (format stream
          "usage: sbcl --script scripts/check-coverage.lisp [--input TSV] [--min-line PERCENT] [--min-branch PERCENT]~%             [--source-root DIRECTORY] [--exclusions FILE]~%~%  --input TSV          per-file coverage summary (default: $COVERAGE_SUMMARY,~%                       else the system temporary directory)~%  --min-line PERCENT   required line coverage (default: $CL_MARKDOWN_COVERAGE_MIN_LINE,~%                       else 100)~%  --min-branch PERCENT required branch coverage (default: $CL_MARKDOWN_COVERAGE_MIN_BRANCH,~%                       else 100)~%  --source-root DIRECTORY source files used to classify exclusions (default: src)~%  --exclusions FILE    exclusion data (default: scripts/coverage-exclusions.sexp)~%~%exit 0 when every file meets both thresholds, 1 when a file is below one, and 2~%when the settings are invalid or the summary cannot be read.~%"))

(defun valid-option-name-p (name)
  (and (plusp (length name))
       (every
        (lambda (character)
          (or (and (char<= #\a character #\z)) (char= character #\-)))
        name)
       (char<= #\a (char name 0) #\z)))

(defun parse-arguments (arguments)
  (let ((given (make-hash-table :test #'equal)))
    (loop while arguments
          for argument = (pop arguments)
          do (unless
                 (and (> (length argument) 2) (string= argument "--" :end1 2))
               (gate-fail (format nil "unexpected argument '~A'" argument) t)) (let ((setting
                                                                                      (subseq
                                                                                       argument
                                                                                       2)))
                                                                                 (unless
                                                                                     (valid-option-name-p
                                                                                      setting)
                                                                                   (gate-fail
                                                                                    (format
                                                                                     nil
                                                                                     "unexpected argument '~A'"
                                                                                     argument)
                                                                                    t))
                                                                                 (unless
                                                                                     (member
                                                                                      setting
                                                                                      '("input"
                                                                                        "min-line"
                                                                                        "min-branch"
                                                                                        "source-root"
                                                                                        "exclusions")
                                                                                      :test
                                                                                      #'equal)
                                                                                   (gate-fail
                                                                                    (format
                                                                                     nil
                                                                                     "unknown option --~A"
                                                                                     setting)
                                                                                    t))
                                                                                 (unless
                                                                                     arguments
                                                                                   (gate-fail
                                                                                    (format
                                                                                     nil
                                                                                     "option --~A requires a value"
                                                                                     setting)
                                                                                    t))
                                                                                 (setf (gethash
                                                                                        setting
                                                                                        given) (pop
                                                                                                arguments))))
    given))

(defun resolve-setting (setting given environment default)
  (multiple-value-bind (value present-p) (gethash setting given)
    (when present-p
      (return-from resolve-setting
        (values value "option")))
    (let ((environment-value (uiop:getenv environment)))
      (when (and environment-value (plusp (length environment-value)))
        (return-from resolve-setting
          (values environment-value environment))))
    (values default "default")))
