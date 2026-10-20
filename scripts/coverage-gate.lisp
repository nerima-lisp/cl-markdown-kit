(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defpackage #:coverage-gate (:use #:cl) (:export #:run))

(in-package #:coverage-gate)

(defparameter *columns*
  '("file" "line-covered" "line-total" "branch-covered" "branch-total"
    "uncovered-lines" "branch-locations"))

(defparameter *default-expression-minimum* 95)
(defparameter *default-branch-minimum* 95)
(defparameter *default-source-root* "src")
(defparameter *program-name* "check-coverage.lisp")

(define-condition coverage-gate-error (error)
  ((message :initarg :message :reader coverage-gate-error-message)
   (usage-p :initarg :usage-p :reader coverage-gate-error-usage-p))
  (:report
   (lambda (condition stream)
     (write-string (coverage-gate-error-message condition) stream))))

(let ((directory (uiop:pathname-directory-pathname *load-truename*)))
  (load (merge-pathnames "coverage-gate-command.lisp" directory))
  (load (merge-pathnames "coverage-gate-input.lisp" directory))
  (load (merge-pathnames "coverage-gate-report.lisp" directory)))

(defun execute-gate (arguments output)
  (let* ((given (parse-arguments arguments))
         (input (resolve-input given))
         (source-root (gethash "source-root" given *default-source-root*))
         (rows (read-rows input)))
    (validate-source-manifest rows source-root)
    (print-report rows output)))

(defun run (arguments &key (output *standard-output*)
                          (error-output *error-output*))
  (handler-case
      (execute-gate arguments output)
    (coverage-gate-error (condition)
      (format error-output "~A: ~A~%"
              *program-name*
              (coverage-gate-error-message condition))
      (when (coverage-gate-error-usage-p condition)
        (usage error-output))
      2)
    (error (condition)
      (format error-output "~A: ~A~%" *program-name* condition)
      2)))
