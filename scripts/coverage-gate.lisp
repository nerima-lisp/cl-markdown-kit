(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defpackage #:coverage-gate (:use #:cl) (:export #:run))

(in-package #:coverage-gate)

(defparameter *columns*
  '("file" "line-covered"
           "line-total"
           "branch-covered"
           "branch-total"
           "uncovered-lines"
           "branch-locations"))

(defparameter *default-line-minimum*
  100)

(defparameter *default-branch-minimum*
  100)

(defparameter *default-source-root*
  "src")

(defparameter *default-exclusions*
  "scripts/coverage-exclusions.sexp")

(defparameter *program-name*
  "check-coverage.lisp")

(define-condition coverage-gate-error
  (error)
  ((message :initarg :message :reader coverage-gate-error-message)
   (usage-p :initarg :usage-p :reader coverage-gate-error-usage-p))
  (:report
   (lambda (condition stream)
     (write-string (coverage-gate-error-message condition) stream))))

(let ((directory (uiop:pathname-directory-pathname *load-truename*)))
  (load (merge-pathnames "coverage-gate-command.lisp" directory))
  (load (merge-pathnames "coverage-gate-input.lisp" directory))
  (load (merge-pathnames "coverage-gate-exclusions.lisp" directory))
  (load (merge-pathnames "coverage-gate-report.lisp" directory)))
