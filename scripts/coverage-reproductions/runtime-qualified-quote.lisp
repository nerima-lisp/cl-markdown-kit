(defpackage #:coverage-reproduction.shadow (:use #:cl) (:shadow #:quote))

(in-package #:coverage-reproduction.shadow)

(defun quote (value)
  (declare (ignore value))
  (compute-default))

(define-condition runtime-quoted-error
  (error)
  ((value :initform (coverage-reproduction.shadow::quote (compute-default)))))
