(defpackage #:coverage-reproduction-constants (:use #:cl))
(defpackage #:coverage-reproduction-runtime (:use #:cl))
(in-package #:coverage-reproduction-constants)
(defconstant +same-name+ 7)
(in-package #:coverage-reproduction-runtime)
(defvar +same-name+ :runtime-value)
(defstruct runtime-sample
  (value coverage-reproduction-runtime::+same-name+))
