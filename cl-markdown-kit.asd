;;;; cl-markdown-kit.asd
(in-package #:asdf-user)

(unless (member :sbcl *features*)
  (error "cl-markdown-kit v0.1.0 supports SBCL only"))

(asdf:defsystem "cl-markdown-kit"
  :description "CommonMark and GFM parser for Common Lisp"
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
  :depends-on ("cl-parser-kit" ; Public spans use cl-parser-kit:span.
               "cl-codec-kit") ; Octet input decoding uses cl-codec-kit.
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "source")
               (:file "nodes"))
  :in-order-to ((test-op (test-op "cl-markdown-kit/test"))))

(asdf:defsystem "cl-markdown-kit/test"
  :description "Tests for cl-markdown-kit"
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
  :depends-on ("cl-markdown-kit" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "conformance-spec")
               (:file "conformance-test")
               (:file "helpers")
               (:file "source")
               (:file "nodes"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (let* ((condition-class
                      (find-symbol "MARKDOWN-RESOURCE-LIMIT-ERROR"
                                   "CL-MARKDOWN-KIT"))
                    (base-class
                      (find-symbol "MARKDOWN-KIT-ERROR" "CL-MARKDOWN-KIT"))
                    (condition
                      (make-condition condition-class
                                      :limit-name :nesting-depth
                                      :limit 64
                                      :actual 65
                                      :context "document"
                                      :message "maximum nesting depth exceeded"))
                    (report (with-output-to-string (stream)
                              (princ condition stream))))
               (assert (typep condition base-class))
               (assert (= 64
                          (uiop:symbol-call
                           :cl-markdown-kit
                           :markdown-resource-limit-error-limit
                           condition)))
               (assert (= 65
                          (uiop:symbol-call
                           :cl-markdown-kit
                           :markdown-resource-limit-error-actual
                           condition)))
               (assert (search "maximum nesting depth exceeded" report))
               (format t "cl-markdown-kit startup tests: 4 passed~%")
               (unless (uiop:symbol-call :cl-markdown-kit/test :run-tests)
                 (error "cl-markdown-kit test suite failed")))))
