;;;; cl-markdown-kit.asd
(in-package #:asdf-user)

(unless (member :sbcl *features*)
  (error "cl-markdown-kit v0.2.0 supports SBCL only"))

(asdf:defsystem "cl-markdown-kit"
  :description "CommonMark and GFM parser for Common Lisp"
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :version "0.2.0"
  :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
  :depends-on ("cl-parser-kit" ; Public spans use cl-parser-kit:span.
               "cl-codec-kit") ; Octet input decoding uses cl-codec-kit.
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "html-entities-data")
               (:file "character-data")
               (:file "parser-common")
               (:file "html-escaping")
               (:file "parser-common-links")
               (:file "source")
               (:file "nodes")
               (:file "tree-walker")
               (:file "block-parser")
               (:file "inline-parser")
               (:file "parse")
               (:file "query")
               (:file "html-writer-output")
               (:file "html-writer-footnotes")
               (:file "html-writer-inline")
               (:file "html-writer-block-elements")
               (:file "html-writer-block-events")
               (:file "html-writer")
               (:file "markdown-writer-escape")
               (:file "markdown-writer-state")
               (:file "markdown-writer-inline")
               (:file "markdown-writer-block")
               (:file "markdown-writer-document")
               (:file "edit"))
  :in-order-to ((test-op (test-op "cl-markdown-kit/test"))))

(asdf:defsystem "cl-markdown-kit/yaml"
  :description "YAML front matter support for cl-markdown-kit"
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :version "0.2.0"
  :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
  :depends-on ("cl-markdown-kit" ; Front matter nodes and source spans.
               "cl-yaml-kit") ; YAML parsing and value construction.
  :pathname "src/yaml"
  :serial t
  :components ((:file "package")
               (:file "front-matter"))
  :in-order-to ((test-op (test-op "cl-markdown-kit/yaml/test"))))

(asdf:defsystem "cl-markdown-kit/yaml/test"
  :description "Tests for cl-markdown-kit YAML front matter support"
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :version "0.2.0"
  :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
  :depends-on ("cl-markdown-kit/yaml" ; Exercise the optional YAML parser.
               "cl-weave") ; Run the test suite.
  :pathname "t"
  :serial t
  :components ((:file "yaml"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-markdown-kit/yaml/test :run-tests)
               (error "cl-markdown-kit YAML test suite failed"))))

(asdf:defsystem "cl-markdown-kit/test"
  :description "Tests for cl-markdown-kit"
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :version "0.2.0"
  :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
  :depends-on ("cl-markdown-kit" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "conformance-spec")
               (:file "conformance-test")
               (:file "positions")
               (:file "helpers")
               (:file "source")
               (:file "nodes")
               (:file "parse")
               (:file "block-parser")
               (:file "inline-parser")
               (:file "character-data")
               (:file "query")
               (:file "html-writer")
               (:file "markdown-writer")
               (:file "edit")
               (:file "resources"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-markdown-kit/test :run-tests)
               (error "cl-markdown-kit test suite failed"))))
