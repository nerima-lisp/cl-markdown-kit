;;;; cl-markdown-kit.asd
(in-package #:asdf-user)

(unless (member :sbcl *features*)
  (error "cl-markdown-kit v0.2.0 supports SBCL only"))

(defmacro define-cl-markdown-kit-system (name &body options)
  `(asdf:defsystem ,name
     :author "takeokunn"
     :maintainer "takeokunn"
     :license "MIT"
     :version "0.2.0"
     :homepage "https://github.com/nerima-lisp/cl-markdown-kit"
     :bug-tracker "https://github.com/nerima-lisp/cl-markdown-kit/issues"
     :source-control (:git "https://github.com/nerima-lisp/cl-markdown-kit.git")
     ,@options))

(define-cl-markdown-kit-system "cl-markdown-kit"
  :description "CommonMark and GFM parser for Common Lisp"
  :depends-on ("cl-parser-kit" ; Public spans use cl-parser-kit:span.
               "cl-codec-kit") ; Octet input decoding uses cl-codec-kit.
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "html-entities-data")
               (:file "character-data")
               (:file "character-logic")
               (:file "parser-common-data")
               (:file "parser-common")
               (:file "html-escaping")
               (:file "parser-common-links")
               (:file "source")
               (:file "nodes")
               (:file "tree-walker")
               (:file "block-parser-data")
               (:file "block-parser-source")
               (:file "block-parser-references")
               (:file "block-parser-classification")
               (:file "block-parser-nodes")
               (:file "block-parser-parse-state")
               (:file "block-parser-parse-open")
               (:file "block-parser-parse-leaf")
               (:file "block-parser")
               (:file "inline-parser-data")
               (:file "inline-parser-model")
               (:file "inline-parser-indexes")
               (:file "inline-parser-links")
               (:file "inline-parser-delimiters")
               (:file "inline-parser-build")
               (:file "inline-parser-range")
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

(define-cl-markdown-kit-system "cl-markdown-kit/yaml"
  :description "YAML front matter support for cl-markdown-kit"
  :depends-on ("cl-markdown-kit" ; Front matter nodes and source spans.
               "cl-yaml-kit") ; YAML parsing and value construction.
  :pathname "src/yaml"
  :serial t
  :components ((:file "package")
               (:file "front-matter"))
  :in-order-to ((test-op (test-op "cl-markdown-kit/yaml/test"))))

(define-cl-markdown-kit-system "cl-markdown-kit/yaml/test"
  :description "Tests for cl-markdown-kit YAML front matter support"
  :depends-on ("cl-markdown-kit/yaml" ; Exercise the optional YAML parser.
               "cl-weave") ; Run the test suite.
  :pathname "t"
  :serial t
  :components ((:file "yaml"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-markdown-kit/yaml/test :run-tests)
               (error "cl-markdown-kit YAML test suite failed"))))

(define-cl-markdown-kit-system "cl-markdown-kit/test"
  :description "Tests for cl-markdown-kit"
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
               (:file "nodes-tree-walker")
               (:file "nodes-registry-dispatch")
               (:file "nodes-tree-walker-callbacks")
               (:file "nodes-registry-dispatch-tail")
               (:file "nodes-tree-walker-results")
               (:file "parse")
               (:file "block-parser")
               (:file "inline-parser")
               (:file "inline-parser-indexes")
               (:file "inline-parser-links")
               (:file "inline-parser-delimiters")
               (:file "inline-parser-range")
               (:file "inline-parser-conformance")
               (:file "inline-parser-edge-cases")
               (:file "character-data")
               (:file "query")
               (:file "html-writer")
               (:file "html-writer-blocks")
               (:file "html-writer-boundaries")
               (:file "html-writer-footnotes")
               (:file "html-writer-html")
               (:file "html-writer-inline")
               (:file "html-writer-streams")
               (:file "markdown-writer")
               (:file "markdown-writer-blocks")
               (:file "markdown-writer-boundaries")
               (:file "markdown-writer-escaping")
               (:file "markdown-writer-inline")
               (:file "markdown-writer-roundtrips")
               (:file "markdown-writer-streams")
               (:file "edit")
               (:file "edit-validation")
               (:file "resources"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-markdown-kit/test :run-tests)
               (error "cl-markdown-kit test suite failed"))))
