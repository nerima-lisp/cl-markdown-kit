(defpackage #:cl-markdown-kit/yaml/test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:run-all)
  (:export #:run-tests))
(in-package #:cl-markdown-kit/yaml/test)

(defun run-tests ()
  (run-all :reporter :spec :pass-with-no-tests nil))

(describe "YAML front matter"
  (it "parses front matter from a document and a front-matter node"
    (let* ((document
             (cl-markdown-kit:parse
              (format nil "---~%title: Example~%tags:~%  - lisp~%  - yaml~%---~%body")))
           (front-matter (first (cl-markdown-kit:document-children document)))
           (from-document (cl-markdown-kit/yaml:parse-front-matter document))
           (from-node (cl-markdown-kit/yaml:parse-front-matter front-matter))
           (tags (gethash "tags" from-document)))
      (expect (hash-table-p from-document) :to-be-truthy)
      (expect (eq (hash-table-test from-document) 'equal) :to-be-truthy)
      (expect (gethash "title" from-document) :to-equal "Example")
      (expect (vectorp tags) :to-be-truthy)
      (expect (coerce tags 'list) :to-equal '("lisp" "yaml"))
      (expect (gethash "title" from-node) :to-equal "Example")))

  (it "returns nil when a document has no front matter"
    (expect (cl-markdown-kit/yaml:parse-front-matter
             (cl-markdown-kit:parse "# Heading"))
            :to-equal nil))

  (it "returns nil for empty front matter"
    (dolist (source (list (format nil "---~%---~%")
                          (format nil "---~%~%---~%")))
      (let* ((document (cl-markdown-kit:parse source))
             (front-matter
               (first (cl-markdown-kit:document-children document))))
        (expect (typep front-matter 'cl-markdown-kit:front-matter)
                :to-be-truthy)
        (expect (cl-markdown-kit/yaml:parse-front-matter document)
                :to-equal nil))))

  (it "rejects values that are neither front-matter nodes nor documents"
    (let ((caught nil))
      (handler-case
          (cl-markdown-kit/yaml:parse-front-matter 17)
        (type-error (condition)
          (setf caught condition)))
      (expect (typep caught 'type-error) :to-be-truthy)))

  (it "wraps YAML syntax errors and retains the original condition"
    (let ((caught nil))
      (handler-case
          (cl-markdown-kit/yaml:parse-front-matter
           (cl-markdown-kit:parse
            (format nil "---~%title: \"bad\\q\"~%---~%")))
        (cl-markdown-kit/yaml:markdown-yaml-error (condition)
          (setf caught condition)))
      (expect (typep caught 'cl-markdown-kit:markdown-kit-error)
              :to-be-truthy)
      (expect (typep
               (cl-markdown-kit/yaml:markdown-yaml-error-yaml-condition caught)
               'yaml-kit:yaml-parse-error)
              :to-be-truthy)
      (expect (cl-markdown-kit/yaml:markdown-yaml-error-line caught)
              :to-equal 2)
      (expect (yaml-kit:yaml-parse-error-line
               (cl-markdown-kit/yaml:markdown-yaml-error-yaml-condition caught))
              :to-equal 0)))

  (it "converts YAML scanner positions to Markdown source positions"
    (let ((caught nil))
      (handler-case
          (cl-markdown-kit/yaml:parse-front-matter
           (cl-markdown-kit:parse
            (format nil "---~%ok: yes~%bad: \"no\\q\"~%---~%")))
        (cl-markdown-kit/yaml:markdown-yaml-error (condition)
          (setf caught condition)))
      (expect (cl-markdown-kit/yaml:markdown-yaml-error-line caught)
              :to-equal 3)
      (expect (cl-markdown-kit/yaml:markdown-yaml-error-column caught)
              :to-equal 6)
      (expect (not (null (search "Markdown line 3, column 6"
                                (princ-to-string caught))))
              :to-be-truthy)
      (let ((yaml-condition
              (cl-markdown-kit/yaml:markdown-yaml-error-yaml-condition caught)))
        (expect (yaml-kit:yaml-parse-error-line yaml-condition) :to-equal 1)
        (expect (yaml-kit:yaml-parse-error-column yaml-condition) :to-equal 5))))

  (it "maps parser errors without marks to the first content character"
    (let* ((document
             (cl-markdown-kit:parse
              (format nil "---~%title: Example~%---~%")))
           (front-matter (first (cl-markdown-kit:document-children document)))
           (yaml-condition
             (make-condition 'yaml-kit:yaml-parse-error
                             :line 1 :column 1 :offset 0)))
      (multiple-value-bind (line column)
          (cl-markdown-kit/yaml::%markdown-yaml-error-location
           front-matter yaml-condition)
        (expect line :to-equal 2)
        (expect column :to-equal 1)))
    (expect (cl-markdown-kit/yaml::%yaml-error-without-mark-p
             (make-condition 'yaml-kit:yaml-parse-error))
            :to-be-truthy))

  (it "leaves Markdown positions nil for front-matter nodes without spans"
    (let ((caught nil))
      (handler-case
          (cl-markdown-kit/yaml:parse-front-matter
           (cl-markdown-kit:make-front-matter :raw-text "title: \"bad\\q\""))
        (cl-markdown-kit/yaml:markdown-yaml-error (condition)
          (setf caught condition)))
      (expect (cl-markdown-kit/yaml:markdown-yaml-error-line caught)
              :to-equal nil)
      (expect (cl-markdown-kit/yaml:markdown-yaml-error-column caught)
              :to-equal nil)))

  (it "reports YAML errors without a Markdown position"
    (let* ((condition
             (make-condition
              'cl-markdown-kit/yaml:markdown-yaml-error
              :yaml-condition (make-condition 'yaml-kit:yaml-parse-error)))
           (report (princ-to-string condition)))
      (expect (search "Invalid YAML front matter:" report) :to-equal 0)
      (expect (search "Markdown line" report) :to-equal nil)))

  (it "passes non-syntax YAML conditions through unchanged"
    (let* ((document
             (cl-markdown-kit:parse
              (format nil "---~%title: Example~%---~%")))
           (caught nil))
      (handler-case
          (cl-markdown-kit/yaml:parse-front-matter
           document :max-input-length 0)
        (yaml-kit:yaml-resource-limit-error (condition)
          (setf caught condition)))
      (expect (typep caught 'yaml-kit:yaml-resource-limit-error)
              :to-be-truthy)
      (setf caught nil)
      (handler-case
          (cl-markdown-kit/yaml:parse-front-matter
           document :mapping-type :unsupported)
        (yaml-kit:yaml-compose-error (condition)
          (setf caught condition)))
      (expect (typep caught 'yaml-kit:yaml-compose-error)
              :to-be-truthy)
      (expect (yaml-kit:yaml-compose-error-cause caught)
              :to-equal "invalid mapping type")))

  (it "parses BOM-prefixed CRLF Markdown without passing the BOM to YAML"
    (let* ((source (format nil "~C---~C~%title: CRLF~C~%---~C~%body"
                           (code-char #xfeff)
                           #\Return #\Return #\Return))
           (document (cl-markdown-kit:parse source))
           (value (cl-markdown-kit/yaml:parse-front-matter document)))
      (expect (cl-markdown-kit:source-format-bom-p
               (cl-markdown-kit:document-source-format document))
              :to-be-truthy)
      (expect (cl-markdown-kit:source-format-newline-style
               (cl-markdown-kit:document-source-format document))
              :to-equal :crlf)
      (expect (cl-markdown-kit:front-matter-raw-text
               (first (cl-markdown-kit:document-children document)))
              :to-equal (format nil "title: CRLF~C~C" #\Return #\Newline))
      (expect (gethash "title" value) :to-equal "CRLF"))))
