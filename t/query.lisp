(in-package #:cl-markdown-kit/test)
(describe "Dispatch verification"
  (it "verifies query node dispatches"
    (expect (cl-markdown-kit::%query-verify-node-dispatches)
            :to-equal t)))

(defun test-source (trailing-newline-p &rest lines)
  (format nil "~{~A~^~%~}~A"
          lines
          (if (and trailing-newline-p lines)
              (string #\Newline)
              "")))

(defun test-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (condition (condition) condition)))

(defun query-source-slices-match-p (results)
  (every (lambda (result)
           (let* ((span (getf result :span))
                  (source (cl-parser-kit:span-source span)))
             (and (getf result :source-slice)
                  (string= (getf result :source-slice)
                           (subseq source
                                   (cl-parser-kit:span-start span)
                                   (cl-parser-kit:span-end span))))))
         results))

(describe "Query API"
  (it "lists inline-rendered headings and assigns spans only to document children"
    (let* ((source
             (test-source t "# **Bold** _em_ ~~gone~~ `code` [link *label*](x) ![alt](y) <i>tag</i>"))
           (document (cl-markdown-kit:parse-string source))
           (record (first (cl-markdown-kit:document-headings document))))
      (expect (cl-markdown-kit:heading-record-plain-text record)
              :to-equal "Bold em gone code link label alt tag")
      (expect (cl-markdown-kit:heading-record-level record) :to-equal 1)
      (expect (cl-markdown-kit:heading-record-source-form record) :to-equal :atx)
      (expect (cl-markdown-kit:heading-record-node record)
              :to-equal (first (cl-markdown-kit:document-children document)))
      (expect (typep (cl-markdown-kit:heading-record-section-span record)
                     'cl-parser-kit:span)
              :to-be-truthy)
      (let* ((nested-source (test-source t "> # Quoted" "" "- # Listed" ""
                                          "# Root"))
             (nested-document (cl-markdown-kit:parse-string nested-source))
             (headings (cl-markdown-kit:document-headings nested-document)))
        (expect (mapcar #'cl-markdown-kit:heading-record-plain-text headings)
                :to-equal '("Quoted" "Listed" "Root"))
        (expect (mapcar #'cl-markdown-kit:heading-record-section-span headings)
                :to-equal (list nil nil
                                (cl-markdown-kit:heading-record-section-span
                                 (third headings)))))))

  (it "constructs query records and computes spans when document metadata is absent"
    (let* ((source (test-source t "# Parent" "" "## Child"))
           (document (cl-markdown-kit:parse-string source))
           (heading (cl-markdown-kit:heading-record-node
                     (first (cl-markdown-kit:document-headings document))))
           (record (cl-markdown-kit::make-heading-record :node heading))
           (empty-section (cl-markdown-kit::make-section-record))
           (span-source-document
             (cl-markdown-kit:make-document
              :span (cl-markdown-kit:node-span document)
              :children (cl-markdown-kit:document-children document)))
           (no-document-span
             (cl-markdown-kit:make-document
              :source source
              :children (cl-markdown-kit:document-children document)))
           (from-span (cl-markdown-kit:document-headings span-source-document))
           (from-source (cl-markdown-kit:document-headings no-document-span))
           (span-source-section
             (cl-markdown-kit:find-section span-source-document "/Parent"))
           (span-source-extract
             (cl-markdown-kit:extract span-source-document :heading
                                      :section span-source-section)))
      (expect (cl-markdown-kit:heading-record-level record) :to-equal 1)
      (expect (cl-markdown-kit:heading-record-plain-text record) :to-equal "")
      (expect (cl-markdown-kit:heading-record-source-form record) :to-equal :atx)
      (expect (cl-markdown-kit:heading-record-section-span record) :to-equal nil)
      (expect (cl-markdown-kit:section-record-heading empty-section) :to-equal nil)
      (expect (cl-markdown-kit:section-record-span empty-section) :to-equal nil)
      (expect (cl-markdown-kit:section-record-address empty-section) :to-equal nil)
      (expect (mapcar #'cl-markdown-kit:heading-record-plain-text from-span)
              :to-equal '("Parent" "Child"))
      (expect (cl-parser-kit:span-source
               (cl-markdown-kit:heading-record-section-span (first from-span)))
              :to-equal source)
      (expect (cl-parser-kit:span-end
               (cl-markdown-kit:heading-record-section-span (second from-source)))
              :to-equal (length source))
      (expect (mapcar (lambda (result) (getf result :plain-text))
                      span-source-extract)
              :to-equal '("Parent" "Child"))))

  (it-each ((:crlf)
            (:cr))
      "preserves source end coordinates for ~A line endings"
      (style)
    (let* ((source
             (ecase style
               (:crlf (format nil "# Parent~C~Cbody~C~C"
                              #\Return #\Newline #\Return #\Newline))
               (:cr (format nil "# Parent~Cbody~C" #\Return #\Return))))
           (document (cl-markdown-kit:parse-string source))
           (without-span
             (cl-markdown-kit:make-document
              :source source
              :children (cl-markdown-kit:document-children document)))
           (section-span
             (cl-markdown-kit:heading-record-section-span
              (first (cl-markdown-kit:document-headings without-span))))
           (document-span (cl-markdown-kit:node-span document)))
      (expect (cl-parser-kit:span-end section-span)
              :to-equal (length source))
      (expect (cl-parser-kit:span-end-line section-span)
              :to-equal (cl-parser-kit:span-end-line document-span))
      (expect (cl-parser-kit:span-end-column section-span)
              :to-equal (cl-parser-kit:span-end-column document-span))))

  (it "renders autolinks, footnote labels, and line breaks in plain text"
    (let* ((autolink-document
             (cl-markdown-kit:parse-string
              (test-source t "# See https://example.test/path")))
           (footnote-document
             (cl-markdown-kit:parse-string
              (test-source t "# Note [^note]" "" "[^note]: footnote")))
           (soft-break-document
             (cl-markdown-kit:parse-string
              (test-source t "soft break" "continued" "----------")))
           (hard-break-document
             (cl-markdown-kit:parse-string
              (test-source t "hard break  " "continued" "----------"))))
      (expect (cl-markdown-kit:heading-record-plain-text
               (first (cl-markdown-kit:document-headings autolink-document)))
              :to-equal "See https://example.test/path")
      (expect (cl-markdown-kit:heading-record-plain-text
               (first (cl-markdown-kit:document-headings footnote-document)))
              :to-equal "Note note")
      (expect (cl-markdown-kit:heading-record-plain-text
               (first (cl-markdown-kit:document-headings soft-break-document)))
              :to-equal "soft break continued")
      (expect (cl-markdown-kit:heading-record-plain-text
               (first (cl-markdown-kit:document-headings hard-break-document)))
              :to-equal "hard break continued")))

  (it "resolves heading paths, setext headings, and source line numbers"
    (let* ((source (test-source t "Setext title" "==============" "body" ""
                                 "# Second"))
           (document (cl-markdown-kit:parse-string source))
           (headings (cl-markdown-kit:document-headings document))
           (setext (cl-markdown-kit:find-section document "/Setext title"))
           (by-line (cl-markdown-kit:find-section document 5)))
      (expect (cl-markdown-kit:heading-record-source-form
               (cl-markdown-kit:section-record-heading setext))
              :to-equal :setext)
      (expect (cl-markdown-kit:heading-record-level (first headings)) :to-equal 1)
      (expect (cl-markdown-kit:heading-record-plain-text
               (cl-markdown-kit:section-record-heading by-line))
              :to-equal "Second")
      (expect (cl-parser-kit:span-start
               (cl-markdown-kit:section-record-span by-line))
              :to-equal
              (cl-parser-kit:span-start
               (cl-markdown-kit:heading-record-section-span (second headings))))
      (expect (cl-parser-kit:span-end
               (cl-markdown-kit:section-record-span by-line))
              :to-equal
              (cl-parser-kit:span-end
               (cl-markdown-kit:heading-record-section-span (second headings))))
      (expect (cl-parser-kit:span-start
               (cl-markdown-kit:section-record-span setext))
              :to-equal (cl-parser-kit:span-start
                         (cl-markdown-kit:node-span
                          (cl-markdown-kit:heading-record-node
                           (cl-markdown-kit:section-record-heading setext)))))))

  (it "resolves nested document heading paths and line numbers"
    (let* ((document
             (cl-markdown-kit:parse-string
              (test-source t "# Parent" "" "## Child" "" "### Grandchild"
                            "" "# Next")))
           (by-path (cl-markdown-kit:find-section document "/Parent/Child"))
           (by-line (cl-markdown-kit:find-section document 3))
           (headings (cl-markdown-kit:extract document :heading :section by-line)))
      (expect (cl-markdown-kit:heading-record-plain-text
               (cl-markdown-kit:section-record-heading by-path))
              :to-equal "Child")
      (expect (cl-markdown-kit:heading-record-plain-text
               (cl-markdown-kit:section-record-heading by-line))
              :to-equal "Child")
      (expect (mapcar (lambda (result) (getf result :plain-text)) headings)
              :to-equal '("Child" "Grandchild"))))

  (it "ignores fence marker lines when collecting headings"
    (let* ((document (cl-markdown-kit:parse-string
                      (test-source t "```markdown" "# fenced text" "```" ""
                                    "# Real heading")))
           (headings (cl-markdown-kit:document-headings document)))
      (expect (mapcar #'cl-markdown-kit:heading-record-plain-text headings)
              :to-equal '("Real heading"))))

  (it "decodes only the two RFC 6901 escape sequences"
    (let* ((document (cl-markdown-kit:parse-string
                      (test-source t "# A~B" "" "# A/B")))
           (tilde (cl-markdown-kit:find-section document "/A~0B"))
           (slash (cl-markdown-kit:find-section document "/A~1B"))
           (invalid (test-condition (lambda ()
                       (cl-markdown-kit:find-section document "/A~2B"))))
           (trailing-tilde (test-condition (lambda ()
                              (cl-markdown-kit:find-section document "/A~"))))
           (invalid-line (test-condition (lambda ()
                            (cl-markdown-kit:find-section document 0))))
           (invalid-type (test-condition (lambda ()
                            (cl-markdown-kit:find-section document :invalid)))))
      (expect (cl-markdown-kit:heading-record-plain-text
               (cl-markdown-kit:section-record-heading tilde))
              :to-equal "A~B")
      (expect (cl-markdown-kit:heading-record-plain-text
               (cl-markdown-kit:section-record-heading slash))
              :to-equal "A/B")
      (expect (typep invalid
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (cl-markdown-kit:markdown-invalid-section-address-error-address
               invalid)
              :to-equal "/A~2B")
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:find-section document "A~0B")))
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep trailing-tilde
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep invalid-line
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep invalid-type
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)))

  (it "reports missing and ambiguous paths with heading lines"
    (let* ((document (cl-markdown-kit:parse-string
                      (test-source t "# Repeated" "" "# Repeated")))
           (missing (test-condition (lambda ()
                       (cl-markdown-kit:find-section document "/Missing"))))
           (ambiguous (test-condition (lambda ()
                         (cl-markdown-kit:find-section document "/Repeated"))))
           (candidates
             (cl-markdown-kit:markdown-ambiguous-match-error-candidates
              ambiguous)))
      (expect (typep missing 'cl-markdown-kit:markdown-no-match-error)
              :to-be-truthy)
      (expect (cl-markdown-kit:markdown-no-match-error-address missing)
              :to-equal "/Missing")
      (expect (typep ambiguous 'cl-markdown-kit:markdown-ambiguous-match-error)
              :to-be-truthy)
      (expect (mapcar (lambda (candidate) (getf candidate :line)) candidates)
              :to-equal '(1 3))
      (expect (mapcar (lambda (candidate)
                        (cl-markdown-kit:heading-record-plain-text
                         (getf candidate :heading)))
                      candidates)
              :to-equal '("Repeated" "Repeated"))))

  (it "uses the empty address for the whole document and excludes nested headings from sections"
    (let* ((document
             (cl-markdown-kit:parse-string
              (test-source t "> # Quoted" "" "- # Listed" "" "# Root")))
           (whole (cl-markdown-kit:find-section document ""))
           (headings (cl-markdown-kit:extract document :heading))
           (quoted (test-condition (lambda ()
                      (cl-markdown-kit:find-section document "/Quoted"))))
           (listed-line (test-condition (lambda ()
                           (cl-markdown-kit:find-section document 3)))))
      (expect (null (cl-markdown-kit:section-record-heading whole))
              :to-be-truthy)
      (expect (cl-parser-kit:span-start (cl-markdown-kit:section-record-span whole))
              :to-equal 0)
      (expect (cl-parser-kit:span-end (cl-markdown-kit:section-record-span whole))
              :to-equal (length (cl-markdown-kit:document-source document)))
      (expect (mapcar (lambda (result) (getf result :plain-text)) headings)
              :to-equal '("Quoted" "Listed" "Root"))
      (expect (typep quoted 'cl-markdown-kit:markdown-no-match-error)
              :to-be-truthy)
      (expect (typep listed-line 'cl-markdown-kit:markdown-no-match-error)
              :to-be-truthy)))

  (it-each ((:code-block)
            (:link)
            (:image)
            (:table)
            (:task-item)
            (:heading)
            (:front-matter))
      "extracts ~A results in source order with exact source slices"
      (kind)
    (let* ((source
             (test-source t "---" "title: x" "---" "# All" ""
                           "```lisp extra" "(+ 1 2)" "```" ""
                           "```text" "plain text" "```" ""
                           "[link](https://example.test \"title\") ![alt](image.png)"
                           "" "https://example.test/path" ""
                           "| A | B |" "| --- | --- |" "| C | D |" ""
                           "- [x] done" "" "## Child" "child body" ""
                           "# Other" "outside"))
           (document (cl-markdown-kit:parse-string source))
           (all-section (cl-markdown-kit:find-section document "/All"))
           (unrestricted
             (cl-markdown-kit:extract document kind :include-source-slice t))
           (restricted
             (cl-markdown-kit:extract document kind :section "/All"
                                      :include-source-slice t))
           (selected
             (cl-markdown-kit:extract document kind :section all-section
                                      :include-source-slice t)))
      (expect unrestricted :to-be-truthy)
      (if (eq kind :front-matter)
          (progn
            (expect restricted :to-equal nil)
            (expect selected :to-equal nil))
          (progn
            (expect restricted :to-be-truthy)
            (expect selected :to-be-truthy)))
      (expect (query-source-slices-match-p unrestricted) :to-be-truthy)
      (expect (query-source-slices-match-p restricted) :to-be-truthy)
      (expect (every (lambda (result) (not (member :source-slice result)))
                     (cl-markdown-kit:extract document kind))
              :to-be-truthy)))
  (it "extracts fields and applies kind filters"
    (let* ((source
             (test-source t "---" "title: x" "---" "# All" ""
                           "```lisp extra" "(+ 1 2)" "```" ""
                           "```text" "plain text" "```" ""
                           "[link](https://example.test \"title\") ![alt](image.png)"
                           "" "https://example.test/path" ""
                           "| A | B |" "| --- | --- |" "| C | D |" ""
                           "- [x] done" "" "## Child" "child body" ""
                           "# Other" "outside"))
           (document (cl-markdown-kit:parse-string source))
           (whole-section (cl-markdown-kit:find-section document ""))
           (code (first (cl-markdown-kit:extract document :code-block)))
           (link (first (cl-markdown-kit:extract document :link)))
           (image (first (cl-markdown-kit:extract document :image)))
           (table (first (cl-markdown-kit:extract document :table)))
           (task (first (cl-markdown-kit:extract document :task-item)))
           (front (first (cl-markdown-kit:extract document :front-matter)))
           (whole-front
             (first (cl-markdown-kit:extract document :front-matter
                                             :section whole-section))))
      (expect (getf code :info-string) :to-equal "lisp extra")
      (expect (length (cl-markdown-kit:extract document :code-block
                                                :language "lisp"))
              :to-equal 1)
      (expect (length (cl-markdown-kit:extract document :code-block
                                                :info-string "lisp extra"))
              :to-equal 1)
      (expect (length (cl-markdown-kit:extract document :code-block
                                                :info-string "lisp extra"
                                                :language "lisp"))
              :to-equal 1)
      (expect (cl-markdown-kit:extract document :code-block
                                       :info-string "lisp extra"
                                       :language "other")
              :to-equal nil)
      (expect (cl-markdown-kit:extract document :code-block :language "other")
              :to-equal nil)
      (expect (length (cl-markdown-kit:extract document :code-block
                                                :language "text"))
              :to-equal 1)
      (expect (length (cl-markdown-kit:extract document :heading :section 4))
              :to-equal 2)
      (expect (getf link :destination) :to-equal "https://example.test")
      (expect (getf link :title) :to-equal "title")
      (expect (getf image :destination) :to-equal "image.png")
      (expect (getf image :alt-text) :to-equal "alt")
      (expect (getf table :header) :to-equal '("A" "B"))
      (expect (getf table :rows) :to-equal '(("C" "D")))
      (expect (getf task :checked-p) :to-be-truthy)
      (expect (getf front :raw-text) :to-equal (format nil "title: x~%"))
      (expect (getf whole-front :raw-text) :to-equal (getf front :raw-text))
      (expect (typep (getf front :span) 'cl-parser-kit:span)
              :to-be-truthy)
      (expect (length (cl-markdown-kit:extract document :link :section "/Other"))
              :to-equal 0)))

  (it "rejects foreign and malformed section records and source spans"
    (let* ((source (test-source t "# Here" "" "text"))
           (document (cl-markdown-kit:parse-string source))
           (other-document (cl-markdown-kit:parse-string
                            (test-source t "# Else")))
           (mixed-source-document
             (cl-markdown-kit:make-document
              :source source
              :span (cl-markdown-kit:node-span document)
              :children (cl-markdown-kit:document-children other-document)))
           (foreign-section (cl-markdown-kit:find-section other-document "/Else"))
           (missing-span (cl-markdown-kit::make-section-record))
           (out-of-range-section
             (cl-markdown-kit::make-section-record
              :span (cl-parser-kit:make-span :source source :start -1 :end 0)))
           (foreign-error
             (test-condition (lambda ()
                                (cl-markdown-kit:extract
                                 document :heading :section foreign-section))))
           (missing-span-error
             (test-condition (lambda ()
                                (cl-markdown-kit:extract
                                 document :heading :section missing-span))))
           (out-of-range-error
             (test-condition (lambda ()
                                (cl-markdown-kit:extract
                                 document :heading :section out-of-range-section))))
           (invalid-section-type
             (test-condition (lambda ()
                                (cl-markdown-kit:extract
                                 document :heading :section :invalid))))
           (invalid-source-span
             (test-condition (lambda ()
                (cl-markdown-kit::%span-source-slice
                 (cl-parser-kit:make-span :source "x" :start 0 :end 2)))))
           (missing-source-span
             (test-condition (lambda ()
                (cl-markdown-kit::%span-source-slice
                 (cl-parser-kit:make-span :start 0 :end 0)))))
           (mixed-source-headings
             (cl-markdown-kit:extract mixed-source-document :heading)))
      (expect (typep foreign-error
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep missing-span-error
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep out-of-range-error
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep invalid-section-type
                     'cl-markdown-kit:markdown-invalid-section-address-error)
              :to-be-truthy)
      (expect (typep invalid-source-span 'simple-error) :to-be-truthy)
      (expect (typep missing-source-span 'simple-error) :to-be-truthy)
      (expect mixed-source-headings :to-equal nil))))
