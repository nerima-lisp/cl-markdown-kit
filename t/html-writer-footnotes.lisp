(in-package #:cl-markdown-kit/test)

(describe "HTML writer footnotes"
(html-writer-it "renders footnote references, definitions, and backreferences"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children
                    (list (cl-markdown-kit:make-text :value "See note")
                          (cl-markdown-kit:make-footnote-reference
                           :raw-label "note" :normalized-label "note")))
                   (cl-markdown-kit:make-footnote-definition
                    :raw-label "note" :normalized-label "note"
                    :children
                    (list (cl-markdown-kit:make-paragraph
                           :children (list (cl-markdown-kit:make-text
                                            :value "Footnote text")))))))))
      (expect (search "<sup class=\"footnote-ref\">" output)
              :to-be-truthy)
      (expect (search "href=\"#fn-note\" id=\"fnref-note\"" output)
              :to-be-truthy)
      (expect (search "data-footnote-ref>1</a>" output)
              :to-be-truthy)
      (expect (search "<section class=\"footnotes\" data-footnotes>"
                      output)
              :to-be-truthy)
      (expect (search "<li id=\"fn-note\">" output) :to-be-truthy)
      (expect (search "href=\"#fnref-note\"" output) :to-be-truthy)
      (expect (search "data-footnote-backref data-footnote-backref-idx" output)
              :to-be-truthy)))

(html-writer-it "keeps footnote IDs unique when a repeated reference meets a suffix label"
    (let ((output
            (cl-markdown-kit:write-html
             (cl-markdown-kit:parse-string
              (format nil "[^a] [^a] [^a-2]~%~%[^a]: one~%~%[^a-2]: two~%")))))
      (expect (html-writer-test-footnote-links-valid-p output)
              :to-be-truthy)
      (expect (search "href=\"#fn-a\" id=\"fnref-a\"" output)
              :to-be-truthy)
      (expect (search "href=\"#fn-a\" id=\"fnref-a-2\"" output)
              :to-be-truthy)
      (expect (search "href=\"#fn-a-2\" id=\"fnref-a-2-2\"" output)
              :to-be-truthy)
      (expect (search "<li id=\"fn-a\">" output) :to-be-truthy)
      (expect (search "<li id=\"fn-a-2\">" output) :to-be-truthy)
      (expect (search "href=\"#fnref-a\" class=\"footnote-backref\""
                      output)
              :to-be-truthy)
      (expect (search "href=\"#fnref-a-2\" class=\"footnote-backref\""
                      output)
              :to-be-truthy)
      (expect (search "href=\"#fnref-a-2-2\" class=\"footnote-backref\""
                      output)
              :to-be-truthy)
      (expect
       (cl-markdown-kit::%hw-allocate-footnote-ids
        '("fnref-a-2" "fnref-a-2" "fnref-a-2" "fnref-a-2-2"))
       :to-equal
       '("fnref-a-2" "fnref-a-2-3" "fnref-a-2-4" "fnref-a-2-2"))))

(html-writer-it "keeps footnote IDs unique when labels percent-encode to the same ID"
    (let ((output
            (cl-markdown-kit:write-html
             (cl-markdown-kit:parse-string
              (format nil
                      "[^a b] [^a%20b]~%~%[^a b]: space~%~%[^a%20b]: percent~%")))))
      (expect (html-writer-test-footnote-links-valid-p output)
              :to-be-truthy)
      (expect (search "href=\"#fn-a%20b\" id=\"fnref-a%20b\"" output)
              :to-be-truthy)
      (expect (search "href=\"#fn-a%20b-2\" id=\"fnref-a%20b-2\""
                      output)
              :to-be-truthy)
      (expect (search "<li id=\"fn-a%20b\">" output) :to-be-truthy)
      (expect (search "<li id=\"fn-a%20b-2\">" output) :to-be-truthy)
      (expect (search "href=\"#fnref-a%20b\" class=\"footnote-backref\""
                      output)
              :to-be-truthy)
      (expect (search "href=\"#fnref-a%20b-2\" class=\"footnote-backref\""
                      output)
              :to-be-truthy)))

(html-writer-it "numbers repeated footnote references and handles definitions without paragraph endings"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children
                    (list (cl-markdown-kit:make-footnote-reference
                           :raw-label "note" :normalized-label "note")
                          (cl-markdown-kit:make-text :value " ")
                          (cl-markdown-kit:make-footnote-reference
                           :raw-label "note" :normalized-label "note")))
                   (cl-markdown-kit:make-footnote-definition
                    :raw-label "note" :normalized-label "note"
                    :children
                    (list (cl-markdown-kit:make-code-block
                           :body (format nil "body~%"))))))))
      (expect (search "id=\"fnref-note-2\"" output) :to-be-truthy)
      (expect (search "href=\"#fnref-note-2\"" output) :to-be-truthy)
      (expect (search "aria-label=\"Back to reference 1-2\"" output)
              :to-be-truthy)))

(html-writer-it "keeps unresolved footnote references and renders header-only tables"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-footnote-reference
                              :raw-label "missing"
                              :normalized-label "missing")))
            (cl-markdown-kit:make-table
             :rows
             (list (cl-markdown-kit:make-table-row
                    :header-row-p t
                    :cells
                    (list (cl-markdown-kit:make-table-cell
                           :children (list (cl-markdown-kit:make-text
                                            :value "Header")))))))))
     :to-match-snapshot
     "html/unresolved-footnote-header-table")))
