(in-package #:cl-markdown-kit/test)

(describe "HTML writer inline"
(html-writer-it "renders escaped text in a paragraph"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-text
                              :value "A & <B>")))))
     :to-match-snapshot "html/escaped-paragraph"))

(html-writer-it "renders links and image alt text with escaped attributes"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-link
                    :destination "/target" :title "tip"
                    :children (list (cl-markdown-kit:make-text
                                     :value "link")))
                   (cl-markdown-kit:make-text :value " ")
                   (cl-markdown-kit:make-image
                    :destination "/image" :title "photo"
                    :children (list (cl-markdown-kit:make-text
                                     :value "A & <B>")))))))
     :to-match-snapshot
     "html/link-image-attributes"))

(html-writer-it "renders strikethrough content as del text"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-strikethrough
                    :children (list (cl-markdown-kit:make-text
                                     :value "removed")))))))
     :to-match-snapshot "html/strikethrough"))

(html-writer-it "escapes quotes in title attributes to prevent attribute injection"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-link
                    :destination "/safe"
                    :title "safe\" onmouseover=\"alert(1)"
                    :children (list (cl-markdown-kit:make-text
                                     :value "link")))))))
     :to-match-snapshot
     "html/escaped-title-attribute"))

(html-writer-it "renders code spans, breaks, emphasis, strong, and autolinks"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-code-span :literal-text "<code>")
                   (cl-markdown-kit:make-soft-break)
                   (cl-markdown-kit:make-hard-break)
                   (cl-markdown-kit:make-emphasis
                    :children (list (cl-markdown-kit:make-text :value "em")))
                   (cl-markdown-kit:make-text :value " ")
                   (cl-markdown-kit:make-strong
                    :children (list (cl-markdown-kit:make-text :value "strong")))
                   (cl-markdown-kit:make-text :value " ")
                   (cl-markdown-kit:make-autolink
                    :destination "https://example.test/?a=\"x"
                    :display-text "<https://example.test>")))))
     :to-match-snapshot
     "html/inline-rendering"))

(html-writer-it "renders all plain inline forms in image alt text"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-image
                    :destination "/alt"
                    :children
                    (list (cl-markdown-kit:make-text :value "text")
                          (cl-markdown-kit:make-code-span :literal-text " <code>")
                          (cl-markdown-kit:make-html-inline :body " <raw>")
                          (cl-markdown-kit:make-soft-break)
                          (cl-markdown-kit:make-hard-break)
                          (cl-markdown-kit:make-autolink
                           :destination "/auto" :display-text "auto")
                          (cl-markdown-kit:make-footnote-reference
                           :raw-label "note" :normalized-label "note")
                          (cl-markdown-kit:make-emphasis
                           :children
                           (list (cl-markdown-kit:make-text :value " nested")))
                          (cl-markdown-kit:make-strong
                           :children
                           (list (cl-markdown-kit:make-text :value " strong")))
                          (cl-markdown-kit:make-strikethrough
                           :children
                           (list (cl-markdown-kit:make-text :value " struck")))
                          (cl-markdown-kit:make-image
                           :destination "/nested"
                           :children
                           (list (cl-markdown-kit:make-text
                                  :value " nested & image")))))))))
     :to-match-snapshot
     "html/image-alt-text")))
