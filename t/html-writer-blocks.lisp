(in-package #:cl-markdown-kit/test)

(describe "HTML writer blocks"
(html-writer-it "renders code blocks with an escaped language class and body"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-code-block
             :style :fenced :body (format nil "<tag>~%")
             :info-string "lisp")))
     :to-match-snapshot
     "html/escaped-code-block"))

(html-writer-it "omits paragraph wrappers in tight list items"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-list-node
             :kind :unordered :tight-p t
             :items
             (list (cl-markdown-kit:make-list-item
                    :children
                    (list (cl-markdown-kit:make-paragraph
                           :children (list (cl-markdown-kit:make-text
                                            :value "first")))))
                   (cl-markdown-kit:make-list-item
                    :children
                    (list (cl-markdown-kit:make-paragraph
                           :children (list (cl-markdown-kit:make-text
                                            :value "second")))))))))
     :to-match-snapshot
     "html/tight-list-items"))

(html-writer-it "renders aligned table headers and body cells"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-table
                    :alignments '(:left :right)
                    :rows
                    (list (cl-markdown-kit:make-table-row
                           :header-row-p t
                           :cells
                           (list (cl-markdown-kit:make-table-cell
                                  :alignment :left
                                  :children (list (cl-markdown-kit:make-text
                                                   :value "Name")))
                                 (cl-markdown-kit:make-table-cell
                                  :alignment :right
                                  :children (list (cl-markdown-kit:make-text
                                                   :value "Value")))))
                          (cl-markdown-kit:make-table-row
                           :header-row-p nil
                           :cells
                           (list (cl-markdown-kit:make-table-cell
                                  :alignment :left
                                  :children (list (cl-markdown-kit:make-text
                                                   :value "x")))
                                 (cl-markdown-kit:make-table-cell
                                  :alignment :right
                                  :children (list (cl-markdown-kit:make-text
                                                   :value "1")))))))))))
      (expect output :to-match-snapshot
              "html/aligned-table")))

(html-writer-it "renders checked task items with disabled checkbox inputs"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-list-node
                    :kind :unordered :tight-p t
                    :items
                    (list (cl-markdown-kit:make-task-list-item
                           :checked-p t
                           :children
                           (list (cl-markdown-kit:make-paragraph
                                  :children (list (cl-markdown-kit:make-text
                                                   :value "done")))))))))))
      (expect (search "<input type=\"checkbox\"" output) :to-be-truthy)
      (expect (search "disabled=\"\"" output) :to-be-truthy)
      (expect (search "checked=\"\"" output) :to-be-truthy)
      (expect (search "done</li>" output) :to-be-truthy)))

(html-writer-it "renders empty documents and omits non-rendered block nodes"
    (expect (cl-markdown-kit:write-html (html-writer-test-document))
            :to-match-snapshot "html/empty-document")
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-front-matter :raw-text "title: test")
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "ref" :normalized-label "ref"
             :destination "/ref" :eligible-p t)
            (cl-markdown-kit:make-footnote-definition
             :raw-label "unused" :normalized-label "unused")))
     :to-match-snapshot "html/non-rendered-block-nodes"))

(html-writer-it "renders block quotes, headings, and thematic breaks"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-block-quote
             :children
             (list (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-text :value "quoted")))))
            (cl-markdown-kit:make-heading
             :level 2
             :children (list (cl-markdown-kit:make-text :value "Heading")))
            (cl-markdown-kit:make-thematic-break)
            (cl-markdown-kit:make-block-quote :children nil)))
     :to-match-snapshot
     "html/block-rendering"))

(html-writer-it "renders loose and ordered lists, unchecked tasks, and empty items"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-list-node
             :kind :ordered :start-number 3 :tight-p nil
             :items
             (list (cl-markdown-kit:make-task-list-item
                    :checked-p nil
                    :children
                    (list (cl-markdown-kit:make-paragraph
                           :children (list (cl-markdown-kit:make-text
                                            :value "todo")))))
                   (cl-markdown-kit:make-list-item :children nil)))))
     :to-match-snapshot
     "html/ordered-task-list")
    (expect (html-writer-test-render
             (list (cl-markdown-kit:make-list-node
                    :kind :ordered :start-number 1 :tight-p t :items nil)))
            :to-match-snapshot "html/empty-ordered-list")
    (expect (html-writer-test-render
             (list (cl-markdown-kit:make-list-node
                    :kind :ordered :start-number 4 :tight-p t :items nil)))
            :to-match-snapshot "html/ordered-list-start-number"))

(html-writer-it "renders missing code info as plain code and uses only the first info token"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-code-block :body (format nil "plain~%"))
            (cl-markdown-kit:make-code-block :body (format nil "blank~%")
                                             :info-string (format nil " ~C " #\Tab))
            (cl-markdown-kit:make-code-block :body (format nil "marked~%")
                                             :info-string " lisp extra")))
     :to-match-snapshot
     "html/code-info-tokens")))
