(in-package #:cl-markdown-kit/test)

(describe "Markdown writer boundaries"
  (it "escapes directly constructed inline AST boundaries"
    (labels ((render-nodes (nodes)
               (cl-markdown-kit:write-markdown
                (cl-markdown-kit:make-document :children nodes)))
             (render-block (node)
               (render-nodes (list node)))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children))
             (text (value)
               (cl-markdown-kit:make-text :value value)))
      (let* ((raw-inline (format nil " ~CTop~% ~CBottom" #\Tab #\Tab))
             (raw-inline-output
               (render-block
                (paragraph
                 (list (cl-markdown-kit:make-html-inline :body raw-inline)))))
             (marker-output
               (render-block (paragraph (list (text "* * *")))))
             (underscore-marker-output
               (render-block (paragraph (list (text "___  ")))))
             (minimal-text "a * b _ c ~ d < e [ref")
             (minimal-text-output
               (render-block (paragraph (list (text minimal-text)))))
             (minimal-text-roundtrip
               (cl-markdown-kit:parse-string minimal-text-output))
             (minimal-heading-output
               (render-block
                (cl-markdown-kit:make-heading
                 :level 2 :children (list (text "# section")))))
             (minimal-heading-roundtrip
               (cl-markdown-kit:parse-string minimal-heading-output))
             (heading-hash-before-inline
               (render-block
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (text "section #")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))))))
             (heading-hash-roundtrip
               (cl-markdown-kit:parse-string heading-hash-before-inline))
             (unmatched-close-bracket
               (render-block (paragraph (list (text "close ]")))))
             (cross-node-autolink
               (render-block
                (paragraph
                 (list (text "https") (text "://example.test")))))
             (cross-node-entity
               (render-block
                (paragraph (list (text "&am") (text "p;")))))
             (reference-text-markdown
               (render-nodes
                (list (paragraph (list (text "see [") (text "ref]")))
                      (cl-markdown-kit:make-link-reference-definition
                       :raw-label "ref"
                       :normalized-label "ref"
                       :destination "target"
                       :eligible-p t))))
             (reference-text-roundtrip
               (cl-markdown-kit:parse-string reference-text-markdown))
             (trailing-inline-output
               (render-block
                (paragraph (list (text (format nil "word  ~C" #\Tab))))))
             (alignment-output
               (render-block
                (paragraph
                 (list (text "a | b")
                       (cl-markdown-kit:make-soft-break)
                       (text ":-: | :")))))
             (broken-schemes
               (render-block
                (paragraph
                 (list
                  (text "https://www.example.test._bad http://www.example.test._bad")
                  (cl-markdown-kit:make-soft-break)
                  (text "mailto:user@example.test._bad xmpp:user@example.test._bad")))))
             (definition-output
               (render-block
                (cl-markdown-kit:make-link-reference-definition
                 :raw-label (format nil "raw&~%label")
                 :normalized-label ""
                 :destination "destination")))
             (link-output
               (render-block
                (cl-markdown-kit:make-table
                 :alignments '(:unknown)
                 :rows
                 (list
                  (cl-markdown-kit:make-table-row
                   :cells
                   (list
                    (cl-markdown-kit:make-table-cell
                     :children
                     (list
                      (cl-markdown-kit:make-link
                       :destination (format nil "a<b>|&amp;~%tail")
                       :title "say \"hi\""
                       :children (list (text "label")))))))))))
             (writer-link (cl-markdown-kit:make-link
                           :destination "/destination"))
             (trailing-bang-link
               (render-block
                (paragraph (list (text "bang!") writer-link))))
             (ordinary-text-link
               (render-block
                (paragraph (list (text "bang") writer-link))))
             (empty-text-link
               (render-block
                (paragraph (list (text "") writer-link))))
             (differing-autolink
               (render-block
                (paragraph
                 (list (cl-markdown-kit:make-autolink
                        :destination "https://example.test"
                        :display-text "example"))))))
        (expect raw-inline-output :to-equal raw-inline)
        (expect marker-output :to-equal "\\* * *")
        (expect underscore-marker-output :to-equal "\\___&#32;&#32;")
        (expect minimal-text-output :to-equal minimal-text)
        (expect (markdown-writer-document-signature minimal-text-roundtrip)
                :to-equal '((:paragraph ((:text "a * b _ c ~ d < e [ref")))))
        (expect minimal-heading-output :to-equal "## # section")
        (expect (markdown-writer-document-signature minimal-heading-roundtrip)
                :to-equal '((:heading 2 ((:text "# section")))))
        (expect heading-hash-before-inline :to-equal "## section #**bold**")
        (expect (markdown-writer-document-signature heading-hash-roundtrip)
                :to-equal
                '((:heading 2 ((:text "section #")
                               (:strong ((:text "bold")))))))
        (expect unmatched-close-bracket :to-equal "close ]")
        (expect cross-node-autolink :to-equal "https\\://example.test")
        (expect cross-node-entity :to-equal "\\&amp;")
        (expect (search "\\[ref\\]" reference-text-markdown)
                :to-be-truthy)
        (expect (markdown-writer-document-signature reference-text-roundtrip)
                :to-equal
                '((:paragraph ((:text "see [ref]")))
                  (:link-reference-definition "ref" "target" nil t)))
        (expect trailing-inline-output :to-equal "word&#32;&#32;&#9;")
        (expect alignment-output :to-be-truthy)
        (expect (search "&#104;ttps://" broken-schemes) :to-be-truthy)
        (expect (search "&#104;ttp://" broken-schemes) :to-be-truthy)
        (expect (search "mailto\\:user@example.test._bad" broken-schemes)
                :to-be-truthy)
        (expect (search "xmpp\\:user@example.test._bad" broken-schemes)
                :to-be-truthy)
        (expect definition-output :to-equal "[raw&amp; label]: <destination>")
        (expect (search "\\<b\\>\\|\\&amp;&#10;tail" link-output)
                :to-be-truthy)
        (expect (search (format nil "say ~C~C~A~C~C" #\\ #\" "hi" #\\ #\") link-output) :to-be-truthy)
        (expect trailing-bang-link :to-equal "bang\\![](</destination>)")
        (expect ordinary-text-link :to-equal "bang[](</destination>)")
        (expect empty-text-link :to-equal "[](</destination>)")
        (expect differing-autolink
                :to-equal "[example](<https://example.test>)"))))

  (it "renders directly constructed front matter and code block boundaries"
    (labels ((render-nodes (nodes)
               (cl-markdown-kit:write-markdown
                (cl-markdown-kit:make-document :children nodes)))
             (render-block (node)
               (render-nodes (list node)))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children))
             (text (value)
               (cl-markdown-kit:make-text :value value)))
      (let* ((front-matter-crlf
               (render-block
                (cl-markdown-kit:make-front-matter
                 :raw-text (format nil "one~C~%two~Cthree" #\Return #\Return))))
             (front-matter-final-newline
               (render-block
                (cl-markdown-kit:make-front-matter
                 :raw-text (format nil "front~%"))))
             (front-matter-trailing-cr
               (render-block
                (cl-markdown-kit:make-front-matter
                 :raw-text (format nil "end~C" #\Return))))
             (empty-front-matter
               (render-block (cl-markdown-kit:make-front-matter)))
             (empty-code
               (render-block
                (cl-markdown-kit:make-code-block :body "")))
             (unterminated-code
               (render-block
                (cl-markdown-kit:make-code-block :body "body")))
             (following-code
               (render-nodes
                (list (cl-markdown-kit:make-code-block :body "body")
                      (paragraph (list (text "after"))))))
             (following-code-nodes
               (cl-markdown-kit:document-children
                (cl-markdown-kit:parse-string following-code))))
        (expect front-matter-crlf :to-equal
                (format nil "---~%one~%two~%three~%---"))
        (expect front-matter-final-newline :to-equal
                (format nil "---~%front~%---"))
        (expect front-matter-trailing-cr :to-equal
                (format nil "---~%end~%---"))
        (expect empty-front-matter :to-equal (format nil "---~%---"))
        (expect empty-code :to-equal (format nil "```~%```"))
        (expect unterminated-code :to-equal (format nil "```~%body"))
        (expect following-code
                :to-equal (format nil "```~%body~%```~%~%after"))
        (expect (length following-code-nodes) :to-equal 2)
        (expect (cl-markdown-kit:code-block-body
                 (first following-code-nodes))
                :to-equal (format nil "body~%"))
        (expect (cl-markdown-kit:text-value
                 (first (cl-markdown-kit:paragraph-children
                         (second following-code-nodes))))
                :to-equal "after"))))

  (it "renders empty block structures and task boundaries"
    (labels ((render-nodes (nodes)
               (cl-markdown-kit:write-markdown
                (cl-markdown-kit:make-document :children nodes)))
             (render-block (node)
               (render-nodes (list node)))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children))
             (text (value)
               (cl-markdown-kit:make-text :value value)))
      (let* ((empty-table
               (render-block (cl-markdown-kit:make-table)))
             (empty-footnote
               (render-block
                (cl-markdown-kit:make-footnote-definition
                 :normalized-label "")))
             (empty-footnote-child
               (render-block
                (cl-markdown-kit:make-footnote-definition
                 :normalized-label "note"
                 :children (list (paragraph nil)))))
             (empty-task
               (render-block
                (cl-markdown-kit:make-list-node
                 :kind :unordered
                 :tight-p t
                 :items
                 (list (cl-markdown-kit:make-task-list-item
                        :checked-p nil)))))
             (block-task
               (render-block
                (cl-markdown-kit:make-list-node
                 :kind :unordered
                 :tight-p t
                 :items
                 (list (cl-markdown-kit:make-task-list-item
                        :checked-p t
                        :children
                        (list (cl-markdown-kit:make-code-block :body "x")))))))
             (empty-list
               (render-block (cl-markdown-kit:make-list-node :items nil)))
             (heading-soft-break
               (render-block
                (cl-markdown-kit:make-heading
                 :level 2
                 :children (list (text "first")
                                 (cl-markdown-kit:make-soft-break)
                                 (text "second"))))))
        (expect empty-table :to-equal "")
        (expect empty-footnote :to-equal "[^]:")
        (expect empty-footnote-child :to-equal (format nil "[^note]:~%"))
        (expect empty-task :to-equal "- [ ]")
        (expect (search (format nil "- [x]~%  ```~%  x") block-task)
                :to-be-truthy)
        (expect empty-list :to-equal "")
        (expect heading-soft-break :to-equal "## first&#10;second"))))

  (it "signals exact writer conditions for directly constructed invalid inputs"
    (labels ((render-nodes (nodes)
               (cl-markdown-kit:write-markdown
                (cl-markdown-kit:make-document :children nodes)))
             (render-block (node)
               (render-nodes (list node)))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children)))
      (expect (handler-case
                  (progn
                    (cl-markdown-kit:write-markdown-to-stream
                     nil (make-string-output-stream))
                    nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn
                    (cl-markdown-kit:write-markdown-to-stream
                     (cl-markdown-kit:make-document) nil)
                    nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn
                    (cl-markdown-kit:write-markdown-to-stream
                     (cl-markdown-kit:make-document)
                     (make-string-input-stream ""))
                    nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn
                    (render-nodes (list (cl-markdown-kit:make-node)))
                    nil)
                (simple-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn
                    (render-block
                     (paragraph (list (cl-markdown-kit:make-node))))
                    nil)
                (simple-error () t))
              :to-be-truthy)))

  (it "handles empty inputs to Markdown writer boundary helpers"
    (let ((text (cl-markdown-kit:make-text :value "visible")))
      (expect (cl-markdown-kit::%mw-heading-marker-p "" 0 0)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-heading-marker-p "#x" 0 2)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-list-marker-p "" 0 0)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-marker-line-p
               "" 0 0 '(#\* #\_ #\-) 3)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-marker-line-p
               "___  " 0 5 '(#\* #\_ #\-) 3)
              :to-be-truthy)
      (expect (cl-markdown-kit::%mw-setext-marker-line-p "" 0 0)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-setext-marker-line-p "==x" 0 3)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-line-prefix-hazard-p "" 0 0)
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-inline-tail-has-nonspace-p
               (list text))
              :to-be-truthy)
      (expect (cl-markdown-kit::%mw-inline-tail-has-nonspace-p
               (list (cl-markdown-kit:make-text :value "  ")))
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-inline-tail-has-nonspace-p
               (list (cl-markdown-kit:make-html-inline :body "<br>")))
              :to-be-truthy)
      (expect (cl-markdown-kit::%mw-inline-tail-has-nonspace-p
               (list (cl-markdown-kit:make-html-inline :body " ")))
              :to-equal nil)
      (expect (cl-markdown-kit::%mw-inline-tail-has-nonspace-p
               (list (cl-markdown-kit:make-soft-break)))
              :to-be-truthy)
      (expect (cl-markdown-kit::%mw-table-alignment-line-p ":---||:")
              :to-equal nil)))
  (it "signals simple-error for list items outside lists"
      (let* ((item (cl-markdown-kit:make-list-item))
             (documents
               (list (cl-markdown-kit:make-document :children (list item))
                     (cl-markdown-kit:make-document
                      :children
                      (list (cl-markdown-kit:make-block-quote
                             :children (list item)))))))
        (dolist (document documents)
          (expect
           (typep
            (handler-case
                (cl-markdown-kit:write-markdown document)
              (condition (condition) condition))
            'simple-error)
           :to-be-truthy)))))
