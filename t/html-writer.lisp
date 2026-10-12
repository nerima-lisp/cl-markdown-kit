(in-package #:cl-markdown-kit/test)

(defun html-writer-test-document (&rest children)
  (cl-markdown-kit:make-document :children children))

(defun html-writer-test-render (children &rest options)
  (apply #'cl-markdown-kit:write-html
         (cl-markdown-kit:make-document :children children)
         options))

(defun html-writer-test-count-substring (needle string)
  (loop with offset = 0
        for position = (search needle string :start2 offset)
        while position
        count position
        do (setf offset (+ position (length needle)))))

(describe "HTML writer"
  (it "renders escaped text in a paragraph"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-text
                              :value "A & <B>")))))
     :to-equal (format nil "<p>A &amp; &lt;B&gt;</p>~%")))

  (it "renders links and image alt text with escaped attributes"
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
     :to-equal
     (format nil
             "<p><a href=\"/target\" title=\"tip\">link</a> <img src=\"/image\" alt=\"A &amp; &lt;B&gt;\" title=\"photo\" /></p>~%")))

  (it "renders code blocks with an escaped language class and body"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-code-block
             :style :fenced :body (format nil "<tag>~%")
             :info-string "lisp")))
     :to-equal
     (format nil "<pre><code class=\"language-lisp\">&lt;tag&gt;~%</code></pre>~%")))

  (it "omits paragraph wrappers in tight list items"
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
     :to-equal
     (format nil "<ul>~%<li>first</li>~%<li>second</li>~%</ul>~%")))

  (it "renders aligned table headers and body cells"
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
      (expect output :to-equal
              (format nil
                      "<table>~%<thead>~%<tr>~%<th align=\"left\">Name</th>~%<th align=\"right\">Value</th>~%</tr>~%</thead>~%<tbody>~%<tr>~%<td align=\"left\">x</td>~%<td align=\"right\">1</td>~%</tr>~%</tbody>~%</table>~%"))))

  (it "renders checked task items with disabled checkbox inputs"
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

  (it "renders strikethrough content as del text"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-strikethrough
                    :children (list (cl-markdown-kit:make-text
                                     :value "removed")))))))
     :to-equal (format nil "<p><del>removed</del></p>~%")))

  (it "renders footnote references, definitions, and backreferences"
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

  (it "omits raw script blocks by default"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body (format nil "<script>alert(1)</script>~%"))))))
      (expect (search "<script" output) :to-equal nil)
      (expect (search "alert(1)" output) :to-equal nil)
      (expect (search "<!-- raw HTML omitted -->" output)
              :to-be-truthy)))

  (it "filters dangerous link and image schemes by default"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children
                    (list (cl-markdown-kit:make-link
                           :destination "javascript:alert(1)"
                           :children (list (cl-markdown-kit:make-text
                                            :value "javascript")))
                          (cl-markdown-kit:make-text :value " ")
                          (cl-markdown-kit:make-link
                           :destination "JaVaScRiPt:alert(1)"
                           :children (list (cl-markdown-kit:make-text
                                            :value "mixed case")))
                          (cl-markdown-kit:make-text :value " ")
                          (cl-markdown-kit:make-image
                           :destination "data:text/html,<script>alert(1)</script>"
                           :children (list (cl-markdown-kit:make-text
                                            :value "unsafe image")))))))))
      (expect (html-writer-test-count-substring "href=\"\"" output)
              :to-equal 2)
      (expect (search "src=\"\"" output) :to-be-truthy)
      (expect (search "javascript:" output) :to-equal nil)
      (expect (search "data:text/html" output) :to-equal nil)))

  (it "escapes raw HTML when the escape mode is selected"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline
                              :body "<em>safe</em><script>x</script>"))))
      :raw-html :escape)
     :to-equal
     (format nil
             "<p>&lt;em&gt;safe&lt;/em&gt;&lt;script&gt;x&lt;/script&gt;</p>~%")))

  (it "passes ordinary tags through but tagfilters unsafe tags"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-html-inline
                                     :body "<em>safe</em><script>x</script>"))))
             :raw-html :pass-through)))
      (expect (search "<em>safe</em>" output) :to-be-truthy)
      (expect (search "&lt;script>" output) :to-be-truthy)
      (expect (search "&lt;/script>" output) :to-be-truthy)
      (expect (search "<script>" output) :to-equal nil)))

  (it "escapes quotes in title attributes to prevent attribute injection"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children
             (list (cl-markdown-kit:make-link
                    :destination "/safe"
                    :title "safe\" onmouseover=\"alert(1)"
                    :children (list (cl-markdown-kit:make-text
                                     :value "link")))))))
     :to-equal
     (format nil
             "<p><a href=\"/safe\" title=\"safe&quot; onmouseover=&quot;alert(1)\">link</a></p>~%")))

  (it "makes string and stream output equivalent"
    (let* ((document
             (html-writer-test-document
              (cl-markdown-kit:make-paragraph
               :children (list (cl-markdown-kit:make-text
                                :value "stream output")))))
           (string-output (cl-markdown-kit:write-html document))
           (stream-output
             (with-output-to-string (stream)
               (cl-markdown-kit:write-html-to-stream document stream))))
      (expect stream-output :to-equal string-output))))
