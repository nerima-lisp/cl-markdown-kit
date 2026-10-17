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

(defun html-writer-test-attribute-values (attribute html)
  (let ((needle (format nil "~A=\"" attribute))
        (offset 0)
        (values nil))
    (loop for start = (search needle html :start2 offset)
          while start
          do (let* ((value-start (+ start (length needle)))
                    (end (position #\" html :start value-start)))
               (push (subseq html value-start end) values)
               (setf offset (1+ end))))
    (nreverse values)))

(defun html-writer-test-footnote-links-valid-p (html)
  (let ((ids (html-writer-test-attribute-values "id" html))
        (hrefs (html-writer-test-attribute-values "href" html)))
    (and (= (length ids) (length (remove-duplicates ids :test #'equal)))
         (every (lambda (href)
                  (and (> (length href) 1)
                       (char= (char href 0) #\#)
                       (= 1 (count (subseq href 1) ids :test #'equal))))
                hrefs))))

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

  (it "keeps footnote IDs unique when a repeated reference meets a suffix label"
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

  (it "keeps footnote IDs unique when labels percent-encode to the same ID"
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

  (it "passes ordinary tags through and applies the GFM tagfilter"
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

(describe "HTML writer boundary coverage"
  (it "renders empty documents and omits non-rendered block nodes"
    (expect (cl-markdown-kit:write-html (html-writer-test-document))
            :to-equal "")
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-front-matter :raw-text "title: test")
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "ref" :normalized-label "ref"
             :destination "/ref" :eligible-p t)
            (cl-markdown-kit:make-footnote-definition
             :raw-label "unused" :normalized-label "unused")))
     :to-equal ""))
  (it "rejects unsupported node values in documents"
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-html
            (cl-markdown-kit:make-document
             :children (list (cl-markdown-kit:make-node))))
           nil)
       (error () t))
     :to-be-truthy))

  (it "renders raw block HTML in escape mode and filters every tagfilter boundary"
    (let ((escaped
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body "<b>raw</b>"))
             :raw-html :escape))
          (filtered
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body (concatenate
                           'string
                           "<script>x</script> <scriptx>safe</scriptx> "
                           "<script/> <style type=\"text/css\">x</style> plain<")))
             :raw-html :pass-through :gfm t)))
      (expect escaped :to-equal (format nil "&lt;b&gt;raw&lt;/b&gt;~%"))
      (expect filtered
              :to-equal
              (format nil
                      "&lt;script>x&lt;/script> <scriptx>safe</scriptx> &lt;script/> &lt;style type=\"text/css\">x&lt;/style> plain<~%"))))

  (it "omits inline HTML and filters pass-through text without tag openers"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline :body "<i>raw</i>")))))
     :to-equal (format nil "<p><!-- raw HTML omitted --></p>~%"))
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline :body "plain text"))))
      :raw-html :pass-through)
     :to-equal (format nil "<p>plain text</p>~%")))

  (it "passes raw CommonMark HTML through without the GFM tagfilter"
    (let* ((inline-body
             "<script>x</script><style>y</style><textarea>z</textarea><pre>w</pre>")
           (block-body
             (format nil
                     "<script>x</script>~%<style>y</style>~%<textarea>z</textarea>~%<pre>w</pre>~%"))
           (inline-document
             (html-writer-test-document
              (cl-markdown-kit:make-paragraph
               :children (list (cl-markdown-kit:make-html-inline
                                :body inline-body)))))
           (block-document
             (html-writer-test-document
              (cl-markdown-kit:make-html-block :body block-body)))
           (stream-output
             (with-output-to-string (stream)
               (cl-markdown-kit:write-html-to-stream
                inline-document stream :raw-html :pass-through :gfm nil))))
      (expect (cl-markdown-kit:write-html
               inline-document :raw-html :pass-through :gfm nil)
              :to-equal (format nil "<p>~A</p>~%" inline-body))
      (expect stream-output :to-equal
              (format nil "<p>~A</p>~%" inline-body))
      (expect (cl-markdown-kit:write-html
               block-document :raw-html :pass-through :gfm nil)
              :to-equal block-body)))

  (it "keeps the GFM tagfilter enabled for inline and block HTML"
    (let ((inline-output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-html-inline
                                     :body "<textarea>x</textarea>"))))
             :raw-html :pass-through :gfm t))
          (block-output
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body "<textarea>x</textarea>"))
             :raw-html :pass-through :gfm t)))
      (expect inline-output :to-equal
              (format nil "<p>&lt;textarea>x&lt;/textarea></p>~%"))
      (expect block-output :to-equal
              (format nil "&lt;textarea>x&lt;/textarea>~%"))))

  (it "renders code spans, breaks, emphasis, strong, and autolinks"
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
     :to-equal
     (format nil
             "<p><code>&lt;code&gt;</code>~%<br />~%<em>em</em> <strong>strong</strong> <a href=\"https://example.test/?a=%22x\">&lt;https://example.test&gt;</a></p>~%")))

  (it "renders block quotes, headings, and thematic breaks"
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
     :to-equal
     (format nil
             "<blockquote>~%<p>quoted</p>~%</blockquote>~%<h2>Heading</h2>~%<hr />~%<blockquote>~%</blockquote>~%")))

  (it "allows dangerous URL schemes only when unsafe URLs are enabled"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children
                    (list (cl-markdown-kit:make-link
                           :destination "javascript:alert(1)"
                           :children (list (cl-markdown-kit:make-text
                                            :value "unsafe"))))))
             :unsafe-urls t)))
      (expect (search "href=\"javascript:alert(1)\"" output) :to-be-truthy)))

  (it "renders loose and ordered lists, unchecked tasks, and empty items"
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
     :to-equal
     (format nil
             "<ol start=\"3\">~%<li><input type=\"checkbox\" disabled=\"\" /> ~%<p>todo</p>~%</li>~%<li></li>~%</ol>~%"))
    (expect (html-writer-test-render
             (list (cl-markdown-kit:make-list-node
                    :kind :ordered :start-number 1 :tight-p t :items nil)))
            :to-equal (format nil "<ol>~%</ol>~%"))
    (expect (html-writer-test-render
             (list (cl-markdown-kit:make-list-node
                    :kind :ordered :start-number 4 :tight-p t :items nil)))
            :to-equal (format nil "<ol start=\"4\">~%</ol>~%")))

  (it "renders missing code info as plain code and uses only the first info token"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-code-block :body (format nil "plain~%"))
            (cl-markdown-kit:make-code-block :body (format nil "blank~%")
                                             :info-string (format nil " ~C " #\Tab))
            (cl-markdown-kit:make-code-block :body (format nil "marked~%")
                                             :info-string " lisp extra")))
     :to-equal
     (format nil
             "<pre><code>plain~%</code></pre>~%<pre><code>blank~%</code></pre>~%<pre><code class=\"language-lisp\">marked~%</code></pre>~%")))

  (it "renders all plain inline forms in image alt text"
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
                           (list (cl-markdown-kit:make-text :value " struck")))))))))
     :to-equal
     (format nil
             "<p><img src=\"/alt\" alt=\"text &lt;code&gt; &lt;raw&gt;  auto nested strong struck\" /></p>~%")))

  (it "numbers repeated footnote references and handles definitions without paragraph endings"
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

  (it "keeps unresolved footnote references and renders header-only tables"
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
     :to-equal
     (format nil "<p></p>~%<table>~%<thead>~%<tr>~%<th>Header</th>~%</tr>~%</thead>~%</table>~%")))

  (it "rejects unsupported nodes, invalid options, and non-output streams"
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html :not-a-document) nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-html
            (html-writer-test-document
             (cl-markdown-kit:make-block-node)))
           nil)
       (error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (html-writer-test-render
            (list (cl-markdown-kit:make-paragraph
                   :children (list (cl-markdown-kit:make-inline-node)))))
           nil)
       (error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html (html-writer-test-document)
                                            :raw-html :invalid)
                nil)
       (error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html (html-writer-test-document)
                                            :unsafe-urls :invalid)
                nil)
       (error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html (html-writer-test-document)
                                            :gfm :invalid)
                nil)
       (error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (with-input-from-string (stream "")
           (cl-markdown-kit:write-html-to-stream
            (html-writer-test-document) stream)
           nil)
       (error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html-to-stream
                 (html-writer-test-document) "not-a-stream")
                nil)
       (type-error () t))
     :to-be-truthy))
  (it "writes deeply nested block and inline trees"
    (let ((inline (cl-markdown-kit:make-text :value "deep")))
      (dotimes (depth 4096)
        (setf inline
              (cl-markdown-kit:make-emphasis
               :children (list inline))))
      (let ((node (cl-markdown-kit:make-paragraph
                   :children (list inline))))
        (dotimes (depth 256)
          (setf node
                (cl-markdown-kit:make-block-quote
                 :children (list node))))
        (let ((html (cl-markdown-kit:write-html
                     (cl-markdown-kit:make-document
                      :children (list node)))))
          (labels ((count-occurrences (needle)
                     (loop with start = 0
                           for position = (search needle html :start2 start)
                           while position
                           count t
                           do (setf start (+ position (length needle))))))
            (expect (count-occurrences "<blockquote>") :to-equal 256)
            (expect (count-occurrences "</blockquote>") :to-equal 256)
            (expect (count-occurrences "<em>") :to-equal 4096)
            (expect (count-occurrences "</em>") :to-equal 4096)
            (expect (search "deep" html) :to-be-truthy)))))))
