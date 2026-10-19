(in-package #:cl-markdown-kit/test)
(describe "Dispatch verification"
  (it "verifies HTML node dispatches"
    (expect (cl-markdown-kit::%hw-verify-node-dispatches)
            :to-equal t)))

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
     :to-match-snapshot "html/escaped-paragraph"))

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
     :to-match-snapshot
     "html/link-image-attributes"))

  (it "renders code blocks with an escaped language class and body"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-code-block
             :style :fenced :body (format nil "<tag>~%")
             :info-string "lisp")))
     :to-match-snapshot
     "html/escaped-code-block"))

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
     :to-match-snapshot
     "html/tight-list-items"))

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
      (expect output :to-match-snapshot
              "html/aligned-table")))

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
     :to-match-snapshot "html/strikethrough"))

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
     :to-match-snapshot
     "html/escaped-raw-inline-html"))

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
     :to-match-snapshot
     "html/escaped-title-attribute"))

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
      (expect escaped :to-match-snapshot "html/escaped-raw-block-html")
      (expect filtered
              :to-match-snapshot
              "html/gfm-tagfilter-boundaries")))

  (it "omits inline HTML and filters pass-through text without tag openers"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline :body "<i>raw</i>")))))
     :to-match-snapshot "html/omitted-inline-html")
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline :body "plain text"))))
      :raw-html :pass-through)
     :to-match-snapshot "html/passthrough-inline-html"))

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
              :to-match-snapshot "html/passthrough-commonmark-inline")
      (expect stream-output :to-match-snapshot
              "html/passthrough-commonmark-inline-stream")
      (expect (cl-markdown-kit:write-html
               block-document :raw-html :pass-through :gfm nil)
              :to-match-snapshot "html/passthrough-commonmark-block")))

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
      (expect inline-output :to-match-snapshot
              "html/gfm-inline-tagfilter")
      (expect block-output :to-match-snapshot
              "html/gfm-block-tagfilter")))

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
     :to-match-snapshot
     "html/inline-rendering"))

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
     :to-match-snapshot
     "html/block-rendering"))

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

  (it "renders missing code info as plain code and uses only the first info token"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-code-block :body (format nil "plain~%"))
            (cl-markdown-kit:make-code-block :body (format nil "blank~%")
                                             :info-string (format nil " ~C " #\Tab))
            (cl-markdown-kit:make-code-block :body (format nil "marked~%")
                                             :info-string " lisp extra")))
     :to-match-snapshot
     "html/code-info-tokens"))

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
                           (list (cl-markdown-kit:make-text :value " struck")))
                          (cl-markdown-kit:make-image
                           :destination "/nested"
                           :children
                           (list (cl-markdown-kit:make-text
                                  :value " nested & image")))))))))
     :to-match-snapshot
     "html/image-alt-text"))

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
     :to-match-snapshot
     "html/unresolved-footnote-header-table"))

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
