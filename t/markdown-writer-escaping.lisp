(in-package #:cl-markdown-kit/test)

(describe "Markdown writer escaping"
(it "escapes text that would become blocks, tables, or bare autolinks"
    (let* ((source
             (format nil
                     "\\## literal heading~%\\1. literal item~%&#48;. zero item~%https\\://example.test~%www\\.example.test~%name\\@example.test~%ordinary text~%"))
           (document (cl-markdown-kit:parse-string source))
           (markdown (cl-markdown-kit:write-markdown document)))
      (expect (markdown-writer-roundtrips-p source) :to-be-truthy)
      (expect (search "\\## literal heading" markdown) :to-be-truthy)
      (expect (search "&#48;. zero item" markdown) :to-be-truthy)
      (expect (search "https\\://example.test" markdown) :to-be-truthy)
      (expect (search "www\\.example.test" markdown) :to-be-truthy)
      (expect (search "name\\@example.test" markdown) :to-be-truthy)))

(it "chooses a safe code fence and preserves code-span literals"
    (let* ((source (format nil "````lisp~%```~%````x~%````~%~%`a `` b`~%"))
           (document (cl-markdown-kit:parse-string source))
           (markdown (cl-markdown-kit:write-markdown document)))
      (expect (search "````` lisp" markdown) :to-be-truthy)
      (expect (markdown-writer-roundtrips-p source) :to-be-truthy)))

(it "preserves trailing text spaces without creating a hard break"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list (cl-markdown-kit:make-paragraph
                     :children
                     (list (cl-markdown-kit:make-text :value "line  "))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown))
           (children
             (cl-markdown-kit:paragraph-children
              (first (cl-markdown-kit:document-children roundtrip)))))
      (expect markdown :to-equal "line&#32;&#32;")
      (expect (mapcar #'cl-markdown-kit:text-value children)
              :to-equal '("line  "))))

(it "keeps ordered list markers within the CommonMark digit limit"
    (let* ((make-item
             (lambda (value)
               (cl-markdown-kit:make-list-item
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children
                       (list (cl-markdown-kit:make-text :value value)))))))
           (document
             (cl-markdown-kit:make-document
              :children
              (list (cl-markdown-kit:make-list-node
                     :kind :ordered
                     :tight-p t
                     :start-number 999999999
                     :items (list (funcall make-item "first")
                                  (funcall make-item "second"))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal
              (format nil "999999999. first~%999999999. second"))
      (expect (markdown-writer-document-signature roundtrip)
              :to-equal (markdown-writer-document-signature document))))

(it "escapes table separators without changing soft breaks in paragraphs"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list (cl-markdown-kit:make-paragraph
                     :children
                     (list (cl-markdown-kit:make-text :value "a|b")
                           (cl-markdown-kit:make-soft-break)
                           (cl-markdown-kit:make-text :value "---|---"))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal (format nil "a|b~%---\\|---"))
      (expect (markdown-writer-document-signature roundtrip)
              :to-equal (markdown-writer-document-signature document))))

(it "tracks line indentation and escapes text that could start a block"
    (let ((input (format nil "  ~Cx~%~C" #\Tab #\Tab)))
      (let ((cl-markdown-kit::*mw-line-start-p* t)
            (cl-markdown-kit::*mw-line-indent* 0))
        (expect
         (with-output-to-string (stream)
           (cl-markdown-kit::%mw-write-string input stream))
         :to-equal input)
        (expect cl-markdown-kit::*mw-line-start-p* :to-be-truthy)
        (expect cl-markdown-kit::*mw-line-indent* :to-equal 4)))
    (with-mw-writer-state
      (flet ((render (text)
               (markdown-writer-render-inline-nodes
                (list (cl-markdown-kit:make-text :value text)))))
        (expect (render (format nil "    x")) :to-equal "&#32;   x")
        (expect (render (format nil "~Cx" #\Tab)) :to-equal "&#9;x")
        (expect (render (format nil " ~C" #\Tab)) :to-equal "&#32;&#9;")
        (expect (render (format nil "## heading~%1. item~%> quote~%---"))
                :to-equal (format nil "\\## heading~%&#49;. item~%\\> quote~%\\---"))
        (expect (render "&copy; &x &amp;") :to-equal "\\&copy; &x \\&amp;")
        (expect
         (render (format nil "http://a.test https://b.test mailto:c@d.test xmpp:e@f.test"))
         :to-equal "http\\://a.test https\\://b.test mailto:c\\@d.test xmpp:e\\@f.test")
        (expect (render (format nil "x~C!y" #\\))
                :to-equal (format nil "x~C~C!y" #\\ #\\))
        (expect (render (format nil "x~Cay" #\\))
                :to-equal (format nil "x~Cay" #\\))
        (expect (render (format nil "line~%next"))
               :to-equal (format nil "line~%next"))
        (expect
         (with-output-to-string (stream)
           (cl-markdown-kit::%mw-write-text
            (format nil "first~%~%last") stream))
         :to-equal "first&#10;&#10;last")
        (expect
         (with-output-to-string (stream)
           (cl-markdown-kit::%mw-write-text (format nil "last~%") stream))
         :to-equal (format nil "last~%"))))
    (expect (cl-markdown-kit::%mw-heading-marker-p "##" 0 2) :to-be-truthy)
    (expect (cl-markdown-kit::%mw-heading-marker-p "#x" 0 2) :to-equal nil)
    (expect (cl-markdown-kit::%mw-heading-marker-p "#######" 0 7) :to-equal nil)
    (expect (cl-markdown-kit::%mw-heading-marker-p "" 0 0) :to-equal nil)
    (expect (cl-markdown-kit::%mw-list-marker-p "-" 0 1) :to-be-truthy)
    (expect (cl-markdown-kit::%mw-list-marker-p "1)" 0 2) :to-be-truthy)
    (expect (cl-markdown-kit::%mw-list-marker-p "123456789. item" 0 15)
            :to-be-truthy)
    (expect (cl-markdown-kit::%mw-list-marker-p "123456789" 0 9)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-list-marker-p "1234567890. item" 0 16)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-list-marker-p "" 0 0) :to-equal nil)
    (expect (cl-markdown-kit::%mw-list-marker-p "1x" 0 2) :to-equal nil)
    (expect (cl-markdown-kit::%mw-marker-line-p "* * *" 0 5 '(#\*) 3)
            :to-be-truthy)
    (expect (cl-markdown-kit::%mw-marker-line-p "* * x" 0 5 '(#\*) 3)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-marker-line-p "* *" 0 3 '(#\*) 3)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-marker-line-p "x" 0 1 '(#\*) 1)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-marker-line-p "" 0 0 '(#\*) 1)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-setext-marker-line-p "= = " 0 4)
            :to-be-truthy)
    (expect (cl-markdown-kit::%mw-setext-marker-line-p "===x" 0 4)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-setext-marker-line-p "= x" 0 3)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-setext-marker-line-p "" 0 0) :to-equal nil)
    (expect (cl-markdown-kit::%mw-line-prefix-hazard-p "> quote" 0 7)
            :to-be-truthy)
    (expect (cl-markdown-kit::%mw-line-prefix-hazard-p "---" 0 3)
            :to-be-truthy)
    (expect (cl-markdown-kit::%mw-line-prefix-hazard-p "ordinary" 0 8)
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-line-prefix-hazard-p "" 0 0)
            :to-equal nil))

(it "keeps scheme text from creating an email autolink"
    (labels ((document (text)
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-paragraph
                  :children (list (cl-markdown-kit:make-text :value text))))))
             (html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t))
             (expect-roundtrip (text expected-markdown)
               (let* ((original (document text))
                      (markdown (cl-markdown-kit:write-markdown original))
                      (roundtrip (cl-markdown-kit:parse-string markdown)))
                 (expect markdown :to-equal expected-markdown)
                 (expect (html roundtrip) :to-equal (html original)))))
      (expect-roundtrip "mailto:c@d.test" "mailto:c\\@d.test")
      (expect-roundtrip "xmpp:e@f.test" "xmpp:e\\@f.test")
      (expect-roundtrip "mailto:literal" "mailto\\:literal")
      (expect-roundtrip "xmpp:literal" "xmpp\\:literal")))

(it "keeps direct text newlines within heading and table syntax"
    (labels ((document (node)
               (cl-markdown-kit:make-document :children (list node)))
             (html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t)))
      (let* ((heading-document
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-text
                        :value (format nil "first~%second"))))))
             (markdown (cl-markdown-kit:write-markdown heading-document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown :to-equal "## first&#10;second")
        (expect (html roundtrip) :to-equal (html heading-document)))
      (let* ((table-document
               (document
                (cl-markdown-kit:make-table
                 :alignments '(:left)
                 :rows
                 (list
                  (cl-markdown-kit:make-table-row
                   :header-row-p t
                   :cells
                   (list
                    (cl-markdown-kit:make-table-cell
                     :children
                     (list (cl-markdown-kit:make-text
                            :value (format nil "first~%second"))))))))))
             (markdown (cl-markdown-kit:write-markdown table-document))
             (roundtrip (cl-markdown-kit:parse-string markdown :gfm t)))
        (expect markdown
                :to-equal (format nil "| first&#10;second |~%| :--- |"))
        (expect (html roundtrip) :to-equal (html table-document)))))

(it "keeps consecutive paragraph text newlines inside the paragraph"
    (dolist (test-case
              (list (list (format nil "first~%~%second")
                          "first&#10;&#10;second")
                    (list (format nil "~%~%first") "&#10;&#10;first")
                    (list (format nil "first~%~%") "first&#10;&#10;")))
      (destructuring-bind (text expected-markdown) test-case
        (let* ((document
                 (cl-markdown-kit:make-document
                  :children
                  (list
                   (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-text :value text))))))
               (markdown (cl-markdown-kit:write-markdown document))
               (roundtrip (cl-markdown-kit:parse-string markdown)))
          (expect markdown :to-equal expected-markdown)
          (expect (cl-markdown-kit:write-html
                   roundtrip :raw-html :pass-through :unsafe-urls t)
                  :to-equal
                  (cl-markdown-kit:write-html
                   document :raw-html :pass-through :unsafe-urls t))))))

(it "keeps spaces before inline siblings literal"
    (dolist (test-case
              (list (list (list (cl-markdown-kit:make-text :value "left ")
                                (cl-markdown-kit:make-code-span :literal-text "code"))
                          "left `code`")
                    (list (list (cl-markdown-kit:make-text
                                 :value (format nil " ~C" #\Tab))
                                (cl-markdown-kit:make-code-span :literal-text "code"))
                          (format nil "&#32;&#9;`code`"))))
      (destructuring-bind (children expected-markdown) test-case
        (let* ((document
                 (cl-markdown-kit:make-document
                  :children
                  (list (cl-markdown-kit:make-paragraph :children children))))
               (markdown (cl-markdown-kit:write-markdown document))
               (roundtrip (cl-markdown-kit:parse-string markdown)))
          (expect markdown :to-equal expected-markdown)
          (expect (cl-markdown-kit:write-html
                   roundtrip :raw-html :pass-through :unsafe-urls t)
                  :to-equal
                  (cl-markdown-kit:write-html
                   document :raw-html :pass-through :unsafe-urls t))))))

(it "writes thematic breaks without opening YAML front matter"
    (labels ((html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list (cl-markdown-kit:make-thematic-break)
                      (cl-markdown-kit:make-heading
                       :level 2
                       :children (list (cl-markdown-kit:make-text :value "foo")))
                      (cl-markdown-kit:make-thematic-break))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown :to-equal
                (format nil "* * *~%~%## foo~%~%* * *"))
        (expect (html roundtrip) :to-equal (html document)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-list-node
                  :kind :unordered
                  :tight-p t
                  :items
                  (list
                   (cl-markdown-kit:make-list-item
                    :children
                    (list
                     (cl-markdown-kit:make-paragraph
                      :children
                      (list (cl-markdown-kit:make-text :value "Foo")))))
                   (cl-markdown-kit:make-list-item
                    :children (list (cl-markdown-kit:make-thematic-break))))))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown :to-equal (format nil "- Foo~%- * * *"))
        (expect (html roundtrip) :to-equal (html document)))))

(it "uses minimum backslash escaping when punctuation needs no escape"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list
               (cl-markdown-kit:make-paragraph
                :children
                (list (cl-markdown-kit:make-text :value (format nil "x~C!y" #\\)))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal (format nil "x~C~C!y" #\\ #\\))
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t))))

(it "escapes a text backslash only when Markdown needs it"
    (labels ((html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-paragraph
                  :children
                  (list (cl-markdown-kit:make-text
                         :value (format nil "x~Cay" #\\)))))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown :to-equal (format nil "x~Cay" #\\))
        (expect (html roundtrip) :to-equal (html document)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-paragraph
                  :children
                  (list (cl-markdown-kit:make-text :value "prefix\\")
                        (cl-markdown-kit:make-strong
                         :children
                         (list (cl-markdown-kit:make-text :value "bold"))))))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown
                :to-equal (format nil "prefix~C~C**bold**" #\\ #\\))
        (expect (html roundtrip) :to-equal (html document)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-paragraph
                  :children
                  (list (cl-markdown-kit:make-text
                         :value (format nil "tail~C" #\\)))))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown :to-equal (format nil "tail~C" #\\))
        (expect (html roundtrip) :to-equal (html document)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-paragraph
                  :children
                  (list (cl-markdown-kit:make-text
                         :value (format nil "tail~C" #\\))
                        (cl-markdown-kit:make-soft-break)
                        (cl-markdown-kit:make-text :value "next"))))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown
                :to-equal (format nil "tail~C~C~%next" #\\ #\\))
        (expect (html roundtrip) :to-equal (html document)))
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list
                 (cl-markdown-kit:make-paragraph
                  :children (list (cl-markdown-kit:make-text
                                   :value (format nil "tail~C" #\\))))
                 (cl-markdown-kit:make-paragraph
                  :children (list (cl-markdown-kit:make-text :value "next"))))))
             (markdown (cl-markdown-kit:write-markdown document))
             (roundtrip (cl-markdown-kit:parse-string markdown)))
        (expect markdown
                :to-equal (format nil "tail~C~C~%~%next" #\\ #\\))
        (expect (html roundtrip) :to-equal (html document)))))

(it "preserves terminal backslashes before inline closing delimiters"
    (labels ((html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t)))
      (let* ((text (cl-markdown-kit:make-text
                    :value (format nil "x~C" #\\)))
             (cases
               (list
                (list (cl-markdown-kit:make-emphasis :children (list text))
                      (format nil "*x~C~C*" #\\ #\\))
                (list (cl-markdown-kit:make-strong :children (list text))
                      (format nil "**x~C~C**" #\\ #\\))
                (list (cl-markdown-kit:make-strikethrough :children (list text))
                      (format nil "~~~~x~C~C~~~~" #\\ #\\))
                (list (cl-markdown-kit:make-link
                       :destination "target" :children (list text))
                      (format nil "[x~C~C](<target>)" #\\ #\\))
                (list (cl-markdown-kit:make-image
                       :destination "target" :title nil :children (list text))
                      (format nil "![x~C~C](<target>)" #\\ #\\)))))
        (dolist (test-case cases)
          (destructuring-bind (inline expected-markdown) test-case
            (let* ((document
                     (cl-markdown-kit:make-document
                      :children
                      (list (cl-markdown-kit:make-paragraph
                             :children (list inline)))))
                   (markdown (cl-markdown-kit:write-markdown document))
                   (roundtrip (cl-markdown-kit:parse-string markdown)))
              (expect markdown :to-equal expected-markdown)
              (expect (html roundtrip) :to-equal (html document)))))))))
