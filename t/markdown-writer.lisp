(in-package #:cl-markdown-kit/test)

(defmacro with-mw-writer-state (&body body)
  `(let ((cl-markdown-kit::*mw-line-start-p* t)
         (cl-markdown-kit::*mw-line-indent* 0)
         (cl-markdown-kit::*mw-table-cell-p* nil)
         (cl-markdown-kit::*mw-escape-table-pipes-p* nil)
         (cl-markdown-kit::*mw-text-autolink-breakpoints* nil)
         (cl-markdown-kit::*mw-heading-p* nil)
         (cl-markdown-kit::*mw-autolink-label-p* nil)
         (cl-markdown-kit::*mw-escape-trailing-bang-p* nil)
         (cl-markdown-kit::*mw-list-boundary-token* nil)
         (cl-markdown-kit::*mw-list-boundary-count* 0)
         (cl-markdown-kit::*mw-following-block-p* nil)
         (cl-markdown-kit::*mw-minimize-inline-escapes-p* nil)
         (cl-markdown-kit::*mw-heading-closing-hash-allowed-p* nil))
     ,@body))

(defun markdown-writer-inline-node-signature (node)
  (typecase node
    (cl-markdown-kit:text
     (list :text (cl-markdown-kit:text-value node)))
    (cl-markdown-kit:code-span
     (list :code (cl-markdown-kit:code-span-literal-text node)))
    (cl-markdown-kit:html-inline
     (list :html (cl-markdown-kit:html-inline-body node)))
    (cl-markdown-kit:soft-break '(:soft-break))
    (cl-markdown-kit:hard-break '(:hard-break))
    (cl-markdown-kit:emphasis
     (list :emphasis
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:emphasis-children node))))
    (cl-markdown-kit:strong
     (list :strong
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:strong-children node))))
    (cl-markdown-kit:strikethrough
     (list :strikethrough
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:strikethrough-children node))))
    (cl-markdown-kit:link
     (list :link
           (cl-markdown-kit:link-destination node)
           (cl-markdown-kit:link-title node)
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:link-children node))))
    (cl-markdown-kit:image
     (list :image
           (cl-markdown-kit:image-destination node)
           (cl-markdown-kit:image-title node)
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:image-children node))))
    (cl-markdown-kit:autolink
     (list :link
           (cl-markdown-kit:autolink-destination node)
           nil
           (list (list :text (cl-markdown-kit:autolink-display-text node)))))
    (cl-markdown-kit:footnote-reference
     (list :footnote-reference
           (cl-markdown-kit:footnote-reference-normalized-label node)))
    (otherwise
     (error "Unsupported inline node in Markdown writer test: ~S" node))))

(defun markdown-writer-inline-sequence-signature (nodes)
  (let ((signatures nil))
    (dolist (node nodes)
      (let ((signature (markdown-writer-inline-node-signature node)))
        (if (and (eq (first signature) :text)
                 signatures
                 (eq (first (first signatures)) :text))
            (setf (second (first signatures))
                  (concatenate 'string (second (first signatures))
                               (second signature)))
            (push signature signatures))))
    (nreverse signatures)))

(defun markdown-writer-block-signature (node)
  (typecase node
    (cl-markdown-kit:front-matter
     (list :front-matter (cl-markdown-kit:front-matter-raw-text node)))
    (cl-markdown-kit:block-quote
     (list :block-quote
           (mapcar #'markdown-writer-block-signature
                   (cl-markdown-kit:block-quote-children node))))
    (cl-markdown-kit:list-node
     (list :list
           (cl-markdown-kit:list-node-kind node)
           (cl-markdown-kit:list-node-tight-p node)
           (cl-markdown-kit:list-node-start-number node)
           (mapcar #'markdown-writer-list-item-signature
                   (cl-markdown-kit:list-node-items node))))
    (cl-markdown-kit:paragraph
     (list :paragraph
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:paragraph-children node))))
    (cl-markdown-kit:heading
     (list :heading
           (cl-markdown-kit:heading-level node)
           (markdown-writer-inline-sequence-signature
            (cl-markdown-kit:heading-children node))))
    (cl-markdown-kit:thematic-break '(:thematic-break))
    (cl-markdown-kit:code-block
     (list :code-block
           (cl-markdown-kit:code-block-body node)
           (or (cl-markdown-kit:code-block-info-string node) "")))
    (cl-markdown-kit:html-block
     (list :html-block (cl-markdown-kit:html-block-body node)))
    (cl-markdown-kit:link-reference-definition
     (list :link-reference-definition
           (cl-markdown-kit:link-reference-definition-normalized-label node)
           (cl-markdown-kit:link-reference-definition-destination node)
           (cl-markdown-kit:link-reference-definition-title node)
           (cl-markdown-kit:link-reference-definition-eligible-p node)))
    (cl-markdown-kit:table
     (list :table
           (cl-markdown-kit:table-alignments node)
           (loop for row in (cl-markdown-kit:table-rows node)
                 collect
                 (mapcar
                  (lambda (cell)
                    (markdown-writer-inline-sequence-signature
                     (cl-markdown-kit:table-cell-children cell)))
                  (cl-markdown-kit:table-row-cells row)))))
    (cl-markdown-kit:footnote-definition
     (list :footnote-definition
           (cl-markdown-kit:footnote-definition-normalized-label node)
           (mapcar #'markdown-writer-block-signature
                   (cl-markdown-kit:footnote-definition-children node))))
    (otherwise
     (error "Unsupported block node in Markdown writer test: ~S" node))))

(defun markdown-writer-list-item-signature (node)
  (list (if (typep node 'cl-markdown-kit:task-list-item) :task-item :item)
        (when (typep node 'cl-markdown-kit:task-list-item)
          (cl-markdown-kit:task-list-item-checked-p node))
        (mapcar #'markdown-writer-block-signature
                (cl-markdown-kit:node-children node))))

(defun markdown-writer-document-signature (document)
  (mapcar #'markdown-writer-block-signature
          (cl-markdown-kit:document-children document)))

(defun markdown-writer-roundtrips-p (source &key (front-matter t))
  (let* ((document (cl-markdown-kit:parse-string
                    source :front-matter front-matter))
         (markdown (cl-markdown-kit:write-markdown document))
         (roundtrip (cl-markdown-kit:parse-string
                     markdown :front-matter front-matter)))
    (equal (markdown-writer-document-signature document)
           (markdown-writer-document-signature roundtrip))))

(describe "Markdown writer"
  (it "roundtrips block, inline, GFM, and front matter structure"
    (let ((source
            (format nil
                    "---~%title: Example~%---~%~%# Heading *emphasis*~%~%A [link][ref] and ![image](<image.png> \"caption\").~%~%[ref]: <docs/page.md> \"reference\"~%~%2. first~%3. second~%~%> quoted~%>~%> - nested~%~%```lisp~%```` closes later~%```~%~%| Name | Note |~%| :--- | ---: |~%| pipe | `x|y` |~%~%- [x] done~%- [ ] pending~%~%[^note]: a footnote~%~%Here[^note].~%")))
      (expect (markdown-writer-roundtrips-p source) :to-be-truthy)))

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

  (it "writes to a character stream and returns the emitted Markdown"
    (let* ((document (cl-markdown-kit:parse-string "# Heading"))
           (written (with-output-to-string (stream)
                      (cl-markdown-kit:write-markdown-to-stream
                       document stream))))
      (expect written :to-equal (cl-markdown-kit:write-markdown document))
      (expect written :to-equal "# Heading")))

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

  (it "preserves leading spaces through a Markdown HTML round trip"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list
               (cl-markdown-kit:make-paragraph
                :children
                (list (cl-markdown-kit:make-text :value "    x"))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal "&#32;   x")
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t))))

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

  (it "preserves an empty block quote through a Markdown HTML round trip"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list (cl-markdown-kit:make-block-quote :children nil))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal ">")
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
              (expect (html roundtrip) :to-equal (html document))))))))

  (it "renders inline node variants and escapes link components"
    (with-mw-writer-state
      (flet ((render (&rest nodes)
               (markdown-writer-render-inline-nodes nodes)))
        (expect (render (cl-markdown-kit:make-code-span :literal-text " a "))
                :to-equal "`  a  `")
        (expect (render (cl-markdown-kit:make-code-span :literal-text "   "))
                :to-equal "`   `")
        (expect (render (cl-markdown-kit:make-code-span :literal-text "a``b"))
                :to-equal "```a``b```")
        (expect (render (cl-markdown-kit:make-code-span
                         :literal-text (format nil "a~%b")))
                :to-equal "`a b`")
        (expect (render (cl-markdown-kit:make-html-inline :body "<i>raw</i>"))
                :to-equal "<i>raw</i>")
        (expect (render (cl-markdown-kit:make-soft-break)
                        (cl-markdown-kit:make-text :value "next")
                        (cl-markdown-kit:make-hard-break)
                        (cl-markdown-kit:make-text :value "last"))
                :to-equal (format nil "~%next\\~%last"))
        (expect (render (cl-markdown-kit:make-emphasis
                         :children (list (cl-markdown-kit:make-text :value "a")))
                        (cl-markdown-kit:make-emphasis
                         :delimiter-character #\_
                         :children (list (cl-markdown-kit:make-text :value "b")))
                        (cl-markdown-kit:make-strong
                         :children (list (cl-markdown-kit:make-text :value "c")))
                        (cl-markdown-kit:make-strong
                         :delimiter-character #\_
                         :children (list (cl-markdown-kit:make-text :value "d")))
                        (cl-markdown-kit:make-strikethrough
                         :children (list (cl-markdown-kit:make-text :value "e"))))
                :to-equal "*a*_b_**c**__d__~~e~~")
        (expect
         (render
          (cl-markdown-kit:make-link
           :destination "a<b>&copy;" :title "a\"b"
           :children (list (cl-markdown-kit:make-text :value "link"))))
         :to-equal
         (format nil "[link](<a~C<b~C>~C&copy;> \"a~C\"b\")"
                 #\\ #\\ #\\ #\\))
        (expect
         (render
          (cl-markdown-kit:make-link
           :destination (format nil "a~C b~%c" #\\)
           :title (format nil "a~C\"b" #\\)
           :children (list (cl-markdown-kit:make-text :value "x"))))
         :to-equal
         (format nil "[x](<a~C~C b&#10;c> \"a~C~C~C\"b\")"
                 #\\ #\\ #\\ #\\ #\\))
        (expect
         (render (cl-markdown-kit:make-image
                  :destination "image.png" :title nil
                  :children (list (cl-markdown-kit:make-text :value "alt"))))
         :to-equal "![alt](<image.png>)")
        (expect
         (render (cl-markdown-kit:make-autolink
                  :destination "https://example.test" :display-text "example"))
         :to-equal "[example](<https://example.test>)")
        (expect
         (markdown-writer-render-inline-nodes (list (cl-markdown-kit:make-text :value "direct")))
         :to-equal "direct")
        (expect
         (render (cl-markdown-kit:make-link
                  :destination "target"
                  :children (list (cl-markdown-kit:make-text :value "link")))
                (cl-markdown-kit:make-text :value "[tail]"))
         :to-equal "[link](<target>)\\[tail\\]")
        (expect
         (render (cl-markdown-kit:make-footnote-reference
                  :normalized-label
                  (format nil "a[b]&~Cc\\d]" #\Newline)))
         :to-equal "[^a\\[b\\]&amp; c\\\\d\\]]")
        (expect
         (handler-case
             (progn
               (markdown-writer-render-inline-nodes (list (cl-markdown-kit:make-node)))
               nil)
           (error () t))
        :to-be-truthy)))
    (let* ((table
             (cl-markdown-kit:make-table
              :alignments '(:left)
              :rows
              (list
               (cl-markdown-kit:make-table-row
                :cells
                (list
                 (cl-markdown-kit:make-table-cell
                  :children
                  (list (cl-markdown-kit:make-text :value "plain|pipe ")
                        (cl-markdown-kit:make-link
                         :destination "/a|b" :title "t|t"
                         :children (list (cl-markdown-kit:make-text
                                          :value "cell"))))))))))
           (output (cl-markdown-kit:write-markdown
                    (cl-markdown-kit:make-document :children (list table)))))
      (expect (search "/a\\|b" output) :to-be-truthy)
      (expect (search "plain\\|pipe" output) :to-be-truthy)
      (expect (search "t\\|t" output) :to-be-truthy)))

  (it "preserves table alignment, list continuation, and code fence behavior"
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "| :--- | ---: | :---: | --- |")
            :to-equal 4)
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "plain text")
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "| : |")
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "||")
            :to-equal nil)
    (with-mw-writer-state
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-paragraph
         :children (list (cl-markdown-kit:make-text
                          :value (format nil "a|b~%---|---")))))
       :to-equal (format nil "a|b~%---\\|---")))
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list
               (cl-markdown-kit:make-paragraph
                :children
                (list (cl-markdown-kit:make-text
                       :value (format nil "a|b~%---|---")))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown :gfm t)))
      (expect markdown :to-equal (format nil "a|b~%---\\|---"))
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t)))
    (dolist (test-case
              (list (list (format nil "a|~%---:")
                          (format nil "a|~%---\\:"))
                    (list (format nil "|a|~%:---")
                          (format nil "|a|~%\\:---"))
                    (list (format nil "a|~%---")
                          (format nil "a|~%\\---"))))
      (destructuring-bind (text expected-markdown) test-case
        (let* ((document
                 (cl-markdown-kit:make-document
                  :children
                  (list
                   (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-text :value text))))))
               (markdown (cl-markdown-kit:write-markdown document))
               (roundtrip (cl-markdown-kit:parse-string markdown :gfm t)))
          (expect markdown :to-equal expected-markdown)
          (expect (cl-markdown-kit:write-html
                   roundtrip :raw-html :pass-through :unsafe-urls t)
                  :to-equal
                  (cl-markdown-kit:write-html
                   document :raw-html :pass-through :unsafe-urls t)))))
    (with-mw-writer-state
      (expect
       (cl-markdown-kit:write-markdown
        (cl-markdown-kit:make-document
         :children
         (list (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text :value "first")))
               (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text :value "second")))
               (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text :value "third"))))))
       :to-equal (format nil "first~%~%second~%~%third")))
    (expect (cl-markdown-kit::%mw-prefix-lines "" "> ") :to-equal "")
    (expect (cl-markdown-kit::%mw-prefix-lines (format nil "a~%b~%") "> ")
            :to-equal (format nil "> a~%> b~%"))
    (expect (cl-markdown-kit::%mw-prefix-continuation-lines "" "  ")
            :to-equal "")
    (expect (cl-markdown-kit::%mw-prefix-continuation-lines
             (format nil "a~%b~%") "  ")
            :to-equal (format nil "a~%  b~%"))
    (with-mw-writer-state
      (expect
       (cl-markdown-kit::%mw-render-code-block
        (cl-markdown-kit:make-code-block
         :info-string "lisp" :body (format nil "line~%")))
       :to-equal (format nil "``` lisp~%line~%```"))
      (expect
       (cl-markdown-kit::%mw-render-code-block
        (cl-markdown-kit:make-code-block :info-string "l`sp" :body "~~~"))
       :to-equal (format nil "~~~~~~~~ l`sp~%~~~~~~"))
      (expect
       (cl-markdown-kit::%mw-render-code-block
        (cl-markdown-kit:make-code-block :body ""))
       :to-equal (format nil "```~%```")))
    (with-mw-writer-state
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-list-node
         :kind :unordered :tight-p t
         :items
         (list (cl-markdown-kit:make-list-item
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value (format nil "one~%two"))))))
               (cl-markdown-kit:make-task-list-item
                :checked-p t
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value "done")))))
               (cl-markdown-kit:make-task-list-item :checked-p nil)
               (cl-markdown-kit:make-list-item :children nil))))
       :to-equal (format nil "- one~%  two~%- [x] done~%- [ ]~%- "))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-list-node
         :kind :ordered :start-number 5 :tight-p nil
         :items
         (list (cl-markdown-kit:make-task-list-item
                :checked-p nil
                :children
                (list (cl-markdown-kit:make-code-block
                       :body (format nil "code~%"))))
               (cl-markdown-kit:make-list-item
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value "regular"))))))))
       :to-equal (format nil "5. [ ]~%   ```~%   code~%   ```~%~%6. regular"))
      (expect (cl-markdown-kit::%mw-alignment-marker :center) :to-equal ":---:")
      (expect (cl-markdown-kit::%mw-alignment-marker :unknown) :to-equal "---")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-list-node :items nil))
              :to-equal "")
      (let ((loose-task
              (markdown-writer-render-node
               (cl-markdown-kit:make-list-node
                :tight-p nil
                :items
                (list (cl-markdown-kit:make-task-list-item
                       :checked-p nil
                       :children
                       (list (cl-markdown-kit:make-paragraph
                              :children
                              (list (cl-markdown-kit:make-text :value "task"))))))))))
        (expect (search "task" loose-task) :to-be-truthy))
      (let ((tight-task-with-code
              (markdown-writer-render-node
               (cl-markdown-kit:make-list-node
                :tight-p t
                :items
                (list (cl-markdown-kit:make-task-list-item
                       :checked-p t
                       :children
                       (list (cl-markdown-kit:make-code-block :body "code"))))))))
        (expect (search "code" tight-task-with-code) :to-be-truthy))
      (expect
       (markdown-writer-render-node (cl-markdown-kit:make-table :rows nil))
       :to-equal "")))
  (it "escapes a dash-only alignment marker in a one-cell paragraph"
    (expect (cl-markdown-kit::%mw-table-alignment-breaker-position
             (format nil "|a|~%---"))
            :to-equal 4)
    (expect
     (cl-markdown-kit:write-markdown
      (cl-markdown-kit:make-document
       :children
       (list
        (cl-markdown-kit:make-paragraph
         :children
         (list (cl-markdown-kit:make-text
                :value (format nil "|a|~%---")))))))
     :to-equal (format nil "|a|~%\\---")))

  (it "renders remaining block variants and rejects invalid writer inputs"
    (expect
     (cl-markdown-kit::%mw-normalize-newlines
      (format nil "a~Cb~C~%c~%" #\Return #\Return))
     :to-equal (format nil "a~%b~%c~%"))
    (expect (cl-markdown-kit::%mw-normalize-newlines
             (format nil "end~C" #\Return))
            :to-equal (format nil "end~%"))
    (with-mw-writer-state
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-front-matter
         :raw-text (format nil "a~C~%b~Cc~%" #\Return #\Return)))
       :to-equal (format nil "---~%a~%b~%c~%---"))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-front-matter :raw-text ""))
       :to-equal (format nil "---~%---"))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-front-matter :raw-text "content"))
       :to-equal (format nil "---~%content~%---"))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-heading :level 2 :children nil))
       :to-equal "##")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-thematic-break))
              :to-equal "* * *")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-html-block :body "<hr>"))
              :to-equal "<hr>")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-block-quote :children nil))
              :to-equal ">")
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-link-reference-definition
         :raw-label "raw[&]" :normalized-label "" :destination "target"
         :title "say \"hi\""))
       :to-equal (format nil "[raw~C[&amp;~C]]: <target> \"say ~C\"hi~C\"\""
                         #\\ #\\ #\\ #\\))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-link-reference-definition
         :raw-label "raw" :normalized-label "normalized" :destination "target"))
       :to-equal "[normalized]: <target>")
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-footnote-definition :normalized-label "note"))
       :to-equal "[^note]:")
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-footnote-definition
         :normalized-label "note"
         :children
         (list (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text
                                 :value (format nil "first~%second")))))))
       :to-equal (format nil "[^note]:~%    first~%    second"))
      (expect
       (handler-case
           (progn
             (markdown-writer-render-node (cl-markdown-kit:make-node))
             nil)
         (error () t))
       :to-be-truthy))
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-markdown :not-a-document) nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-markdown-to-stream
            (cl-markdown-kit:make-document :children nil) "not a stream")
           nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-markdown-to-stream
            (cl-markdown-kit:make-document :children nil)
            (make-string-input-stream "input"))
           nil)
       (type-error () t))
     :to-be-truthy))

  (it "covers writer boundaries in directly constructed AST nodes"
    (labels ((render-nodes (nodes)
               (cl-markdown-kit:write-markdown
                (cl-markdown-kit:make-document :children nodes)))
             (render-block (node)
               (render-nodes (list node)))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children))
             (text (value)
               (cl-markdown-kit:make-text :value value))
             (signals-error-p (thunk)
               (handler-case
                   (progn (funcall thunk) nil)
                 (error () t))))
      (let* ((raw-inline (format nil " ~CTop~% ~CBottom" #\Tab #\Tab))
             (raw-inline-output
               (render-block
                (paragraph
                 (list (cl-markdown-kit:make-html-inline :body raw-inline)))))
             (marker-output
               (render-block (paragraph (list (text "* * *")))))
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
             (underscore-marker-output
               (render-block (paragraph (list (text "___  ")))))
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
                        :display-text "example")))))
             (front-matter-crlf
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
                (cl-markdown-kit:parse-string following-code)))
             (empty-table
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
             (list-item
               (cl-markdown-kit:make-list-item
                :children (list (paragraph (list (text "item"))))))
             (separator-label "cl-markdown-kit-writer-list-separator-0")
             (list-boundary-collision
               (render-nodes
                (list (paragraph (list (text separator-label)))
                      (cl-markdown-kit:make-list-node
                       :kind :unordered :tight-p t :items (list list-item))
                      (cl-markdown-kit:make-list-node
                       :kind :unordered :tight-p t :items (list list-item)))))
             (token-collision
               (format nil "~Cmarkdown-writer-list-boundary~C" #\Null #\Null))
             (list-token-collision
               (render-nodes
                (list (cl-markdown-kit:make-list-node
                       :kind :unordered :tight-p t :items (list list-item))
                      (cl-markdown-kit:make-list-node
                       :kind :unordered :tight-p t :items (list list-item))
                      (paragraph (list (text token-collision))))))
             (heading-soft-break
               (render-block
                (cl-markdown-kit:make-heading
                 :level 2
                 :children (list (text "first")
                                 (cl-markdown-kit:make-soft-break)
                                 (text "second"))))))
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
        (expect (search "say \\\"hi\\\"" link-output) :to-be-truthy)
        (expect trailing-bang-link :to-equal "bang\\![](</destination>)")
        (expect ordinary-text-link :to-equal "bang[](</destination>)")
        (expect empty-text-link :to-equal "[](</destination>)")
        (expect differing-autolink
                :to-equal "[example](<https://example.test>)")
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
                :to-equal "after")
        (expect empty-table :to-equal "")
        (expect empty-footnote :to-equal "[^]:")
        (expect empty-footnote-child :to-equal (format nil "[^note]:~%"))
        (expect empty-task :to-equal "- [ ]")
        (expect (search (format nil "- [x]~%  ```~%  x") block-task)
                :to-be-truthy)
        (expect empty-list :to-equal "")
        (expect (search "[cl-markdown-kit-writer-list-separator-1]: <>"
                        list-boundary-collision)
                :to-be-truthy)
        (expect (search token-collision list-token-collision) :to-be-truthy)
        (expect heading-soft-break :to-equal "## first&#10;second")
        (expect (signals-error-p
                 (lambda ()
                   (cl-markdown-kit:write-markdown-to-stream
                    nil (make-string-output-stream))))
                :to-be-truthy)
        (expect (signals-error-p
                 (lambda ()
                   (cl-markdown-kit:write-markdown-to-stream
                    (cl-markdown-kit:make-document) nil)))
                :to-be-truthy)
        (expect (signals-error-p
                 (lambda ()
                   (cl-markdown-kit:write-markdown-to-stream
                    (cl-markdown-kit:make-document)
                    (make-string-input-stream ""))))
                :to-be-truthy)
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
                 (list (text "visible")))
                :to-be-truthy)
        (expect (cl-markdown-kit::%mw-inline-tail-has-nonspace-p
                 (list (text "  ")))
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
                :to-equal nil)
        (expect (signals-error-p
                 (lambda ()
                   (render-nodes (list (cl-markdown-kit:make-node)))))
                :to-be-truthy)
        (expect (signals-error-p
                 (lambda ()
                   (render-block
                    (paragraph (list (cl-markdown-kit:make-node))))))
                :to-be-truthy))))

  (it "preserves inline writer context across nested runs"
    (labels ((text (value)
               (cl-markdown-kit:make-text :value value))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children))
             (document (block)
               (cl-markdown-kit:make-document :children (list block)))
             (roundtrip (document)
               (cl-markdown-kit:parse-string
                (cl-markdown-kit:write-markdown document)))
             (html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t))
             (expect-roundtrip (original expected-markdown expected-signature)
               (let ((markdown (cl-markdown-kit:write-markdown original))
                     (parsed (roundtrip original)))
                 (expect markdown :to-equal expected-markdown)
                 (expect (markdown-writer-document-signature parsed)
                         :to-equal expected-signature)
                 (expect (html parsed) :to-equal (html original)))))
      (let* ((strong-heading
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-strong
                        :children (list (text "section #")))
                       (text " follows")))))
             (strong-heading-end
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-strong
                        :children (list (text "section #")))))))
             (emphasis-heading
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-emphasis
                        :children (list (text "section #")))
                       (text " follows")))))
             (safe-mixed-inline
               (document
                (paragraph
                 (list (text "a ~ b")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))))))
             (mixed-delimiter-boundary
               (document
                (paragraph
                 (list (text "left ~ *")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))))))
             (delimiter-boundary
               (document
                (paragraph
                 (list (text "left*")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))
                       (text "*right")))))
             (code-span-boundary
               (document
                (paragraph
                 (list (text "`foo")
                       (cl-markdown-kit:make-code-span
                        :literal-text "bar")))))
             (split-autolink
               (document
                (paragraph
                 (list (text "https")
                       (cl-markdown-kit:make-html-inline :body "")
                       (text "://example.test")))))
             (split-entity
               (document
                (paragraph
                 (list (text "&am")
                       (cl-markdown-kit:make-html-inline :body "")
                       (text "p;"))))))
        (expect-roundtrip
         strong-heading "## **section #** follows"
         '((:heading 2 ((:strong ((:text "section #")))
                       (:text " follows")))))
        (expect-roundtrip
         strong-heading-end "## **section #**"
         '((:heading 2 ((:strong ((:text "section #")))))))
        (expect-roundtrip
         emphasis-heading "## *section #* follows"
         '((:heading 2 ((:emphasis ((:text "section #")))
                       (:text " follows")))))
        (expect-roundtrip
         safe-mixed-inline "a ~ b**bold**"
         '((:paragraph ((:text "a ~ b") (:strong ((:text "bold")))))))
        (expect-roundtrip
         mixed-delimiter-boundary "left ~ \\***bold**"
         '((:paragraph ((:text "left ~ *")
                        (:strong ((:text "bold")))))))
        (expect-roundtrip
         delimiter-boundary "left\\***bold**\\*right"
         '((:paragraph ((:text "left*")
                        (:strong ((:text "bold")))
                        (:text "*right")))))
        (expect-roundtrip
         code-span-boundary "\\`foo`bar`"
         '((:paragraph ((:text "`foo") (:code "bar")))))
        (expect-roundtrip
         split-autolink "https\\://example.test"
         '((:paragraph ((:text "https://example.test")))))
        (expect-roundtrip
         split-entity "\\&amp;"
         '((:paragraph ((:text "&amp;"))))))))
  (it "writes deeply nested Markdown block and inline trees"
    (let ((inline (cl-markdown-kit:make-text :value "deep")))
      (dotimes (depth 4096)
        (setf inline
              (cl-markdown-kit:make-emphasis
               :children (list inline))))
      (let ((block (cl-markdown-kit:make-paragraph
                    :children (list inline))))
        (dotimes (depth 256)
          (setf block
                (cl-markdown-kit:make-block-quote
                 :children (list block))))
        (let ((markdown
                (cl-markdown-kit:write-markdown
                 (cl-markdown-kit:make-document
                  :children (list block)))))
          (labels ((count-character (character)
                     (loop for candidate across markdown
                           count (char= candidate character))))
            (expect (search "deep" markdown) :to-be-truthy)
            (expect (count-character #\*) :to-equal 8192)
            (expect (count-character #\>) :to-equal 256))))))

  (it "normalizes front matter line endings without changing HTML round trips"
    (let* ((source
             (format nil "---~C~%title: value~Ctail~C---~C~%body"
                     #\Return #\Return #\Return #\Return))
           (document (cl-markdown-kit:parse-string source))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown))
           (stream-markdown
             (with-output-to-string (stream)
               (cl-markdown-kit:write-markdown-to-stream document stream))))
      (expect markdown :to-equal
              (format nil "---~%title: value~%tail~%---~%~%body"))
      (expect stream-markdown :to-equal markdown)
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t))))

  (it "preserves rendered HTML across every CommonMark and GFM fixture"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm))
           (exclusions (conformance-read-exclusions))
           (matches 0)
           (failures nil)
           (unexpected-failures nil))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (length examples) :to-equal 682)
      (expect (conformance-exclusions-valid-p examples exclusions)
              :to-be-truthy)
      (dolist (example examples)
        (let ((stage :parse-original)
              (original-html nil)
              (markdown nil)
              (roundtrip-html nil)
              (condition-message nil))
          (handler-case
              (let ((document
                      (cl-markdown-kit:parse-string
                       (spec-example-markdown example))))
                (setf stage :render-original-html
                      original-html
                      (cl-markdown-kit:write-html
                       document :raw-html :pass-through :unsafe-urls t))
                (setf stage :write-markdown
                      markdown (cl-markdown-kit:write-markdown document))
                (setf stage :parse-roundtrip)
                (let ((roundtrip-document
                        (cl-markdown-kit:parse-string markdown)))
                  (setf stage :render-roundtrip-html
                        roundtrip-html
                        (cl-markdown-kit:write-html
                         roundtrip-document
                         :raw-html :pass-through :unsafe-urls t)))
                (setf stage :compare-html))
            (error (condition)
              (setf condition-message (princ-to-string condition))))
          (if (and (null condition-message)
                   (string= original-html roundtrip-html))
              (incf matches)
              (let ((failure
                      (list :id (spec-example-id example)
                            :section (spec-example-section example)
                            :stage stage
                            :condition condition-message
                            :input (spec-example-markdown example)
                            :reference-html (spec-example-html example)
                            :original-html original-html
                            :markdown markdown
                            :roundtrip-html roundtrip-html)))
                (push failure failures)
                (unless (and (eq stage :compare-html)
                             (null condition-message)
                             (member (spec-example-id example)
                                     exclusions :key #'first :test #'string=))
                  (push failure unexpected-failures))))))
      (dolist (failure (reverse failures))
        (format t
                "T-003 mismatch ~A (~A)~%  stage: ~S~%  condition: ~S~%  original matches fixture HTML: ~S~%  input: ~S~%  fixture HTML: ~S~%  original HTML: ~S~%  writer Markdown: ~S~%  round-trip HTML: ~S~%"
                (getf failure :id)
                (getf failure :section)
                (getf failure :stage)
                (getf failure :condition)
                (and (getf failure :original-html)
                     (string= (getf failure :original-html)
                              (getf failure :reference-html)))
                (getf failure :input)
                (getf failure :reference-html)
                (getf failure :original-html)
                (getf failure :markdown)
                (getf failure :roundtrip-html)))
      (format t "T-003 Markdown round trips: ~D CommonMark + ~D GFM; ~D match, ~D documented parser mismatches, ~D declared exclusions.~%"
              (length commonmark) (length gfm) matches
              (- (length failures) (length unexpected-failures))
              (length exclusions))
      (expect unexpected-failures :to-equal nil)
      (expect (+ matches (length failures)) :to-equal 682)))
)
