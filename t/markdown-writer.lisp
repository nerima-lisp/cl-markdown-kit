(in-package #:cl-markdown-kit/test)

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
  (list (if (cl-markdown-kit:task-list-item-p node) :task-item :item)
        (when (cl-markdown-kit:task-list-item-p node)
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
      (expect markdown :to-equal (format nil "a\\|b~%---\\|---"))
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
    (flet ((render (text)
             (cl-markdown-kit::%mw-render-inline-string
              (list (cl-markdown-kit:make-text :value text)))))
      (expect (render (format nil "    x")) :to-equal "   &#32;x")
      (expect (render (format nil "~Cx" #\Tab)) :to-equal "&#9;x")
      (expect (render (format nil " ~C" #\Tab)) :to-equal "&#32;&#9;")
      (expect (render (format nil "## heading~%1. item~%> quote~%---"))
              :to-equal (format nil "\\## heading~%&#49;. item~%\\> quote~%\\---"))
      (expect (render "&copy; &x &amp;") :to-equal "\\&copy; &x \\&amp;")
      (expect
       (render (format nil "http://a.test https://b.test mailto:c@d.test xmpp:e@f.test"))
       :to-equal "http\\://a.test https\\://b.test mailto\\:c@d.test xmpp\\:e@f.test")
      (expect (render (format nil "x~C!y" #\\))
              :to-equal (format nil "x~C~C~C!y" #\\ #\\ #\\))
      (expect (render (format nil "x~Cay" #\\))
              :to-equal (format nil "x~C~Cay" #\\ #\\))
      (expect (render (format nil "line~%next"))
              :to-equal (format nil "line~%next")))
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

  (it "renders inline node variants and escapes link components"
    (flet ((render (&rest nodes)
             (cl-markdown-kit::%mw-render-inline-string nodes)))
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
       (render (cl-markdown-kit:make-footnote-reference
                :normalized-label
                (format nil "a[b]&~Cc\\d]" #\Newline)))
       :to-equal "[^a\\[b\\]&amp; c\\\\d\\]]")
      (expect
       (handler-case
           (progn
             (cl-markdown-kit::%mw-render-inline-string
              (list (cl-markdown-kit:make-node)))
             nil)
         (error () t))
       :to-be-truthy))
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
    (expect (cl-markdown-kit::%mw-table-start-follows-p
             (format nil "a|b~%---|---"))
            :to-be-truthy)
    (expect (cl-markdown-kit::%mw-table-start-follows-p
             (format nil "a|b~%---"))
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-table-start-follows-p "a|b")
            :to-equal nil)
    (expect
     (cl-markdown-kit::%mw-render-paragraph
      (cl-markdown-kit:make-paragraph
       :children (list (cl-markdown-kit:make-text
                        :value (format nil "a|b~%---|---")))))
     :to-equal (format nil "a\\|b~%---\\|---"))
    (expect
     (cl-markdown-kit::%mw-render-blocks
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-text :value "first")))
            (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-text :value "second")))
            (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-text :value "third"))))
      (format nil "~%"))
     :to-equal (format nil "first~%second~%third"))
    (expect (cl-markdown-kit::%mw-prefix-lines "" "> ") :to-equal "")
    (expect (cl-markdown-kit::%mw-prefix-lines (format nil "a~%b~%") "> ")
            :to-equal (format nil "> a~%> b~%"))
    (expect (cl-markdown-kit::%mw-prefix-continuation-lines "" "  ")
            :to-equal "")
    (expect (cl-markdown-kit::%mw-prefix-continuation-lines
             (format nil "a~%b~%") "  ")
            :to-equal (format nil "a~%  b~%"))
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
     :to-equal (format nil "```~%```"))
    (expect
     (cl-markdown-kit::%mw-render-list
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
     (cl-markdown-kit::%mw-render-list
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
    (expect (cl-markdown-kit::%mw-render-list
             (cl-markdown-kit:make-list-node :items nil))
            :to-equal "")
    (let ((loose-task
            (cl-markdown-kit::%mw-render-list
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
            (cl-markdown-kit::%mw-render-list
             (cl-markdown-kit:make-list-node
              :tight-p t
              :items
              (list (cl-markdown-kit:make-task-list-item
                     :checked-p t
                     :children
                     (list (cl-markdown-kit:make-code-block :body "code"))))))))
      (expect (search "code" tight-task-with-code) :to-be-truthy))
    (expect
     (cl-markdown-kit::%mw-render-table (cl-markdown-kit:make-table :rows nil))
     :to-equal ""))

  (it "renders remaining block variants and rejects invalid writer inputs"
    (expect
     (cl-markdown-kit::%mw-normalize-newlines
      (format nil "a~Cb~C~%c~%" #\Return #\Return))
     :to-equal (format nil "a~%b~%c~%"))
    (expect (cl-markdown-kit::%mw-normalize-newlines
             (format nil "end~C" #\Return))
            :to-equal (format nil "end~%"))
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-front-matter
       :raw-text (format nil "a~C~%b~Cc~%" #\Return #\Return)))
     :to-equal (format nil "---~%a~%b~%c~%---"))
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-front-matter :raw-text ""))
     :to-equal (format nil "---~%---"))
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-front-matter :raw-text "content"))
     :to-equal (format nil "---~%content~%---"))
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-heading :level 2 :children nil))
     :to-equal "##")
    (expect (cl-markdown-kit::%mw-render-block
             (cl-markdown-kit:make-thematic-break))
            :to-equal "---")
    (expect (cl-markdown-kit::%mw-render-block
             (cl-markdown-kit:make-html-block :body "<hr>"))
            :to-equal "<hr>")
    (expect (cl-markdown-kit::%mw-render-block
             (cl-markdown-kit:make-block-quote :children nil))
            :to-equal "")
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-link-reference-definition
       :raw-label "raw[&]" :normalized-label "" :destination "target"
       :title "say \"hi\""))
     :to-equal (format nil "[raw~C[&amp;~C]]: <target> \"say ~C\"hi~C\"\""
                       #\\ #\\ #\\ #\\))
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-link-reference-definition
       :raw-label "raw" :normalized-label "normalized" :destination "target"))
     :to-equal "[normalized]: <target>")
    (expect
     (cl-markdown-kit::%mw-render-block
      (cl-markdown-kit:make-footnote-definition :normalized-label "note"))
     :to-equal "[^note]:")
    (expect
     (cl-markdown-kit::%mw-render-block
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
           (cl-markdown-kit::%mw-render-block (cl-markdown-kit:make-node))
           nil)
       (error () t))
     :to-be-truthy)
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
     :to-be-truthy)))
