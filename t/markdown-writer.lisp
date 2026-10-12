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
      (expect written :to-equal "# Heading"))))
