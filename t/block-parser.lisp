(in-package #:cl-markdown-kit/test)

(defun block-parser-test-result (source)
  (cl-markdown-kit::parse-blocks
   (cl-markdown-kit:make-source-buffer source)))

(defun block-parser-test-line (text)
  (aref (cl-markdown-kit::%bp-split-lines text) 0))

(defun block-parser-test-document (result)
  (cl-markdown-kit::block-parse-result-document result))

(defun block-parser-test-span-is (node start end)
  (let ((span (cl-markdown-kit:node-span node)))
    (and span
         (= (cl-parser-kit:span-start span) start)
         (= (cl-parser-kit:span-end span) end))))

(defun block-parser-test-ranges-valid-p (node source)
  (let* ((span (cl-markdown-kit:node-span node))
         (start (and span (cl-parser-kit:span-start span)))
         (end (and span (cl-parser-kit:span-end span)))
         (children (cl-markdown-kit:node-children node)))
    (and span
         (string= (cl-parser-kit:span-source span) source)
         (<= 0 start end (length source))
         (loop with previous-end = start
               for child in children
               for child-span = (cl-markdown-kit:node-span child)
               for child-start = (and child-span
                                      (cl-parser-kit:span-start child-span))
               for child-end = (and child-span
                                    (cl-parser-kit:span-end child-span))
               do (unless (and child-span
                               (<= start child-start child-end end)
                               (<= previous-end child-start)
                               (block-parser-test-ranges-valid-p child source))
                    (return-from block-parser-test-ranges-valid-p nil))
                  (setf previous-end child-end)
               finally (return t)))))

(defun block-parser-test-only-child (node)
  (let ((children (cl-markdown-kit:node-children node)))
    (and (= (length children) 1) (first children))))

(defun block-parser-test-gfm-example (id)
  (or (find id (gfm-extension-examples)
            :key #'spec-example-id :test #'string=)
      (error "Missing GFM fixture ~A" id)))

(defun block-parser-test-commonmark-example (id)
  (or (find id (commonmark-spec-examples)
            :key #'spec-example-id :test #'string=)
      (error "Missing CommonMark fixture ~A" id)))

(defun block-parser-test-gfm-result (id)
  (block-parser-test-result
   (spec-example-markdown (block-parser-test-gfm-example id))))

(defun block-parser-test-nodes-of-type (root predicate)
  (let ((nodes nil))
    (cl-markdown-kit:walk-nodes
     (lambda (node)
       (when (funcall predicate node)
         (push node nodes)))
     root)
    (nreverse nodes)))

(defun block-parser-test-fragments-for (result node)
  (remove-if-not
   (lambda (fragment)
     (eq (cl-markdown-kit::inline-fragment-node fragment) node))
   (cl-markdown-kit::block-parse-result-inline-fragments result)))

(defun block-parser-test-fragment-texts (result)
  (mapcar #'cl-markdown-kit::inline-fragment-text
          (cl-markdown-kit::block-parse-result-inline-fragments result)))

(defun block-parser-test-span-text (span)
  (subseq (cl-parser-kit:span-source span)
          (cl-parser-kit:span-start span)
          (cl-parser-kit:span-end span)))

(defun block-parser-test-substring-count (needle text)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count position
        do (setf start (+ position (length needle)))))

(describe "Leaf block parser"
  (it "parses thematic breaks, blank separators, and excludes line endings from spans"
    (let* ((source (format nil "***~%~%---~%"))
           (document
             (block-parser-test-document
              (block-parser-test-result source)))
           (children (cl-markdown-kit:document-children document)))
      (expect (length children) :to-equal 2)
      (expect (every #'cl-markdown-kit:thematic-break-p children)
              :to-be-truthy)
      (expect (mapcar #'cl-markdown-kit:thematic-break-marker-character children)
              :to-equal (list #\* #\-))
      (expect (mapcar #'cl-markdown-kit:thematic-break-marker-count children)
              :to-equal '(3 3))
      (expect (and (block-parser-test-span-is (first children) 0 3)
                   (block-parser-test-span-is (second children) 5 8))
              :to-be-truthy)))

  (it "retains ATX and setext heading syntax in the block structure"
    (let* ((atx-source (format nil "## title ###~%"))
           (atx (block-parser-test-only-child
                 (block-parser-test-document
                  (block-parser-test-result atx-source))))
           (setext-source (format nil "Title~%===~%"))
           (setext (block-parser-test-only-child
                    (block-parser-test-document
                     (block-parser-test-result setext-source)))))
      (expect (and (cl-markdown-kit:heading-p atx)
                   (= (cl-markdown-kit:heading-level atx) 2)
                   (eq (cl-markdown-kit:heading-style atx) :atx)
                   (char= (cl-markdown-kit:heading-marker-character atx) #\#)
                   (= (cl-markdown-kit:heading-opening-marker-length atx) 2)
                   (= (cl-markdown-kit:heading-closing-marker-length atx) 3)
                   (block-parser-test-span-is atx 0 12))
              :to-be-truthy)
      (expect (and (cl-markdown-kit:heading-p setext)
                   (= (cl-markdown-kit:heading-level setext) 1)
                   (eq (cl-markdown-kit:heading-style setext) :setext)
                   (char= (cl-markdown-kit:heading-marker-character setext) #\=)
                   (block-parser-test-span-is setext 0 9))
              :to-be-truthy)))

  (it "recognizes delimited front matter as a source-backed leaf block"
    (let* ((source (format nil "---~%title: x~%---~%# Body"))
           (document (block-parser-test-document
                      (block-parser-test-result source)))
           (children (cl-markdown-kit:document-children document))
           (front-matter (first children))
           (unclosed-source (format nil "---~%title: x~%# Body"))
           (unclosed-children
             (cl-markdown-kit:document-children
              (block-parser-test-document
               (block-parser-test-result unclosed-source)))))
      (expect (and (= (length children) 2)
                   (cl-markdown-kit:front-matter-p front-matter)
                   (string= (cl-markdown-kit:front-matter-raw-text front-matter)
                            (format nil "title: x~%"))
                   (string= (cl-markdown-kit:front-matter-opening-delimiter
                             front-matter)
                            "---")
                   (string= (cl-markdown-kit:front-matter-closing-delimiter
                             front-matter)
                            "---")
                   (= (cl-parser-kit:span-end
                       (cl-markdown-kit:node-span front-matter))
                      16)
                   (cl-markdown-kit:heading-p (second children))
                   (= (length unclosed-children) 3)
                   (cl-markdown-kit:thematic-break-p
                    (first unclosed-children))
                   (cl-markdown-kit:paragraph-p (second unclosed-children))
                   (cl-markdown-kit:heading-p (third unclosed-children)))
              :to-be-truthy)))

  (it "defers paragraph text as a source-mapped inline fragment"
    (let* ((source (format nil "plain paragraph~%~%"))
           (result (block-parser-test-result source))
           (paragraph (block-parser-test-only-child
                       (block-parser-test-document result)))
           (fragments
             (cl-markdown-kit::block-parse-result-inline-fragments result))
           (fragment (first fragments)))
      (expect (and (= (length fragments) 1)
                   (cl-markdown-kit:paragraph-p paragraph)
                   (eq (cl-markdown-kit::inline-fragment-node fragment)
                       paragraph)
                   (string= (cl-markdown-kit::inline-fragment-text fragment)
                            "plain paragraph")
                   (= (cl-markdown-kit::boundary-map-length
                       (cl-markdown-kit::inline-fragment-boundary-map fragment))
                      (length (cl-markdown-kit::inline-fragment-text fragment)))
                   (block-parser-test-span-is paragraph 0 15))
              :to-be-truthy)))

  (it "preserves fenced and indented code block attributes"
    (let* ((fenced-source (format nil "```lisp~%(print 1)~%```"))
           (fenced (block-parser-test-only-child
                    (block-parser-test-document
                     (block-parser-test-result fenced-source))))
           (indented-source (format nil "    one~%    two~%~%"))
           (indented (block-parser-test-only-child
                      (block-parser-test-document
                       (block-parser-test-result indented-source)))))
      (expect (and (cl-markdown-kit:code-block-p fenced)
                   (eq (cl-markdown-kit:code-block-style fenced) :fenced)
                   (char= (cl-markdown-kit:code-block-fence-character fenced)
                          #\`)
                   (= (cl-markdown-kit:code-block-fence-length fenced) 3)
                   (string= (cl-markdown-kit:code-block-info-string fenced)
                            "lisp")
                   (string= (cl-markdown-kit:code-block-body fenced)
                            (format nil "(print 1)~%"))
                   (block-parser-test-span-is fenced 0 (length fenced-source)))
              :to-be-truthy)
      (expect (and (cl-markdown-kit:code-block-p indented)
                   (eq (cl-markdown-kit:code-block-style indented) :indented)
                   (null (cl-markdown-kit:code-block-info-string indented))
                   (search "one" (cl-markdown-kit:code-block-body indented))
                   (search "two" (cl-markdown-kit:code-block-body indented))
                   (block-parser-test-ranges-valid-p indented indented-source))
              :to-be-truthy)))

  (it "excludes trailing blank lines from indented code blocks"
    (let* ((source (format nil "    code~%    ~%~%"))
           (children (cl-markdown-kit:document-children
                      (block-parser-test-document
                       (block-parser-test-result source))))
           (code (first children)))
      (expect (and (= (length children) 1)
                   (cl-markdown-kit:code-block-p code)
                   (string= (cl-markdown-kit:code-block-body code)
                            (format nil "code~%"))
                   (block-parser-test-span-is code 0 8))
              :to-be-truthy)))

  (it "recognizes each CommonMark HTML block start condition"
    (let ((examples
            (list "<!-- comment -->"
                  "<?xml version=\"1.0\"?>"
                  "<script>run()</script>"
                  "<!DOCTYPE html>"
                  "<![CDATA[data]]>"
                  "<div>block</div>"
                  (format nil "<custom-element>~%block~%</custom-element>")))
          (kinds nil))
      (dolist (source examples)
        (let* ((html (block-parser-test-only-child
                      (block-parser-test-document
                       (block-parser-test-result source)))))
          (push (cl-markdown-kit:html-block-kind html) kinds)
          (expect (and (cl-markdown-kit:html-block-p html)
                       (cl-markdown-kit:html-block-kind html)
                       (string= (cl-markdown-kit:html-block-body html) source)
                       (block-parser-test-span-is html 0 (length source)))
                  :to-be-truthy)))
      (expect (= (length (remove-duplicates kinds :test #'equal)) 7)
              :to-be-truthy)))

  (it "requires complete HTML block start and end tags"
    (let* ((self-closing
             (block-parser-test-only-child
              (block-parser-test-document
               (block-parser-test-result "<custom-element data-x=\"1\"/>"))))
           (inline-tag
             (block-parser-test-only-child
              (block-parser-test-document
               (block-parser-test-result "<custom-element>inline</custom-element>"))))
           (bare-declaration
             (block-parser-test-only-child
              (block-parser-test-document (block-parser-test-result "<!"))))
           (invalid-end-source
             (format nil "<script>~%before~%</script/>~%after"))
           (invalid-end
             (block-parser-test-only-child
              (block-parser-test-document
               (block-parser-test-result invalid-end-source))))
           (valid-end-source
             (format nil "<script>~%before~%</script   >~%after"))
           (valid-end-children
             (cl-markdown-kit:document-children
              (block-parser-test-document
               (block-parser-test-result valid-end-source)))))
      (expect (and (cl-markdown-kit:html-block-p self-closing)
                   (= (cl-markdown-kit:html-block-kind self-closing) 7)
                   (cl-markdown-kit:paragraph-p inline-tag)
                   (cl-markdown-kit:paragraph-p bare-declaration)
                   (cl-markdown-kit:html-block-p invalid-end)
                   (= (cl-markdown-kit:html-block-kind invalid-end) 1)
                   (string= (cl-markdown-kit:html-block-body invalid-end)
                            invalid-end-source)
                   (= (length valid-end-children) 2)
                   (cl-markdown-kit:html-block-p (first valid-end-children))
                   (cl-markdown-kit:paragraph-p (second valid-end-children)))
              :to-be-truthy)))

  (it "indexes source-ordered link reference definitions"
    (let* ((source (format nil "[Ref]: /target \"Title\"~%[ref]: /later~%~%plain~%"))
           (result (block-parser-test-result source))
           (document (block-parser-test-document result))
           (definition (first (cl-markdown-kit:document-children document)))
           (duplicate (second (cl-markdown-kit:document-children document)))
           (reference-index
             (cl-markdown-kit::block-parse-result-reference-index result)))
      (expect (and (cl-markdown-kit:link-reference-definition-p definition)
                   (cl-markdown-kit:link-reference-definition-p duplicate)
                   (string= (cl-markdown-kit:link-reference-definition-raw-label
                             definition)
                            "Ref")
                   (string= (cl-markdown-kit:link-reference-definition-normalized-label
                             definition)
                            "ref")
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             definition)
                            "/target")
                   (string= (cl-markdown-kit:link-reference-definition-title
                             definition)
                            "Title")
                   (cl-markdown-kit:link-reference-definition-eligible-p
                    definition)
                   (not (cl-markdown-kit:link-reference-definition-eligible-p
                         duplicate))
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             duplicate)
                            "/later")
                   (eq (gethash "ref" reference-index) definition))
              :to-be-truthy)))

  (it "parses multiline reference titles and consumes only their source lines"
    (let* ((source (format nil
                           "[multi]: /target 'title~%continued'~%[next]: /next~%plain"))
           (result (block-parser-test-result source))
           (children (cl-markdown-kit:document-children
                      (block-parser-test-document result)))
           (index (cl-markdown-kit::block-parse-result-reference-index result))
           (destination-source (format nil "[remote]:~%  /destination~%paragraph"))
           (destination-result (block-parser-test-result destination-source))
           (destination-children
             (cl-markdown-kit:document-children
              (block-parser-test-document destination-result))))
      (expect (and (= (length children) 3)
                   (every #'cl-markdown-kit:link-reference-definition-p
                          (subseq children 0 2))
                   (string= (cl-markdown-kit:link-reference-definition-title
                             (first children))
                            (format nil "title~%continued"))
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             (second children))
                            "/next")
                   (cl-markdown-kit:paragraph-p (third children))
                   (eq (gethash "multi" index) (first children))
                   (eq (gethash "next" index) (second children)))
              :to-be-truthy)
      (expect (and (= (length destination-children) 2)
                   (cl-markdown-kit:link-reference-definition-p
                    (first destination-children))
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             (first destination-children))
                            "/destination")
                   (cl-markdown-kit:paragraph-p (second destination-children)))
              :to-be-truthy)))

  (it "signals the configured limit before entering a nested block quote"
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::parse-blocks
                   (cl-markdown-kit:make-source-buffer "> > nested")
                   :max-nesting-depth 1)
                  nil)
              (cl-markdown-kit:markdown-nesting-depth-exceeded (condition)
                condition))))
      (expect (and condition
                   (= (cl-markdown-kit:markdown-resource-limit-error-limit
                       condition)
                      1)
                   (= (cl-markdown-kit:markdown-resource-limit-error-actual
                       condition)
                      2))
              :to-be-truthy)))

  (it "preserves block quote nesting, separate quotes, and lazy continuation"
    (let* ((quoted (block-parser-test-document
                    (block-parser-test-result
                     (spec-example-markdown
                      (block-parser-test-commonmark-example
                       "commonmark-0228")))))
           (quoted-block (block-parser-test-only-child quoted))
           (quoted-children (cl-markdown-kit:block-quote-children quoted-block))
           (lazy-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0232"))))
           (lazy (block-parser-test-document lazy-result))
           (lazy-block (block-parser-test-only-child lazy))
           (lazy-children (cl-markdown-kit:block-quote-children lazy-block))
           (separate (block-parser-test-document
                      (block-parser-test-result
                       (spec-example-markdown
                        (block-parser-test-commonmark-example
                         "commonmark-0242")))))
           (separate-quotes (cl-markdown-kit:document-children separate))
           (nested-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0250"))))
           (nested (block-parser-test-document nested-result))
           (outer-quote (block-parser-test-only-child nested)))
      (expect (and (cl-markdown-kit:block-quote-p quoted-block)
                   (= (length quoted-children) 2)
                   (cl-markdown-kit:heading-p (first quoted-children))
                   (cl-markdown-kit:paragraph-p (second quoted-children)))
              :to-be-truthy)
      (expect (and (= (length lazy-children) 2)
                   (cl-markdown-kit:heading-p (first lazy-children))
                   (cl-markdown-kit:paragraph-p (second lazy-children))
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     lazy-result (second lazy-children))))
                            (format nil "bar~%baz")))
              :to-be-truthy)
      (expect (and (= (length separate-quotes) 2)
                   (every #'cl-markdown-kit:block-quote-p separate-quotes)
                   (every (lambda (quote)
                            (and (= (length (cl-markdown-kit:block-quote-children
                                            quote))
                                    1)
                                 (cl-markdown-kit:paragraph-p
                                  (first (cl-markdown-kit:block-quote-children
                                          quote)))))
                          separate-quotes))
              :to-be-truthy)
      (expect (and (cl-markdown-kit:block-quote-p outer-quote)
                   (cl-markdown-kit:block-quote-p
                    (block-parser-test-only-child outer-quote))
                   (cl-markdown-kit:block-quote-p
                    (block-parser-test-only-child
                     (block-parser-test-only-child outer-quote)))
                   (let* ((inner-quote
                            (block-parser-test-only-child
                             (block-parser-test-only-child outer-quote)))
                          (paragraph
                            (block-parser-test-only-child inner-quote))
                          (result nested-result))
                     (and (cl-markdown-kit:paragraph-p paragraph)
                          (string= (cl-markdown-kit::inline-fragment-text
                                    (first (block-parser-test-fragments-for
                                            result paragraph)))
                                   (format nil "foo~%bar")))))
              :to-be-truthy)))

  (it "preserves tight and loose list nesting from CommonMark fixtures"
    (let* ((nested-loose-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0319"))))
           (nested-loose-document
             (block-parser-test-document nested-loose-result))
           (nested-loose-lists
             (block-parser-test-nodes-of-type
              nested-loose-document #'cl-markdown-kit:list-node-p))
           (loose-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0325"))))
           (loose-document (block-parser-test-document loose-result))
           (loose-lists
             (block-parser-test-nodes-of-type
              loose-document #'cl-markdown-kit:list-node-p))
           (separate-items-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0326"))))
           (separate-items-document
             (block-parser-test-document separate-items-result))
           (separate-items-lists
             (block-parser-test-nodes-of-type
              separate-items-document #'cl-markdown-kit:list-node-p)))
      (expect (and (= (length nested-loose-lists) 2)
                   (equal (mapcar #'cl-markdown-kit:list-node-tight-p
                                  nested-loose-lists)
                          '(t nil))
                   (= (length (cl-markdown-kit:list-node-items
                               (first nested-loose-lists)))
                      2)
                   (= (length (cl-markdown-kit:list-node-items
                               (second nested-loose-lists)))
                      1))
              :to-be-truthy)
      (expect (and (= (length loose-lists) 2)
                   (equal (mapcar #'cl-markdown-kit:list-node-tight-p loose-lists)
                          '(nil t))
                   (= (length (cl-markdown-kit:list-node-items
                               (first loose-lists)))
                      1)
                   (= (length (cl-markdown-kit:list-node-items
                               (second loose-lists)))
                      1))
              :to-be-truthy)
      (expect (and (= (length separate-items-lists) 3)
                   (equal (mapcar #'cl-markdown-kit:list-node-tight-p
                                  separate-items-lists)
                          '(nil t t))
                   (= (length (cl-markdown-kit:list-node-items
                               (first separate-items-lists)))
                      2))
              :to-be-truthy)))

  (it "keeps GFM table alignments and splits embedded pipes without changing columns"
    (let* ((alignment-result (block-parser-test-gfm-result "gfm-0006"))
           (alignment-table
             (block-parser-test-only-child
              (block-parser-test-document alignment-result)))
           (pipe-result (block-parser-test-gfm-result "gfm-0009"))
           (pipe-table
             (block-parser-test-only-child
              (block-parser-test-document pipe-result)))
           (rows (cl-markdown-kit:table-rows pipe-table))
           (escaped-row (second rows))
           (code-row (third rows))
           (escaped-cells (cl-markdown-kit:table-row-cells escaped-row))
           (code-cells (cl-markdown-kit:table-row-cells code-row))
           (escaped-text
             (cl-markdown-kit::inline-fragment-text
              (first (block-parser-test-fragments-for
                      pipe-result (first escaped-cells)))))
           (code-text
             (cl-markdown-kit::inline-fragment-text
              (first (block-parser-test-fragments-for
                      pipe-result (first code-cells)))))
           (unmatched-source
             (format nil "| a | b |~%| - | - |~%| `open | split |~%"))
           (unmatched-rows
             (cl-markdown-kit:table-rows
              (block-parser-test-only-child
               (block-parser-test-document
                (block-parser-test-result unmatched-source))))))
      (expect (and (cl-markdown-kit:table-p alignment-table)
                   (equal (cl-markdown-kit:table-alignments alignment-table)
                          '(:left nil :center nil :right)))
              :to-be-truthy)
      (expect (and (cl-markdown-kit:table-p pipe-table)
                   (= (length rows) 5)
                   (every (lambda (row)
                            (= (length (cl-markdown-kit:table-row-cells row)) 2))
                          rows)
                   (search (format nil "~C|okay~C|" #\\ #\\) escaped-text)
                   (search (format nil "`~C|code~C| is okay`" #\\ #\\)
                           code-text)
                   (= (length (cl-markdown-kit:table-row-cells
                               (second unmatched-rows)))
                      2)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     pipe-result (second code-cells))))
                            ""))
              :to-be-truthy)))

  (it "recognizes a table immediately after a paragraph and preserves table-cell fragments"
    (let* ((result (block-parser-test-gfm-result "gfm-0016"))
           (document (block-parser-test-document result))
           (children (cl-markdown-kit:document-children document))
           (paragraph (first children))
           (table (second children))
           (table-rows (cl-markdown-kit:table-rows table))
           (body-cells (cl-markdown-kit:table-row-cells (second table-rows))))
      (expect (and (= (length children) 2)
                   (cl-markdown-kit:paragraph-p paragraph)
                   (cl-markdown-kit:table-p table)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     result paragraph)))
                            (format nil "123~%456"))
                   (= (length table-rows) 2)
                   (equal (mapcar
                           (lambda (cell)
                             (cl-markdown-kit::inline-fragment-text
                              (first (block-parser-test-fragments-for result cell))))
                           body-cells)
                          '("d" "e")))
              :to-be-truthy)))

  (it "keeps table cells and source text in the GFM interop fixture"
    (let* ((result (block-parser-test-gfm-result "gfm-0027"))
           (table (block-parser-test-only-child
                   (block-parser-test-document result)))
           (rows (cl-markdown-kit:table-rows table))
           (body-cells (cl-markdown-kit:table-row-cells (second rows))))
      (expect (and (cl-markdown-kit:table-p table)
                   (= (length rows) 2)
                   (= (length body-cells) 2)
                   (equal (mapcar
                           (lambda (cell)
                             (cl-markdown-kit::inline-fragment-text
                              (first (block-parser-test-fragments-for result cell))))
                           body-cells)
                          '("https://github.com www.github.com"
                            "http://pokemon.com")))
              :to-be-truthy)))

  (it "keeps footnote definitions as blocks and references in deferred fragments"
    (let* ((result (block-parser-test-gfm-result "gfm-0023"))
           (document (block-parser-test-document result))
           (definitions
             (block-parser-test-nodes-of-type
              document #'cl-markdown-kit:footnote-definition-p))
           (labels (mapcar #'cl-markdown-kit:footnote-definition-raw-label
                           definitions))
           (fragment-texts (block-parser-test-fragment-texts result)))
      (expect (equal labels
                     '("other-note" "codeblock-note" "1" "footnote" "unused"))
              :to-be-truthy)
      (expect (and (some (lambda (text) (search "[^1]" text)) fragment-texts)
                   (some (lambda (text) (search "[^footnote]" text))
                         fragment-texts)
                   (some (lambda (node)
                           (and (cl-markdown-kit:footnote-definition-p node)
                                (some #'cl-markdown-kit:block-quote-p
                                      (cl-markdown-kit:footnote-definition-children
                                       node))))
                         definitions))
              :to-be-truthy)))

  (it "keeps repeated and escaped GFM footnote labels source-backed"
    (let* ((repeated-result (block-parser-test-gfm-result "gfm-0024"))
           (repeated-document
             (block-parser-test-document repeated-result))
           (repeated-definitions
             (block-parser-test-nodes-of-type
              repeated-document #'cl-markdown-kit:footnote-definition-p))
           (repeated-text (format nil "~{~A~^ ~}"
                                  (block-parser-test-fragment-texts
                                   repeated-result)))
           (escaped-result (block-parser-test-gfm-result "gfm-0025"))
           (escaped-definitions
             (block-parser-test-nodes-of-type
              (block-parser-test-document escaped-result)
              #'cl-markdown-kit:footnote-definition-p)))
      (expect (and (equal (mapcar
                           #'cl-markdown-kit:footnote-definition-raw-label
                           repeated-definitions)
                          '("a-footnote"))
                   (= (block-parser-test-substring-count
                       "[^a-footnote]" repeated-text)
                      3))
              :to-be-truthy)
      (expect (and (equal (mapcar
                           #'cl-markdown-kit:footnote-definition-raw-label
                           escaped-definitions)
                          '("\"><script>alert(1)</script>"))
                   (some (lambda (text)
                           (search "[^\"><script>alert(1)</script>]" text))
                         (block-parser-test-fragment-texts escaped-result)))
              :to-be-truthy)))

  (it "keeps GFM task states, exact marker spans, and ordinary list items distinct"
    (dolist (id '("gfm-0028" "gfm-0029" "gfm-0030"))
      (let* ((source (spec-example-markdown
                      (block-parser-test-gfm-example id)))
             (result (block-parser-test-result source))
             (document (block-parser-test-document result))
             (tasks (block-parser-test-nodes-of-type
                     document #'cl-markdown-kit:task-list-item-p)))
        (let ((expected-states
                (if (string= id "gfm-0028")
                    '(nil t)
                    '(t nil t nil))))
          (expect (and (equal (mapcar
                               #'cl-markdown-kit:task-list-item-checked-p tasks)
                              expected-states)
                       (every (lambda (task)
                                (let ((span
                                        (cl-markdown-kit:task-list-item-task-marker-span
                                         task))
                                      (item-span
                                        (cl-markdown-kit:node-span task)))
                                  (and (string= (cl-parser-kit:span-source span)
                                               source)
                                       (= (- (cl-parser-kit:span-end span)
                                             (cl-parser-kit:span-start span))
                                          3)
                                       (<= (cl-parser-kit:span-start item-span)
                                           (cl-parser-kit:span-start span)
                                           (cl-parser-kit:span-end span)
                                           (cl-parser-kit:span-end item-span))
                                       (member (block-parser-test-span-text span)
                                               '("[ ]" "[x]" "[X]")
                                               :test #'string=))))
                              tasks))
                  :to-be-truthy))
        (when (member id '("gfm-0029" "gfm-0030") :test #'string=)
          (expect (and (= (length tasks) 4)
                       (= (length (remove-if
                                   #'cl-markdown-kit:task-list-item-p
                                   (block-parser-test-nodes-of-type
                                    document #'cl-markdown-kit:list-item-p)))
                          4))
                  :to-be-truthy))))
    (let* ((source "- [X] uppercase")
           (document (block-parser-test-document
                      (block-parser-test-result source)))
           (task (first (block-parser-test-nodes-of-type
                         document #'cl-markdown-kit:task-list-item-p)))
           (marker (cl-markdown-kit:task-list-item-task-marker-span task)))
      (expect (and (cl-markdown-kit:task-list-item-checked-p task)
                   (char= (cl-markdown-kit:task-list-item-original-marker-character
                           task)
                          #\X)
                   (string= (cl-parser-kit:span-source marker) source)
                   (string= (block-parser-test-span-text marker) "[X]")
                   (= (cl-parser-kit:span-start marker) 2)
                   (= (cl-parser-kit:span-end marker) 5))
              :to-be-truthy)))

  (it "handles tab stops and partial indentation removal with source boundaries"
    (let* ((tab-line
             (block-parser-test-line (format nil "~Cword" #\Tab)))
           (partial (cl-markdown-kit::%bp-strip-columns tab-line 1))
           (full (cl-markdown-kit::%bp-strip-columns tab-line 4))
           (quote-line (block-parser-test-line "> text"))
           (quote-indent
             (multiple-value-list
              (cl-markdown-kit::%bp-leading-indent
               (format nil "~CX" #\Tab) 4))))
      (expect (and (string= (cl-markdown-kit::%block-line-text partial)
                            (format nil "   word"))
                   (equal (coerce
                           (cl-markdown-kit::%block-line-boundaries partial)
                           'list)
                          '(0 0 0 1 2 3 4 5))
                   (string= (cl-markdown-kit::%block-line-text full) "word")
                   (= (cl-markdown-kit::%bp-indent-columns
                       (format nil "~CX" #\Tab))
                      4)
                   (= (cl-markdown-kit::%bp-whitespace-width
                       (format nil "~C " #\Tab) 0 2 2)
                      3)
                   (equal quote-indent '(1 4))
                   (cl-markdown-kit::%bp-prefix-p "Ab" "ab" 0 t)
                   (string= (cl-markdown-kit::%bp-casefold-label "ASCII" nil)
                            "ascii")
                   (string= (cl-markdown-kit::%bp-casefold-label "ASCII" #'identity)
                            "ASCII"))
              :to-be-truthy)
      (let ((cl-markdown-kit::*block-lazy-paragraph-cache* nil))
        (expect (and (cl-markdown-kit::%bp-lazy-paragraph-open-p quote-line)
                     (not (cl-markdown-kit::%bp-lazy-paragraph-open-p
                           (block-parser-test-line ">"))))
                :to-be-truthy))))

  (it "parses escaped link delimiters and rejects malformed definitions"
    (let* ((angle
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-destination
               (format nil "<a~C>b>" #\\) 0)))
           (escaped-space
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-destination
               (format nil "/a~C b" #\\) 0)))
           (balanced
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-destination "/a(b)" 0)))
           (unmatched-close
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-destination "/a)" 0)))
           (invalid-angle
             (cl-markdown-kit::%bp-parse-destination "<a<b>" 0))
           (invalid-newline
             (cl-markdown-kit::%bp-parse-destination
              (format nil "<a~%b>") 0))
           (unterminated-angle
             (cl-markdown-kit::%bp-parse-destination "<" 0))
           (invalid-depth
             (cl-markdown-kit::%bp-parse-destination "/a(b" 0))
           (empty-destination
             (cl-markdown-kit::%bp-parse-destination "" 0))
           (escaped-title
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-title
               "\"escaped \\\" title\"" 0)))
           (unfinished-title
             (cl-markdown-kit::%bp-parse-title "\"unfinished" 0))
           (empty-title (cl-markdown-kit::%bp-parse-title "" 0))
           (ordinary-title (cl-markdown-kit::%bp-parse-title "plain" 0))
           (valid-definition
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition
               (block-parser-test-line
                (format nil "[angle]: <a~C>b> \"title\"" #\\)))))
           (bad-tail
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[bad]: /target extra")))
           (nested-label
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[a[b]: /target")))
           (ordinary-definition
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "ordinary")))
           (footnote-link
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[^note]: /target")))
           (empty-label
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[]: /target")))
           (multiline-tail
             (cl-markdown-kit::%bp-link-definition
              (cl-markdown-kit::%bp-join-lines
               (cl-markdown-kit::%bp-split-lines
                (format nil "[tail]: /target~%continued")))))
           (multi-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[multi]:~%  /target")))
           (multi-definition
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at multi-lines 0)))
           (title-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[title]: /url~%  \"escaped ~C\" title\"" #\\)))
           (title-definition
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at title-lines 0)))
           (paren-title-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[paren]: /url~%  (open~%  close)")))
           (paren-title
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at paren-title-lines 0)))
           (incomplete-title-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[unfinished]: /url \"open~%  close\"")))
           (incomplete-title
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at
               incomplete-title-lines 0)))
           (broken-label-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[broken~%label")))
           (blank-label-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[broken~%~%label"))))
      (expect (and (string= (first angle) "a>b")
                   (= (second angle) 6)
                   (string= (first escaped-space) "/a b")
                   (string= (first balanced) "/a(b)")
                   (string= (first unmatched-close) "/a")
                   (null invalid-angle)
                   (null invalid-newline)
                   (null unterminated-angle)
                   (null invalid-depth)
                   (null empty-destination)
                   (string= (first escaped-title) "escaped \" title")
                   (null unfinished-title)
                   (null empty-title)
                   (null ordinary-title)
                   (string= (third valid-definition) "a>b")
                   (string= (fourth valid-definition) "title")
                   (null bad-tail)
                   (null nested-label)
                   (null ordinary-definition)
                   (null footnote-link)
                   (null empty-label)
                   (string= multiline-tail "tail")
                   (first multi-definition)
                   (string= (third (second multi-definition)) "/target")
                   (string= (fourth (second title-definition))
                            "escaped \" title")
                   (string= (fourth (second paren-title))
                            (format nil "open~%  close"))
                   (string= (fourth (second incomplete-title))
                            (format nil "open~%  close"))
                   (null (cl-markdown-kit::%bp-link-label-final-line
                          broken-label-lines 0))
                   (null (cl-markdown-kit::%bp-link-label-final-line
                          blank-label-lines 0)))
              :to-be-truthy)
      (expect (and (= (cl-markdown-kit::%bp-link-title-close-line
                       title-lines 1 3 #\")
                      1)
                   (null (cl-markdown-kit::%bp-link-title-close-line
                          title-lines 1 3 #\')))
              :to-be-truthy)))

  (it "validates HTML tag attributes and closing-tag boundaries"
    (let* ((valid "<x-tag _id:part=bare title=\"x>y\" data=raw />")
           (valid-name (multiple-value-list
                        (cl-markdown-kit::%bp-tag-name-at valid 0)))
           (closing "</x-tag>")
           (closing-name (multiple-value-list
                          (cl-markdown-kit::%bp-tag-name-at closing 0)))
           (invalid-tags '("<x-tag 1bad=x>"
                           "<x-tag attr"
                           "<x-tag attr="
                           "<x-tag attr=>"
                           "<x-tag attr=\"unterminated>"
                           "<x-tag /x>"))
           (invalid-ends
             (mapcar
              (lambda (text)
                (multiple-value-bind (name name-end)
                    (cl-markdown-kit::%bp-tag-name-at text 0)
                  (and name
                       (cl-markdown-kit::%bp-html-type-seven-tag-end
                        text 0 name-end))))
              invalid-tags))
           (html-type
             (multiple-value-list
              (cl-markdown-kit::%bp-html-block-type
               (block-parser-test-line valid))))
           (multiline-destination
             (cl-markdown-kit::%bp-link-definition
              (cl-markdown-kit::%bp-join-lines
               (cl-markdown-kit::%bp-split-lines
                (format nil "[link]: /target~%continued")))))
           (empty-tag (cl-markdown-kit::%bp-tag-name-at "<" 0))
           (invalid-tag (cl-markdown-kit::%bp-tag-name-at "<1tag>" 0))
           (plain-tag (cl-markdown-kit::%bp-tag-name-at "ordinary" 0)))
      (let ((name-end (second valid-name)))
        (expect (and (string= (first valid-name) "x-tag")
                     (= (cl-markdown-kit::%bp-html-type-seven-tag-end
                         valid 0 name-end)
                        (length valid))
                     (equal invalid-ends (make-list (length invalid-tags)))
                     (string= (first closing-name) "x-tag")
                     (= (cl-markdown-kit::%bp-html-type-seven-tag-end
                         closing 0 (second closing-name))
                        (length closing))
                     (null (cl-markdown-kit::%bp-html-type-seven-tag-end
                            "</x-tag extra>" 0 7))
                     (= (first html-type) 7)
                     (string= multiline-destination "link")
                     (null (first empty-tag))
                     (null (first invalid-tag))
                     (null (first plain-tag)))
                :to-be-truthy))
      (expect (and (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "</script >") "</script" 1)
                   (not (cl-markdown-kit::%bp-html-terminator-p
                         (block-parser-test-line "</script/x>")
                         "</script" 1))
                   (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "data ]]>") nil 5)
                   (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "anything") "" 7)
                   (not (cl-markdown-kit::%bp-html-terminator-p
                         (block-parser-test-line "</script") "</script" 1))
                   (not (cl-markdown-kit::%bp-html-terminator-p
                         (block-parser-test-line "</script ") "</script" 1))
                   (not (cl-markdown-kit::%bp-html-terminator-p
                         (block-parser-test-line "</script x>") "</script" 1)))
              :to-be-truthy)))

  (it "covers block-helper rejection paths and validates direct depth limits"
    (let* ((source (cl-markdown-kit:make-source-buffer "ordinary"))
           (line (block-parser-test-line "ordinary"))
           (lines (cl-markdown-kit::%bp-split-lines "ordinary"))
           (blank-code-lines (cl-markdown-kit::%bp-split-lines "    "))
           (table-lines (vector (block-parser-test-line "a|b")
                                (block-parser-test-line "-"))))
      (labels ((invalid-depth-p (value)
                 (handler-case
                     (progn
                       (cl-markdown-kit::parse-blocks
                        source :max-nesting-depth value)
                       nil)
                   (type-error () t))))
        (expect (and (null (cl-markdown-kit::%bp-parse-fenced-code
                            source lines 0 ""))
                     (null (cl-markdown-kit::%bp-parse-indented-code
                            source lines 0 ""))
                     (null (cl-markdown-kit::%bp-parse-indented-code
                            source blank-code-lines 0 ""))
                     (null (cl-markdown-kit::%bp-parse-html-block
                            source lines 0 ""))
                     (null (cl-markdown-kit::%bp-parse-list
                            source lines 0 "" 0 256))
                     (null (cl-markdown-kit::%bp-parse-footnote-definition
                            source lines 0 "" 0 256))
                     (null (cl-markdown-kit::%bp-parse-table
                            source table-lines 0 ""
                            (aref table-lines 0)))
                     (null (cl-markdown-kit::%bp-list-marker-same-level-p
                            line :ordered #\. 0))
                     (not (cl-markdown-kit::%bp-item-continuation-p
                           (block-parser-test-line " ") 2 t))
                     (cl-markdown-kit::%bp-item-continuation-p
                      (block-parser-test-line "lazy") 2 t)
                     (null (cl-markdown-kit::%bp-task-marker
                            (block-parser-test-line "ordinary")))
                     (string= (cl-markdown-kit::%block-line-text
                               (third (multiple-value-list
                                       (cl-markdown-kit::%bp-task-marker
                                        (block-parser-test-line "[X]")))))
                              "")
                     (invalid-depth-p 0)
                     (invalid-depth-p "deep")
                     (null (cl-markdown-kit::%bp-front-matter
                            source #()))
                     (null (cl-markdown-kit::%bp-front-matter
                            source (vector line))))
                :to-be-truthy))))

  (it "covers malformed definitions and parser boundary decisions"
    (let* ((short-label
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[")))
           (missing-colon
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[label]x")))
           (no-label-tail
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[label]")))
           (long-label
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line
               (concatenate 'string "[" (make-string 1000 :initial-element #\a)
                            "]: /target"))))
           (paren-title-lines
             (cl-markdown-kit::%bp-split-lines
              (format nil "[paren]: /url (open~%  middle~%  close)")))
           (paren-title
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at paren-title-lines 0)))
           (reparse-failure
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at
               (cl-markdown-kit::%bp-split-lines
                (format nil "[bad]: /url \"open~%  middle~%  close\" trailing"))
               0)))
           (escaped-footnote
             (multiple-value-list
              (cl-markdown-kit::%bp-footnote-start
               (block-parser-test-line
                (format nil "[^escaped~C]label]: body" #\\)))))
           (empty-footnote
             (cl-markdown-kit::%bp-footnote-start
              (block-parser-test-line "[^]: body")))
           (missing-footnote-colon
             (cl-markdown-kit::%bp-footnote-start
              (block-parser-test-line "[^label] body")))
           (dash-setext
             (multiple-value-list
              (cl-markdown-kit::%bp-setext-level
               (block-parser-test-line "-"))))
           (blank-setext
             (cl-markdown-kit::%bp-setext-level
              (block-parser-test-line "   ")))
           (plain-fence
             (cl-markdown-kit::%bp-fence-start
              (block-parser-test-line "ordinary")))
           (empty-fence
             (cl-markdown-kit::%bp-fence-start
              (block-parser-test-line "")))
           (empty-heading
             (cl-markdown-kit::%bp-atx-heading
              (block-parser-test-line "")))
           (empty-title-opener
             (cl-markdown-kit::%bp-link-title-opener
              (block-parser-test-line "")))
           (empty-tag-name (cl-markdown-kit::%bp-tag-name-at "" 0))
           (closing-script
             (cl-markdown-kit::%bp-html-block-type
              (block-parser-test-line "</script>")))
           (closing-empty-tag
             (cl-markdown-kit::%bp-html-type-seven-tag-end "" 0 0))
           (lazy-empty-marker
             (let ((cl-markdown-kit::*block-lazy-paragraph-cache* nil))
               (cl-markdown-kit::%bp-lazy-paragraph-open-p
                (block-parser-test-line "-"))))
           (list-document
             (block-parser-test-document
              (block-parser-test-result
               (format nil "- first~%  # nested"))))
           (footnote-document
             (block-parser-test-document
              (block-parser-test-result
               (format nil "[^note]: first~%    code~%# outside")))))
      (expect (and (null short-label)
                   (null missing-colon)
                   (null no-label-tail)
                   (null long-label)
                   (first paren-title)
                   (string= (fourth (second paren-title))
                            (format nil "open~%  middle~%  close"))
                   (null (first reparse-failure))
                   (string= (first escaped-footnote) "escaped]label")
                   (null empty-footnote)
                   (null missing-footnote-colon)
                   (= (first dash-setext) 2)
                   (null blank-setext)
                   (null plain-fence)
                   (null empty-fence)
                   (null empty-heading)
                   (null empty-title-opener)
                   (null (first empty-tag-name))
                   (null closing-script)
                   (null closing-empty-tag)
                   (not lazy-empty-marker)
                   (= (length (block-parser-test-nodes-of-type
                               list-document #'cl-markdown-kit:list-node-p))
                      1)
                   (= (length (block-parser-test-nodes-of-type
                               footnote-document
                               #'cl-markdown-kit:footnote-definition-p))
                      1)
                   (null (cl-markdown-kit::%bp-unicode-casefold-function nil))
                   (null (cl-markdown-kit::%bp-unicode-casefold-function
                          (find-package '#:cl-user)))
                   (null (cl-markdown-kit::%bp-link-title-opener
                          (block-parser-test-line "ordinary")))
                   (not (cl-markdown-kit::%bp-html-terminator-p
                         (block-parser-test-line "ordinary") nil 0)))
              :to-be-truthy)))

  (it "keeps lazy list and duplicate footnote continuation source-backed"
    (let* ((list-source
             (format nil "- first~%lazy continuation~%# outside~%~%- [X]"))
           (list-result (block-parser-test-result list-source))
           (list-document (block-parser-test-document list-result))
           (lists (block-parser-test-nodes-of-type
                   list-document #'cl-markdown-kit:list-node-p))
           (tasks (block-parser-test-nodes-of-type
                   list-document #'cl-markdown-kit:task-list-item-p))
           (first-item (first (cl-markdown-kit:list-node-items (first lists))))
           (first-paragraph (block-parser-test-only-child first-item))
           (footnote-source
             (format nil "[^note]: first~%continued~%~%    code~%[^NOTE]: second"))
           (footnote-result (block-parser-test-result footnote-source))
           (footnotes (block-parser-test-nodes-of-type
                       (block-parser-test-document footnote-result)
                       #'cl-markdown-kit:footnote-definition-p))
           (index (cl-markdown-kit::block-parse-result-reference-index
                   footnote-result)))
      (expect (and (= (length lists) 2)
                   (= (length tasks) 1)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     list-result first-paragraph)))
                            (format nil "first~%lazy continuation"))
                   (= (length footnotes) 2)
                   (string= (cl-markdown-kit:footnote-definition-raw-label
                             (first footnotes))
                            "note")
                   (eq (gethash (cons :footnote "note") index)
                       (first footnotes)))
              :to-be-truthy)))

  (it "checks T-004 block span properties for every CommonMark and GFM example"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm))
           (checked 0))
      (dolist (example examples)
        (let* ((source (spec-example-markdown example))
               (document
                 (block-parser-test-document
                  (block-parser-test-result source))))
          (expect (block-parser-test-ranges-valid-p document source)
                  :to-be-truthy)
          (incf checked)))
      (expect (= checked (+ (length commonmark) (length gfm))) :to-be-truthy)
      (format t "T-004 block range properties: ~D examples (~D CommonMark, ~D GFM)~%"
              checked (length commonmark) (length gfm)))))
