(in-package #:cl-markdown-kit/test)

(defun block-parser-test-result (source &key (front-matter t) (gfm t))
  (cl-markdown-kit::parse-blocks
   (cl-markdown-kit:make-source-buffer source)
   :front-matter front-matter
   :gfm gfm))

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

(defun block-parser-test-example-source (example)
  (substitute #\Tab #\→ (spec-example-markdown example)))

(defun block-parser-test-gfm-result (id)
  (block-parser-test-result
   (spec-example-markdown (block-parser-test-gfm-example id))))

(defun block-parser-test-nodes-of-type (root predicate)
  (let ((nodes nil))
    (cl-markdown-kit:walk-node-tree root
     (lambda (frame)
       (let ((node (cl-markdown-kit:node-tree-frame-node frame))
             (phase (cl-markdown-kit:node-tree-frame-phase frame)))
         (when (and (eq phase :enter) (funcall predicate node))
           (push node nodes))
         :descend)))
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

(defun block-parser-test-data-path (name)
  (merge-pathnames (format nil "t/data/~A" name)
                   (asdf:system-source-directory :cl-markdown-kit)))

(defun block-parser-test-read-data (name)
  (with-open-file (stream (block-parser-test-data-path name)
                          :direction :input :external-format :utf-8)
    (let* ((*read-eval* nil)
           (eof (gensym "EOF"))
           (data (read stream nil eof)))
      (when (eq data eof)
        (error "The block structure data file is empty: ~A" name))
      (unless (eq (read stream nil eof) eof)
        (error "The block structure data file has multiple forms: ~A" name))
      data)))

(declaim (ftype function block-parser-test-node-sequence-signature
                  block-parser-test-node-signature))

(defun block-parser-test-node-sequence-signature (nodes)
  (let* ((nodes (remove-if (lambda (node)
                             (typep node
                                    'cl-markdown-kit:link-reference-definition))
                           nodes))
         (count (length nodes)))
    (loop for node in nodes
          for index from 0
          collect
          (block-parser-test-node-signature
           node
           (cond ((= count 1) :only)
                 ((zerop index) :first)
                 ((= index (1- count)) :last)
                 (t :middle))))))

(defun block-parser-test-node-form (kind attributes children)
  (append (list kind attributes) children))

(defun block-parser-test-node-signature (node &optional placement)
  (typecase node
    (cl-markdown-kit:document
     (block-parser-test-node-form
      :document nil
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:document-children node))))
    (cl-markdown-kit:block-quote
     (block-parser-test-node-form
      :block-quote nil
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:block-quote-children node))))
    (cl-markdown-kit:list-node
     (block-parser-test-node-form
      :list
      (list :kind (cl-markdown-kit:list-node-kind node)
            :start (cl-markdown-kit:list-node-start-number node)
            :tight (cl-markdown-kit:list-node-tight-p node)
            :delimiter
            (let ((delimiter (cl-markdown-kit:list-node-delimiter-character node)))
              (cond ((null delimiter) nil)
                    ((char= delimiter #\.) :period)
                    ((char= delimiter #\)) :paren)
                    (t delimiter))))
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:list-node-items node))))
    (cl-markdown-kit:task-list-item
     (block-parser-test-node-form
      :item (list :task-checked (cl-markdown-kit:task-list-item-checked-p node))
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:list-item-children node))))
    (cl-markdown-kit:list-item
     (block-parser-test-node-form
      :item nil
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:list-item-children node))))
    (cl-markdown-kit:paragraph
     (block-parser-test-node-form :paragraph (list :placement placement) nil))
    (cl-markdown-kit:heading
     (block-parser-test-node-form
      :heading (list :level (cl-markdown-kit:heading-level node)) nil))
    (cl-markdown-kit:thematic-break
     (block-parser-test-node-form :thematic-break nil nil))
    (cl-markdown-kit:code-block
     (block-parser-test-node-form
      :code-block
      (list :info (or (cl-markdown-kit:code-block-info-string node) "")
            :literal (cl-markdown-kit:code-block-body node))
      nil))
    (cl-markdown-kit:html-block
     (block-parser-test-node-form
      :html-block (list :literal (cl-markdown-kit:html-block-body node)) nil))
    (cl-markdown-kit:table
     (let* ((rows (cl-markdown-kit:table-rows node))
            (alignments (cl-markdown-kit:table-alignments node)))
       (block-parser-test-node-form
        :table
        (list :columns (length alignments)
              :alignments alignments
              :row-cells (mapcar
                          (lambda (row)
                            (length (cl-markdown-kit:table-row-cells row)))
                          rows))
        (mapcar #'block-parser-test-node-signature rows))))
    (cl-markdown-kit:table-row
     (block-parser-test-node-form
      :row (list :header (cl-markdown-kit:table-row-header-row-p node))
      (mapcar #'block-parser-test-node-signature
              (cl-markdown-kit:table-row-cells node))))
    (cl-markdown-kit:table-cell
     (block-parser-test-node-form :cell nil nil))
    (cl-markdown-kit:footnote-definition
     (block-parser-test-node-form
      :footnote-definition
      (list :label (cl-markdown-kit:footnote-definition-raw-label node))
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:footnote-definition-children node))))
    (cl-markdown-kit:front-matter
     (block-parser-test-node-form :front-matter nil nil))
    (cl-markdown-kit:link-reference-definition
     (block-parser-test-node-form
      :link-reference-definition
      (list :label (cl-markdown-kit:link-reference-definition-raw-label node)
            :destination (cl-markdown-kit:link-reference-definition-destination node)
            :title (cl-markdown-kit:link-reference-definition-title node))
      nil))
    (otherwise
     (error "Unsupported block node in structural signature: ~S" node))))

(defun block-parser-test-short-value (value)
  (if (and (stringp value) (> (length value) 120))
      (list :string-length (length value)
            :prefix (subseq value 0 80)
            :suffix (subseq value (- (length value) 24)))
      value))

(defun block-parser-test-first-difference (expected actual &optional path)
  (cond
    ((equal expected actual) nil)
    ((and (consp expected) (consp actual))
     (loop for left in expected
           for right in actual
           for index from 0
           for difference =
             (block-parser-test-first-difference
              left right (cons index path))
           when difference do (return-from block-parser-test-first-difference
                                difference))
     (unless (= (length expected) (length actual))
       (list (reverse path)
             (list :length (length expected))
             (list :length (length actual)))))
    (t
     (list (reverse path)
           (block-parser-test-short-value expected)
           (block-parser-test-short-value actual)))))

(defun block-parser-test-structure-signature (example)
  (handler-case
      (values
       (block-parser-test-node-signature
        (block-parser-test-document
         (block-parser-test-result (spec-example-markdown example)
                                   :front-matter nil)))
       nil)
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun block-parser-test-footnote-node-p (signature)
  (and (consp signature)
       (eq (first signature) :footnote-definition)))

(defun block-parser-test-footnote-label (signature)
  (getf (second signature) :label))

(defun block-parser-test-document-without-footnotes (signature)
  (destructuring-bind (kind attributes &rest children) signature
    (block-parser-test-node-form
     kind attributes
     (remove-if #'block-parser-test-footnote-node-p children))))

(defun block-parser-test-gfm-footnote-difference (expected actual)
  ;; cmark XML orders used definitions by reference and omits unused definitions.
  (let* ((expected-footnotes
           (remove-if-not #'block-parser-test-footnote-node-p (cddr expected)))
         (actual-footnotes
           (remove-if-not #'block-parser-test-footnote-node-p (cddr actual)))
         (document-difference
           (block-parser-test-first-difference
            (block-parser-test-document-without-footnotes expected)
            (block-parser-test-document-without-footnotes actual))))
    (or document-difference
        (loop for expected-footnote in expected-footnotes
              do (let* ((label
                          (block-parser-test-footnote-label expected-footnote))
                        (actual-footnote
                          (find label actual-footnotes
                                :key #'block-parser-test-footnote-label
                                :test #'string=)))
                   (unless actual-footnote
                     (return (list :missing-footnote label)))
                   (let ((difference
                           (block-parser-test-first-difference
                            expected-footnote actual-footnote)))
                     (when difference
                       (return (list :footnote label difference)))))))))

(defun block-parser-test-structure-difference
    (expected actual error-message &optional example-id)
  (cond
    (error-message (list :parse-error error-message))
    ((equal example-id "gfm-0023")
     (block-parser-test-gfm-footnote-difference expected actual))
    (t (block-parser-test-first-difference expected actual))))

(describe "Leaf block parser"
  (it "compares every CommonMark and GFM block structure with pinned cmark output"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm))
           (fixtures (block-parser-test-read-data "block-structure-fixtures.lisp"))
           (exclusions (block-parser-test-read-data
                        "block-structure-exclusions.lisp"))
           (known-ids (mapcar #'spec-example-id examples))
           (fixture-ids (mapcar #'first fixtures))
           (fixture-index (make-hash-table :test 'equal))
           (matches 0)
           (failures nil))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (length fixtures) :to-equal 682)
      (expect fixture-ids :to-equal known-ids)
      (expect exclusions :to-equal nil)
      (dolist (entry fixtures)
        (setf (gethash (first entry) fixture-index) (second entry)))
      (dolist (example examples)
        (let* ((id (spec-example-id example))
               (expected (gethash id fixture-index)))
          (multiple-value-bind (actual error-message)
              (block-parser-test-structure-signature example)
            (let ((difference
                    (block-parser-test-structure-difference
                     expected actual error-message id)))
              (cond
                ((null difference)
                 (incf matches))
                (t
                 (push (list id difference) failures)))))))
      (dolist (failure (nreverse failures))
        (format t "block structure mismatch ~A: ~S~%"
                (first failure) (second failure)))
      (format t "Block structures: ~D CommonMark + ~D GFM; ~D match, 0 exclusions.~%"
              (length commonmark) (length gfm) matches)
      (expect failures :to-equal nil)
      (expect matches :to-equal 682)))

  (it "parses thematic breaks, blank separators, and excludes line endings from spans"
    (let* ((source (format nil "***~%~%---~%"))
           (document
             (block-parser-test-document
              (block-parser-test-result source)))
           (children (cl-markdown-kit:document-children document)))
      (expect (length children) :to-equal 2)
      (expect (every #'(lambda (node) (typep node 'cl-markdown-kit:thematic-break)) children)
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
      (expect (and (typep atx 'cl-markdown-kit:heading)
                   (= (cl-markdown-kit:heading-level atx) 2)
                   (eq (cl-markdown-kit:heading-style atx) :atx)
                   (char= (cl-markdown-kit:heading-marker-character atx) #\#)
                   (= (cl-markdown-kit:heading-opening-marker-length atx) 2)
                   (= (cl-markdown-kit:heading-closing-marker-length atx) 3)
                   (block-parser-test-span-is atx 0 12))
              :to-be-truthy)
      (expect (and (typep setext 'cl-markdown-kit:heading)
                   (= (cl-markdown-kit:heading-level setext) 1)
                   (eq (cl-markdown-kit:heading-style setext) :setext)
                   (char= (cl-markdown-kit:heading-marker-character setext) #\=)
                   (block-parser-test-span-is setext 0 9))
              :to-be-truthy)))

  (it "normalizes CommonMark paragraph continuation and setext heading text"
    (let ((cases
            (list
             (list "commonmark-0049" (list (format nil "Foo~%***")))
             (list "commonmark-0070" (list (format nil "foo~%# bar")))
             (list "commonmark-0087" (list (format nil "Foo~%---")))
             (list "commonmark-0113" (list (format nil "Foo~%bar")))
             (list "commonmark-0223"
                   (list (format nil "aaa~%bbb~%ccc")))
             (list "commonmark-0238" (list (format nil "foo~%- bar")))
             (list "commonmark-0291"
                   (list (format nil "A paragraph~%with two lines.")))
             (list "commonmark-0312"
                   (list "a" "b" "c" (format nil "d~%- e")))
             (list "commonmark-0082"
                   (list (format nil "Foo *bar~%baz*")))
             (list "commonmark-0089" (list "Foo")))))
      (dolist (case cases)
        (let* ((example (block-parser-test-commonmark-example (first case)))
               (source (block-parser-test-example-source example))
               (result (block-parser-test-result source :front-matter nil))
               (document (block-parser-test-document result))
               (inline-fragments
                 (loop for inline-owner in
                       (block-parser-test-nodes-of-type
                        document
                        (lambda (node)
                          (or (typep node 'cl-markdown-kit:paragraph)
                              (typep node 'cl-markdown-kit:heading))))
                       append
                       (mapcar
                        #'cl-markdown-kit::inline-fragment-text
                        (block-parser-test-fragments-for result inline-owner)))))
          (expect inline-fragments :to-equal (second case))
          (expect (block-parser-test-ranges-valid-p document source)
                  :to-be-truthy)))))

  (it "removes closing ATX markers and preserves link definitions before paragraphs"
    (let* ((heading-example
             (block-parser-test-commonmark-example "commonmark-0079"))
           (heading-source (block-parser-test-example-source heading-example))
           (heading-result (block-parser-test-result
                            heading-source :front-matter nil))
           (heading-document (block-parser-test-document heading-result))
           (headings
             (block-parser-test-nodes-of-type
              heading-document #'(lambda (node) (typep node 'cl-markdown-kit:heading))))
           (reference-example
             (block-parser-test-commonmark-example "commonmark-0210"))
           (reference-source
             (block-parser-test-example-source reference-example))
           (reference-result
             (block-parser-test-result reference-source :front-matter nil))
           (reference-document (block-parser-test-document reference-result))
           (reference-children
             (cl-markdown-kit:document-children reference-document))
           (paragraph (second reference-children)))
      (expect (and (equal (mapcar
                           (lambda (heading)
                             (cl-markdown-kit::inline-fragment-text
                              (first (block-parser-test-fragments-for
                                      heading-result heading))))
                           headings)
                          '("" "" ""))
                   (block-parser-test-ranges-valid-p
                    heading-document heading-source))
              :to-be-truthy)
      (expect (and (= (length reference-children) 2)
                   (typep (first reference-children) 'cl-markdown-kit:link-reference-definition)
                   (typep paragraph 'cl-markdown-kit:paragraph)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     reference-result paragraph)))
                            "\"title\" ok")
                   (block-parser-test-ranges-valid-p
                    reference-document reference-source))
              :to-be-truthy)))

  (it "preserves tab-relative code indentation in list items"
    (let* ((example (block-parser-test-commonmark-example "commonmark-0007"))
           (source (block-parser-test-example-source example))
           (document (block-parser-test-document
                      (block-parser-test-result source :front-matter nil)))
           (code (first (block-parser-test-nodes-of-type
                         document #'(lambda (node) (typep node 'cl-markdown-kit:code-block))))))
      (expect (and (typep code 'cl-markdown-kit:code-block)
                   (string= (cl-markdown-kit:code-block-body code)
                            (format nil "  foo~%"))
                   (block-parser-test-ranges-valid-p document source))
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
                   (typep (second children) 'cl-markdown-kit:heading)
                   (= (length unclosed-children) 3)
                   (typep (first unclosed-children) 'cl-markdown-kit:thematic-break)
                   (typep (second unclosed-children) 'cl-markdown-kit:paragraph)
                   (typep (third unclosed-children) 'cl-markdown-kit:heading))
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
                   (typep paragraph 'cl-markdown-kit:paragraph)
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
      (expect (and (typep fenced 'cl-markdown-kit:code-block)
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
      (expect (and (typep indented 'cl-markdown-kit:code-block)
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
                   (typep code 'cl-markdown-kit:code-block)
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
          (expect (and (typep html 'cl-markdown-kit:html-block)
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
      (expect (and (typep self-closing 'cl-markdown-kit:html-block)
                   (= (cl-markdown-kit:html-block-kind self-closing) 7)
                   (typep inline-tag 'cl-markdown-kit:paragraph)
                   (typep bare-declaration 'cl-markdown-kit:paragraph)
                   (typep invalid-end 'cl-markdown-kit:html-block)
                   (= (cl-markdown-kit:html-block-kind invalid-end) 1)
                   (string= (cl-markdown-kit:html-block-body invalid-end)
                            invalid-end-source)
                   (= (length valid-end-children) 2)
                   (typep (first valid-end-children) 'cl-markdown-kit:html-block)
                   (typep (second valid-end-children) 'cl-markdown-kit:paragraph))
              :to-be-truthy)))

  (it "indexes source-ordered link reference definitions"
    (let* ((source (format nil "[Ref]: /target \"Title\"~%[ref]: /later~%~%plain~%"))
           (result (block-parser-test-result source))
           (document (block-parser-test-document result))
           (definition (first (cl-markdown-kit:document-children document)))
           (duplicate (second (cl-markdown-kit:document-children document)))
           (reference-index
             (cl-markdown-kit::block-parse-result-reference-index result)))
      (expect (and (typep definition 'cl-markdown-kit:link-reference-definition)
                   (typep duplicate 'cl-markdown-kit:link-reference-definition)
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

  (it "decodes entities and backslash escapes in reference destinations and titles"
    (let* ((children
             (cl-markdown-kit:document-children
              (block-parser-test-document
               (block-parser-test-result
                "[ref]: /a\\+&amp;&#66; \"t\\+&quot;\""))))
           (definition (first children)))
      (expect (and (= (length children) 1)
                   (typep definition 'cl-markdown-kit:link-reference-definition)
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             definition)
                            "/a+&B")
                   (string= (cl-markdown-kit:link-reference-definition-title
                             definition)
                            "t+\""))
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
                   (every #'(lambda (node) (typep node 'cl-markdown-kit:link-reference-definition))
                          (subseq children 0 2))
                   (string= (cl-markdown-kit:link-reference-definition-title
                             (first children))
                            (format nil "title~%continued"))
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             (second children))
                            "/next")
                   (typep (third children) 'cl-markdown-kit:paragraph)
                   (eq (gethash "multi" index) (first children))
                   (eq (gethash "next" index) (second children)))
              :to-be-truthy)
      (expect (and (= (length destination-children) 2)
                   (typep (first destination-children) 'cl-markdown-kit:link-reference-definition)
                   (string= (cl-markdown-kit:link-reference-definition-destination
                             (first destination-children))
                            "/destination")
                   (typep (second destination-children) 'cl-markdown-kit:paragraph))
              :to-be-truthy)))

  (it "accepts the block nesting limit and signals one level beyond it"
    (labels ((nested-quote-source (depth)
               (with-output-to-string (output)
                 (loop repeat depth do (write-string "> " output))
                 (write-string "nested" output)))
             (parse-depth-condition (depth limit)
               (handler-case
                   (progn
                     (cl-markdown-kit::parse-blocks
                      (cl-markdown-kit:make-source-buffer
                       (nested-quote-source depth))
                      :max-nesting-depth limit)
                     nil)
                 (cl-markdown-kit:markdown-nesting-depth-exceeded
                     (condition)
                   condition)))
             (quote-depth (node)
               (loop with depth = 0
                     while (typep node 'cl-markdown-kit:block-quote)
                     do (incf depth)
                        (setf node
                              (first
                               (cl-markdown-kit:block-quote-children node)))
                     finally (return depth)))
             (depth-condition-matches-p (condition limit actual)
               (and condition
                    (eq (cl-markdown-kit:markdown-resource-limit-error-limit-name
                         condition)
                        :nesting-depth)
                    (= (cl-markdown-kit:markdown-resource-limit-error-limit
                        condition)
                       limit)
                    (= (cl-markdown-kit:markdown-resource-limit-error-actual
                        condition)
                       actual))))
      (let* ((limit 8)
             (accepted-document
               (block-parser-test-document
                (cl-markdown-kit::parse-blocks
                 (cl-markdown-kit:make-source-buffer
                  (nested-quote-source limit))
                 :max-nesting-depth limit)))
             (condition (parse-depth-condition (1+ limit) limit)))
        (expect (= (quote-depth
                    (first
                     (cl-markdown-kit:document-children accepted-document)))
                   limit)
                :to-be-truthy)
        (expect (depth-condition-matches-p condition limit (1+ limit))
                :to-be-truthy))))

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
      (expect (and (typep quoted-block 'cl-markdown-kit:block-quote)
                   (= (length quoted-children) 2)
                   (typep (first quoted-children) 'cl-markdown-kit:heading)
                   (typep (second quoted-children) 'cl-markdown-kit:paragraph))
              :to-be-truthy)
      (expect (and (= (length lazy-children) 2)
                   (typep (first lazy-children) 'cl-markdown-kit:heading)
                   (typep (second lazy-children) 'cl-markdown-kit:paragraph)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     lazy-result (second lazy-children))))
                            (format nil "bar~%baz")))
              :to-be-truthy)
      (expect (and (= (length separate-quotes) 2)
                   (every #'(lambda (node) (typep node 'cl-markdown-kit:block-quote)) separate-quotes)
                   (every (lambda (quote)
                            (and (= (length (cl-markdown-kit:block-quote-children
                                            quote))
                                    1)
                                 (typep (first (cl-markdown-kit:block-quote-children
                                          quote)) 'cl-markdown-kit:paragraph)))
                          separate-quotes))
              :to-be-truthy)
      (expect (and (typep outer-quote 'cl-markdown-kit:block-quote)
                   (typep (block-parser-test-only-child outer-quote) 'cl-markdown-kit:block-quote)
                   (typep (block-parser-test-only-child
                     (block-parser-test-only-child outer-quote)) 'cl-markdown-kit:block-quote)
                   (let* ((inner-quote
                            (block-parser-test-only-child
                             (block-parser-test-only-child outer-quote)))
                          (paragraph
                            (block-parser-test-only-child inner-quote))
                          (result nested-result))
                     (and (typep paragraph 'cl-markdown-kit:paragraph)
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
              nested-loose-document #'(lambda (node) (typep node 'cl-markdown-kit:list-node))))
           (loose-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0325"))))
           (loose-document (block-parser-test-document loose-result))
           (loose-lists
             (block-parser-test-nodes-of-type
              loose-document #'(lambda (node) (typep node 'cl-markdown-kit:list-node))))
           (separate-items-result
             (block-parser-test-result
              (spec-example-markdown
               (block-parser-test-commonmark-example "commonmark-0326"))))
           (separate-items-document
             (block-parser-test-document separate-items-result))
           (separate-items-lists
             (block-parser-test-nodes-of-type
              separate-items-document #'(lambda (node) (typep node 'cl-markdown-kit:list-node)))))
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
      (expect (and (typep alignment-table 'cl-markdown-kit:table)
                   (equal (cl-markdown-kit:table-alignments alignment-table)
                          '(:left nil :center nil :right)))
              :to-be-truthy)
      (expect (and (typep pipe-table 'cl-markdown-kit:table)
                   (= (length rows) 5)
                   (every (lambda (row)
                            (= (length (cl-markdown-kit:table-row-cells row)) 2))
                          rows)
                   (string= escaped-text "Escaped pipes are |okay|.")
                   (search "`|code| is okay`" code-text)
                   (= (length (cl-markdown-kit:table-row-cells
                               (second unmatched-rows)))
                      2)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     pipe-result (second code-cells))))
                            ""))
              :to-be-truthy)))

  (it "removes GFM table pipe escapes inside and outside code spans"
    (let* ((result (block-parser-test-gfm-result "gfm-0011"))
           (table (first
                   (cl-markdown-kit:document-children
                    (block-parser-test-document result))))
           (row (fifth (cl-markdown-kit:table-rows table)))
           (cells (cl-markdown-kit:table-row-cells row))
           (cell-texts
             (mapcar
              (lambda (cell)
                (cl-markdown-kit::inline-fragment-text
                 (first (block-parser-test-fragments-for result cell))))
              cells)))
      (expect cell-texts :to-equal (list "|" "`|`"))))

  (it "disables GFM tables, task markers, and footnote definitions on request"
    (let* ((table-source (format nil "| a | b |~%| --- | --- |"))
           (table-document
             (block-parser-test-document
              (block-parser-test-result table-source :gfm nil)))
           (task-document
             (block-parser-test-document
              (block-parser-test-result "- [ ] todo" :gfm nil)))
           (task-list
             (block-parser-test-only-child task-document))
           (footnote-document
             (block-parser-test-document
              (block-parser-test-result "[^note]: body" :gfm nil)))
           (gfm-footnote-document
             (block-parser-test-document
              (block-parser-test-result "[^note]: body")))
           (table-default-document
             (block-parser-test-document
              (block-parser-test-result table-source))))
      (expect (and (= (length (cl-markdown-kit:document-children table-document))
                      1)
                   (typep (first (cl-markdown-kit:document-children table-document)) 'cl-markdown-kit:paragraph)
                   (typep (first (cl-markdown-kit:list-node-items task-list)) 'cl-markdown-kit:list-item)
                   (not (typep (first (cl-markdown-kit:list-node-items task-list)) 'cl-markdown-kit:task-list-item))
                   (typep (block-parser-test-only-child footnote-document) 'cl-markdown-kit:paragraph)
                   (typep (block-parser-test-only-child gfm-footnote-document) 'cl-markdown-kit:footnote-definition)
                   (typep (block-parser-test-only-child table-default-document) 'cl-markdown-kit:table))
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
                   (typep paragraph 'cl-markdown-kit:paragraph)
                   (typep table 'cl-markdown-kit:table)
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

  (it "accepts a pipe-less body row in a single-column GFM table"
    (let* ((source (format nil "| header |~%| --- |~%body~%"))
           (result (block-parser-test-result source))
           (table (block-parser-test-only-child
                   (block-parser-test-document result)))
           (rows (cl-markdown-kit:table-rows table))
           (cells (cl-markdown-kit:table-row-cells (second rows))))
      (expect (and (typep table 'cl-markdown-kit:table)
                   (= (length rows) 2)
                   (= (length cells) 1)
                   (string= (cl-markdown-kit::inline-fragment-text
                             (first (block-parser-test-fragments-for
                                     result (first cells))))
                            "body"))
              :to-be-truthy)))

  (it "keeps table cells and source text in the GFM interop fixture"
    (let* ((result (block-parser-test-gfm-result "gfm-0027"))
           (table (block-parser-test-only-child
                   (block-parser-test-document result)))
           (rows (cl-markdown-kit:table-rows table))
           (body-cells (cl-markdown-kit:table-row-cells (second rows))))
      (expect (and (typep table 'cl-markdown-kit:table)
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
              document #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition))))
           (labels (mapcar #'cl-markdown-kit:footnote-definition-raw-label
                           definitions))
           (unused (find "unused" definitions
                         :key #'cl-markdown-kit:footnote-definition-raw-label
                         :test #'string=))
           (unused-paragraph
             (first (cl-markdown-kit:footnote-definition-children unused)))
           (unused-fragment
             (first (block-parser-test-fragments-for result unused-paragraph)))
           (fragment-texts (block-parser-test-fragment-texts result)))
      (expect (equal labels
                     '("other-note" "codeblock-note" "1" "footnote" "unused"))
              :to-be-truthy)
      (expect (and (some (lambda (text) (search "[^1]" text)) fragment-texts)
                   (some (lambda (text) (search "[^footnote]" text))
                         fragment-texts)
                   (some (lambda (node)
                           (and (typep node 'cl-markdown-kit:footnote-definition)
                                (some #'(lambda (node) (typep node 'cl-markdown-kit:block-quote))
                                      (cl-markdown-kit:footnote-definition-children
                                       node))))
                         definitions))
              :to-be-truthy)
      (expect (equal (block-parser-test-node-signature unused)
                     '(:footnote-definition (:label "unused")
                       (:paragraph (:placement :only))))
              :to-be-truthy)
      (expect (and unused-fragment
                   (string= (cl-markdown-kit::inline-fragment-text
                             unused-fragment)
                            "This is unused."))
              :to-be-truthy)
      (expect (eq (gethash (cons :footnote "unused")
                           (cl-markdown-kit::block-parse-result-reference-index
                            result))
                  unused)
              :to-be-truthy)))

  (it "keeps repeated and escaped GFM footnote labels source-backed"
    (let* ((repeated-result (block-parser-test-gfm-result "gfm-0024"))
           (repeated-document
             (block-parser-test-document repeated-result))
           (repeated-definitions
             (block-parser-test-nodes-of-type
              repeated-document #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition))))
           (repeated-text (format nil "~{~A~^ ~}"
                                  (block-parser-test-fragment-texts
                                   repeated-result)))
           (escaped-result (block-parser-test-gfm-result "gfm-0025"))
           (escaped-definitions
             (block-parser-test-nodes-of-type
              (block-parser-test-document escaped-result)
              #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition)))))
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
                     document #'(lambda (node) (typep node 'cl-markdown-kit:task-list-item)))))
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
                                   #'(lambda (node) (typep node 'cl-markdown-kit:task-list-item))
                                   (block-parser-test-nodes-of-type
                                    document #'(lambda (node) (typep node 'cl-markdown-kit:list-item)))))
                          4))
                  :to-be-truthy))))
    (let* ((source "- [X] uppercase")
           (document (block-parser-test-document
                      (block-parser-test-result source)))
           (task (first (block-parser-test-nodes-of-type
                         document #'(lambda (node) (typep node 'cl-markdown-kit:task-list-item)))))
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
                   (cl-markdown-kit::%bp-prefix-p "Ab" "Ab" 0)
                   (string= (cl-markdown-kit::normalize-link-label "ASCII")
                            "ascii")
                   (string= (cl-markdown-kit::normalize-link-label "Ä")
                            "ä"))
              :to-be-truthy)))

  (it "parses escaped link delimiters and rejects malformed definitions"
    (let* ((angle
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-destination
               (format nil "<a~C>b>" #\\) 0)))
           (backslash-before-space
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
              (cl-markdown-kit::%pc-parse-title "\"escaped \\\" title\"" 0 (length "\"escaped \\\" title\"") :backslash-mode :any-character)))
           (unfinished-title
             (cl-markdown-kit::%pc-parse-title "\"unfinished" 0 (length "\"unfinished") :backslash-mode :any-character))
           (empty-title (cl-markdown-kit::%pc-parse-title "" 0 (length "") :backslash-mode :any-character))
           (ordinary-title (cl-markdown-kit::%pc-parse-title "plain" 0 (length "plain") :backslash-mode :any-character))
           (valid-definition
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition
               (block-parser-test-line
                (format nil "[angle]: <a~C>b> \"title\"" #\\)))))
           (bad-tail
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line "[bad]: /target extra")))
           (escaped-space-definition
             (cl-markdown-kit::%bp-link-definition
              (block-parser-test-line (format nil "[bad]: /a~C b" #\\))))
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
                   (string= (first backslash-before-space) "/a\\")
                   (= (second backslash-before-space) 3)
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
                   (null escaped-space-definition)
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
           (line (block-parser-test-line "ordinary")))
      (labels ((invalid-depth-p (value)
                 (handler-case
                     (progn
                       (cl-markdown-kit::parse-blocks
                        source :max-nesting-depth value)
                       nil)
                   (type-error () t)))
               (invalid-gfm-p (value)
                 (handler-case
                     (progn
                       (cl-markdown-kit::parse-blocks source :gfm value)
                       nil)
                   (type-error () t))))
        (expect (and (null (cl-markdown-kit::%bp-task-marker
                            (block-parser-test-line "ordinary")))
                     (string= (cl-markdown-kit::%block-line-text
                               (third (multiple-value-list
                                       (cl-markdown-kit::%bp-task-marker
                                        (block-parser-test-line "[X]")))))
                              "")
                     (invalid-depth-p 0)
                     (invalid-depth-p "deep")
                     (invalid-gfm-p :enabled)
                     (null (cl-markdown-kit::%bp-front-matter
                            source #()))
                     (null (cl-markdown-kit::%bp-front-matter
                            source (vector line))))
                :to-be-truthy))))

  (it "covers block parser helper boundaries and open frame transitions"
    (let* ((source (cl-markdown-kit:make-source-buffer "plain"))
           (plain-line (block-parser-test-line "plain"))
           (trimmed-empty-line
             (cl-markdown-kit::%bp-trim-line-end
              (block-parser-test-line "   ")))
           (decoded-references
             (cl-markdown-kit::%bp-decode-character-references
              "&amp;&#66;&#x42;&#X42;&#xZZ;&#x;&#;&; &amp"))
           (trailing-backslash
             (multiple-value-list
              (cl-markdown-kit::%bp-parse-destination
               (format nil "/a~C" #\\) 0)))
           (trailing-angle-backslash
             (cl-markdown-kit::%bp-parse-destination
              (format nil "<a~C" #\\) 0))
           (invalid-empty-line
             (cl-markdown-kit::%bp-link-definition-at
              (cl-markdown-kit::%bp-split-lines "") 0))
           (ordinary-definition
             (cl-markdown-kit::%bp-link-definition-at
              (cl-markdown-kit::%bp-split-lines "ordinary") 0))
           (footnote-definition
             (cl-markdown-kit::%bp-link-definition-at
              (cl-markdown-kit::%bp-split-lines "[^note]: /url") 0))
           (blank-reparse
             (cl-markdown-kit::%bp-link-definition-at
              (cl-markdown-kit::%bp-split-lines
               (format nil "[multi]:~%~%  /url"))
              0))
           (blank-title
             (cl-markdown-kit::%bp-link-definition-at
              (cl-markdown-kit::%bp-split-lines
               (format nil "[title]: /url \"open~%~%close\""))
              0))
           (definition-before-blank
             (multiple-value-list
              (cl-markdown-kit::%bp-link-definition-at
               (cl-markdown-kit::%bp-split-lines
                (format nil "[valid]: /url~%~%body"))
               0)))
           (direct-lines
             (let ((cl-markdown-kit::*block-inline-fragments* nil)
                   (cl-markdown-kit::*block-reference-index*
                     (make-hash-table :test 'equal)))
               (cl-markdown-kit::%bp-parse-lines
                source (list plain-line) "" 0 256 t)))
           (default-parse
             (block-parser-test-document
              (cl-markdown-kit::parse-blocks source)))
           (vector-body
             (cl-markdown-kit::%bp-body-from-lines
              (cl-markdown-kit::%bp-split-lines "body")))
           (empty-list-marker
             (cl-markdown-kit::%bp-list-marker
              (block-parser-test-line "")))
           (ordinary-link-start
             (cl-markdown-kit::%bp-link-definition-start-p
              (block-parser-test-line "ordinary")))
           (footnote-link-start
             (cl-markdown-kit::%bp-link-definition-start-p
              (block-parser-test-line "[^note]: /url")))
           (root-frame
             (cl-markdown-kit::%bp-make-open-frame :kind :root))
           (item-frame
             (cl-markdown-kit::%bp-make-open-frame
              :kind :item :content-indent 2 :has-content t
              :end-line plain-line))
           (continuation (block-parser-test-line "  continuation"))
           (blank-item
             (cl-markdown-kit::%bp-make-open-frame
              :kind :item :content-indent 2))
           (blank-line (block-parser-test-line ""))
           (footnote-frame
             (cl-markdown-kit::%bp-make-open-frame
              :kind :footnote :content-indent 4 :end-line plain-line))
           (footnote-continuation (block-parser-test-line "    body"))
           (unknown-node
             (cl-markdown-kit:make-paragraph
              :span nil :continuation-prefix "" :children nil))
           (unknown-frame
             (cl-markdown-kit::%bp-make-open-frame
              :kind :unknown :node unknown-node
              :start-line plain-line :start-position 0 :end-line plain-line))
           (quoted-footnote
             (block-parser-test-nodes-of-type
              (block-parser-test-document
               (block-parser-test-result "> [^quote]: body"))
              #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition))))
           (listed-footnote
             (block-parser-test-nodes-of-type
              (block-parser-test-document
               (block-parser-test-result "- [^list]: body"))
              #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition)))))
      (expect (and (string= decoded-references
                            "&BBB&#xZZ;&#x;&#;&; &amp")
                   (string= vector-body "body")
                   (string= (cl-markdown-kit::%block-line-text
                             trimmed-empty-line)
                            "")
                   (string= (first trailing-backslash) "/a\\")
                   (= (second trailing-backslash) 3)
                   (null trailing-angle-backslash)
                   (null empty-list-marker)
                   (not ordinary-link-start)
                   (not footnote-link-start)
                   (null invalid-empty-line)
                   (null ordinary-definition)
                   (null footnote-definition)
                   (null blank-reparse)
                   (null blank-title)
                   (first definition-before-blank)
                   (= (length direct-lines) 1)
                   (typep (first direct-lines) 'cl-markdown-kit:paragraph)
                   (typep (block-parser-test-only-child default-parse) 'cl-markdown-kit:paragraph)
                   (= (length quoted-footnote) 1)
                   (= (length listed-footnote) 1))
              :to-be-truthy)
      (multiple-value-bind (transformed mismatch)
          (cl-markdown-kit::%bp-container-transform-line
           continuation (list item-frame root-frame))
        (expect (and (null mismatch)
                     (string= (cl-markdown-kit::%block-line-text transformed)
                              "continuation")
                     (eq (cl-markdown-kit::%bp-open-frame-end-line item-frame)
                         plain-line))
                :to-be-truthy))
      (multiple-value-bind (transformed mismatch)
          (cl-markdown-kit::%bp-container-transform-line
           continuation (list item-frame root-frame) t)
        (expect (and (null mismatch)
                     (string= (cl-markdown-kit::%block-line-text transformed)
                              "continuation")
                     (eq (cl-markdown-kit::%bp-open-frame-end-line item-frame)
                         continuation))
                :to-be-truthy))
      (cl-markdown-kit::%bp-container-transform-line
       blank-line (list blank-item root-frame) t)
      (multiple-value-bind (transformed mismatch)
          (cl-markdown-kit::%bp-container-transform-line
           continuation (list blank-item root-frame) t)
        (declare (ignore transformed))
        (expect (eq mismatch blank-item) :to-be-truthy))
      (multiple-value-bind (transformed mismatch)
          (cl-markdown-kit::%bp-container-transform-line
           footnote-continuation (list footnote-frame root-frame) t)
        (expect (and (null mismatch)
                     (string= (cl-markdown-kit::%block-line-text transformed)
                              "body")
                     (eq (cl-markdown-kit::%bp-open-frame-end-line footnote-frame)
                         footnote-continuation))
                :to-be-truthy))
      (expect (eq (cl-markdown-kit::%bp-finish-frame source unknown-frame)
                  unknown-node)
              :to-be-truthy)))

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
                   (null long-label))
              :to-be-truthy)
      (expect (and (first paren-title)
                   (string= (fourth (second paren-title))
                            (format nil "open~%  middle~%  close"))
                   (null (first reparse-failure)))
              :to-be-truthy)
      (expect (and (string= (first escaped-footnote)
                            (format nil "escaped~C]label" #\\))
                   (string= (second escaped-footnote) "escaped]label"))
              :to-be-truthy)
      (expect (and (null empty-footnote)
                   (null missing-footnote-colon))
              :to-be-truthy)
      (expect (and (= (first dash-setext) 2)
                   (null blank-setext)
                   (null plain-fence)
                   (null empty-fence)
                   (null empty-heading)
                   (null empty-title-opener))
              :to-be-truthy)
      (expect (and (= (length (block-parser-test-nodes-of-type
                               list-document #'(lambda (node) (typep node 'cl-markdown-kit:list-node))))
                      1)
                   (= (length (block-parser-test-nodes-of-type
                               footnote-document
                               #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition))))
                      1))
              :to-be-truthy)
      (expect (and (null (first empty-tag-name))
                   (null closing-script)
                   (null closing-empty-tag)
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
                   list-document #'(lambda (node) (typep node 'cl-markdown-kit:list-node))))
           (tasks (block-parser-test-nodes-of-type
                   list-document #'(lambda (node) (typep node 'cl-markdown-kit:task-list-item))))
           (first-item (first (cl-markdown-kit:list-node-items (first lists))))
           (first-paragraph (block-parser-test-only-child first-item))
           (footnote-source
             (format nil "[^note]: first~%continued~%~%    code~%[^NOTE]: second"))
           (footnote-result (block-parser-test-result footnote-source))
           (footnotes (block-parser-test-nodes-of-type
                       (block-parser-test-document footnote-result)
                       #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition))))
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
