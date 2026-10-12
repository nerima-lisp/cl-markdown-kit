(in-package #:cl-markdown-kit/test)

(defun inline-parser-test-parse (text &optional reference-index &rest options)
  (destructuring-bind (&key (max-inline-delimiter-depth 4096) (gfm t)) options
    (let* ((buffer (cl-markdown-kit:make-source-buffer text))
           (index (or reference-index (make-hash-table :test #'equal))))
      (cl-markdown-kit::parse-inline
       (cl-markdown-kit:source-buffer-normalized-text buffer)
       (cl-markdown-kit::source-buffer-boundary-map buffer)
       index
       :max-inline-delimiter-depth max-inline-delimiter-depth
       :gfm gfm))))

(defun inline-parser-test-parse-range (text link-allowed &optional start end)
  (let* ((buffer (cl-markdown-kit:make-source-buffer text))
         (normalized (cl-markdown-kit:source-buffer-normalized-text buffer))
         (boundary-map (cl-markdown-kit::source-buffer-boundary-map buffer))
         (index (make-hash-table :test #'equal))
         (limit 4096)
         (backtick-next (cl-markdown-kit::%ip-build-backtick-index normalized))
         (html-closes (cl-markdown-kit::%ip-build-html-closes normalized)))
    (multiple-value-bind (opens closes prefix bracket-depths)
        (cl-markdown-kit::%ip-build-bracket-index
         normalized backtick-next html-closes limit)
      (cl-markdown-kit::%ip-parse-range
       (cl-markdown-kit::%ip-make-context
        normalized boundary-map index limit backtick-next opens closes prefix
        bracket-depths (cl-markdown-kit::%ip-build-bare-links normalized)
        html-closes)
       (or start 0) (or end (length normalized)) link-allowed 0))))

(defun inline-parser-test-span (node)
  (let ((span (cl-markdown-kit:node-span node)))
    (list (cl-parser-kit:span-start span)
          (cl-parser-kit:span-end span)
          (cl-parser-kit:span-start-line span)
          (cl-parser-kit:span-start-column span)
          (cl-parser-kit:span-end-line span)
          (cl-parser-kit:span-end-column span))))

(defun inline-parser-test-ranges-valid-p (node source)
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
                               (inline-parser-test-ranges-valid-p child source))
                    (return-from inline-parser-test-ranges-valid-p nil))
                  (setf previous-end child-end)
               finally (return t)))))

(defun inline-parser-test-reference-index ()
  (let ((index (make-hash-table :test #'equal)))
    (setf (gethash "r" index)
          (cl-markdown-kit:make-link-reference-definition
           :raw-label "r" :normalized-label "r"
           :destination "/target" :title "Title" :eligible-p t)
          (gethash (cons :footnote "note") index)
          (cl-markdown-kit:make-footnote-definition
           :raw-label "note" :normalized-label "note"))
    index))

(defun inline-parser-test-signals-depth-p (text limit)
  (handler-case
      (progn
        (inline-parser-test-parse text nil :max-inline-delimiter-depth limit)
        nil)
    (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded () t)))

(defun inline-parser-test-read-data (name)
  (let ((path (merge-pathnames
               (format nil "t/data/~A" name)
               (asdf:system-source-directory :cl-markdown-kit))))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (let* ((*read-eval* nil)
             (eof (gensym "EOF"))
             (data (read stream nil eof)))
        (when (eq data eof)
          (error "The inline structure data file is empty: ~A" name))
        (unless (eq (read stream nil eof) eof)
          (error "The inline structure data file has multiple forms: ~A" name))
        data))))

(declaim (ftype function inline-parser-test-node-sequence-signature
                  inline-parser-test-node-signature))

(defun inline-parser-test-node-sequence-signature (nodes)
  (let ((result nil))
    (dolist (node nodes (nreverse result))
      (let ((signature (inline-parser-test-node-signature node)))
        (cond
          ((and (eq (first signature) :text)
                (string= (second signature) "")))
          ((and (eq (first signature) :text)
                result
                (eq (first (first result)) :text))
           (setf (second (first result))
                 (concatenate 'string (second (first result))
                              (second signature))))
          (t (push signature result)))))))

(defun inline-parser-test-node-signature (node)
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
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:emphasis-children node))))
    (cl-markdown-kit:strong
     (list :strong
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:strong-children node))))
    (cl-markdown-kit:strikethrough
     (list :strikethrough
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:strikethrough-children node))))
    (cl-markdown-kit:link
     (list :link
           (cl-markdown-kit:link-destination node)
           (cl-markdown-kit:link-title node)
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:link-children node))))
    (cl-markdown-kit:image
     (list :image
           (cl-markdown-kit:image-destination node)
           (cl-markdown-kit:image-title node)
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:image-children node))))
    (cl-markdown-kit:autolink
     (list :link
           (cl-markdown-kit:autolink-destination node)
           nil
           (list (list :text (cl-markdown-kit:autolink-display-text node)))))
    (cl-markdown-kit:footnote-reference
     (list :footnote-reference
           (cl-markdown-kit:footnote-reference-raw-label node)))
    (otherwise
     (error "Unsupported inline node in structural signature: ~S" node))))

(defun inline-parser-test-block-fragments (node)
  (typecase node
    (cl-markdown-kit:paragraph
     (list (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:paragraph-children node))))
    (cl-markdown-kit:heading
     (list (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:heading-children node))))
    (cl-markdown-kit:table-cell
     (list (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:table-cell-children node))))
    (cl-markdown-kit:footnote-definition nil)
    (otherwise
     (mapcan #'inline-parser-test-block-fragments
             (cl-markdown-kit:node-children node)))))

(defun inline-parser-test-footnote-definitions (root)
  (let ((definitions nil))
    (labels ((visit (node)
               (when (cl-markdown-kit:footnote-definition-p node)
                 (push node definitions))
               (dolist (child (cl-markdown-kit:node-children node))
                 (visit child))))
      (visit root))
    (nreverse definitions)))

(defun inline-parser-test-structure-signature (example)
  (handler-case
      (let* ((document
               (cl-markdown-kit:parse-string
                (spec-example-markdown example)
                :front-matter nil
                :gfm (string= "gfm-"
                              (spec-example-id example)
                              :end2 4)))
             (fragments (inline-parser-test-block-fragments document))
             (footnotes
               (loop for definition in
                       (inline-parser-test-footnote-definitions document)
                     collect
                     (list
                      (cl-markdown-kit:footnote-definition-raw-label definition)
                      (mapcan #'inline-parser-test-block-fragments
                              (cl-markdown-kit:footnote-definition-children
                               definition))))))
        (values (list fragments footnotes) nil))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun inline-parser-test-normalize-node-signature (signature)
  (case (first signature)
    ((:link :image)
     (list (first signature)
           (cl-markdown-kit::normalize-uri-percent-encoding
            (second signature))
           (third signature)
           (mapcar #'inline-parser-test-normalize-node-signature
                   (fourth signature))))
    ((:emphasis :strong :strikethrough)
     (list (first signature)
           (mapcar #'inline-parser-test-normalize-node-signature
                   (second signature))))
    (otherwise signature)))

(defun inline-parser-test-trim-text-signature (signature side)
  (when (eq (first signature) :text)
    (setf (second signature)
          (ecase side
            (:left (string-left-trim '(#\Space #\Tab) (second signature)))
            (:right (string-right-trim '(#\Space #\Tab) (second signature)))))))

(defun inline-parser-test-canonicalize-fragment (signatures)
  (let ((signatures
          (mapcar #'inline-parser-test-normalize-node-signature
                  (copy-tree signatures))))
    (when signatures
      (inline-parser-test-trim-text-signature (first signatures) :left)
      (inline-parser-test-trim-text-signature (car (last signatures)) :right)
      (loop for index below (length signatures)
            for signature = (nth index signatures)
            when (member (first signature) '(:soft-break :hard-break))
              do (when (plusp index)
                   (inline-parser-test-trim-text-signature
                    (nth (1- index) signatures) :right))
                 (when (< index (1- (length signatures)))
                   (inline-parser-test-trim-text-signature
                    (nth (1+ index) signatures) :left))))
    (let ((result nil))
      (dolist (signature signatures (nreverse result))
        (unless (and (eq (first signature) :text)
                     (zerop (length (second signature))))
          (if (and (eq (first signature) :text)
                   result
                   (eq (first (first result)) :text))
              (setf (second (first result))
                    (concatenate 'string (second (first result))
                                 (second signature)))
              (push signature result)))))))

(defun inline-parser-test-canonicalize-fragments (fragments)
  (mapcar #'inline-parser-test-canonicalize-fragment fragments))

(defun inline-parser-test-structure-difference (expected actual error-message)
  (if error-message
      (list :parse-error error-message)
      (destructuring-bind (expected-fragments expected-footnotes) expected
        (destructuring-bind (actual-fragments actual-footnotes) actual
          (let ((difference
                  (block-parser-test-first-difference
                   (inline-parser-test-canonicalize-fragments expected-fragments)
                   (inline-parser-test-canonicalize-fragments actual-fragments))))
            (or difference
                (loop for (label expected-content) in expected-footnotes
                      for actual-footnote =
                        (find label actual-footnotes :key #'first :test #'string=)
                      unless actual-footnote
                        do (return (list :missing-footnote label))
                      do (let ((difference
                                 (block-parser-test-first-difference
                                  (inline-parser-test-canonicalize-fragments
                                   expected-content)
                                  (inline-parser-test-canonicalize-fragments
                                   (second actual-footnote)))))
                           (when difference
                             (return (list :footnote label difference)))))))))))

(describe "Inline parser"
  (it "builds nested emphasis and strong nodes with delimiter-inclusive spans"
    (let* ((node (first (inline-parser-test-parse "***word***")))
           (strong (first (cl-markdown-kit:emphasis-children node)))
           (word (first (cl-markdown-kit:strong-children strong))))
      (expect (and (cl-markdown-kit:emphasis-p node)
                   (cl-markdown-kit:strong-p strong)
                   (cl-markdown-kit:text-p word))
              :to-be-truthy)
      (expect (list (inline-parser-test-span node)
                    (inline-parser-test-span strong)
                    (cl-markdown-kit:text-value word)
                    (inline-parser-test-span word))
              :to-equal '((0 10 1 1 1 11)
                          (1 9 1 2 1 10)
                          "word"
                          (3 7 1 4 1 8)))))

  (it "respects underscore flanking rules and unmatched delimiters"
    (let* ((intraword (inline-parser-test-parse "foo_bar_baz"))
           (punctuation (second (inline-parser-test-parse "(_word_)"))))
      (expect (mapcar #'cl-markdown-kit:text-value intraword)
              :to-equal '("foo_bar_baz"))
      (expect (and (cl-markdown-kit:emphasis-p punctuation)
                   (string= (cl-markdown-kit:text-value
                             (first (cl-markdown-kit:emphasis-children punctuation)))
                            "word"))
              :to-be-truthy)))

  (it "chooses the nearest delimiter across opener buckets"
    (let* ((nodes (inline-parser-test-parse "**outer *inner* tail**"))
           (strong (first nodes))
           (emphasis (second (cl-markdown-kit:strong-children strong))))
      (expect (and (cl-markdown-kit:strong-p strong)
                   (cl-markdown-kit:emphasis-p emphasis))
              :to-be-truthy)
      (expect (cl-markdown-kit:text-value
               (first (cl-markdown-kit:emphasis-children emphasis)))
              :to-equal "inner")
      (expect (cl-markdown-kit:strong-p
               (first (inline-parser-test-parse "**strong**")))
              :to-be-truthy)))

  (it "decodes escaped punctuation and named and numeric references"
    (let* ((nodes (inline-parser-test-parse "\\* &amp; &#x1F600;")))
      (expect (mapcar #'cl-markdown-kit:text-value nodes)
              :to-equal (list (format nil "* & ~C" (code-char #x1F600))))
      (expect (mapcar #'inline-parser-test-span nodes)
              :to-equal '((0 18 1 1 1 19)))))

  (it "applies the HTML C1 numeric-reference replacements"
    (let ((nodes (inline-parser-test-parse
                  "&#128; &#x80; &#130; &#x9F; &#129;")))
      (expect (mapcar #'cl-markdown-kit:text-value nodes)
              :to-equal
              (list (format nil "~C ~C ~C ~C ~C"
                            (code-char #x20AC) (code-char #x20AC)
                            (code-char #x201A) (code-char #x0178)
                            (code-char #x81))))))

  (it "keeps code-span contents literal and normalizes line endings"
    (let* ((node (first (inline-parser-test-parse "`a &amp; *b*`")))
           (crlf-nodes (inline-parser-test-parse
                        (format nil "a  ~C~%b" #\Return)))
           (hard-break (second crlf-nodes)))
      (expect (list (cl-markdown-kit:code-span-literal-text node)
                    (cl-markdown-kit:code-span-backtick-run-length node)
                    (inline-parser-test-span node))
              :to-equal '("a &amp; *b*" 1 (0 13 1 1 1 14)))
      (expect (and (= (length crlf-nodes) 3)
                   (cl-markdown-kit:hard-break-p hard-break)
                   (equal (inline-parser-test-span hard-break)
                          '(1 5 1 2 2 1)))
              :to-be-truthy)))

  (it "closes code spans on backticks preceded by a backslash"
    (let* ((nodes (inline-parser-test-parse "`foo\\`bar`"))
           (code (first nodes)))
      (expect (and (cl-markdown-kit:code-span-p code)
                   (string= (cl-markdown-kit:code-span-literal-text code)
                            "foo\\")
                   (equal (mapcar #'cl-markdown-kit:text-value (rest nodes))
                          '("bar`")))
              :to-be-truthy)))

  (it "parses angle autolinks, raw HTML, and GFM bare links"
    (let* ((nodes (inline-parser-test-parse
                   "<https://example.com> <user@example.com> <span title=\"x\"> www.example.com."))
           (uri (first nodes))
           (email (third nodes))
           (html (fifth nodes))
           (url (seventh nodes))
           (trailing (eighth nodes)))
      (expect (list (cl-markdown-kit:autolink-destination uri)
                    (cl-markdown-kit:autolink-display-text uri)
                    (cl-markdown-kit:autolink-kind uri)
                    (inline-parser-test-span uri))
              :to-equal '("https://example.com" "https://example.com"
                          :angle-uri (0 21 1 1 1 22)))
      (expect (list (cl-markdown-kit:autolink-destination email)
                    (cl-markdown-kit:autolink-kind email)
                    (inline-parser-test-span email))
              :to-equal '("mailto:user@example.com" :angle-email
                          (22 40 1 23 1 41)))
      (expect (and (cl-markdown-kit:html-inline-p html)
                   (string= (cl-markdown-kit:html-inline-body html)
                            "<span title=\"x\">"))
              :to-be-truthy)
      (expect (list (cl-markdown-kit:autolink-destination url)
                    (cl-markdown-kit:autolink-display-text url)
                    (cl-markdown-kit:autolink-kind url)
                    (inline-parser-test-span url)
                    (cl-markdown-kit:text-value trailing))
              :to-equal '("http://www.example.com" "www.example.com"
                          :bare-url (58 73 1 59 1 74) "."))))

  (it "disables GFM-only inline syntax in CommonMark mode"
    (let* ((index (inline-parser-test-reference-index))
           (source "www.example.com foo@bar.example.com ~~gone~~ [^Note]")
           (commonmark (inline-parser-test-parse source index :gfm nil))
           (gfm (inline-parser-test-parse source index :gfm t)))
      (expect (list (notany #'cl-markdown-kit:autolink-p commonmark)
                    (notany #'cl-markdown-kit:strikethrough-p commonmark)
                    (notany #'cl-markdown-kit:footnote-reference-p commonmark)
                    (some #'cl-markdown-kit:autolink-p gfm)
                    (some #'cl-markdown-kit:strikethrough-p gfm)
                    (some #'cl-markdown-kit:footnote-reference-p gfm))
              :to-equal '(t t t t t t))))

  (it "validates angle autolinks and GFM URL boundaries and punctuation"
    (let* ((index (make-hash-table :test #'equal))
           (nodes (inline-parser-test-parse
                   "<a@b> <a@b_c.com> http://example.com/a(b(c)) http://example.com/a(b)) https://foo_bar.com" index))
           (autolinks (remove-if-not #'cl-markdown-kit:autolink-p nodes)))
      (expect (mapcar #'cl-markdown-kit:autolink-kind autolinks)
              :to-equal '(:angle-email :bare-url :bare-url))
      (expect (mapcar #'cl-markdown-kit:autolink-display-text autolinks)
              :to-equal '("a@b" "http://example.com/a(b(c))"
                          "http://example.com/a(b)"))
      (expect (some (lambda (node)
                      (and (cl-markdown-kit:text-p node)
                           (search "foo_bar.com"
                                   (cl-markdown-kit:text-value node))))
                    nodes)
              :to-be-truthy)))

  (it "preserves entity text inside angle autolinks"
    (let ((uri (first (inline-parser-test-parse
                       "<https://example.com/?a=1&amp;b=2>"))))
      (expect (list (cl-markdown-kit:autolink-destination uri)
                    (cl-markdown-kit:autolink-display-text uri))
              :to-equal '("https://example.com/?a=1&amp;b=2"
                          "https://example.com/?a=1&amp;b=2"))))

  (it "resolves full, collapsed, and shortcut references and parses images"
    (let* ((index (inline-parser-test-reference-index))
           (full (first (inline-parser-test-parse "[A][R]" index)))
           (collapsed (first (inline-parser-test-parse "[R][]" index)))
           (shortcut (first (inline-parser-test-parse "[R]" index)))
           (image (first (inline-parser-test-parse "![alt][R]" index))))
      (expect (list (cl-markdown-kit:link-destination full)
                    (cl-markdown-kit:link-title full)
                    (cl-markdown-kit:link-reference-kind full)
                    (cl-markdown-kit:link-reference-label full)
                    (inline-parser-test-span full)
                    (cl-markdown-kit:text-value
                     (first (cl-markdown-kit:link-children full))))
              :to-equal '("/target" "Title" :full "R" (0 6 1 1 1 7) "A"))
      (expect (list (cl-markdown-kit:link-reference-kind collapsed)
                    (cl-markdown-kit:link-reference-kind shortcut))
              :to-equal '(:collapsed :shortcut))
      (expect (list (cl-markdown-kit:image-destination image)
                    (cl-markdown-kit:image-reference-kind image)
                    (inline-parser-test-span image)
                    (cl-markdown-kit:text-value
                     (first (cl-markdown-kit:image-children image))))
              :to-equal '("/target" :full (0 9 1 1 1 10) "alt"))))

  (it "does not nest links and allows links inside image descriptions"
    (let* ((nested (inline-parser-test-parse
                    "[outer [inner](/inner)](/outer)"))
           (links (remove-if-not #'cl-markdown-kit:link-p nested))
           (image-index (inline-parser-test-reference-index))
           (image (first (inline-parser-test-parse
                          "![a [b](/inner)][R]" image-index))))
      (expect (= (length links) 1) :to-be-truthy)
      (expect (and (cl-markdown-kit:image-p image)
                   (= (length (remove-if-not #'cl-markdown-kit:link-p
                                             (cl-markdown-kit:image-children image)))
                      1))
              :to-be-truthy)))

  (it "applies ASCII link-destination boundaries"
    (let* ((angle-line-break
             (inline-parser-test-parse
              (format nil "[x](<a\\~%b>)")))
           (control-destination
             (inline-parser-test-parse
              (format nil "[x](a~Cb)" (code-char 1))))
           (delete-destination
             (inline-parser-test-parse
              (format nil "[x](a~Cb)" (code-char #x7F))))
           (nbsp (code-char #xA0))
           (unicode-space-destination
             (first (inline-parser-test-parse
                     (format nil "[x](a~Cb)" nbsp)))))
      (expect (every (lambda (nodes)
                       (notany #'cl-markdown-kit:link-p nodes))
                     (list angle-line-break control-destination
                           delete-destination))
              :to-be-truthy)
      (expect (and (cl-markdown-kit:link-p unicode-space-destination)
                   (string= (cl-markdown-kit:link-destination
                             unicode-space-destination)
                            "a%C2%A0b"))
              :to-be-truthy)))

  (it "rejects unmatched link destination parentheses and preserves escaped title text"
    (let* ((unmatched (inline-parser-test-parse "[x](a((b)"))
           (escaped-title (first (inline-parser-test-parse
                                  "[x](dest \"a\\q\")"))))
      (expect (notany #'cl-markdown-kit:link-p unmatched) :to-be-truthy)
      (expect (list (cl-markdown-kit:link-destination escaped-title)
                    (cl-markdown-kit:link-title escaped-title))
              :to-equal '("dest" "a\\q"))))

  (it "handles empty and trailing escape boundaries in link syntax"
    (let* ((empty-destination
             (cl-markdown-kit::%ip-parse-link-destination "" 0 0))
           (angle-destination
             (cl-markdown-kit::%ip-parse-link-destination "<a\\" 0 3))
           (bare-destination
             (cl-markdown-kit::%ip-parse-link-destination "a\\" 0 2))
           (title-values (multiple-value-list
                          (cl-markdown-kit::%ip-parse-title
                           "\"title\\" 0 7)))
           (buffer (cl-markdown-kit:make-source-buffer ""))
           (default-parse
             (cl-markdown-kit::parse-inline
              "" (cl-markdown-kit::source-buffer-boundary-map buffer)
              (make-hash-table :test #'equal))))
      (expect (and (null empty-destination)
                   (null angle-destination)
                   (null bare-destination))
              :to-be-truthy)
      (expect title-values :to-equal '(nil nil))
      (expect default-parse :to-equal nil)))

  (it "does not fall back from an unresolved full reference to a shortcut"
    (let ((index (make-hash-table :test #'equal)))
      (setf (gethash "foo" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "foo" :normalized-label "foo"
             :destination "/foo" :eligible-p t)
            (gethash "baz" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "baz" :normalized-label "baz"
             :destination "/baz" :eligible-p t))
      (let* ((nodes (inline-parser-test-parse "[foo][bar][baz]" index))
             (links (remove-if-not #'cl-markdown-kit:link-p nodes)))
        (expect (list (length links)
                      (cl-markdown-kit:link-destination (first links))
                      (mapcar #'cl-markdown-kit:text-value
                              (remove-if-not #'cl-markdown-kit:text-p nodes)))
                :to-equal '(1 "/baz" ("[foo]"))))))

  (it "treats an invalid bracketed reference label as text"
    (let ((index (make-hash-table :test #'equal)))
      (setf (gethash "foo" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "foo" :normalized-label "foo"
             :destination "/foo" :eligible-p t))
      (let* ((nodes (inline-parser-test-parse "[foo][bar[baz]]" index))
             (link (find-if #'cl-markdown-kit:link-p nodes)))
        (expect (and link
                     (eq (cl-markdown-kit:link-reference-kind link) :shortcut)
                     (string= (cl-markdown-kit:link-destination link) "/foo"))
                :to-be-truthy))))

  (it "decodes escaped backslashes and brackets in reference labels"
    (let ((index (make-hash-table :test #'equal)))
      (setf (gethash "ref[" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "ref[" :normalized-label "ref["
             :destination "/bracket" :eligible-p t)
            (gethash "foo!" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "foo!" :normalized-label "foo!"
             :destination "/punctuation" :eligible-p t)
            (gethash "bar\\" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "bar\\" :normalized-label "bar\\"
             :destination "/backslash" :eligible-p t))
      (let ((bracket-link
              (first (inline-parser-test-parse "[x][ref\\[]" index)))
            (escaped-punctuation
              (inline-parser-test-parse "[x][foo\\!]" index))
            (escaped-backslash
              (first (inline-parser-test-parse "[x][bar\\\\]" index))))
        (expect (list (cl-markdown-kit:link-destination bracket-link)
                      (inline-parser-test-span bracket-link)
                      (cl-markdown-kit:link-destination escaped-backslash)
                      (some #'cl-markdown-kit:link-p escaped-punctuation))
                :to-equal '("/bracket" (0 10 1 1 1 11)
                            "/backslash" nil)))))

  (it "creates GFM strikethrough and resolves footnote references"
    (let* ((index (inline-parser-test-reference-index))
           (strike (first (inline-parser-test-parse "~~gone~~" index)))
           (footnote (first (inline-parser-test-parse "[^Note]" index)))
           (image-prefix (inline-parser-test-parse "![^Note]" index)))
      (expect (list (cl-markdown-kit:strikethrough-delimiter-character strike)
                    (inline-parser-test-span strike)
                    (cl-markdown-kit:text-value
                     (first (cl-markdown-kit:strikethrough-children strike))))
              :to-equal (list #\~ '(0 8 1 1 1 9) "gone"))
      (expect (list (cl-markdown-kit:footnote-reference-raw-label footnote)
                    (cl-markdown-kit:footnote-reference-normalized-label footnote)
                    (inline-parser-test-span footnote))
              :to-equal '("Note" "note" (0 7 1 1 1 8)))
      (expect (and (= (length image-prefix) 2)
                   (string= (cl-markdown-kit:text-value (first image-prefix)) "!")
                   (cl-markdown-kit:footnote-reference-p (second image-prefix)))
              :to-be-truthy)))

  (it "uses Unicode punctuation categories and GFM email boundaries"
    (let* ((currency (inline-parser-test-parse "*£*bravo."))
           (email (inline-parser-test-parse "Email me at:scyther@pokemon.com"))
           (invalid-email (inline-parser-test-parse "(_A_@_.A")))
      (expect (and (notany #'cl-markdown-kit:emphasis-p currency)
                   (cl-markdown-kit::%ip-commonmark-punctuation-p
                    (code-char #x00A3))
                   (cl-markdown-kit::%ip-commonmark-punctuation-p
                    (code-char #x20AC))
                   (cl-markdown-kit::%ip-commonmark-punctuation-p
                    (code-char #x2014)))
              :to-be-truthy)
      (expect (and (cl-markdown-kit:autolink-p (second email))
                   (string= (cl-markdown-kit:autolink-destination
                             (second email))
                            "mailto:scyther@pokemon.com")
                   (notany #'cl-markdown-kit:autolink-p invalid-email))
              :to-be-truthy)))

  (it "classifies GFM domain characters and validates email schemes"
    (let* ((punctuation (code-char #x2014))
           (space (code-char #x3000))
           (format-control (code-char #x200B))
           (private-use (code-char #xE000))
           (mailto (first (inline-parser-test-parse "mailto:mail@example.com")))
           (invalid-mailto (inline-parser-test-parse "mailto:mail"))
           (xmpp (first (inline-parser-test-parse "xmpp:chat@example.com")))
           (invalid-xmpp (inline-parser-test-parse "xmpp:chat"))
           (invalid-hyphens
             (inline-parser-test-parse "user@-bad.example user@bad-.example")))
      (expect (list (not (null (cl-markdown-kit::%ip-gfm-domain-character-p #\A)))
                    (not (null (cl-markdown-kit::%ip-gfm-domain-character-p #\-)))
                    (not (null
                          (cl-markdown-kit::%ip-gfm-domain-character-p
                           (code-char #x1F344))))
                    (not (null (cl-markdown-kit::%ip-gfm-domain-character-p space)))
                    (not (null
                          (cl-markdown-kit::%ip-gfm-domain-character-p punctuation)))
                    (not (null
                          (cl-markdown-kit::%ip-gfm-domain-character-p format-control)))
                    (not (null
                          (cl-markdown-kit::%ip-gfm-domain-character-p private-use))))
              :to-equal '(t t t nil nil nil nil))
      (expect (list (cl-markdown-kit:autolink-p mailto)
                    (cl-markdown-kit:autolink-destination mailto)
                    (notany #'cl-markdown-kit:autolink-p invalid-mailto)
                    (cl-markdown-kit:autolink-p xmpp)
                    (cl-markdown-kit:autolink-destination xmpp)
                    (notany #'cl-markdown-kit:autolink-p invalid-xmpp)
                    (notany #'cl-markdown-kit:autolink-p invalid-hyphens))
              :to-equal '(t "mailto:mail@example.com" t t
                            "xmpp:chat@example.com" t t))))

  (it "checks inline delimiter depth at exact limits"
    (labels ((depth-condition (text limit)
               (handler-case
                   (progn
                     (inline-parser-test-parse
                      text nil :max-inline-delimiter-depth limit)
                     nil)
                 (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded
                     (condition)
                   condition)))
             (depth-condition-matches-p (condition limit actual)
               (and condition
                    (eq (cl-markdown-kit:markdown-resource-limit-error-limit-name
                         condition)
                        :inline-delimiter-depth)
                    (= (cl-markdown-kit:markdown-resource-limit-error-limit
                        condition)
                       limit)
                    (= (cl-markdown-kit:markdown-resource-limit-error-actual
                        condition)
                       actual)))
             (repeated-opener (marker count)
               (with-output-to-string (output)
                 (dotimes (index count)
                   (when (plusp index)
                     (write-char #\Space output))
                   (write-string marker output)
                   (write-char #\a output))))
             (combined-openers (bracket-count)
               (concatenate 'string
                            (make-string bracket-count :initial-element #\[)
                            "*a b"))
             (repeated-brackets (count)
               (concatenate 'string
                            (make-string count :initial-element #\[)
                            "x"
                            (make-string count :initial-element #\])))
             (sequential-openers ()
               (concatenate 'string (repeated-brackets 8) " "
                            (repeated-opener "*" 8))))
      (expect (inline-parser-test-signals-depth-p "*a *b* c*" 1)
              :to-be-truthy)
      (expect (inline-parser-test-signals-depth-p "[[x]]" 1)
              :to-be-truthy)
      (expect (not (inline-parser-test-signals-depth-p "*a*" 1))
              :to-be-truthy)
      (expect (not (inline-parser-test-signals-depth-p "[x]" 1))
              :to-be-truthy)
      (dolist (marker '("*" "**" "~~"))
        (expect (null (depth-condition (repeated-opener marker 8) 8))
                :to-be-truthy)
        (expect (depth-condition-matches-p
                 (depth-condition (repeated-opener marker 9) 8) 8 9)
                :to-be-truthy))
      (expect (null (depth-condition (repeated-brackets 8) 8))
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition (repeated-brackets 9) 8) 8 9)
              :to-be-truthy)
      (expect (null (depth-condition (combined-openers 7) 8))
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition (combined-openers 8) 8) 8 9)
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition "[*a]" 1) 1 2)
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition "*[a]" 1) 1 2)
              :to-be-truthy)
      (expect (null (depth-condition (sequential-openers) 8))
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition "[*a](u) *b *c" 2) 2 3)
              :to-be-truthy)))

  (it "handles long unmatched bracket and angle sequences"
    (let* ((brackets (make-string 2000 :initial-element #\[))
           (angles (make-string 2000 :initial-element #\<))
           (bracket-nodes (inline-parser-test-parse brackets))
           (angle-nodes (inline-parser-test-parse angles)))
      (expect (mapcar #'cl-markdown-kit:text-value bracket-nodes)
              :to-equal (list brackets))
      (expect (mapcar #'cl-markdown-kit:text-value angle-nodes)
              :to-equal (list angles))))

  (it "handles unmatched backtick runs and paired delimiter stress"
    (let* ((backticks (concatenate 'string (make-string 2000 :initial-element #\`)
                                   " text "
                                   (make-string 1999 :initial-element #\`)))
           (backtick-nodes (inline-parser-test-parse backticks))
           (delimiter-text (format nil "~{~A~}" (make-list 600 :initial-element
                                                              "*a **a")))
           (delimiter-nodes (inline-parser-test-parse delimiter-text)))
      (expect (mapcar #'cl-markdown-kit:text-value backtick-nodes)
              :to-equal (list backticks))
      (expect (plusp (length delimiter-nodes)) :to-be-truthy)))

  (it "counts unmatched delimiter openers against the depth limit"
    (let* ((limit 4096)
           (text (with-output-to-string (output)
                   (loop repeat (1+ limit) do (write-string "*a " output))))
           (condition
             (handler-case
                 (progn
                   (inline-parser-test-parse
                    text nil :max-inline-delimiter-depth limit)
                   nil)
               (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded
                   (condition)
                 condition))))
      (expect (and condition
                   (eq (cl-markdown-kit:markdown-resource-limit-error-limit-name
                        condition)
                       :inline-delimiter-depth)
                   (= (cl-markdown-kit:markdown-resource-limit-error-limit
                       condition)
                      limit)
                   (= (cl-markdown-kit:markdown-resource-limit-error-actual
                       condition)
                      (1+ limit)))
              :to-be-truthy)))

  (it "parses deeply nested bracket links without reparsing each inner range"
    (let* ((count 1000)
           (source (concatenate 'string
                                (make-string count :initial-element #\[)
                                "a"
                                (with-output-to-string (output)
                                  (loop repeat count
                                        do (write-string "](u)" output)))))
           (completed-p
             (handler-case
                 (progn
                   (sb-ext:with-timeout 2
                     (inline-parser-test-parse source))
                   t)
               (sb-ext:timeout () nil))))
      (expect completed-p :to-be-truthy)))

  (it "parses soft breaks, backslash hard breaks, and raw HTML comments"
    (let* ((soft (inline-parser-test-parse (format nil "a~%b")))
           (soft-spaced (inline-parser-test-parse (format nil "a ~%b")))
           (hard (inline-parser-test-parse (format nil "a\\~%b")))
           (comment (first (inline-parser-test-parse "<!-- comment -->"))))
      (expect (and (= (length soft) 3)
                   (cl-markdown-kit:soft-break-p (second soft))
                   (cl-markdown-kit:soft-break-p (second soft-spaced))
                   (= (length hard) 3)
                   (cl-markdown-kit:hard-break-p (second hard))
                   (cl-markdown-kit:html-inline-p comment)
                   (string= (cl-markdown-kit:html-inline-body comment)
                            "<!-- comment -->"))
              :to-be-truthy)))

  (it "trims spaces at inline line boundaries and at the fragment end"
    (let* ((hard (inline-parser-test-parse (format nil "foo  ~%  bar")))
           (backslash-hard
             (inline-parser-test-parse (format nil "foo\\~%     bar")))
           (soft (inline-parser-test-parse (format nil "foo ~%  bar")))
           (trailing (inline-parser-test-parse "foo  ")))
      (expect (mapcar #'inline-parser-test-node-sequence-signature
                      (list hard backslash-hard soft trailing))
              :to-equal '(((:text "foo") (:hard-break) (:text "bar"))
                          ((:text "foo") (:hard-break) (:text "bar"))
                          ((:text "foo") (:soft-break) (:text "bar"))
                          ((:text "foo"))))))

  (it "recognizes multiline raw HTML tags with later attributes"
    (let* ((source (format nil "<a foo=\"bar\" bam = 'baz <em>\"</em>'~%_boolean zoop:33=zoop:33 />"))
           (html (first (inline-parser-test-parse source))))
      (expect (and (cl-markdown-kit:html-inline-p html)
                   (string= (cl-markdown-kit:html-inline-body html) source))
              :to-be-truthy)))

  (it "decodes every HTML C1 numeric-reference replacement"
    (let* ((source (format nil "~{&#~D;~^ ~}"
                           (loop for code from #x80 to #x9f collect code)))
           (replacement-codes
             '(#x20ac #x81 #x201a #x0192 #x201e #x2026 #x2020 #x2021
               #x02c6 #x2030 #x0160 #x2039 #x0152 #x8d #x017d #x8f
               #x90 #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
               #x02dc #x2122 #x0161 #x203a #x0153 #x9d #x017e #x0178))
           (expected (format nil "~{~C~^ ~}"
                             (mapcar #'code-char replacement-codes)))
           (nodes (inline-parser-test-parse source)))
      (expect (apply #'concatenate 'string
                     (mapcar #'cl-markdown-kit:text-value nodes))
              :to-equal expected)
      (expect (mapcar #'cl-markdown-kit:text-value
                      (inline-parser-test-parse "&#0; &#xD800; &unknown;"))
              :to-equal (list (format nil "~C ~C &unknown;"
                                      (code-char #xfffd)
                                      (code-char #xfffd))))))

  (it "parses link destinations, escapes, whitespace, and all title delimiters"
    (let* ((escaped-angle (first (inline-parser-test-parse "[x](<a\\>b>)")))
           (escaped-bare (first (inline-parser-test-parse "[x](a\\)b)")))
           (nested-bare (first (inline-parser-test-parse "[x](a(b)c)")))
           (empty (first (inline-parser-test-parse "[x]( )")))
           (spaced-angle (first (inline-parser-test-parse "[x](<a b>)")))
           (multiline (first (inline-parser-test-parse
                              (format nil "[x]( ~% a \"a\\!b\" )"))))
           (single-quoted (first (inline-parser-test-parse "[x](a 'title')")))
           (parenthesized (first (inline-parser-test-parse "[x](a (title))")))
           (invalid (list (inline-parser-test-parse
                           (format nil "[x](<a~%b>)"))
                          (inline-parser-test-parse "[x](<unterminated")
                          (inline-parser-test-parse "[x](a(b)")
                          (inline-parser-test-parse "[x](a(b")
                          (inline-parser-test-parse "[x](a (nested(inner)))")
                          (inline-parser-test-parse "[x](a \"unclosed)"))))
      (expect (mapcar #'cl-markdown-kit:link-destination
                      (list escaped-angle escaped-bare nested-bare empty))
              :to-equal '("a%3Eb" "a)b" "a(b)c" ""))
      (expect (mapcar #'cl-markdown-kit:link-title
                      (list multiline single-quoted parenthesized))
              :to-equal '("a!b" "title" "title"))
      (expect (mapcar (lambda (nodes) (notany #'cl-markdown-kit:link-p nodes))
                      invalid)
              :to-equal '(t t t t t t))
      (expect (and (cl-markdown-kit:link-p spaced-angle)
                   (string= (cl-markdown-kit:link-destination spaced-angle)
                            "a%20b"))
              :to-be-truthy)))

  (it "decodes entities and preserves non-escapable backslashes in link data"
    (let* ((destination (first (inline-parser-test-parse "[x](a&amp;b)")))
           (backslash-destination
             (first (inline-parser-test-parse "[x](foo\\bar)")))
           (title (first (inline-parser-test-parse "[x](a \"title\\q\")"))))
      (expect (list (cl-markdown-kit:link-destination destination)
                    (cl-markdown-kit:link-destination backslash-destination)
                    (cl-markdown-kit:link-title title))
              :to-equal '("a&b" "foo%5Cbar" "title\\q"))))

  (it "covers raw HTML tag attributes, closing forms, and terminators"
    (let ((valid '("<tag>" "<tag/>" "<tag disabled>"
                   "<tag title=bare>" "<tag title='single'>"
                   "<tag title=\"double\">" "</tag>" "<!-->" "<!--->"
                   "<!-- comment -->" "<?pi?>" "<![CDATA[x]]>"
                   "<!DOCTYPE html>" "<!doctype html>")))
      (expect (every (lambda (source)
                       (let ((node (first (inline-parser-test-parse source))))
                         (and (cl-markdown-kit:html-inline-p node)
                              (string= (cl-markdown-kit:html-inline-body node)
                                       source))))
                     valid)
              :to-be-truthy)
      (expect (every (lambda (source)
                       (notany #'cl-markdown-kit:html-inline-p
                               (inline-parser-test-parse source)))
                     '("<tag title=>" "<tag title=\"unclosed>"
                       "<tag" "<tag " "<tag title" "<tag title="
                       "<tag title=bare" "</"
                       "<!-- unclosed" "<?unclosed" "<![CDATA[unclosed"
                       "<a b=\"c\"d=\"e\">"))
              :to-be-truthy)
      (expect (list (cl-markdown-kit::%ip-tag-name-end "tag" 0 3)
                    (cl-markdown-kit::%ip-tag-name-end "t" 0 1)
                    (cl-markdown-kit::%ip-tag-name-end "t" 1 1)
                    (cl-markdown-kit::%ip-attribute-name-end "title" 0 5)
                    (cl-markdown-kit::%ip-attribute-name-end "a" 0 1)
                    (cl-markdown-kit::%ip-attribute-name-end "a" 1 1))
              :to-equal '(3 1 nil 5 1 nil))
      (expect (list (cl-markdown-kit::%ip-raw-html-end
                     "<!--body-->" 0 11
                     (cl-markdown-kit::%ip-build-html-closes "<!--body-->"))
                    (cl-markdown-kit::%ip-raw-html-end
                     "<?pi?>" 0 6 (cl-markdown-kit::%ip-build-html-closes "<?pi?>"))
                    (cl-markdown-kit::%ip-raw-html-end
                     "<![CDATA[x]]>" 0 13
                     (cl-markdown-kit::%ip-build-html-closes "<![CDATA[x]]>"))
                    (cl-markdown-kit::%ip-raw-html-end
                     "<!DOCTYPE>" 0 10
                     (cl-markdown-kit::%ip-build-html-closes "<!DOCTYPE>"))
                    (cl-markdown-kit::%ip-raw-html-end
                     "<!--" 0 4 (cl-markdown-kit::%ip-build-html-closes "<!--"))
                    (cl-markdown-kit::%ip-raw-html-end
                     "<!---x" 0 6 (cl-markdown-kit::%ip-build-html-closes "<!---x"))
                    (cl-markdown-kit::%ip-raw-html-end
                     "x!A" 0 3 (cl-markdown-kit::%ip-build-html-closes "x!A")))
              :to-equal '(11 6 13 10 nil nil nil))))

  (it "validates angle email autolinks and links bare email addresses"
    (let* ((uri (first (inline-parser-test-parse "<git+ssh:user@host>")))
           (email (first (inline-parser-test-parse "person@example.com")))
           (tagged-email
             (first (inline-parser-test-parse "first.last+tag@example.com")))
           (invalid (mapcar #'inline-parser-test-parse
                            '("<1scheme:value>" "<a b:c>" "<a@-host>"
                              "<a@host.>" "<a@host_name>"
                              "name@.example")))
           (trailing-domain (inline-parser-test-parse "person@example.com_")))
      (expect (list (cl-markdown-kit:autolink-kind uri)
                    (cl-markdown-kit:autolink-destination uri)
                    (cl-markdown-kit:autolink-kind email)
                    (cl-markdown-kit:autolink-destination email)
                    (cl-markdown-kit:autolink-display-text tagged-email))
              :to-equal '(:angle-uri "git+ssh:user@host" :bare-email
                          "mailto:person@example.com"
                          "first.last+tag@example.com"))
      (expect (every (lambda (nodes)
                       (notany #'cl-markdown-kit:autolink-p nodes))
                     invalid)
              :to-be-truthy)
      (expect (notany #'cl-markdown-kit:autolink-p trailing-domain)
              :to-be-truthy)
      (expect (and (cl-markdown-kit::%ip-angle-email-p "a@b" 0 3 1)
                   (not (cl-markdown-kit::%ip-angle-email-p "a@-b" 0 4 1))
                   (not (cl-markdown-kit::%ip-angle-email-p "a@b-" 0 4 1))
                   (not (cl-markdown-kit::%ip-angle-email-p "a@b." 0 4 1))
                   (not (cl-markdown-kit::%ip-angle-email-p "@b" 0 2 0))
                   (not (cl-markdown-kit::%ip-angle-email-p "a@" 0 2 1)))
              :to-be-truthy)))

  (it "uses the indexed angle-autolink scanner"
    (expect (multiple-value-list
             (cl-markdown-kit::%ip-angle-autolink
              "<https://example.com>" 0 21
              (cl-markdown-kit::%ip-build-html-closes
               "<https://example.com>")))
            :to-equal '("https://example.com" "https://example.com"
                        :angle-uri 21)))

  (it "keeps absent inline indexes sparse"
    (let* ((backtick-index
             (cl-markdown-kit::%ip-build-backtick-index "`a`b``c``"))
           (html-closes (cl-markdown-kit::%ip-build-html-closes "plain"))
           (bracket-indexes
             (multiple-value-list
              (cl-markdown-kit::%ip-build-bracket-index
               "plain" backtick-index html-closes 8)))
           (nodes (inline-parser-test-parse "plain ]")))
      (expect (list (hash-table-p backtick-index)
                    (hash-table-count backtick-index)
                    (cl-markdown-kit::%ip-backtick-close backtick-index 0)
                    (cl-markdown-kit::%ip-backtick-close backtick-index 4)
                    (cl-markdown-kit::%ip-backtick-close backtick-index 2))
              :to-equal '(t 4 2 7 -1))
      (expect (coerce html-closes 'list) :to-equal '(nil nil nil nil))
      (expect (cl-markdown-kit::%ip-html-close-at html-closes 0 0 5)
              :to-be nil)
      (expect bracket-indexes :to-equal '(nil nil nil nil))
      (expect (mapcar #'cl-markdown-kit:text-value nodes)
              :to-equal '("plain ]"))))

  (it "trims URL punctuation and handles bare URL query entities"
    (let* ((nodes (inline-parser-test-parse
                   "http://example.com/?q=a&amp;."))
           (link (find-if #'cl-markdown-kit:autolink-p nodes)))
      (expect (list (cl-markdown-kit:autolink-kind link)
                    (cl-markdown-kit:autolink-display-text link))
              :to-equal '(:bare-url "http://example.com/?q=a"))
      (expect (string= (cl-markdown-kit:text-value (car (last nodes))) "&.")
              :to-be-truthy)))

  (it "preserves link-like text when parsing disallows autolinks and links"
    (let* ((nodes (inline-parser-test-parse-range
                   "www.example.com <https://example.com> [x](/target)" nil)))
      (expect (notany (lambda (node)
                        (or (cl-markdown-kit:autolink-p node)
                            (cl-markdown-kit:link-p node)))
                      nodes)
              :to-be-truthy)))

  (it "keeps a bare-link candidate inside its containing link text"
    (let ((link (first (inline-parser-test-parse "[x www.example.com](/target)"))))
      (expect (and (cl-markdown-kit:link-p link)
                   (string= (cl-markdown-kit:link-destination link) "/target")
                   (some (lambda (node)
                           (and (cl-markdown-kit:text-p node)
                                (search "www.example.com"
                                        (cl-markdown-kit:text-value node))))
                         (cl-markdown-kit:link-children link)))
              :to-be-truthy)))

  (it "matches single and double tilde runs without accepting unequal runs"
    (let* ((single (first (inline-parser-test-parse "~plain~")))
           (double (first (inline-parser-test-parse "~~gone~~")))
           (mismatched (inline-parser-test-parse "~~~gone~~")))
      (expect (list (cl-markdown-kit:strikethrough-p single)
                    (cl-markdown-kit:strikethrough-p double)
                    (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value mismatched)))
              :to-equal '(t t "~~~gone~~"))))

  (it "normalizes padded code spans and preserves non-escapable backslashes"
    (let* ((padded (first (inline-parser-test-parse "`  code  `")))
           (spaces (first (inline-parser-test-parse "`  `")))
           (plain (inline-parser-test-parse "ordinary text \\q \\")))
      (expect (list (cl-markdown-kit:code-span-literal-text padded)
                    (cl-markdown-kit:code-span-literal-text spaces)
                    (mapcar #'cl-markdown-kit:text-value plain))
              :to-equal '(" code " "  " ("ordinary text \\q \\")))))

  (it "rejects invalid inline parser arguments"
    (let* ((buffer (cl-markdown-kit:make-source-buffer "x"))
           (text (cl-markdown-kit:source-buffer-normalized-text buffer))
           (boundary-map (cl-markdown-kit::source-buffer-boundary-map buffer))
           (index (make-hash-table :test #'equal)))
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline text boundary-map index
                                                        :max-inline-delimiter-depth 0)
                         nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline text boundary-map nil) nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline 1 boundary-map index) nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline text nil index) nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline
                          text boundary-map index :max-inline-delimiter-depth "bad")
                         nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline "xx" boundary-map index) nil)
                (error () t))
              :to-be-truthy))))

(describe "Inline conformance fixtures"
  (it "parses inline content in every pinned CommonMark and GFM example"
    (let ((examples (append (commonmark-spec-examples)
                            (gfm-extension-examples))))
      (dolist (example examples)
        (let* ((source (spec-example-markdown example))
               (document (cl-markdown-kit:parse-string source)))
          (expect (inline-parser-test-ranges-valid-p document source)
                  :to-be-truthy)))
      (expect (length examples) :to-equal 682))))

(describe "Inline structure conformance fixtures"
  (it "compares every CommonMark and GFM inline structure with pinned cmark output"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm))
           (fixtures (inline-parser-test-read-data
                      "inline-structure-fixtures.lisp"))
           (exclusions (inline-parser-test-read-data
                        "inline-structure-exclusions.lisp"))
           (known-ids (mapcar #'spec-example-id examples))
           (fixture-ids (mapcar #'first fixtures))
           (fixture-index (make-hash-table :test 'equal))
           (exclusion-index (make-hash-table :test 'equal))
           (matches 0)
           (failures nil))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (length fixtures) :to-equal 682)
      (expect fixture-ids :to-equal known-ids)
      (expect (every (lambda (entry)
                       (and (= (length entry) 2)
                            (stringp (first entry))
                            (stringp (second entry))
                            (plusp (length (second entry)))
                            (member (first entry) known-ids :test #'string=)))
                     exclusions)
              :to-be-truthy)
      (expect (length (remove-duplicates (mapcar #'first exclusions)
                                         :test #'string=))
              :to-equal (length exclusions))
      (dolist (entry fixtures)
        (setf (gethash (first entry) fixture-index) (second entry)))
      (dolist (entry exclusions)
        (setf (gethash (first entry) exclusion-index) (second entry)))
      (dolist (example examples)
        (let* ((id (spec-example-id example))
               (reason (gethash id exclusion-index)))
          (unless reason
            (multiple-value-bind (actual error-message)
                (inline-parser-test-structure-signature example)
              (let ((difference
                      (inline-parser-test-structure-difference
                       (gethash id fixture-index) actual error-message)))
                (if difference
                    (push (list id difference) failures)
                    (incf matches)))))))
      (dolist (failure (nreverse failures))
        (format t "inline structure mismatch ~A: ~S~%"
                (first failure) (second failure)))
      (format t "Inline structures: ~D CommonMark + ~D GFM; ~D match, ~D excluded.~%"
              (length commonmark) (length gfm) matches (length exclusions))
      (expect matches :to-equal (- 682 (length exclusions)))
      (expect failures :to-equal nil))))

(describe "Inline parser edge cases"
  (it "preserves malformed entities and unresolved inline references"
    (let* ((invalid-entities '("&" "&x" "&;" "&#" "&#;" "&#x;"
                               "&#12" "&unknown;"
                               "&abcdefghijklmnopqrstuvwxyz123456;"))
           (literal-values
             (mapcar (lambda (input)
                       (apply #'concatenate 'string
                              (mapcar #'cl-markdown-kit:text-value
                                      (inline-parser-test-parse input))))
                     invalid-entities))
           (missing-footnote (inline-parser-test-parse "[^missing]"))
           (empty-footnote (inline-parser-test-parse "[^]"))
           (unclosed-footnote (inline-parser-test-parse "[^missing"))
           (blank-reference (inline-parser-test-parse "[   ]"))
           (blank-full-reference (inline-parser-test-parse "[missing][   ]"))
           (unclosed-reference (inline-parser-test-parse "[missing")))
      (expect literal-values :to-equal invalid-entities)
      (expect (and (notany #'cl-markdown-kit:footnote-reference-p
                           missing-footnote)
                   (notany #'cl-markdown-kit:footnote-reference-p
                           empty-footnote)
                   (notany #'cl-markdown-kit:footnote-reference-p
                           unclosed-footnote)
                   (notany #'cl-markdown-kit:link-p blank-reference)
                   (notany #'cl-markdown-kit:link-p blank-full-reference)
                   (notany #'cl-markdown-kit:link-p unclosed-reference))
              :to-be-truthy)))

  (it "checks raw HTML names, attributes, and truncated openers"
    (let ((valid '("<tag _name>" "<tag :name>" "<tag ns:attr=value>"
                   "<tag title='value'>" "<tag title=bare>"))
          (invalid '("<1tag>" "<tag .name>" "<tag title=>"
                     "<tag title=`value`>" "<tag title='unclosed>"
                     "<tag" "</tag" "<" "<!" "<!D")))
      (expect (every (lambda (source)
                       (let ((node (first (inline-parser-test-parse source))))
                         (and (cl-markdown-kit:html-inline-p node)
                              (string= (cl-markdown-kit:html-inline-body node)
                                       source))))
                     valid)
              :to-be-truthy)
      (expect (every (lambda (source)
                       (notany #'cl-markdown-kit:html-inline-p
                               (inline-parser-test-parse source)))
                     invalid)
              :to-be-truthy)))

  (it "handles empty destinations and dangling link escapes"
    (let* ((empty (first (inline-parser-test-parse "[x]()")))
           (angle (first (inline-parser-test-parse "[x](<a\\q>)")))
           (invalid (mapcar #'inline-parser-test-parse
                            '("[x](a\\" "[x](/ \"title\\"
                              "[x](" "[x](<a" "[x](<a\\"))))
      (expect (list (cl-markdown-kit:link-destination empty)
                    (cl-markdown-kit:link-destination angle))
              :to-equal '("" "a%5Cq"))
      (expect (every (lambda (nodes)
                       (notany #'cl-markdown-kit:link-p nodes))
                     invalid)
              :to-be-truthy)))

  (it "bounds angle and bare autolinks at invalid characters"
    (let* ((invalid-autolinks
             (mapcar #'inline-parser-test-parse
                     '("<a_b:value>" "<https://example.com/<x>>"
                       "<a@b..c>")))
           (url-nodes (inline-parser-test-parse
                       "http://example.com/path<tail"))
           (url (find-if #'cl-markdown-kit:autolink-p url-nodes))
           (without-entity
             "http://example.com/path;")
           (empty-entity "http://example.com/path&;")
           (invalid-entity "http://example.com/path&x-y;")
           (punctuation "?!."))
      (expect (every (lambda (nodes)
                       (notany #'cl-markdown-kit:autolink-p nodes))
                     invalid-autolinks)
              :to-be-truthy)
      (expect (and url
                   (string= (cl-markdown-kit:autolink-display-text url)
                            "http://example.com/path")
                   (string= (cl-markdown-kit:text-value (car (last url-nodes)))
                            "<tail"))
              :to-be-truthy)
      (expect (list (cl-markdown-kit::%ip-trim-url-end
                     without-entity 0 (length without-entity))
                    (cl-markdown-kit::%ip-trim-url-end
                     empty-entity 0 (length empty-entity))
                    (cl-markdown-kit::%ip-trim-url-end
                     invalid-entity 0 (length invalid-entity))
                    (cl-markdown-kit::%ip-trim-url-end punctuation 0 0)
                    (cl-markdown-kit::%ip-trim-url-end
                     punctuation 0 (length punctuation)))
              :to-equal (list (length without-entity)
                              (length empty-entity)
                              (length invalid-entity) 0 0))))

  (it "handles delimiter leftovers, image link activation, and bounded ranges"
    (let* ((tilde (inline-parser-test-parse "~~a ~~b~~~"))
           (crossed (inline-parser-test-parse "*a **b* c**"))
           (image-text (inline-parser-test-parse "![a [link](/target)]"))
           (tab-break
             (inline-parser-test-parse (format nil "a~C~%b" #\Tab)))
           (middle (inline-parser-test-parse-range "*word*" nil 1 5))
           (prefix (inline-parser-test-parse-range "*word*" nil 0 5))
           (suffix (inline-parser-test-parse-range "*word*" nil 1 6))
           (partial-footnote
             (inline-parser-test-parse-range "![^Note]" nil 0 4)))
      (expect (notany #'cl-markdown-kit:strikethrough-p tilde)
              :to-be-truthy)
      (expect (some #'cl-markdown-kit:emphasis-p crossed)
              :to-be-truthy)
      (expect (and (notany #'cl-markdown-kit:image-p image-text)
                   (some #'cl-markdown-kit:link-p image-text))
              :to-be-truthy)
      (expect (cl-markdown-kit:soft-break-p (second tab-break))
              :to-be-truthy)
      (expect (notany #'cl-markdown-kit:footnote-reference-p partial-footnote)
              :to-be-truthy)
      (expect (list (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value middle))
                    (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value prefix))
                    (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value suffix)))
              :to-equal '("word" "*word" "word*"))))

  (it "selects tilde openers only for matching one or two marker runs"
    (let* ((openers (make-hash-table :test #'eql))
           (character (code-char 126))
           (buckets (cl-markdown-kit::%ip-delimiter-buckets openers character))
           (opener (cl-markdown-kit::%ip-make-delimiter
                    character 0 2 t nil))
           (closer (cl-markdown-kit::%ip-make-delimiter
                    character 4 2 nil t))
           (asterisk-buckets
             (cl-markdown-kit::%ip-delimiter-buckets openers #\*))
           (asterisk-opener
             (cl-markdown-kit::%ip-make-delimiter #\* 8 1 t nil))
           (asterisk-closer
             (cl-markdown-kit::%ip-make-delimiter #\* 10 1 nil t)))
      (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 1
            (aref buckets 6) (list opener)
            (cl-markdown-kit::%ip-delimiter-remaining closer) 1)
      (expect (cl-markdown-kit::%ip-best-opener openers closer)
              :to-be opener)
      (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 3)
      (expect (cl-markdown-kit::%ip-best-opener openers closer)
              :to-equal nil)
      (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 2)
      (expect (cl-markdown-kit::%ip-best-opener openers closer)
              :to-equal nil)
      (setf (cl-markdown-kit::%ip-delimiter-remaining closer) 2)
      (expect (cl-markdown-kit::%ip-best-opener openers closer)
              :to-be opener)
      (setf (cl-markdown-kit::%ip-delimiter-remaining asterisk-opener) 1
            (aref asterisk-buckets 1) (list asterisk-opener)
            (cl-markdown-kit::%ip-delimiter-remaining asterisk-closer) 1)
      (expect (cl-markdown-kit::%ip-best-opener openers asterisk-closer)
              :to-be asterisk-opener)))

  (it "keeps only complete atoms and pairs inside a rendered range"
    (let* ((text "abcdefgh")
           (buffer (cl-markdown-kit:make-source-buffer text))
           (context
             (cl-markdown-kit::%ip-make-context
              text (cl-markdown-kit::source-buffer-boundary-map buffer)
              nil 4096 nil nil nil nil nil nil nil))
           (atoms (list
                   (cl-markdown-kit::%ip-make-atom
                    1 2 (cl-markdown-kit::%ip-text-node context "b" 1 2))
                   (cl-markdown-kit::%ip-make-atom
                    3 4 (cl-markdown-kit::%ip-text-node context "d" 3 4))
                   (cl-markdown-kit::%ip-make-atom
                    5 7 (cl-markdown-kit::%ip-text-node context "fg" 5 7))
                   (cl-markdown-kit::%ip-make-atom
                    6 7 (cl-markdown-kit::%ip-text-node context "g" 6 7))))
           (pairs (list
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 0 0 1 1)
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 2 3 5 6)
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 5 6 6 7)
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 6 7 7 8)))
           (nodes (cl-markdown-kit::%ip-build-range
                   context 2 6 atoms pairs))
           (emphasis (first nodes)))
      (expect (and (= (length nodes) 1)
                   (cl-markdown-kit:emphasis-p emphasis)
                   (string= (apply #'concatenate 'string
                                   (mapcar #'cl-markdown-kit:text-value
                                           (cl-markdown-kit:emphasis-children
                                            emphasis)))
                            "de"))
              :to-be-truthy))))
