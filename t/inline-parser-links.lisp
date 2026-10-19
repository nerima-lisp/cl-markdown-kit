(in-package #:cl-markdown-kit/test)

(describe "Inline parser links and references"
(it "disables GFM-only inline syntax in CommonMark mode"
    (let* ((index (inline-parser-test-reference-index))
           (source "www.example.com foo@bar.example.com ~~gone~~ [^Note]")
           (commonmark (inline-parser-test-parse source index :gfm nil))
           (gfm (inline-parser-test-parse source index :gfm t)))
      (expect (list (notany #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) commonmark)
                    (notany #'(lambda (node) (typep node 'cl-markdown-kit:strikethrough)) commonmark)
                    (notany #'(lambda (node) (typep node 'cl-markdown-kit:footnote-reference)) commonmark)
                    (some #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) gfm)
                    (some #'(lambda (node) (typep node 'cl-markdown-kit:strikethrough)) gfm)
                    (some #'(lambda (node) (typep node 'cl-markdown-kit:footnote-reference)) gfm))
              :to-equal '(t t t t t t))))

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
           (links (remove-if-not #'(lambda (node) (typep node 'cl-markdown-kit:link)) nested))
           (image-index (inline-parser-test-reference-index))
           (image (first (inline-parser-test-parse
                          "![a [b](/inner)][R]" image-index))))
      (expect (= (length links) 1) :to-be-truthy)
      (expect (and (typep image 'cl-markdown-kit:image)
                   (= (length (remove-if-not #'(lambda (node) (typep node 'cl-markdown-kit:link))
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
                       (notany #'(lambda (node) (typep node 'cl-markdown-kit:link)) nodes))
                     (list angle-line-break control-destination
                           delete-destination))
              :to-be-truthy)
      (expect (and (typep unicode-space-destination 'cl-markdown-kit:link)
                   (string= (cl-markdown-kit:link-destination
                             unicode-space-destination)
                            "a%C2%A0b"))
              :to-be-truthy)))

(it "rejects unmatched link destination parentheses and preserves escaped title text"
    (let* ((unmatched (inline-parser-test-parse "[x](a((b)"))
           (escaped-title (first (inline-parser-test-parse
                                  "[x](dest \"a\\q\")"))))
      (expect (notany #'(lambda (node) (typep node 'cl-markdown-kit:link)) unmatched) :to-be-truthy)
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
                          (cl-markdown-kit::%pc-parse-title "\"title\\" 0 7 :reject-nested-parentheses-p t)))
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
             (links (remove-if-not #'(lambda (node) (typep node 'cl-markdown-kit:link)) nodes)))
        (expect (list (length links)
                      (cl-markdown-kit:link-destination (first links))
                      (mapcar #'cl-markdown-kit:text-value
                              (remove-if-not #'(lambda (node) (typep node 'cl-markdown-kit:text)) nodes)))
                :to-equal '(1 "/baz" ("[foo]"))))))

(it "treats an invalid bracketed reference label as text"
    (let ((index (make-hash-table :test #'equal)))
      (setf (gethash "foo" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "foo" :normalized-label "foo"
             :destination "/foo" :eligible-p t))
      (let* ((nodes (inline-parser-test-parse "[foo][bar[baz]]" index))
             (link (find-if #'(lambda (node) (typep node 'cl-markdown-kit:link)) nodes)))
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
                      (some #'(lambda (node) (typep node 'cl-markdown-kit:link)) escaped-punctuation))
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
                   (typep (second image-prefix) 'cl-markdown-kit:footnote-reference))
              :to-be-truthy)))

(cl-weave:it-each
    (("[x](<a
b>)")
     ("[x](<unterminated")
     ("[x](a(b)")
     ("[x](a(b")
     ("[x](a (nested(inner)))")
     ("[x](a \"unclosed)"))
    "rejects malformed link destinations ~S"
    (source)
  (let ((nodes (inline-parser-test-parse source)))
    (expect (notany #'(lambda (node)
                        (typep node 'cl-markdown-kit:link))
                    nodes)
            :to-be-truthy)))
(it "parses valid link destinations, escapes, whitespace, and title delimiters"
    (let* ((escaped-angle (first (inline-parser-test-parse "[x](<a\\>b>)")))
           (escaped-bare (first (inline-parser-test-parse "[x](a\\)b)")))
           (nested-bare (first (inline-parser-test-parse "[x](a(b)c)")))
           (empty (first (inline-parser-test-parse "[x]( )")))
           (spaced-angle (first (inline-parser-test-parse "[x](<a b>)")))
           (multiline (first (inline-parser-test-parse
                              (format nil "[x]( ~% a \"a\\!b\" )"))))
           (single-quoted (first (inline-parser-test-parse "[x](a 'title')")))
           (parenthesized (first (inline-parser-test-parse "[x](a (title))"))))
      (expect (mapcar #'cl-markdown-kit:link-destination
                      (list escaped-angle escaped-bare nested-bare empty))
              :to-equal '("a%3Eb" "a)b" "a(b)c" ""))
      (expect (mapcar #'cl-markdown-kit:link-title
                      (list multiline single-quoted parenthesized))
              :to-equal '("a!b" "title" "title"))
      (expect (and (typep spaced-angle 'cl-markdown-kit:link)
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

(it "keeps a bare-link candidate inside its containing link text"
    (let ((link (first (inline-parser-test-parse "[x www.example.com](/target)"))))
      (expect (and (typep link 'cl-markdown-kit:link)
                   (string= (cl-markdown-kit:link-destination link) "/target")
                   (some (lambda (node)
                           (and (typep node 'cl-markdown-kit:text)
                                (search "www.example.com"
                                        (cl-markdown-kit:text-value node))))
                         (cl-markdown-kit:link-children link)))
              :to-be-truthy)))
(it "calls the link match continuation with parsed fields"
  (let* ((text "[x](/target)")
         (context (inline-parser-test-context text))
         (match-calls 0)
         (fail-calls 0)
         (continuation-calls 0))
    (cl-weave:with-continuation-result (result next calledp)
      (cl-markdown-kit::%ip-link-candidate
       context 0 2 nil (length text) t
       (lambda (destination title kind label link-end)
         (incf match-calls)
         (incf continuation-calls)
         (funcall next (list destination title kind label link-end)))
       (lambda ()
         (incf fail-calls)
         (incf continuation-calls)
         (funcall next :failure)))
      (expect calledp :to-be-truthy)
      (expect result :to-equal '("/target" nil :inline nil 12))
      (expect (list match-calls fail-calls continuation-calls)
              :to-equal '(1 0 1)))))
(it "calls the link failure continuation when no candidate matches"
  (let* ((text "[x]")
         (context (inline-parser-test-context text)))
    (cl-weave:with-continuation-result (result next calledp)
      (cl-markdown-kit::%ip-link-candidate
       context 0 2 nil (length text) t
       (lambda (&rest values)
         (declare (ignore values)))
       next)
      (expect calledp :to-be-truthy)
      (expect result :to-be nil))))

(it "allows links inside image descriptions"
    (let ((image-text (inline-parser-test-parse "![a [link](/target)]")))
      (expect (and (notany #'(lambda (node)
                              (typep node 'cl-markdown-kit:image))
                          image-text)
                   (some #'(lambda (node)
                             (typep node 'cl-markdown-kit:link))
                         image-text))
              :to-be-truthy))))
