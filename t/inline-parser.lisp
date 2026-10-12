(in-package #:cl-markdown-kit/test)

(defun inline-parser-test-parse (text &optional reference-index
                                 &key (max-inline-delimiter-depth 4096))
  (let* ((buffer (cl-markdown-kit:make-source-buffer text))
         (index (or reference-index (make-hash-table :test #'equal))))
    (cl-markdown-kit::parse-inline
     (cl-markdown-kit:source-buffer-normalized-text buffer)
     (cl-markdown-kit::source-buffer-boundary-map buffer)
     index
     :max-inline-delimiter-depth max-inline-delimiter-depth)))

(defun inline-parser-test-parse-range (text link-allowed)
  (let* ((buffer (cl-markdown-kit:make-source-buffer text))
         (normalized (cl-markdown-kit:source-buffer-normalized-text buffer))
         (boundary-map (cl-markdown-kit::source-buffer-boundary-map buffer))
         (index (make-hash-table :test #'equal))
         (limit 4096)
         (backtick-next (cl-markdown-kit::%ip-build-backtick-index normalized))
         (html-closes (cl-markdown-kit::%ip-build-html-closes normalized)))
    (multiple-value-bind (opens closes prefix)
        (cl-markdown-kit::%ip-build-bracket-index
         normalized backtick-next html-closes limit)
      (cl-markdown-kit::%ip-parse-range
       (cl-markdown-kit::%ip-make-context
        normalized boundary-map index limit backtick-next opens closes prefix
        (cl-markdown-kit::%ip-build-bare-links normalized) html-closes)
       0 (length normalized) link-allowed 0))))

(defun inline-parser-test-span (node)
  (let ((span (cl-markdown-kit:node-span node)))
    (list (cl-parser-kit:span-start span)
          (cl-parser-kit:span-end span)
          (cl-parser-kit:span-start-line span)
          (cl-parser-kit:span-start-column span)
          (cl-parser-kit:span-end-line span)
          (cl-parser-kit:span-end-column span))))

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

  (it "matches only escaped square brackets in reference labels"
    (let ((index (make-hash-table :test #'equal)))
      (setf (gethash "ref[" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "ref[" :normalized-label "ref["
             :destination "/bracket" :eligible-p t)
            (gethash "foo!" index)
            (cl-markdown-kit:make-link-reference-definition
             :raw-label "foo!" :normalized-label "foo!"
             :destination "/punctuation" :eligible-p t))
      (let ((bracket-link
              (first (inline-parser-test-parse "[x][ref\\[]" index)))
            (escaped-punctuation
              (inline-parser-test-parse "[x][foo\\!]" index)))
        (expect (list (cl-markdown-kit:link-destination bracket-link)
                      (inline-parser-test-span bracket-link)
                      (some #'cl-markdown-kit:link-p escaped-punctuation))
                :to-equal '("/bracket" (0 10 1 1 1 11) nil)))))

  (it "creates GFM strikethrough and resolves footnote references"
    (let* ((index (inline-parser-test-reference-index))
           (strike (first (inline-parser-test-parse "~~gone~~" index)))
           (footnote (first (inline-parser-test-parse "[^Note]" index))))
      (expect (list (cl-markdown-kit:strikethrough-delimiter-character strike)
                    (inline-parser-test-span strike)
                    (cl-markdown-kit:text-value
                     (first (cl-markdown-kit:strikethrough-children strike))))
              :to-equal (list #\~ '(0 8 1 1 1 9) "gone"))
      (expect (list (cl-markdown-kit:footnote-reference-raw-label footnote)
                    (cl-markdown-kit:footnote-reference-normalized-label footnote)
                    (inline-parser-test-span footnote))
              :to-equal '("Note" "note" (0 7 1 1 1 8)))))

  (it "signals the configured delimiter-depth limit"
    (expect (inline-parser-test-signals-depth-p "*a *b* c*" 1)
            :to-be-truthy)
    (expect (inline-parser-test-signals-depth-p "[[x]]" 1)
            :to-be-truthy)
    (expect (not (inline-parser-test-signals-depth-p "*a*" 1))
            :to-be-truthy)
    (expect (not (inline-parser-test-signals-depth-p "[x]" 1))
            :to-be-truthy))

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
                          (inline-parser-test-parse "[x](a(b)")
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
              :to-equal '(t t t t))
      (expect (and (cl-markdown-kit:link-p spaced-angle)
                   (string= (cl-markdown-kit:link-destination spaced-angle)
                            "a%20b"))
              :to-be-truthy)))

  (it "covers raw HTML tag attributes, closing forms, and terminators"
    (let ((valid '("<tag>" "<tag/>" "<tag disabled>"
                   "<tag title=bare>" "<tag title='single'>"
                   "<tag title=\"double\">" "</tag>" "<!-->" "<!--->"
                   "<!-- comment -->" "<?pi?>" "<![CDATA[x]]>"
                   "<!DOCTYPE html>")))
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
                       "<!-- unclosed" "<?unclosed" "<![CDATA[unclosed"
                       "<!doctype html>"))
              :to-be-truthy)
      (expect (list (cl-markdown-kit::%ip-raw-html-end "<!--body-->" 0 11)
                    (cl-markdown-kit::%ip-raw-html-end "<?pi?>" 0 6)
                    (cl-markdown-kit::%ip-raw-html-end "<![CDATA[x]]>" 0 13)
                    (cl-markdown-kit::%ip-raw-html-end "<!DOCTYPE>" 0 10))
              :to-equal '(11 6 13 10))))

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

  (it "uses the fallback angle-autolink scanner without an HTML index"
    (expect (multiple-value-list
             (cl-markdown-kit::%ip-angle-autolink
              "<https://example.com>" 0 21))
            :to-equal '("https://example.com" "https://example.com"
                        :angle-uri 21)))

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
