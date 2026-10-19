(in-package #:cl-markdown-kit/test)

(describe "Inline parser indexes and scanning"
(it "decodes escaped punctuation and named and numeric references"
    (let* ((nodes (inline-parser-test-parse "\\* &amp; &#x1F600;")))
      (expect (mapcar #'cl-markdown-kit:text-value nodes)
              :to-equal (list (format nil "* & ~C" (code-char #x1F600))))
      (expect (mapcar #'inline-parser-test-span nodes)
              :to-equal '((0 18 1 1 1 19)))))

(it "applies the HTML C1 numeric-reference replacements"
    (let* ((nodes (inline-parser-test-parse
                   "&#128; &#x80; &#130; &#x9F; &#129;"))
           (reference-source
             (format nil "[reference]: /&#128;~%~%[reference] [inline](/&#128;)"))
           (html (cl-markdown-kit:write-html
                  (cl-markdown-kit:parse-string reference-source)))
           (expected-href "href=\"/%E2%82%AC\"")
           (count 0)
           (start 0))
      (expect (mapcar #'cl-markdown-kit:text-value nodes)
              :to-equal
              (list (format nil "~C ~C ~C ~C ~C"
                            (code-char #x20AC) (code-char #x20AC)
                            (code-char #x201A) (code-char #x0178)
                            (code-char #x81))))
      (loop for position = (search expected-href html :start2 start)
            while position
            do (incf count)
               (setf start (+ position (length expected-href))))
      (expect count :to-equal 2)))

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
      (expect (and (typep html 'cl-markdown-kit:html-inline)
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
           (autolinks (remove-if-not #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) nodes)))
      (expect (mapcar #'cl-markdown-kit:autolink-kind autolinks)
              :to-equal '(:angle-email :bare-url :bare-url))
      (expect (mapcar #'cl-markdown-kit:autolink-display-text autolinks)
              :to-equal '("a@b" "http://example.com/a(b(c))"
                          "http://example.com/a(b)"))
      (expect (some (lambda (node)
                      (and (typep node 'cl-markdown-kit:text)
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

(cl-weave:it-each
    ((#x203F nil)
     (#x2014 nil)
     (#x3008 nil)
     (#x3009 nil)
     (#x00AB nil)
     (#x2019 nil)
     (#x00A1 nil)
     (#x00D7 nil)
     (#x00A3 nil)
     (#x02C2 nil)
     (#x2603 nil)
     (#x0041 t))
    "classifies Unicode punctuation and symbols at U+~X with emphasis ~S"
    (codepoint expected-emphasis)
  (let* ((source (format nil "*~C*bravo." (code-char codepoint)))
         (html (cl-markdown-kit:write-html
                (cl-markdown-kit:parse-string source))))
    (expect (not (null (search "<em>" html))) :to-equal expected-emphasis)))

(it "recognizes GFM email autolinks and rejects invalid boundaries"
    (let* ((email (inline-parser-test-parse "Email me at:scyther@pokemon.com"))
           (invalid-email (inline-parser-test-parse "(_A_@_.A")))
      (expect (and (typep (second email) 'cl-markdown-kit:autolink)
                   (string= (cl-markdown-kit:autolink-destination
                             (second email))
                            "mailto:scyther@pokemon.com")
                   (notany #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) invalid-email))
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
      (expect (list (not (null (cl-markdown-kit::%pc-gfm-domain-character-p #\A)))
                    (not (null (cl-markdown-kit::%pc-gfm-domain-character-p #\-)))
                    (not (null
                          (cl-markdown-kit::%pc-gfm-domain-character-p (code-char #x1F344))))
                    (not (null (cl-markdown-kit::%pc-gfm-domain-character-p space)))
                    (not (null
                          (cl-markdown-kit::%pc-gfm-domain-character-p punctuation)))
                    (not (null
                          (cl-markdown-kit::%pc-gfm-domain-character-p format-control)))
                    (not (null
                          (cl-markdown-kit::%pc-gfm-domain-character-p private-use))))
              :to-equal '(t t t nil nil nil nil))
      (expect (list (typep mailto 'cl-markdown-kit:autolink)
                    (cl-markdown-kit:autolink-destination mailto)
                    (notany #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) invalid-mailto)
                    (typep xmpp 'cl-markdown-kit:autolink)
                    (cl-markdown-kit:autolink-destination xmpp)
                    (notany #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) invalid-xmpp)
                    (notany #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) invalid-hyphens))
              :to-equal '(t "mailto:mail@example.com" t t
                            "xmpp:chat@example.com" t t))))

(it "recognizes multiline raw HTML tags with later attributes"
    (let* ((source (format nil "<a foo=\"bar\" bam = 'baz <em>\"</em>'~%_boolean zoop:33=zoop:33 />"))
           (html (first (inline-parser-test-parse source))))
      (expect (and (typep html 'cl-markdown-kit:html-inline)
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

(cl-weave:it-each
    (("<tag>" t)
     ("<tag/>" t)
     ("<tag disabled>" t)
     ("<tag title=bare>" t)
     ("<tag title='single'>" t)
     ("<tag title=\"double\">" t)
     ("</tag>" t)
     ("<!-->" t)
     ("<!--->" t)
     ("<!-- comment -->" t)
     ("<?pi?>" t)
     ("<![CDATA[x]]>" t)
     ("<!DOCTYPE html>" t)
     ("<!doctype html>" t)
     ("<tag title=>" nil)
     ("<tag title=\"unclosed>" nil)
     ("<tag" nil)
     ("<tag " nil)
     ("<tag title" nil)
     ("<tag title=" nil)
     ("<tag title=bare" nil)
     ("</" nil)
     ("<!-- unclosed" nil)
     ("<?unclosed" nil)
     ("<![CDATA[unclosed" nil)
     ("<a b=\"c\"d=\"e\">" nil))
    "classifies raw HTML forms for ~S"
    (source expected-html)
  (let* ((nodes (inline-parser-test-parse source))
         (node (first nodes)))
    (expect (if expected-html
                (and (typep node 'cl-markdown-kit:html-inline)
                     (string= (cl-markdown-kit:html-inline-body node)
                              source))
                (notany #'(lambda (candidate)
                            (typep candidate 'cl-markdown-kit:html-inline))
                        nodes))
            :to-be-truthy)))
(it "checks raw HTML scanner boundaries"
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
          :to-equal '(11 6 13 10 nil nil nil)))

(cl-weave:it-each
    (("<1scheme:value>")
     ("<a b:c>")
     ("<a@-host>")
     ("<a@host.>")
     ("<a@host_name>")
     ("name@.example")
     ("person@example.com_"))
    "rejects invalid email autolink boundaries for ~S"
    (source)
  (let ((nodes (inline-parser-test-parse source)))
    (expect (notany #'(lambda (node)
                        (typep node 'cl-markdown-kit:autolink))
                    nodes)
            :to-be-truthy)))
(it "accepts valid angle URI and bare email autolinks"
  (let* ((uri (first (inline-parser-test-parse "<git+ssh:user@host>")))
         (email (first (inline-parser-test-parse "person@example.com")))
         (tagged-email
           (first (inline-parser-test-parse "first.last+tag@example.com"))))
    (expect (list (cl-markdown-kit:autolink-kind uri)
                  (cl-markdown-kit:autolink-destination uri)
                  (cl-markdown-kit:autolink-kind email)
                  (cl-markdown-kit:autolink-destination email)
                  (cl-markdown-kit:autolink-display-text tagged-email))
            :to-equal '(:angle-uri "git+ssh:user@host" :bare-email
                        "mailto:person@example.com"
                        "first.last+tag@example.com"))
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
           (link (find-if #'(lambda (node) (typep node 'cl-markdown-kit:autolink)) nodes)))
      (expect (list (cl-markdown-kit:autolink-kind link)
                    (cl-markdown-kit:autolink-display-text link))
              :to-equal '(:bare-url "http://example.com/?q=a"))
      (expect (string= (cl-markdown-kit:text-value (car (last nodes))) "&.")
              :to-be-truthy))))
