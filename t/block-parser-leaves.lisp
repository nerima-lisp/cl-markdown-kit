(in-package #:cl-markdown-kit/test)

(describe "Block parser leaf blocks"
          (it "expands block character references only once"
              (let* ((fence (make-string 3 :initial-element #\`))
                     (info-node
                      (first
                       (block-parser-test-nodes-of-type
                        (block-parser-test-document
                         (block-parser-test-result
                          (format nil "~A&amp;copy;~%code~%~A" fence fence)))
                        #'cl-markdown-kit:code-block-p)))
                     (reference-node
                      (first
                       (block-parser-test-nodes-of-type
                        (block-parser-test-document
                         (block-parser-test-result
                          "[&amp;copy;]: <&amp;copy;> \"&amp;copy;\""))
                        #'cl-markdown-kit:link-reference-definition-p)))
                     (footnote-node
                      (first
                       (block-parser-test-nodes-of-type
                        (block-parser-test-document
                         (block-parser-test-result "[^&amp;copy;]: note"))
                        #'cl-markdown-kit:footnote-definition-p))))
                (expect
                 (list (cl-markdown-kit:code-block-info-string info-node)
                       (cl-markdown-kit:link-reference-definition-raw-label
                        reference-node)
                       (cl-markdown-kit:link-reference-definition-normalized-label
                        reference-node)
                       (cl-markdown-kit:link-reference-definition-destination
                        reference-node)
                       (cl-markdown-kit:link-reference-definition-title
                        reference-node)
                       (cl-markdown-kit:footnote-definition-raw-label
                        footnote-node)
                       (cl-markdown-kit:footnote-definition-normalized-label
                        footnote-node))
                 :to-equal
                 '("&copy;" "&amp;copy;"
                            "&copy;"
                            "&copy;"
                            "&copy;"
                            "&amp;copy;"
                            "&copy;"))))
          (it
           "parses thematic breaks, blank separators, and excludes line endings from spans"
           (let* ((source (format nil "***~%~%---~%"))
                  (document
                   (block-parser-test-document
                    (block-parser-test-result source)))
                  (children (cl-markdown-kit:document-children document)))
             (expect (length children) :to-equal 2)
             (expect
              (every
               #'(lambda (node)
                   (typep node 'cl-markdown-kit:thematic-break))
               children)
              :to-be-truthy)
             (expect
              (mapcar #'cl-markdown-kit:thematic-break-marker-character
                      children)
              :to-equal
              (list #\* #\-))
             (expect
              (mapcar #'cl-markdown-kit:thematic-break-marker-count children)
              :to-equal
              '(3 3))
             (expect
              (and (block-parser-test-span-is (first children) 0 3)
                   (block-parser-test-span-is (second children) 5 8))
              :to-be-truthy)))
          (it "retains ATX and setext heading syntax in the block structure"
              (let* ((atx-source (format nil "## title ###~%"))
                     (atx
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result atx-source))))
                     (setext-source (format nil "Title~%===~%"))
                     (setext
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result setext-source)))))
                (expect
                 (and (typep atx 'cl-markdown-kit:heading)
                      (= (cl-markdown-kit:heading-level atx) 2)
                      (eq (cl-markdown-kit:heading-style atx) :atx)
                      (char= (cl-markdown-kit:heading-marker-character atx) #\#)
                      (= (cl-markdown-kit:heading-opening-marker-length atx) 2)
                      (= (cl-markdown-kit:heading-closing-marker-length atx) 3)
                      (block-parser-test-span-is atx 0 12))
                 :to-be-truthy)
                (expect
                 (and (typep setext 'cl-markdown-kit:heading)
                      (= (cl-markdown-kit:heading-level setext) 1)
                      (eq (cl-markdown-kit:heading-style setext) :setext)
                      (char= (cl-markdown-kit:heading-marker-character setext)
                             #\=)
                      (block-parser-test-span-is setext 0 9))
                 :to-be-truthy)))
          (it
           "normalizes CommonMark paragraph continuation and setext heading text"
           (let ((cases
                  (list (list "commonmark-0049" (list (format nil "Foo~%***")))
                        (list "commonmark-0070"
                              (list (format nil "foo~%# bar")))
                        (list "commonmark-0087" (list (format nil "Foo~%---")))
                        (list "commonmark-0113" (list (format nil "Foo~%bar")))
                        (list "commonmark-0223"
                              (list (format nil "aaa~%bbb~%ccc")))
                        (list "commonmark-0238"
                              (list (format nil "foo~%- bar")))
                        (list "commonmark-0291"
                              (list (format nil "A paragraph~%with two lines.")))
                        (list "commonmark-0312"
                              (list "a" "b" "c" (format nil "d~%- e")))
                        (list "commonmark-0082"
                              (list (format nil "Foo *bar~%baz*")))
                        (list "commonmark-0089" (list "Foo")))))
             (dolist
                 (case cases)
               (let* ((example
                       (block-parser-test-commonmark-example (first case)))
                      (source (block-parser-test-example-source example))
                      (result
                       (block-parser-test-result source :front-matter nil))
                      (document (block-parser-test-document result))
                      (inline-fragments
                       (loop for inline-owner in (block-parser-test-nodes-of-type
                                                  document
                                                  (lambda (node)
                                                    (or
                                                     (typep node
                                                            'cl-markdown-kit:paragraph)
                                                     (typep node
                                                            'cl-markdown-kit:heading))))
                             append (mapcar
                                     #'cl-markdown-kit::inline-fragment-text
                                     (block-parser-test-fragments-for result
                                                                      inline-owner)))))
                 (expect inline-fragments :to-equal (second case))
                 (expect (block-parser-test-ranges-valid-p document source)
                         :to-be-truthy)))))
          (it "recognizes delimited front matter as a source-backed leaf block"
              (let* ((source (format nil "---~%title: x~%---~%# Body"))
                     (document
                      (block-parser-test-document
                       (block-parser-test-result source)))
                     (children (cl-markdown-kit:document-children document))
                     (front-matter (first children))
                     (unclosed-source (format nil "---~%title: x~%# Body"))
                     (unclosed-children
                      (cl-markdown-kit:document-children
                       (block-parser-test-document
                        (block-parser-test-result unclosed-source)))))
                (expect
                 (and (= (length children) 2)
                      (cl-markdown-kit:front-matter-p front-matter)
                      (string=
                       (cl-markdown-kit:front-matter-raw-text front-matter)
                       (format nil "title: x~%"))
                      (string=
                       (cl-markdown-kit:front-matter-opening-delimiter
                        front-matter)
                       "---")
                      (string=
                       (cl-markdown-kit:front-matter-closing-delimiter
                        front-matter)
                       "---")
                      (=
                       (cl-parser-kit:span-end
                        (cl-markdown-kit:node-span front-matter))
                       16)
                      (typep (second children) 'cl-markdown-kit:heading)
                      (= (length unclosed-children) 3)
                      (typep (first unclosed-children)
                             'cl-markdown-kit:thematic-break)
                      (typep (second unclosed-children)
                             'cl-markdown-kit:paragraph)
                      (typep (third unclosed-children) 'cl-markdown-kit:heading))
                 :to-be-truthy)))
          (it "defers paragraph text as a source-mapped inline fragment"
              (let* ((source (format nil "plain paragraph~%~%"))
                     (result (block-parser-test-result source))
                     (paragraph
                      (block-parser-test-only-child
                       (block-parser-test-document result)))
                     (fragments
                      (cl-markdown-kit::block-parse-result-inline-fragments
                       result))
                     (fragment (first fragments)))
                (expect
                 (and (= (length fragments) 1)
                      (typep paragraph 'cl-markdown-kit:paragraph)
                      (eq (cl-markdown-kit::inline-fragment-node fragment)
                          paragraph)
                      (string= (cl-markdown-kit::inline-fragment-text fragment)
                               "plain paragraph")
                      (=
                       (cl-markdown-kit::boundary-map-length
                        (cl-markdown-kit::inline-fragment-boundary-map fragment))
                       (length (cl-markdown-kit::inline-fragment-text fragment)))
                      (block-parser-test-span-is paragraph 0 15))
                 :to-be-truthy)))
          (it "preserves fenced and indented code block attributes"
              (let* ((fenced-source (format nil "```lisp~%(print 1)~%```"))
                     (fenced
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result fenced-source))))
                     (indented-source (format nil "    one~%    two~%~%"))
                     (indented
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result indented-source)))))
                (expect
                 (and (typep fenced 'cl-markdown-kit:code-block)
                      (eq (cl-markdown-kit:code-block-style fenced) :fenced)
                      (char= (cl-markdown-kit:code-block-fence-character fenced)
                             #\`)
                      (= (cl-markdown-kit:code-block-fence-length fenced) 3)
                      (string= (cl-markdown-kit:code-block-info-string fenced)
                               "lisp")
                      (string= (cl-markdown-kit:code-block-body fenced)
                               (format nil "(print 1)~%"))
                      (block-parser-test-span-is fenced
                                                 0
                                                 (length fenced-source)))
                 :to-be-truthy)
                (expect
                 (and (typep indented 'cl-markdown-kit:code-block)
                      (eq (cl-markdown-kit:code-block-style indented) :indented)
                      (null (cl-markdown-kit:code-block-info-string indented))
                      (search "one" (cl-markdown-kit:code-block-body indented))
                      (search "two" (cl-markdown-kit:code-block-body indented))
                      (block-parser-test-ranges-valid-p indented
                                                        indented-source))
                 :to-be-truthy)))
          (it "excludes trailing blank lines from indented code blocks"
              (let* ((source (format nil "    code~%    ~%~%"))
                     (children
                      (cl-markdown-kit:document-children
                       (block-parser-test-document
                        (block-parser-test-result source))))
                     (code (first children)))
                (expect
                 (and (= (length children) 1)
                      (typep code 'cl-markdown-kit:code-block)
                      (string= (cl-markdown-kit:code-block-body code)
                               (format nil "code~%"))
                      (block-parser-test-span-is code 0 8))
                 :to-be-truthy))))
