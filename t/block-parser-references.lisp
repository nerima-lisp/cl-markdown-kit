(in-package #:cl-markdown-kit/test)

(describe "Block parser references"
          (it
           "removes closing ATX markers and preserves link definitions before paragraphs"
           (let* ((heading-example
                   (block-parser-test-commonmark-example "commonmark-0079"))
                  (heading-source
                   (block-parser-test-example-source heading-example))
                  (heading-result
                   (block-parser-test-result heading-source :front-matter nil))
                  (heading-document (block-parser-test-document heading-result))
                  (headings
                   (block-parser-test-nodes-of-type heading-document
                                                    #'(lambda (node)
                                                        (typep node
                                                               'cl-markdown-kit:heading))))
                  (reference-example
                   (block-parser-test-commonmark-example "commonmark-0210"))
                  (reference-source
                   (block-parser-test-example-source reference-example))
                  (reference-result
                   (block-parser-test-result reference-source :front-matter nil))
                  (reference-document
                   (block-parser-test-document reference-result))
                  (reference-children
                   (cl-markdown-kit:document-children reference-document))
                  (paragraph (second reference-children)))
             (expect
              (and
               (equal
                (mapcar
                 (lambda (heading)
                   (cl-markdown-kit::inline-fragment-text
                    (first
                     (block-parser-test-fragments-for heading-result heading))))
                 headings)
                '("" "" ""))
               (block-parser-test-ranges-valid-p heading-document
                                                 heading-source))
              :to-be-truthy)
             (expect
              (and (= (length reference-children) 2)
                   (typep (first reference-children)
                          'cl-markdown-kit:link-reference-definition)
                   (typep paragraph 'cl-markdown-kit:paragraph)
                   (string=
                    (cl-markdown-kit::inline-fragment-text
                     (first
                      (block-parser-test-fragments-for reference-result
                                                       paragraph)))
                    "\"title\" ok")
                   (block-parser-test-ranges-valid-p reference-document
                                                     reference-source))
              :to-be-truthy)))
          (it "indexes source-ordered link reference definitions"
              (let* ((source
                      (format nil
                              "[Ref]: /target \"Title\"~%[ref]: /later~%~%plain~%"))
                     (result (block-parser-test-result source))
                     (document (block-parser-test-document result))
                     (definition
                      (first (cl-markdown-kit:document-children document)))
                     (duplicate
                      (second (cl-markdown-kit:document-children document)))
                     (reference-index
                      (cl-markdown-kit::block-parse-result-reference-index
                       result)))
                (expect
                 (and
                  (typep definition 'cl-markdown-kit:link-reference-definition)
                  (typep duplicate 'cl-markdown-kit:link-reference-definition)
                  (string=
                   (cl-markdown-kit:link-reference-definition-raw-label
                    definition)
                   "Ref")
                  (string=
                   (cl-markdown-kit:link-reference-definition-normalized-label
                    definition)
                   "ref")
                  (string=
                   (cl-markdown-kit:link-reference-definition-destination
                    definition)
                   "/target")
                  (string=
                   (cl-markdown-kit:link-reference-definition-title definition)
                   "Title")
                  (cl-markdown-kit:link-reference-definition-eligible-p
                   definition)
                  (not
                   (cl-markdown-kit:link-reference-definition-eligible-p
                    duplicate))
                  (string=
                   (cl-markdown-kit:link-reference-definition-destination
                    duplicate)
                   "/later")
                  (eq (gethash "ref" reference-index) definition))
                 :to-be-truthy)))
          (it
           "decodes entities and backslash escapes in reference destinations and titles"
           (let* ((children
                   (cl-markdown-kit:document-children
                    (block-parser-test-document
                     (block-parser-test-result
                      "[ref]: /a\\+&amp;&#66; \"t\\+&quot;\""))))
                  (definition (first children)))
             (expect
              (and (= (length children) 1)
                   (typep definition 'cl-markdown-kit:link-reference-definition)
                   (string=
                    (cl-markdown-kit:link-reference-definition-destination
                     definition)
                    "/a+&B")
                   (string=
                    (cl-markdown-kit:link-reference-definition-title definition)
                    "t+\""))
              :to-be-truthy)))
          (it
           "parses multiline reference titles and consumes only their source lines"
           (let* ((source
                   (format nil
                           "[multi]: /target 'title~%continued'~%[next]: /next~%plain"))
                  (result (block-parser-test-result source))
                  (children
                   (cl-markdown-kit:document-children
                    (block-parser-test-document result)))
                  (index
                   (cl-markdown-kit::block-parse-result-reference-index result))
                  (destination-source
                   (format nil "[remote]:~%  /destination~%paragraph"))
                  (destination-result
                   (block-parser-test-result destination-source))
                  (destination-children
                   (cl-markdown-kit:document-children
                    (block-parser-test-document destination-result))))
             (expect
              (and (= (length children) 3)
                   (every
                    #'(lambda (node)
                        (typep node 'cl-markdown-kit:link-reference-definition))
                    (subseq children 0 2))
                   (string=
                    (cl-markdown-kit:link-reference-definition-title
                     (first children))
                    (format nil "title~%continued"))
                   (string=
                    (cl-markdown-kit:link-reference-definition-destination
                     (second children))
                    "/next")
                   (typep (third children) 'cl-markdown-kit:paragraph)
                   (eq (gethash "multi" index) (first children))
                   (eq (gethash "next" index) (second children)))
              :to-be-truthy)
             (expect
              (and (= (length destination-children) 2)
                   (typep (first destination-children)
                          'cl-markdown-kit:link-reference-definition)
                   (string=
                    (cl-markdown-kit:link-reference-definition-destination
                     (first destination-children))
                    "/destination")
                   (typep (second destination-children)
                          'cl-markdown-kit:paragraph))
              :to-be-truthy)))
          (it "parses escaped link delimiters and rejects malformed definitions"
              (let* ((angle
                      (multiple-value-list
                       (cl-markdown-kit::%bp-parse-destination
                        (format nil "<a~C>b>" #\\)
                        0)))
                     (backslash-before-space
                      (multiple-value-list
                       (cl-markdown-kit::%bp-parse-destination
                        (format nil "/a~C b" #\\)
                        0)))
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
                       (format nil "<a~%b>")
                       0))
                     (unterminated-angle
                      (cl-markdown-kit::%bp-parse-destination "<" 0))
                     (invalid-depth
                      (cl-markdown-kit::%bp-parse-destination "/a(b" 0))
                     (empty-destination
                      (cl-markdown-kit::%bp-parse-destination "" 0))
                     (escaped-title
                      (multiple-value-list
                       (cl-markdown-kit::%pc-parse-title
                        "\"escaped \\\" title\""
                        0
                        (length "\"escaped \\\" title\"")
                        :backslash-mode
                        :any-character)))
                     (unfinished-title
                      (cl-markdown-kit::%pc-parse-title "\"unfinished"
                                                        0
                                                        (length "\"unfinished")
                                                        :backslash-mode
                                                        :any-character))
                     (empty-title
                      (cl-markdown-kit::%pc-parse-title ""
                                                        0
                                                        (length "")
                                                        :backslash-mode
                                                        :any-character))
                     (ordinary-title
                      (cl-markdown-kit::%pc-parse-title "plain"
                                                        0
                                                        (length "plain")
                                                        :backslash-mode
                                                        :any-character))
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
                       (format nil
                               "[title]: /url~%  \"escaped ~C\" title\""
                               #\\)))
                     (title-definition
                      (multiple-value-list
                       (cl-markdown-kit::%bp-link-definition-at title-lines 0)))
                     (paren-title-lines
                      (cl-markdown-kit::%bp-split-lines
                       (format nil "[paren]: /url~%  (open~%  close)")))
                     (paren-title
                      (multiple-value-list
                       (cl-markdown-kit::%bp-link-definition-at
                        paren-title-lines
                        0)))
                     (incomplete-title-lines
                      (cl-markdown-kit::%bp-split-lines
                       (format nil "[unfinished]: /url \"open~%  close\"")))
                     (incomplete-title
                      (multiple-value-list
                       (cl-markdown-kit::%bp-link-definition-at
                        incomplete-title-lines
                        0)))
                     (broken-label-lines
                      (cl-markdown-kit::%bp-split-lines
                       (format nil "[broken~%label")))
                     (blank-label-lines
                      (cl-markdown-kit::%bp-split-lines
                       (format nil "[broken~%~%label"))))
                (expect
                 (and (string= (first angle) "a>b")
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
                      (null
                       (cl-markdown-kit::%bp-link-label-final-line
                        broken-label-lines
                        0))
                      (null
                       (cl-markdown-kit::%bp-link-label-final-line
                        blank-label-lines
                        0)))
                 :to-be-truthy)
                (expect
                 (and
                  (=
                   (cl-markdown-kit::%bp-link-title-close-line title-lines
                                                               1
                                                               3
                                                               #\")
                   1)
                  (null
                   (cl-markdown-kit::%bp-link-title-close-line title-lines
                                                               1
                                                               3
                                                               #\')))
                 :to-be-truthy))))
