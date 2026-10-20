(in-package #:cl-markdown-kit/test)

(describe "Block parser containers"
          (it "preserves tab-relative code indentation in list items"
              (let* ((example
                      (block-parser-test-commonmark-example "commonmark-0007"))
                     (source (block-parser-test-example-source example))
                     (document
                      (block-parser-test-document
                       (block-parser-test-result source :front-matter nil)))
                     (code
                      (first
                       (block-parser-test-nodes-of-type document
                                                        #'(lambda (node)
                                                            (typep node
                                                                   'cl-markdown-kit:code-block))))))
                (expect
                 (and (typep code 'cl-markdown-kit:code-block)
                      (string= (cl-markdown-kit:code-block-body code)
                               (format nil "  foo~%"))
                      (block-parser-test-ranges-valid-p document source))
                 :to-be-truthy)))
          (it
           "preserves block quote nesting, separate quotes, and lazy continuation"
           (let* ((quoted
                   (block-parser-test-document
                    (block-parser-test-result
                     (spec-example-markdown
                      (block-parser-test-commonmark-example "commonmark-0228")))))
                  (quoted-block (block-parser-test-only-child quoted))
                  (quoted-children
                   (cl-markdown-kit:block-quote-children quoted-block))
                  (lazy-result
                   (block-parser-test-result
                    (spec-example-markdown
                     (block-parser-test-commonmark-example "commonmark-0232"))))
                  (lazy (block-parser-test-document lazy-result))
                  (lazy-block (block-parser-test-only-child lazy))
                  (lazy-children
                   (cl-markdown-kit:block-quote-children lazy-block))
                  (separate
                   (block-parser-test-document
                    (block-parser-test-result
                     (spec-example-markdown
                      (block-parser-test-commonmark-example "commonmark-0242")))))
                  (separate-quotes (cl-markdown-kit:document-children separate))
                  (nested-result
                   (block-parser-test-result
                    (spec-example-markdown
                     (block-parser-test-commonmark-example "commonmark-0250"))))
                  (nested (block-parser-test-document nested-result))
                  (outer-quote (block-parser-test-only-child nested)))
             (expect
              (and (typep quoted-block 'cl-markdown-kit:block-quote)
                   (= (length quoted-children) 2)
                   (typep (first quoted-children) 'cl-markdown-kit:heading)
                   (typep (second quoted-children) 'cl-markdown-kit:paragraph))
              :to-be-truthy)
             (expect
              (and (= (length lazy-children) 2)
                   (typep (first lazy-children) 'cl-markdown-kit:heading)
                   (typep (second lazy-children) 'cl-markdown-kit:paragraph)
                   (string=
                    (cl-markdown-kit::inline-fragment-text
                     (first
                      (block-parser-test-fragments-for lazy-result
                                                       (second lazy-children))))
                    (format nil "bar~%baz")))
              :to-be-truthy)
             (expect
              (and (= (length separate-quotes) 2)
                   (every
                    #'(lambda (node)
                        (typep node 'cl-markdown-kit:block-quote))
                    separate-quotes)
                   (every
                    (lambda (quote)
                      (and
                       (= (length (cl-markdown-kit:block-quote-children quote))
                          1)
                       (typep
                        (first (cl-markdown-kit:block-quote-children quote))
                        'cl-markdown-kit:paragraph)))
                    separate-quotes))
              :to-be-truthy)
             (expect
              (and (typep outer-quote 'cl-markdown-kit:block-quote)
                   (typep (block-parser-test-only-child outer-quote)
                          'cl-markdown-kit:block-quote)
                   (typep
                    (block-parser-test-only-child
                     (block-parser-test-only-child outer-quote))
                    'cl-markdown-kit:block-quote)
                   (let* ((inner-quote
                           (block-parser-test-only-child
                            (block-parser-test-only-child outer-quote)))
                          (paragraph (block-parser-test-only-child inner-quote))
                          (result nested-result))
                     (and (typep paragraph 'cl-markdown-kit:paragraph)
                          (string=
                           (cl-markdown-kit::inline-fragment-text
                            (first
                             (block-parser-test-fragments-for result paragraph)))
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
                      (block-parser-test-nodes-of-type nested-loose-document
                                                       #'(lambda (node)
                                                           (typep node
                                                                  'cl-markdown-kit:list-node))))
                     (loose-result
                      (block-parser-test-result
                       (spec-example-markdown
                        (block-parser-test-commonmark-example "commonmark-0325"))))
                     (loose-document (block-parser-test-document loose-result))
                     (loose-lists
                      (block-parser-test-nodes-of-type loose-document
                                                       #'(lambda (node)
                                                           (typep node
                                                                  'cl-markdown-kit:list-node))))
                     (separate-items-result
                      (block-parser-test-result
                       (spec-example-markdown
                        (block-parser-test-commonmark-example "commonmark-0326"))))
                     (separate-items-document
                      (block-parser-test-document separate-items-result))
                     (separate-items-lists
                      (block-parser-test-nodes-of-type separate-items-document
                                                       #'(lambda (node)
                                                           (typep node
                                                                  'cl-markdown-kit:list-node)))))
                (expect
                 (and (= (length nested-loose-lists) 2)
                      (equal
                       (mapcar #'cl-markdown-kit:list-node-tight-p
                               nested-loose-lists)
                       '(t nil))
                      (=
                       (length
                        (cl-markdown-kit:list-node-items
                         (first nested-loose-lists)))
                       2)
                      (=
                       (length
                        (cl-markdown-kit:list-node-items
                         (second nested-loose-lists)))
                       1))
                 :to-be-truthy)
                (expect
                 (and (= (length loose-lists) 2)
                      (equal
                       (mapcar #'cl-markdown-kit:list-node-tight-p loose-lists)
                       '(nil t))
                      (=
                       (length
                        (cl-markdown-kit:list-node-items (first loose-lists)))
                       1)
                      (=
                       (length
                        (cl-markdown-kit:list-node-items (second loose-lists)))
                       1))
                 :to-be-truthy)
                (expect
                 (and (= (length separate-items-lists) 3)
                      (equal
                       (mapcar #'cl-markdown-kit:list-node-tight-p
                               separate-items-lists)
                       '(nil t t))
                      (=
                       (length
                        (cl-markdown-kit:list-node-items
                         (first separate-items-lists)))
                       2))
                 :to-be-truthy)))
          (it
           "keeps lazy list and duplicate footnote continuation source-backed"
           (let* ((list-source
                   (format nil "- first~%lazy continuation~%# outside~%~%- [X]"))
                  (list-result (block-parser-test-result list-source))
                  (list-document (block-parser-test-document list-result))
                  (lists
                   (block-parser-test-nodes-of-type list-document
                                                    #'(lambda (node)
                                                        (typep node
                                                               'cl-markdown-kit:list-node))))
                  (tasks
                   (block-parser-test-nodes-of-type list-document
                                                    #'(lambda (node)
                                                        (typep node
                                                               'cl-markdown-kit:task-list-item))))
                  (first-item
                   (first (cl-markdown-kit:list-node-items (first lists))))
                  (first-paragraph (block-parser-test-only-child first-item))
                  (footnote-source
                   (format nil
                           "[^note]: first~%continued~%~%    code~%[^NOTE]: second"))
                  (footnote-result (block-parser-test-result footnote-source))
                  (footnotes
                   (block-parser-test-nodes-of-type
                    (block-parser-test-document footnote-result)
                    #'(lambda (node)
                        (typep node 'cl-markdown-kit:footnote-definition))))
                  (index
                   (cl-markdown-kit::block-parse-result-reference-index
                    footnote-result)))
             (expect
              (and (= (length lists) 2)
                   (= (length tasks) 1)
                   (string=
                    (cl-markdown-kit::inline-fragment-text
                     (first
                      (block-parser-test-fragments-for list-result
                                                       first-paragraph)))
                    (format nil "first~%lazy continuation"))
                   (= (length footnotes) 2)
                   (string=
                    (cl-markdown-kit:footnote-definition-raw-label
                     (first footnotes))
                    "note")
                   (eq (gethash (cons :footnote "note") index)
                       (first footnotes)))
              :to-be-truthy))))
