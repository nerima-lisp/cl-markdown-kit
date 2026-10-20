(in-package #:cl-markdown-kit/test)

(describe "Block parser GFM"
          (it
           "disables GFM tables, task markers, and footnote definitions on request"
           (let* ((table-source (format nil "| a | b |~%| --- | --- |"))
                  (table-document
                   (block-parser-test-document
                    (block-parser-test-result table-source :gfm nil)))
                  (task-document
                   (block-parser-test-document
                    (block-parser-test-result "- [ ] todo" :gfm nil)))
                  (task-list (block-parser-test-only-child task-document))
                  (footnote-document
                   (block-parser-test-document
                    (block-parser-test-result "[^note]: body" :gfm nil)))
                  (gfm-footnote-document
                   (block-parser-test-document
                    (block-parser-test-result "[^note]: body")))
                  (table-default-document
                   (block-parser-test-document
                    (block-parser-test-result table-source))))
             (expect
              (and
               (= (length (cl-markdown-kit:document-children table-document)) 1)
               (typep (first (cl-markdown-kit:document-children table-document))
                      'cl-markdown-kit:paragraph)
               (typep (first (cl-markdown-kit:list-node-items task-list))
                      'cl-markdown-kit:list-item)
               (not
                (typep (first (cl-markdown-kit:list-node-items task-list))
                       'cl-markdown-kit:task-list-item))
               (typep (block-parser-test-only-child footnote-document)
                      'cl-markdown-kit:paragraph)
               (typep (block-parser-test-only-child gfm-footnote-document)
                      'cl-markdown-kit:footnote-definition)
               (typep (block-parser-test-only-child table-default-document)
                      'cl-markdown-kit:table))
              :to-be-truthy)))
          (it
           "keeps footnote definitions as blocks and references in deferred fragments"
           (let* ((result (block-parser-test-gfm-result "gfm-0023"))
                  (document (block-parser-test-document result))
                  (definitions
                   (block-parser-test-nodes-of-type document
                                                    #'(lambda (node)
                                                        (typep node
                                                               'cl-markdown-kit:footnote-definition))))
                  (labels (mapcar
                           #'cl-markdown-kit:footnote-definition-raw-label
                           definitions))
                  (unused
                   (find "unused"
                         definitions
                         :key
                         #'cl-markdown-kit:footnote-definition-raw-label
                         :test
                         #'string=))
                  (unused-paragraph
                   (first (cl-markdown-kit:footnote-definition-children unused)))
                  (unused-fragment
                   (first
                    (block-parser-test-fragments-for result unused-paragraph)))
                  (fragment-texts (block-parser-test-fragment-texts result)))
             (expect
              (equal labels
                     '("other-note" "codeblock-note" "1" "footnote" "unused"))
              :to-be-truthy)
             (expect
              (and
               (some
                (lambda (text)
                  (search "[^1]" text))
                fragment-texts)
               (some
                (lambda (text)
                  (search "[^footnote]" text))
                fragment-texts)
               (some
                (lambda (node)
                  (and (typep node 'cl-markdown-kit:footnote-definition)
                       (some
                        #'(lambda (node)
                            (typep node 'cl-markdown-kit:block-quote))
                        (cl-markdown-kit:footnote-definition-children node))))
                definitions))
              :to-be-truthy)
             (expect
              (equal (block-parser-test-node-signature unused)
                     '(:footnote-definition (:label "unused")
                                            (:paragraph (:placement :only))))
              :to-be-truthy)
             (expect
              (and unused-fragment
                   (string=
                    (cl-markdown-kit::inline-fragment-text unused-fragment)
                    "This is unused."))
              :to-be-truthy)
             (expect
              (eq
               (gethash (cons :footnote "unused")
                        (cl-markdown-kit::block-parse-result-reference-index
                         result))
               unused)
              :to-be-truthy)))
          (it "keeps repeated and escaped GFM footnote labels source-backed"
              (let* ((repeated-result (block-parser-test-gfm-result "gfm-0024"))
                     (repeated-document
                      (block-parser-test-document repeated-result))
                     (repeated-definitions
                      (block-parser-test-nodes-of-type repeated-document
                                                       #'(lambda (node)
                                                           (typep node
                                                                  'cl-markdown-kit:footnote-definition))))
                     (repeated-text
                      (format nil
                              "~{~A~^ ~}"
                              (block-parser-test-fragment-texts repeated-result)))
                     (escaped-result (block-parser-test-gfm-result "gfm-0025"))
                     (escaped-definitions
                      (block-parser-test-nodes-of-type
                       (block-parser-test-document escaped-result)
                       #'(lambda (node)
                           (typep node 'cl-markdown-kit:footnote-definition)))))
                (expect
                 (and
                  (equal
                   (mapcar #'cl-markdown-kit:footnote-definition-raw-label
                           repeated-definitions)
                   '("a-footnote"))
                  (=
                   (block-parser-test-substring-count "[^a-footnote]"
                                                      repeated-text)
                   3))
                 :to-be-truthy)
                (expect
                 (and
                  (equal
                   (mapcar #'cl-markdown-kit:footnote-definition-raw-label
                           escaped-definitions)
                   '("\"><script>alert(1)</script>"))
                  (some
                   (lambda (text)
                     (search "[^\"><script>alert(1)</script>]" text))
                   (block-parser-test-fragment-texts escaped-result)))
                 :to-be-truthy)))
          (it
           "keeps GFM task states, exact marker spans, and ordinary list items distinct"
           (dolist (id '("gfm-0028" "gfm-0029" "gfm-0030"))
             (let* ((source
                     (spec-example-markdown (block-parser-test-gfm-example id)))
                    (result (block-parser-test-result source))
                    (document (block-parser-test-document result))
                    (tasks
                     (block-parser-test-nodes-of-type document
                                                      #'(lambda (node)
                                                          (typep node
                                                                 'cl-markdown-kit:task-list-item)))))
               (let ((expected-states
                      (if (string= id "gfm-0028")
                          '(nil t)
                          '(t nil t nil))))
                 (expect
                  (and
                   (equal
                    (mapcar #'cl-markdown-kit:task-list-item-checked-p tasks)
                    expected-states)
                   (every
                    (lambda (task)
                      (let ((span
                             (cl-markdown-kit:task-list-item-task-marker-span
                              task))
                            (item-span (cl-markdown-kit:node-span task)))
                        (and (string= (cl-parser-kit:span-source span) source)
                             (=
                              (- (cl-parser-kit:span-end span)
                                 (cl-parser-kit:span-start span))
                              3)
                             (<= (cl-parser-kit:span-start item-span)
                                 (cl-parser-kit:span-start span)
                                 (cl-parser-kit:span-end span)
                                 (cl-parser-kit:span-end item-span))
                             (member (block-parser-test-span-text span)
                                     '("[ ]" "[x]" "[X]")
                                     :test
                                     #'string=))))
                    tasks))
                  :to-be-truthy))
               (when (member id '("gfm-0029" "gfm-0030") :test #'string=)
                 (expect
                  (and (= (length tasks) 4)
                       (=
                        (length
                         (remove-if
                          #'(lambda (node)
                              (typep node 'cl-markdown-kit:task-list-item))
                          (block-parser-test-nodes-of-type document
                                                           #'(lambda (node)
                                                               (typep node
                                                                      'cl-markdown-kit:list-item)))))
                        4))
                  :to-be-truthy))))
           (let* ((source "- [X] uppercase")
                  (document
                   (block-parser-test-document
                    (block-parser-test-result source)))
                  (task
                   (first
                    (block-parser-test-nodes-of-type document
                                                     #'(lambda (node)
                                                         (typep node
                                                                'cl-markdown-kit:task-list-item)))))
                  (marker
                   (cl-markdown-kit:task-list-item-task-marker-span task)))
             (expect
              (and (cl-markdown-kit:task-list-item-checked-p task)
                   (char=
                    (cl-markdown-kit:task-list-item-original-marker-character
                     task)
                    #\X)
                   (string= (cl-parser-kit:span-source marker) source)
                   (string= (block-parser-test-span-text marker) "[X]")
                   (= (cl-parser-kit:span-start marker) 2)
                   (= (cl-parser-kit:span-end marker) 5))
              :to-be-truthy))))
