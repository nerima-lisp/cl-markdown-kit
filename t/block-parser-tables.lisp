(in-package #:cl-markdown-kit/test)

(describe "Block parser GFM tables"
          (it
           "keeps GFM table alignments and splits embedded pipes without changing columns"
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
                    (first
                     (block-parser-test-fragments-for pipe-result
                                                      (first escaped-cells)))))
                  (code-text
                   (cl-markdown-kit::inline-fragment-text
                    (first
                     (block-parser-test-fragments-for pipe-result
                                                      (first code-cells)))))
                  (unmatched-source
                   (format nil "| a | b |~%| - | - |~%| `open | split |~%"))
                  (unmatched-rows
                   (cl-markdown-kit:table-rows
                    (block-parser-test-only-child
                     (block-parser-test-document
                      (block-parser-test-result unmatched-source))))))
             (expect
              (and (typep alignment-table 'cl-markdown-kit:table)
                   (equal (cl-markdown-kit:table-alignments alignment-table)
                          '(:left nil :center nil :right)))
              :to-be-truthy)
             (expect
              (and (typep pipe-table 'cl-markdown-kit:table)
                   (= (length rows) 5)
                   (every
                    (lambda (row)
                      (= (length (cl-markdown-kit:table-row-cells row)) 2))
                    rows)
                   (string= escaped-text "Escaped pipes are |okay|.")
                   (search "`|code| is okay`" code-text)
                   (=
                    (length
                     (cl-markdown-kit:table-row-cells (second unmatched-rows)))
                    2)
                   (string=
                    (cl-markdown-kit::inline-fragment-text
                     (first
                      (block-parser-test-fragments-for pipe-result
                                                       (second code-cells))))
                    ""))
              :to-be-truthy)))
          (it "removes GFM table pipe escapes inside and outside code spans"
              (let* ((result (block-parser-test-gfm-result "gfm-0011"))
                     (table
                      (first
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
          (it
           "recognizes a table immediately after a paragraph and preserves table-cell fragments"
           (let* ((result (block-parser-test-gfm-result "gfm-0016"))
                  (document (block-parser-test-document result))
                  (children (cl-markdown-kit:document-children document))
                  (paragraph (first children))
                  (table (second children))
                  (table-rows (cl-markdown-kit:table-rows table))
                  (body-cells
                   (cl-markdown-kit:table-row-cells (second table-rows))))
             (expect
              (and (= (length children) 2)
                   (typep paragraph 'cl-markdown-kit:paragraph)
                   (typep table 'cl-markdown-kit:table)
                   (string=
                    (cl-markdown-kit::inline-fragment-text
                     (first (block-parser-test-fragments-for result paragraph)))
                    (format nil "123~%456"))
                   (= (length table-rows) 2)
                   (equal
                    (mapcar
                     (lambda (cell)
                       (cl-markdown-kit::inline-fragment-text
                        (first (block-parser-test-fragments-for result cell))))
                     body-cells)
                    '("d" "e")))
              :to-be-truthy)))
          (it "accepts a pipe-less body row in a single-column GFM table"
              (let* ((source (format nil "| header |~%| --- |~%body~%"))
                     (result (block-parser-test-result source))
                     (table
                      (block-parser-test-only-child
                       (block-parser-test-document result)))
                     (rows (cl-markdown-kit:table-rows table))
                     (cells (cl-markdown-kit:table-row-cells (second rows))))
                (expect
                 (and (typep table 'cl-markdown-kit:table)
                      (= (length rows) 2)
                      (= (length cells) 1)
                      (string=
                       (cl-markdown-kit::inline-fragment-text
                        (first
                         (block-parser-test-fragments-for result (first cells))))
                       "body"))
                 :to-be-truthy)))
          (it "keeps table cells and source text in the GFM interop fixture"
              (let* ((result (block-parser-test-gfm-result "gfm-0027"))
                     (table
                      (block-parser-test-only-child
                       (block-parser-test-document result)))
                     (rows (cl-markdown-kit:table-rows table))
                     (body-cells
                      (cl-markdown-kit:table-row-cells (second rows))))
                (expect
                 (and (typep table 'cl-markdown-kit:table)
                      (= (length rows) 2)
                      (= (length body-cells) 2)
                      (equal
                       (mapcar
                        (lambda (cell)
                          (cl-markdown-kit::inline-fragment-text
                           (first (block-parser-test-fragments-for result cell))))
                        body-cells)
                       '("https://github.com www.github.com"
                         "http://pokemon.com")))
                 :to-be-truthy))))
