(in-package #:cl-markdown-kit/test)

(describe "Block parser helper boundaries"
          (it "accepts the block nesting limit and signals one level beyond it"
              (labels ((nested-quote-source (depth)
                         (with-output-to-string (output)
                           (loop repeat depth
                                 do (write-string "> " output))
                           (write-string "nested" output)))
                       (parse-depth-condition (depth limit)
                         (handler-case (progn
                                         (cl-markdown-kit::parse-blocks
                                          (cl-markdown-kit:make-source-buffer
                                           (nested-quote-source depth))
                                          :max-nesting-depth
                                          limit)
                                         nil)
                           (cl-markdown-kit:markdown-nesting-depth-exceeded (condition)
                             condition)))
                       (quote-depth (node)
                         (loop with depth = 0
                               while (typep node 'cl-markdown-kit:block-quote)
                               do (incf depth) (setf node (first
                                                           (cl-markdown-kit:block-quote-children
                                                            node)))
                               finally (return depth)))
                       (depth-condition-matches-p (condition limit actual)
                         (and condition
                              (eq
                               (cl-markdown-kit:markdown-resource-limit-error-limit-name
                                condition)
                               :nesting-depth)
                              (=
                               (cl-markdown-kit:markdown-resource-limit-error-limit
                                condition)
                               limit)
                              (=
                               (cl-markdown-kit:markdown-resource-limit-error-actual
                                condition)
                               actual))))
                (let* ((limit 8)
                       (accepted-document
                        (block-parser-test-document
                         (cl-markdown-kit::parse-blocks
                          (cl-markdown-kit:make-source-buffer
                           (nested-quote-source limit))
                          :max-nesting-depth
                          limit)))
                       (condition (parse-depth-condition (1+ limit) limit)))
                  (expect
                   (=
                    (quote-depth
                     (first
                      (cl-markdown-kit:document-children accepted-document)))
                    limit)
                   :to-be-truthy)
                  (expect (depth-condition-matches-p condition limit (1+ limit))
                          :to-be-truthy))))
          (it "uses the public parse-blocks defaults"
              (let* ((source
                       (format nil
                               "---~%title: defaults~%---~%~%| header |~%| --- |~%| body |"))
                     (result
                       (cl-markdown-kit:parse-blocks
                        (cl-markdown-kit:make-source-buffer source)))
                     (document
                       (cl-markdown-kit::block-parse-result-document result))
                     (children (cl-markdown-kit:document-children document))
                     (front-matter (first children))
                     (table (second children)))
                (expect
                 (and (= (length children) 2)
                      (cl-markdown-kit:front-matter-p front-matter)
                      (typep table 'cl-markdown-kit:table)
                      (= (cl-markdown-kit:document-max-nesting-depth document)
                         cl-markdown-kit::+default-max-nesting-depth+))
                 :to-be-truthy)))
          (it "uses offset boundaries and displaced views for ordinary lines and fragments"
              (let* ((bom (or (code-char #xfeff) (error "No BOM character available")))
                     (source
                       (concatenate 'string (string bom) "first"
                                    (string #\Return) (string #\Newline) "second"))
                     (buffer (cl-markdown-kit:make-source-buffer source))
                     (text (cl-markdown-kit:source-buffer-normalized-text buffer))
                     (lines (cl-markdown-kit::%bp-split-lines text))
                     (first-line (aref lines 0))
                     (second-line (aref lines 1))
                     (slice (cl-markdown-kit::%bp-line-slice second-line 1 4))
                     (owner :boundary-owner))
                (let ((cl-markdown-kit::*block-inline-fragments* nil))
                  (cl-markdown-kit::%bp-add-fragment
                   buffer owner (list first-line second-line))
                  (let* ((fragment (first cl-markdown-kit::*block-inline-fragments*))
                         (boundary-map
                           (cl-markdown-kit::inline-fragment-boundary-map fragment))
                         (span (cl-markdown-kit::make-source-span boundary-map 5 6)))
                    (expect
                     (and
                      (eq (cl-markdown-kit::inline-fragment-node fragment) owner)
                      (eql (cl-markdown-kit::%block-line-boundaries first-line) 0)
                      (eql (cl-markdown-kit::%block-line-boundaries second-line) 6)
                      (= (cl-markdown-kit::%bp-line-boundary first-line 5) 5)
                      (= (cl-markdown-kit::%bp-line-boundary second-line 6) 12)
                      (string= (cl-markdown-kit::%block-line-text first-line) "first")
                      (string= (cl-markdown-kit::%block-line-text second-line) "second")
                      (multiple-value-bind (displaced-to displacement-offset)
                          (array-displacement
                           (cl-markdown-kit::%block-line-text first-line))
                        (and (eq displaced-to text) (= displacement-offset 0)))
                      (eql (cl-markdown-kit::%block-line-boundaries slice) 7)
                      (string= (cl-markdown-kit::%block-line-text slice) "eco")
                      (= (cl-markdown-kit::%bp-line-boundary slice 3) 10)
                      (multiple-value-bind (displaced-to displacement-offset)
                          (array-displacement
                           (cl-markdown-kit::%block-line-text slice))
                        (and (eq displaced-to text) (= displacement-offset 7)))
                      (string= (cl-markdown-kit::inline-fragment-text fragment)
                               (format nil "first~%second"))
                      (equalp (cl-markdown-kit::boundary-map-segment-maps boundary-map)
                              #(0 6))
                      (= (cl-markdown-kit::boundary-map-length boundary-map) 12)
                      (= (cl-markdown-kit::source-boundary-offset
                          (cl-markdown-kit::boundary-map-boundary boundary-map 0))
                         1)
                      (= (cl-markdown-kit::source-boundary-offset
                          (cl-markdown-kit::boundary-map-boundary boundary-map 5))
                         6)
                      (= (cl-markdown-kit::source-boundary-offset
                          (cl-markdown-kit::boundary-map-boundary boundary-map 6))
                         8)
                      (= (cl-markdown-kit::source-boundary-offset
                          (cl-markdown-kit::boundary-map-boundary boundary-map 12))
                         14)
                      (equal
                       (list (cl-parser-kit:span-start span)
                             (cl-parser-kit:span-end span)
                             (cl-parser-kit:span-start-line span)
                             (cl-parser-kit:span-start-column span)
                             (cl-parser-kit:span-end-line span)
                             (cl-parser-kit:span-end-column span))
                       '(6 8 1 6 2 1))
                      (equal (cl-parser-kit:span-source span) source))
                     :to-be-truthy)))))
          (it "keeps explicit maps for transformed line text"
              (let* ((source-line
                       (aref (cl-markdown-kit::%bp-split-lines "  abc") 0))
                     (plain-line (cl-markdown-kit::%bp-line-slice source-line 2))
                     (prefixed (cl-markdown-kit::%bp-line-prefix-spaces
                                plain-line 2))
                     (pipe-line
                       (aref (cl-markdown-kit::%bp-split-lines "a\\|b") 0))
                     (unescaped (cl-markdown-kit::%bp-unescape-table-pipes
                                 pipe-line))
                     (joined
                       (cl-markdown-kit::%bp-join-lines
                        (cl-markdown-kit::%bp-split-lines
                         (format nil "a~%b")))))
                (expect
                 (and
                  (string= (cl-markdown-kit::%block-line-text prefixed)
                           "  abc")
                  (vectorp (cl-markdown-kit::%block-line-boundaries prefixed))
                  (equal (coerce (cl-markdown-kit::%block-line-boundaries prefixed)
                                 'list)
                         '(2 2 2 3 4 5))
                  (string= (cl-markdown-kit::%block-line-text unescaped) "a|b")
                  (vectorp (cl-markdown-kit::%block-line-boundaries unescaped))
                  (equal (coerce (cl-markdown-kit::%block-line-boundaries unescaped)
                                 'list)
                         '(0 1 3 4))
                  (string= (cl-markdown-kit::%block-line-text joined)
                           (format nil "a~%b"))
                  (vectorp (cl-markdown-kit::%block-line-boundaries joined))
                  (equal (coerce (cl-markdown-kit::%block-line-boundaries joined)
                                 'list)
                         '(0 1 2 3)))
                 :to-be-truthy)
                (let* ((fragment-source (format nil "  abc~%rest"))
                       (fragment-buffer (cl-markdown-kit:make-source-buffer fragment-source))
                       (fragment-lines
                         (cl-markdown-kit::%bp-split-lines
                          (cl-markdown-kit:source-buffer-normalized-text fragment-buffer)))
                       (plain-fragment-line
                         (cl-markdown-kit::%bp-line-slice (aref fragment-lines 0) 2))
                       (prefixed-fragment-line
                         (cl-markdown-kit::%bp-line-prefix-spaces plain-fragment-line 2))
                       (owner :boundary-owner))
                  (let ((cl-markdown-kit::*block-inline-fragments* nil))
                    (cl-markdown-kit::%bp-add-fragment
                     fragment-buffer owner
                     (list prefixed-fragment-line (aref fragment-lines 1)))
                    (let* ((fragment (first cl-markdown-kit::*block-inline-fragments*))
                           (boundary-map
                             (cl-markdown-kit::inline-fragment-boundary-map fragment))
                           (span (cl-markdown-kit::make-source-span boundary-map 0 10)))
                      (expect
                       (and
                        (eq (cl-markdown-kit::inline-fragment-node fragment) owner)
                        (string= (cl-markdown-kit::inline-fragment-text fragment)
                                 (format nil "  abc~%rest"))
                        (equalp (cl-markdown-kit::boundary-map-segment-maps boundary-map)
                                #(#(2 2 2 3 4 5) 6))
                        (= (cl-markdown-kit::boundary-map-length boundary-map) 10)
                        (= (cl-markdown-kit::source-boundary-offset
                            (cl-markdown-kit::boundary-map-boundary boundary-map 0))
                           2)
                        (= (cl-markdown-kit::source-boundary-offset
                            (cl-markdown-kit::boundary-map-boundary boundary-map 2))
                           2)
                        (= (cl-markdown-kit::source-boundary-offset
                            (cl-markdown-kit::boundary-map-boundary boundary-map 5))
                           5)
                        (= (cl-markdown-kit::source-boundary-offset
                            (cl-markdown-kit::boundary-map-boundary boundary-map 6))
                           6)
                        (= (cl-markdown-kit::source-boundary-offset
                            (cl-markdown-kit::boundary-map-boundary boundary-map 10))
                           10)
                        (= (cl-parser-kit:span-start span) 2)
                        (= (cl-parser-kit:span-end span) 10)
                        (equal (cl-parser-kit:span-source span) fragment-source))
                       :to-be-truthy))))))
          (it
           "handles tab stops and partial indentation removal with source boundaries"
           (let* ((tab-line
                   (block-parser-test-line (format nil "~Cword" #\Tab)))
                  (partial (cl-markdown-kit::%bp-strip-columns tab-line 1))
                  (full (cl-markdown-kit::%bp-strip-columns tab-line 4))
                  (quote-indent
                   (multiple-value-list
                    (cl-markdown-kit::%bp-leading-indent
                     (format nil "~CX" #\Tab)
                     4))))
             (expect
              (and
               (string= (cl-markdown-kit::%block-line-text partial)
                        (format nil "   word"))
               (equal
                (coerce (cl-markdown-kit::%block-line-boundaries partial) 'list)
                '(0 0 0 1 2 3 4 5))
               (string= (cl-markdown-kit::%block-line-text full) "word")
               (= (cl-markdown-kit::%bp-indent-columns (format nil "~CX" #\Tab))
                  4)
               (=
                (cl-markdown-kit::%bp-whitespace-width (format nil "~C " #\Tab)
                                                       0
                                                       2
                                                       2)
                3)
               (equal quote-indent '(1 4))
               (cl-markdown-kit::%bp-prefix-p "Ab" "Ab" 0)
               (string= (cl-markdown-kit::normalize-link-label "ASCII") "ascii")
               (string= (cl-markdown-kit::normalize-link-label "Ä") "ä"))
              :to-be-truthy)))
          (it "routes a matching container line through its continuation"
              (let* ((continuation (block-parser-test-line "  continuation"))
                     (root-frame
                      (cl-markdown-kit::%bp-make-open-frame :kind :root))
                     (item-frame
                      (cl-markdown-kit::%bp-make-open-frame :kind
                                                            :item
                                                            :content-indent
                                                            2
                                                            :has-content
                                                            t))
                     (state
                      (cl-markdown-kit::%bp-make-parser-state :stack
                                                              (list item-frame
                                                                    root-frame)))
                     (match-count 0)
                     (fail-count 0))
                (cl-weave:with-continuation-result (transformed on-match)
                                                   (cl-markdown-kit::%bp-state-parse-current-line
                                                    state
                                                    continuation
                                                    (lambda (content)
                                                      (incf match-count)
                                                      (funcall
                                                       (function on-match)
                                                       content))
                                                    (lambda
                                                        (content mismatch-frame)
                                                      (declare (ignore content
                                                                       mismatch-frame))
                                                      (incf fail-count)))
                                                   (expect
                                                    (string=
                                                     (cl-markdown-kit::%block-line-text
                                                      transformed)
                                                     "continuation")
                                                    :to-be-truthy))
                (expect
                 (and (= match-count 1)
                      (zerop fail-count)
                      (eq (cl-markdown-kit::%bp-open-frame-end-line item-frame)
                          continuation))
                 :to-be-truthy)))
          (it
           "routes a mismatching container line through its failure continuation"
           (let* ((blank-line (block-parser-test-line ""))
                  (continuation (block-parser-test-line "  continuation"))
                  (root-frame
                   (cl-markdown-kit::%bp-make-open-frame :kind :root))
                  (blank-item
                   (cl-markdown-kit::%bp-make-open-frame :kind
                                                         :item
                                                         :content-indent
                                                         2))
                  (state
                   (cl-markdown-kit::%bp-make-parser-state :stack
                                                           (list blank-item
                                                                 root-frame)))
                  (match-count 0)
                  (fail-count 0))
             (multiple-value-bind (content mismatch)
                 (cl-markdown-kit::%bp-container-transform-line blank-line
                                                                (list blank-item
                                                                      root-frame)
                                                                t)
               (expect
                (and (eq content blank-line)
                     (null mismatch)
                     (cl-markdown-kit::%bp-open-frame-blank-seen blank-item))
                :to-be-truthy))
             (cl-weave:with-continuation-result (failure on-fail)
                                                (cl-markdown-kit::%bp-state-parse-current-line
                                                 state
                                                 continuation
                                                 (lambda (content)
                                                   (declare (ignore content))
                                                   (incf match-count))
                                                 (lambda
                                                     (content mismatch-frame)
                                                   (incf fail-count)
                                                   (funcall (function on-fail)
                                                            (list content
                                                                  mismatch-frame))))
                                                (expect
                                                 (and
                                                  (string=
                                                   (cl-markdown-kit::%block-line-text
                                                    (first failure))
                                                   "  continuation")
                                                  (eq (second failure)
                                                      blank-item))
                                                 :to-be-truthy))
             (expect (and (= fail-count 1) (zerop match-count)) :to-be-truthy)))
          (it
           "covers block-helper rejection paths and validates direct depth limits"
           (let* ((source (cl-markdown-kit:make-source-buffer "ordinary"))
                  (line (block-parser-test-line "ordinary")))
             (labels ((invalid-depth-p (value)
                        (handler-case (progn
                                        (cl-markdown-kit::parse-blocks source
                                                                       :max-nesting-depth
                                                                       value)
                                        nil)
                          (type-error ()
                            t)))
                      (invalid-gfm-p (value)
                        (handler-case (progn
                                        (cl-markdown-kit::parse-blocks source
                                                                       :gfm
                                                                       value)
                                        nil)
                          (type-error ()
                            t))))
               (expect
                (and
                 (null
                  (cl-markdown-kit::%bp-task-marker
                   (block-parser-test-line "ordinary")))
                 (string=
                  (cl-markdown-kit::%block-line-text
                   (third
                    (multiple-value-list
                     (cl-markdown-kit::%bp-task-marker
                      (block-parser-test-line "[X]")))))
                  "")
                 (invalid-depth-p 0)
                 (invalid-depth-p "deep")
                 (invalid-gfm-p :enabled)
                 (null (cl-markdown-kit::%bp-front-matter source #()))
                 (null (cl-markdown-kit::%bp-front-matter source (vector line))))
                :to-be-truthy))))
          (it "covers block parser helper boundaries and open frame transitions"
              (let* ((source (cl-markdown-kit:make-source-buffer "plain"))
                     (plain-line (block-parser-test-line "plain"))
                     (trimmed-empty-line
                      (cl-markdown-kit::%bp-trim-line-end
                       (block-parser-test-line "   ")))
                     (decoded-references
                      (cl-markdown-kit::%pc-unescape
                       "&amp;&#66;&#x42;&#X42;&#xZZ;&#x;&#;&; &amp"))
                     (trailing-backslash
                      (multiple-value-list
                       (cl-markdown-kit::%bp-parse-destination
                        (format nil "/a~C" #\\)
                        0)))
                     (trailing-angle-backslash
                      (cl-markdown-kit::%bp-parse-destination
                       (format nil "<a~C" #\\)
                       0))
                     (invalid-empty-line
                      (cl-markdown-kit::%bp-link-definition-at
                       (cl-markdown-kit::%bp-split-lines "")
                       0))
                     (ordinary-definition
                      (cl-markdown-kit::%bp-link-definition-at
                       (cl-markdown-kit::%bp-split-lines "ordinary")
                       0))
                     (footnote-definition
                      (cl-markdown-kit::%bp-link-definition-at
                       (cl-markdown-kit::%bp-split-lines "[^note]: /url")
                       0))
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
                        (cl-markdown-kit::%bp-parse-lines source
                                                          (list plain-line)
                                                          ""
                                                          0
                                                          256
                                                          t)))
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
                      (cl-markdown-kit::%bp-make-open-frame :kind
                                                            :item
                                                            :content-indent
                                                            2
                                                            :has-content
                                                            t
                                                            :end-line
                                                            plain-line))
                     (continuation (block-parser-test-line "  continuation"))
                     (blank-item
                      (cl-markdown-kit::%bp-make-open-frame :kind
                                                            :item
                                                            :content-indent
                                                            2))
                     (blank-line (block-parser-test-line ""))
                     (footnote-frame
                      (cl-markdown-kit::%bp-make-open-frame :kind
                                                            :footnote
                                                            :content-indent
                                                            4
                                                            :end-line
                                                            plain-line))
                     (footnote-continuation (block-parser-test-line "    body"))
                     (unknown-node
                      (cl-markdown-kit:make-paragraph :span
                                                      nil
                                                      :continuation-prefix
                                                      ""
                                                      :children
                                                      nil))
                     (unknown-frame
                      (cl-markdown-kit::%bp-make-open-frame :kind
                                                            :unknown
                                                            :node
                                                            unknown-node
                                                            :start-line
                                                            plain-line
                                                            :start-position
                                                            0
                                                            :end-line
                                                            plain-line))
                     (quoted-footnote
                      (block-parser-test-nodes-of-type
                       (block-parser-test-document
                        (block-parser-test-result "> [^quote]: body"))
                       #'(lambda (node)
                           (typep node 'cl-markdown-kit:footnote-definition))))
                     (listed-footnote
                      (block-parser-test-nodes-of-type
                       (block-parser-test-document
                        (block-parser-test-result "- [^list]: body"))
                       #'(lambda (node)
                           (typep node 'cl-markdown-kit:footnote-definition)))))
                (expect
                 (and (string= decoded-references "&BBB&#xZZ;&#x;&#;&; &amp")
                      (string= vector-body "body")
                      (string=
                       (cl-markdown-kit::%block-line-text trimmed-empty-line)
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
                      (typep (block-parser-test-only-child default-parse)
                             'cl-markdown-kit:paragraph)
                      (= (length quoted-footnote) 1)
                      (= (length listed-footnote) 1))
                 :to-be-truthy)
                (multiple-value-bind (transformed mismatch)
                    (cl-markdown-kit::%bp-container-transform-line continuation
                                                                   (list
                                                                    item-frame
                                                                    root-frame))
                  (expect
                   (and (not mismatch)
                        (string= (cl-markdown-kit::%block-line-text transformed)
                                 "continuation")
                        (eq
                         (cl-markdown-kit::%bp-open-frame-end-line item-frame)
                         plain-line))
                   :to-be-truthy))
                (multiple-value-bind (transformed mismatch)
                    (cl-markdown-kit::%bp-container-transform-line continuation
                                                                   (list
                                                                    item-frame
                                                                    root-frame)
                                                                   t)
                  (expect
                   (and (not mismatch)
                        (string= (cl-markdown-kit::%block-line-text transformed)
                                 "continuation")
                        (eq
                         (cl-markdown-kit::%bp-open-frame-end-line item-frame)
                         continuation))
                   :to-be-truthy))
                (multiple-value-bind (content mismatch)
                    (cl-markdown-kit::%bp-container-transform-line blank-line
                                                                   (list
                                                                    blank-item
                                                                    root-frame)
                                                                   t)
                  (expect
                   (and (eq content blank-line)
                        (null mismatch)
                        (cl-markdown-kit::%bp-open-frame-blank-seen blank-item))
                   :to-be-truthy))
                (multiple-value-bind (content mismatch)
                    (cl-markdown-kit::%bp-container-transform-line continuation
                                                                   (list
                                                                    blank-item
                                                                    root-frame)
                                                                   t)
                  (expect
                   (and
                    (string= (cl-markdown-kit::%block-line-text content)
                             "  continuation")
                    (eq mismatch blank-item))
                   :to-be-truthy))
                (multiple-value-bind (transformed mismatch)
                    (cl-markdown-kit::%bp-container-transform-line
                     footnote-continuation
                     (list footnote-frame root-frame)
                     t)
                  (expect
                   (and (not mismatch)
                        (string= (cl-markdown-kit::%block-line-text transformed)
                                 "body")
                        (eq
                         (cl-markdown-kit::%bp-open-frame-end-line
                          footnote-frame)
                         footnote-continuation))
                   :to-be-truthy))
                (expect
                 (eq (cl-markdown-kit::%bp-finish-frame source unknown-frame)
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
                        (concatenate 'string
                                     "["
                                     (make-string 1000 :initial-element #\a)
                                     "]: /target"))))
                     (paren-title-lines
                      (cl-markdown-kit::%bp-split-lines
                       (format nil "[paren]: /url (open~%  middle~%  close)")))
                     (paren-title
                      (multiple-value-list
                       (cl-markdown-kit::%bp-link-definition-at
                        paren-title-lines
                        0)))
                     (reparse-failure
                      (multiple-value-list
                       (cl-markdown-kit::%bp-link-definition-at
                        (cl-markdown-kit::%bp-split-lines
                         (format nil
                                 "[bad]: /url \"open~%  middle~%  close\" trailing"))
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
                (expect
                 (and (null short-label)
                      (null missing-colon)
                      (null no-label-tail)
                      (null long-label))
                 :to-be-truthy)
                (expect
                 (and (first paren-title)
                      (string= (fourth (second paren-title))
                               (format nil "open~%  middle~%  close"))
                      (null (first reparse-failure)))
                 :to-be-truthy)
                (expect
                 (and
                  (string= (first escaped-footnote)
                           (format nil "escaped~C]label" #\\))
                  (string= (second escaped-footnote) "escaped]label"))
                 :to-be-truthy)
                (expect
                 (and (null empty-footnote) (null missing-footnote-colon))
                 :to-be-truthy)
                (expect
                 (and (= (first dash-setext) 2)
                      (null blank-setext)
                      (null plain-fence)
                      (null empty-fence)
                      (null empty-heading)
                      (null empty-title-opener))
                 :to-be-truthy)
                (expect
                 (and
                  (=
                   (length
                    (block-parser-test-nodes-of-type list-document
                                                     #'(lambda (node)
                                                         (typep node
                                                                'cl-markdown-kit:list-node))))
                   1)
                  (=
                   (length
                    (block-parser-test-nodes-of-type footnote-document
                                                     #'(lambda (node)
                                                         (typep node
                                                                'cl-markdown-kit:footnote-definition))))
                   1))
                 :to-be-truthy)
                (expect
                 (and (null (first empty-tag-name))
                      (null closing-script)
                      (null closing-empty-tag)
                      (null
                       (cl-markdown-kit::%bp-link-title-opener
                        (block-parser-test-line "ordinary")))
                      (not
                       (cl-markdown-kit::%bp-html-terminator-p
                        (block-parser-test-line "ordinary")
                        nil
                        0)))
                 :to-be-truthy))))
