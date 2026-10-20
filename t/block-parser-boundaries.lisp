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
