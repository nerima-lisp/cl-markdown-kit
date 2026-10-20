(in-package #:cl-markdown-kit/test)

(defun node-contract-same-order-p (actual expected)
  (and (= (length actual) (length expected))
       (every #'eq actual expected)))

(describe "AST node contract"
  (it "constructs every public node kind and exposes its semantic readers"
    (let* ((span (cl-parser-kit:make-span
                  :source "x" :start 0 :end 1
                  :start-line 1 :start-column 1
                  :end-line 1 :end-column 2))
           (node (cl-markdown-kit:make-node :span span))
           (block (cl-markdown-kit:make-block-node
                   :span span :continuation-prefix "> "))
           (inline (cl-markdown-kit:make-inline-node :span span))
           (document (cl-markdown-kit:make-document
                      :span span :children nil :source "x"
                      :source-format nil :reference-index nil
                      :max-nesting-depth 12
                      :max-inline-delimiter-depth 34))
           (front-matter (cl-markdown-kit:make-front-matter
                          :span span :raw-text "title: x"
                          :opening-delimiter "---"
                          :closing-delimiter "---"))
           (block-quote (cl-markdown-kit:make-block-quote
                         :span span :continuation-prefix "> "))
           (list-node (cl-markdown-kit:make-list-node
                       :span span :kind :ordered :tight-p t :start-number 4
                       :marker-character #\4 :delimiter-character #\)
                       :items nil))
           (list-item (cl-markdown-kit:make-list-item :span span))
           (task-list-item (cl-markdown-kit:make-task-list-item
                            :span span :checked-p t :task-marker-span span
                            :original-marker-character #\X))
           (paragraph (cl-markdown-kit:make-paragraph :span span))
           (heading (cl-markdown-kit:make-heading
                     :span span :level 2 :style :setext
                     :marker-character #\= :opening-marker-length 0
                     :closing-marker-length 3))
           (thematic-break (cl-markdown-kit:make-thematic-break
                            :span span :marker-character #\* :marker-count 5))
           (code-block (cl-markdown-kit:make-code-block
                        :span span :style :fenced :body "x"
                        :fence-character #\~ :fence-length 4
                        :indentation 2 :info-string "lisp"))
           (html-block (cl-markdown-kit:make-html-block
                        :span span :body "<p>x</p>" :kind 7))
           (definition (cl-markdown-kit:make-link-reference-definition
                        :span span :raw-label "A" :normalized-label "a"
                        :destination "/a" :title "title" :eligible-p t))
           (table (cl-markdown-kit:make-table
                   :span span :alignments '(:left :right)))
           (table-row (cl-markdown-kit:make-table-row
                       :span span :header-row-p t))
           (table-cell (cl-markdown-kit:make-table-cell
                        :span span :alignment :center))
           (footnote-definition (cl-markdown-kit:make-footnote-definition
                                 :span span :raw-label "N"
                                 :normalized-label "n"))
           (text (cl-markdown-kit:make-text :span span :value "x"))
           (code-span (cl-markdown-kit:make-code-span
                       :span span :literal-text "x" :backtick-run-length 2))
           (html-inline (cl-markdown-kit:make-html-inline
                         :span span :body "<i>x</i>"))
           (soft-break (cl-markdown-kit:make-soft-break :span span))
           (hard-break (cl-markdown-kit:make-hard-break :span span))
           (emphasis (cl-markdown-kit:make-emphasis
                      :span span :delimiter-character #\_))
           (strong (cl-markdown-kit:make-strong
                    :span span :delimiter-character #\*))
           (link (cl-markdown-kit:make-link
                  :span span :destination "/link" :title "L"
                  :reference-kind :full :reference-label "ref"))
           (image (cl-markdown-kit:make-image
                   :span span :destination "/image" :title "I"
                   :reference-kind :shortcut :reference-label "img"))
           (strikethrough (cl-markdown-kit:make-strikethrough
                           :span span :delimiter-character #\~))
           (autolink (cl-markdown-kit:make-autolink
                      :span span :destination "https://example.test"
                      :display-text "example" :kind :bare-url))
           (footnote-reference (cl-markdown-kit:make-footnote-reference
                                :span span :raw-label "N"
                                :normalized-label "n"))
           (nodes (list node block inline document front-matter block-quote
                        list-node list-item task-list-item paragraph heading
                        thematic-break code-block html-block definition table
                        table-row table-cell footnote-definition text code-span
                        html-inline soft-break hard-break emphasis strong link
                        image strikethrough autolink footnote-reference))
           (predicates (list #'(lambda (node) (typep node 'cl-markdown-kit:node))
                             #'(lambda (node) (typep node 'cl-markdown-kit:block-node))
                             #'(lambda (node) (typep node 'cl-markdown-kit:inline-node))
                             #'(lambda (node) (typep node 'cl-markdown-kit:document))
                             #'cl-markdown-kit:front-matter-p
                             #'(lambda (node) (typep node 'cl-markdown-kit:block-quote))
                             #'(lambda (node) (typep node 'cl-markdown-kit:list-node))
                             #'(lambda (node) (typep node 'cl-markdown-kit:list-item))
                             #'(lambda (node) (typep node 'cl-markdown-kit:task-list-item))
                             #'(lambda (node) (typep node 'cl-markdown-kit:paragraph))
                             #'(lambda (node) (typep node 'cl-markdown-kit:heading))
                             #'(lambda (node) (typep node 'cl-markdown-kit:thematic-break))
                             #'(lambda (node) (typep node 'cl-markdown-kit:code-block))
                             #'(lambda (node) (typep node 'cl-markdown-kit:html-block))
                             #'(lambda (node) (typep node 'cl-markdown-kit:link-reference-definition))
                             #'(lambda (node) (typep node 'cl-markdown-kit:table))
                             #'(lambda (node) (typep node 'cl-markdown-kit:table-row))
                             #'(lambda (node) (typep node 'cl-markdown-kit:table-cell))
                             #'(lambda (node) (typep node 'cl-markdown-kit:footnote-definition))
                             #'(lambda (node) (typep node 'cl-markdown-kit:text))
                             #'(lambda (node) (typep node 'cl-markdown-kit:code-span))
                             #'(lambda (node) (typep node 'cl-markdown-kit:html-inline))
                             #'(lambda (node) (typep node 'cl-markdown-kit:soft-break))
                             #'(lambda (node) (typep node 'cl-markdown-kit:hard-break))
                             #'(lambda (node) (typep node 'cl-markdown-kit:emphasis))
                             #'(lambda (node) (typep node 'cl-markdown-kit:strong))
                             #'(lambda (node) (typep node 'cl-markdown-kit:link))
                             #'(lambda (node) (typep node 'cl-markdown-kit:image))
                             #'(lambda (node) (typep node 'cl-markdown-kit:strikethrough))
                             #'(lambda (node) (typep node 'cl-markdown-kit:autolink))
                             #'(lambda (node) (typep node 'cl-markdown-kit:footnote-reference)))))
      (expect (length nodes) :to-equal 31)
      (let ((concrete-nodes (nthcdr 3 nodes)))
        (expect (mapcar (function cl-markdown-kit:node-kind) concrete-nodes)
                :to-equal
                (quote
                 (:document :front-matter :block-quote :list :list-item
                  :task-list-item :paragraph :heading :thematic-break
                  :code-block :html-block :link-reference-definition :table
                  :table-row :table-cell :footnote-definition :text :code-span
                  :html-inline :soft-break :hard-break :emphasis :strong :link
                  :image :strikethrough :autolink :footnote-reference)))
        (expect (mapcar (function cl-markdown-kit:node-family) concrete-nodes)
                :to-equal
                (quote
                 (:block :block :block :block :block :block :block :block
                  :block :block :block :block :block :block :block :block
                  :inline :inline :inline :inline :inline :inline :inline :inline
                  :inline :inline :inline :inline)))
        (expect (mapcar (function cl-markdown-kit:node-semantic-attributes)
                        concrete-nodes)
                :to-equal
                (list
                 (quote (:front-matter-p t :gfm-p t))
                 (quote (:opening-delimiter "---" :closing-delimiter "---"))
                 nil
                 (list :kind :ordered :tight-p t :start-number 4
                       :marker-character #\4 :delimiter-character (code-char 41))
                 nil
                 (list :checked-p t :task-marker-span span
                       :original-marker-character #\X)
                 nil
                 (list :level 2 :style :setext :marker-character #\=
                       :opening-marker-length 0 :closing-marker-length 3)
                 (list :marker-character #\* :marker-count 5)
                 (list :style :fenced :fence-character #\~
                       :fence-length 4 :indentation 2 :info-string "lisp")
                 (quote (:kind 7))
                 (quote (:raw-label "A" :normalized-label "a"
                         :destination "/a" :title "title" :eligible-p t))
                 (list :alignments (quote (:left :right)))
                 (list :header-row-p t)
                 (list :alignment :center)
                 (quote (:raw-label "N" :normalized-label "n"))
                 (quote (:value "x"))
                 (list :backtick-run-length 2)
                 nil nil nil
                 (list :delimiter-character #\_)
                 (list :delimiter-character #\*)
                 (quote (:destination "/link" :title "L"
                         :reference-kind :full :reference-label "ref"))
                 (quote (:destination "/image" :title "I"
                         :reference-kind :shortcut :reference-label "img"))
                 (list :delimiter-character #\~)
                 (quote (:destination "https://example.test" :kind :bare-url))
                 (quote (:raw-label "N" :normalized-label "n"))))
        (expect (mapcar (function cl-markdown-kit:node-edit-text-projection)
                        concrete-nodes)
                :to-equal
                (list nil "title: x" nil nil nil nil nil nil "" "x" "<p>x</p>" "" nil nil nil nil "x" "x" "<i>x</i>" "" "" nil nil nil nil nil "example" "")))
      (expect (cl-markdown-kit:node-family block) :to-equal :abstract)
      (expect (lambda () (cl-markdown-kit:node-kind block))
              :to-throw (quote simple-error))
      (expect (every (lambda (predicate object)
                       (funcall predicate object))
                     predicates nodes)
              :to-be-truthy)
      (expect (every (lambda (object)
                       (typep object 'cl-markdown-kit:node))
                     nodes)
              :to-be-truthy)
      (expect (cl-markdown-kit:node-span node) :to-equal span)
      (expect (cl-markdown-kit:block-node-continuation-prefix block)
              :to-equal "> ")
      (expect (list (cl-markdown-kit:document-source document)
                    (cl-markdown-kit:document-max-nesting-depth document)
                    (cl-markdown-kit:document-max-inline-delimiter-depth document))
              :to-equal '("x" 12 34))
      (expect (list (cl-markdown-kit:front-matter-raw-text front-matter)
                    (cl-markdown-kit:front-matter-opening-delimiter front-matter)
                    (cl-markdown-kit:front-matter-closing-delimiter front-matter))
              :to-equal '("title: x" "---" "---"))
      (expect (list (cl-markdown-kit:list-node-kind list-node)
                    (cl-markdown-kit:list-node-tight-p list-node)
                    (cl-markdown-kit:list-node-start-number list-node)
                    (cl-markdown-kit:list-node-marker-character list-node)
                    (cl-markdown-kit:list-node-delimiter-character list-node))
              :to-equal (list :ordered t 4 #\4 #\)))
      (expect (list (cl-markdown-kit:task-list-item-checked-p task-list-item)
                    (cl-markdown-kit:task-list-item-task-marker-span task-list-item)
                    (cl-markdown-kit:task-list-item-original-marker-character
                     task-list-item))
              :to-equal (list t span #\X))
      (expect (list (cl-markdown-kit:heading-level heading)
                    (cl-markdown-kit:heading-style heading)
                    (cl-markdown-kit:heading-marker-character heading)
                    (cl-markdown-kit:heading-opening-marker-length heading)
                    (cl-markdown-kit:heading-closing-marker-length heading))
              :to-equal (list 2 :setext #\= 0 3))
      (expect (list (cl-markdown-kit:thematic-break-marker-character thematic-break)
                    (cl-markdown-kit:thematic-break-marker-count thematic-break))
              :to-equal (list #\* 5))
      (expect (list (cl-markdown-kit:code-block-style code-block)
                    (cl-markdown-kit:code-block-body code-block)
                    (cl-markdown-kit:code-block-fence-character code-block)
                    (cl-markdown-kit:code-block-fence-length code-block)
                    (cl-markdown-kit:code-block-indentation code-block)
                    (cl-markdown-kit:code-block-info-string code-block))
              :to-equal (list :fenced "x" #\~ 4 2 "lisp"))
      (expect (list (cl-markdown-kit:html-block-body html-block)
                    (cl-markdown-kit:html-block-kind html-block))
              :to-equal (list "<p>x</p>" 7))
      (expect (list (cl-markdown-kit:link-reference-definition-raw-label definition)
                    (cl-markdown-kit:link-reference-definition-normalized-label
                     definition)
                    (cl-markdown-kit:link-reference-definition-destination definition)
                    (cl-markdown-kit:link-reference-definition-title definition)
                    (cl-markdown-kit:link-reference-definition-eligible-p definition))
              :to-equal '("A" "a" "/a" "title" t))
      (expect (list (cl-markdown-kit:table-alignments table)
                    (cl-markdown-kit:table-row-header-row-p table-row)
                    (cl-markdown-kit:table-cell-alignment table-cell))
              :to-equal '((:left :right) t :center))
      (expect (list (cl-markdown-kit:footnote-definition-raw-label
                     footnote-definition)
                    (cl-markdown-kit:footnote-definition-normalized-label
                     footnote-definition))
              :to-equal '("N" "n"))
      (expect (cl-markdown-kit:text-value text) :to-equal "x")
      (expect (list (cl-markdown-kit:code-span-literal-text code-span)
                    (cl-markdown-kit:code-span-backtick-run-length code-span))
              :to-equal '("x" 2))
      (expect (cl-markdown-kit:html-inline-body html-inline)
              :to-equal "<i>x</i>")
      (expect (list (cl-markdown-kit:emphasis-delimiter-character emphasis)
                    (cl-markdown-kit:strong-delimiter-character strong)
                    (cl-markdown-kit:strikethrough-delimiter-character
                     strikethrough))
              :to-equal (list #\_ #\* #\~))
      (expect (list (cl-markdown-kit:link-destination link)
                    (cl-markdown-kit:link-title link)
                    (cl-markdown-kit:link-reference-kind link)
                    (cl-markdown-kit:link-reference-label link)
                    (cl-markdown-kit:image-destination image)
                    (cl-markdown-kit:image-title image)
                    (cl-markdown-kit:image-reference-kind image)
                    (cl-markdown-kit:image-reference-label image))
              :to-equal '("/link" "L" :full "ref"
                          "/image" "I" :shortcut "img"))
      (expect (list (cl-markdown-kit:autolink-destination autolink)
                    (cl-markdown-kit:autolink-display-text autolink)
                    (cl-markdown-kit:autolink-kind autolink)
                    (cl-markdown-kit:footnote-reference-raw-label
                     footnote-reference)
                    (cl-markdown-kit:footnote-reference-normalized-label
                     footnote-reference))
              :to-equal '("https://example.test" "example" :bare-url "N" "n"))
      (expect (typep task-list-item 'cl-markdown-kit:list-item) :to-be-truthy)
      (expect (typep task-list-item 'cl-markdown-kit:block-node) :to-be-truthy)))

  (it "returns ordered children for every container and NIL for leaves"
    (let* ((child-a (cl-markdown-kit:make-text :value "a"))
           (child-b (cl-markdown-kit:make-text :value "b"))
           (inline-children (list child-a child-b))
           (paragraph (cl-markdown-kit:make-paragraph
                       :children inline-children))
           (heading (cl-markdown-kit:make-heading
                     :children inline-children))
           (cell (cl-markdown-kit:make-table-cell
                  :children inline-children))
           (definition (cl-markdown-kit:make-footnote-definition
                        :children (list paragraph)))
           (containers
             (list (cons (cl-markdown-kit:make-document
                          :children (list paragraph heading))
                         (list paragraph heading))
                   (cons (cl-markdown-kit:make-block-quote
                          :children (list paragraph heading))
                         (list paragraph heading))
                   (cons (cl-markdown-kit:make-list-node
                          :items (list paragraph heading))
                         (list paragraph heading))
                   (cons (cl-markdown-kit:make-list-item
                          :children (list paragraph heading))
                         (list paragraph heading))
                   (cons (cl-markdown-kit:make-task-list-item
                          :children (list paragraph heading))
                         (list paragraph heading))
                   (cons paragraph inline-children)
                   (cons heading inline-children)
                   (cons (cl-markdown-kit:make-table
                          :rows (list cell))
                         (list cell))
                   (cons (cl-markdown-kit:make-table-row
                          :cells (list cell))
                         (list cell))
                   (cons cell inline-children)
                   (cons definition (list paragraph))
                   (cons (cl-markdown-kit:make-emphasis
                          :children inline-children)
                         inline-children)
                   (cons (cl-markdown-kit:make-strong
                          :children inline-children)
                         inline-children)
                   (cons (cl-markdown-kit:make-link
                          :children inline-children)
                         inline-children)
                   (cons (cl-markdown-kit:make-image
                          :children inline-children)
                         inline-children)
                   (cons (cl-markdown-kit:make-strikethrough
                          :children inline-children)
                         inline-children)))
           (leaves (list (cl-markdown-kit:make-front-matter)
                         (cl-markdown-kit:make-thematic-break)
                         (cl-markdown-kit:make-code-block)
                         (cl-markdown-kit:make-html-block)
                         (cl-markdown-kit:make-link-reference-definition)
                         child-a (cl-markdown-kit:make-code-span)
                         (cl-markdown-kit:make-html-inline)
                         (cl-markdown-kit:make-soft-break)
                         (cl-markdown-kit:make-hard-break)
                         (cl-markdown-kit:make-autolink)
                         (cl-markdown-kit:make-footnote-reference))))
      (expect (every (lambda (pair)
                       (node-contract-same-order-p
                        (cl-markdown-kit:node-children (car pair))
                        (cdr pair)))
                     containers)
              :to-be-truthy)
      (expect (every (lambda (leaf)
                       (null (cl-markdown-kit:node-children leaf)))
                     leaves)
              :to-be-truthy))))
