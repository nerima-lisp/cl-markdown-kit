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
              :to-be-truthy)))

  (it "walks the tree pre-order, depth-first, preserving sibling order"
    (let* ((first-leaf (cl-markdown-kit:make-text :value "first"))
           (nested-leaf (cl-markdown-kit:make-text :value "nested"))
           (last-leaf (cl-markdown-kit:make-text :value "last"))
           (emphasis (cl-markdown-kit:make-emphasis
                      :children (list nested-leaf)))
           (paragraph (cl-markdown-kit:make-paragraph
                       :children (list first-leaf emphasis)))
           (code (cl-markdown-kit:make-code-block :body "code"))
           (last-paragraph (cl-markdown-kit:make-paragraph
                            :children (list last-leaf)))
           (root (cl-markdown-kit:make-document
                  :children (list paragraph code last-paragraph)))
           (visited nil)
           (result (cl-markdown-kit:walk-node-tree root
            (lambda (frame)
              (let ((node (cl-markdown-kit:node-tree-frame-node frame))
                    (phase (cl-markdown-kit:node-tree-frame-phase frame)))
                (when (eq phase :enter)
                  (push node visited))
                :descend))))
           (expected (list root paragraph first-leaf emphasis nested-leaf
                           code last-paragraph last-leaf)))
      (expect (eq result root) :to-be-truthy)
      (expect (node-contract-same-order-p (nreverse visited) expected)
              :to-be-truthy)))
  (it "walks node trees with enter and leave context"
    (let* ((first-leaf (cl-markdown-kit:make-text :value "first"))
           (nested-inline
             (cl-markdown-kit:make-emphasis :children (list first-leaf)))
           (last-leaf (cl-markdown-kit:make-text :value "last"))
           (paragraph
             (cl-markdown-kit:make-paragraph
              :children (list nested-inline last-leaf)))
           (sibling (cl-markdown-kit:make-code-block :body "code"))
           (root
             (cl-markdown-kit:make-document
              :children (list paragraph sibling)))
           (events nil)
           (result
             (cl-markdown-kit:walk-node-tree root
              (lambda (frame)
                (push (list (cl-markdown-kit:node-tree-frame-node frame)
                            (cl-markdown-kit:node-tree-frame-phase frame)
                            (cl-markdown-kit:node-tree-frame-parent frame)
                            (cl-markdown-kit:node-tree-frame-grandparent frame)
                            (cl-markdown-kit:node-tree-frame-previous-sibling frame)
                            (cl-markdown-kit:node-tree-frame-next-sibling frame)
                            (cl-markdown-kit:node-tree-frame-child-index frame)
                            (cl-markdown-kit:node-tree-frame-last-child-p frame))
                      events)
                (if (eq (cl-markdown-kit:node-tree-frame-phase frame) :leave)
                    :stop
                    :descend)))))
      (setf events (nreverse events))
      (expect (eq result root) :to-be-truthy)
      (expect (mapcar (lambda (event) (subseq event 0 2)) events)
              :to-equal
              (list (list root :enter)
                    (list paragraph :enter)
                    (list nested-inline :enter)
                    (list first-leaf :enter)
                    (list first-leaf :leave)
                    (list nested-inline :leave)
                    (list last-leaf :enter)
                    (list last-leaf :leave)
                    (list paragraph :leave)
                    (list sibling :enter)
                    (list sibling :leave)
                    (list root :leave)))
      (expect (subseq (find root events :key (function first)) 2)
              :to-equal (list nil nil nil nil 0 t))
      (expect (subseq (find paragraph events :key (function first)) 2)
              :to-equal (list root nil nil sibling 0 nil))
      (expect (subseq (find sibling events :key (function first)) 2)
              :to-equal (list root nil paragraph nil 1 t))
      (expect (subseq (find last-leaf events :key (function first)) 2)
              :to-equal (list paragraph root nested-inline nil 1 t))
      (expect (subseq (find first-leaf events :key (function first)) 2)
              :to-equal (list nested-inline paragraph nil nil 0 t))))
  (it-each
    ((:skip) (:stop))
    "honors the ~A walker continuation"
    (decision)
    (let* ((leaf (cl-markdown-kit:make-text :value "hidden"))
           (inner
             (cl-markdown-kit:make-paragraph :children (list leaf)))
           (branch
             (cl-markdown-kit:make-block-quote :children (list inner)))
           (sibling (cl-markdown-kit:make-paragraph))
           (root
             (cl-markdown-kit:make-document
              :children (list branch sibling)))
           (events nil)
           (result
             (cl-markdown-kit:walk-node-tree root
              (lambda (frame)
                (let ((node (cl-markdown-kit:node-tree-frame-node frame))
                      (phase (cl-markdown-kit:node-tree-frame-phase frame)))
                  (push (list node phase) events)
                  (if (and (eq node branch) (eq phase :enter))
                      decision
                      :descend))))))
      (setf events (nreverse events))
      (expect (eq result root) :to-be-truthy)
      (expect events
              :to-equal
              (case decision
                (:skip
                 (list (list root :enter)
                       (list branch :enter)
                       (list branch :leave)
                       (list sibling :enter)
                       (list sibling :leave)
                       (list root :leave)))
                (:stop
                 (list (list root :enter)
                       (list branch :enter)))))))
  (it-each
    ((:malformed-metadata-plist
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-plist)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes)
      "metadata must be a property list")
     (:duplicate-metadata-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-metadata)
        :kind :contract :kind :other :family :inline :children nil
        :attributes nil :edit-text :empty :edit-attributes ())
      "metadata contains duplicate keys")
     (:unknown-metadata-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-unknown-metadata)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes () :unknown t)
      "Unknown DEFINE-NODE metadata key")
     (:missing-metadata-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-missing-metadata)
        :kind :contract :family :inline :children nil :edit-text :empty
        :edit-attributes ())
      "is missing required")
     (:invalid-kind
      (cl-markdown-kit::define-node
        (defstruct node-contract-invalid-kind)
        :kind "contract" :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE kind/family")
     (:invalid-family
      (cl-markdown-kit::define-node
        (defstruct (node-contract-invalid-family-node
                    (:constructor make-node-contract-invalid-family-node ())))
        :kind :invalid :family :unknown :children nil :attributes nil
        :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE kind/family")
     (:abstract-family-mismatch
      (cl-markdown-kit::define-node
        (defstruct node-contract-abstract-family)
        :abstract t :kind nil :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE kind/family")
     (:non-keyword-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-keyword-attribute value)
        :kind :contract :family :inline :children nil :attributes (value)
        :edit-text :empty :edit-attributes ())
      "attributes must be distinct keyword names")
     (:duplicate-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-attribute value)
        :kind :contract :family :inline :children nil
        :attributes (:value :value) :edit-text :empty :edit-attributes ())
      "attributes must be distinct keyword names")
     (:attribute-without-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-attribute-without-slot)
        :kind :contract :family :inline :children nil
        :attributes (:missing) :edit-text :empty :edit-attributes ())
      "has no slot")
     (:malformed-edit-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-edit-attribute cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((:value)))
      "edit attributes must map declared attribute slots")
     (:undeclared-edit-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-undeclared-edit-attribute value)
        :kind :contract :family :inline :children nil :attributes ()
        :edit-text :empty :edit-attributes ((:value :before)))
      "edit attributes must map declared attribute slots")
     (:duplicate-edit-attribute-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-edit-slot cl-markdown-kit::value cl-markdown-kit::other)
        :kind :contract :family :inline :children nil
        :attributes (:value :other) :edit-text :empty
        :edit-attributes ((:value :before) (:value :after)))
      "edit attributes contain a duplicate slot")
     (:duplicate-edit-difference-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-edit-key cl-markdown-kit::value cl-markdown-kit::other)
        :kind :contract :family :inline :children nil
        :attributes (:value :other) :edit-text :empty
        :edit-attributes ((:value :same) (:other :same)))
      "edit attributes contain a duplicate difference key")
     (:malformed-children
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-children value)
        :kind :contract :family :inline :children (:unknown value)
        :attributes nil :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE :CHILDREN value")
     (:child-without-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-child-without-slot value)
        :kind :contract :family :inline :children missing
        :attributes nil :edit-text :empty :edit-attributes ())
      "child slot")
     (:malformed-edit-text
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-edit-text value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:slot value :extra) :edit-attributes ())
      "invalid or missing edit text projection")
     (:edit-text-without-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-edit-text-without-slot value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:slot missing) :edit-attributes ())
      "invalid or missing edit text projection")
     (:malformed-dispatch-definition
      (cl-markdown-kit::define-node-dispatch
        (node-contract-invalid-dispatch)
        :operation :contract :family :inline
        (cl-markdown-kit:text nil))
      "requires (function-name lambda-list)")
     (:malformed-dispatch-options
      (cl-markdown-kit::define-node-dispatch
        (node-contract-missing-dispatch-options (context cl-markdown-kit::node phase))
        :operation :contract :family :inline)
      "requires :OPERATION and :FAMILY before clauses")
     (:missing-dispatch-type
      (cl-markdown-kit::define-node-dispatch
        (node-contract-incomplete-dispatch (context cl-markdown-kit::node phase))
        :operation :html-renderer :family :inline
        (cl-markdown-kit:text nil))
      "Missing node type clause")
     (:duplicate-dispatch-type
      (cl-markdown-kit::define-node-dispatch
        (node-contract-duplicate-dispatch (context cl-markdown-kit::node phase))
        :operation :html-renderer :family :inline
        (cl-markdown-kit:text nil)
        (cl-markdown-kit:text nil))
      "Duplicate node type clause")
     (:extra-dispatch-type
      (cl-markdown-kit::define-node-dispatch
        (node-contract-extra-dispatch (context cl-markdown-kit::node phase))
        :operation :html-renderer :family :inline
        (cl-markdown-kit:paragraph nil))
      "Extra node type clause")
     (:non-list-edit-attributes
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-list-edit-attributes cl-markdown-kit::value)
        :kind :contract :family :inline :children nil
        :attributes (:value) :edit-text :empty :edit-attributes 42)
      "edit attributes must map declared attribute slots")
     (:non-keyword-edit-attribute-name
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-keyword-edit-attribute cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((value :before)))
      "edit attributes must map declared attribute slots")
     (:non-keyword-edit-difference-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-keyword-edit-key cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((:value before)))
      "edit attributes must map declared attribute slots")
     (:extra-edit-attribute-elements
      (cl-markdown-kit::define-node
        (defstruct node-contract-extra-edit-attribute-elements cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((:value :before :extra)))
      "edit attributes must map declared attribute slots")
     (:child-with-non-symbol-accessor
      (cl-markdown-kit::define-node
        (defstruct node-contract-child-invalid-accessor value)
        :kind :contract :family :inline
        :children (:slot value :accessor 42)
        :attributes nil :edit-text :empty :edit-attributes ())
      "child slot")
     (:edit-text-with-invalid-head
      (cl-markdown-kit::define-node
        (defstruct node-contract-edit-text-invalid-head value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:other value) :edit-attributes ())
      "invalid or missing edit text projection")
     (:edit-text-with-non-symbol-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-edit-text-non-symbol-slot value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:slot 42) :edit-attributes ())
      "invalid or missing edit text projection")
     (:child-with-non-symbol-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-child-non-symbol-slot)
        :kind :contract :family :inline :children (:slot 42)
        :attributes nil :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE :CHILDREN value")
     (:non-list-child-spec
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-list-child-spec)
        :kind :contract :family :inline :children 42
        :attributes nil :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE :CHILDREN value"))
    "rejects invalid node and dispatch declarations during macro expansion: ~A"
    (failure form expected-message)
    (let ((condition
            (handler-case (progn (macroexpand-1 form) nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (let ((condition-message (format nil "~A" condition)))
        (unless (search expected-message condition-message)
          (error "Expected condition message containing ~S, got ~S."
                 expected-message condition-message)))))
  (it "expands node declarations, projects the registry, and registers idempotently"
    (let* ((node-form
             '(cl-markdown-kit::define-node
                (defstruct (coverage-contract-node
                            (:constructor make-coverage-contract-node ()))
                  value)
                :kind :coverage-contract
                :family :inline
                :children value
                :attributes ()
                :edit-text (:slot value)
                :edit-attributes ()))
           (node-expansion (macroexpand-1 node-form))
           (registry-expansion
             (macroexpand-1 '(cl-markdown-kit::define-node-registry)))
           (registry-function-names
             (mapcar #'second (rest registry-expansion))))
      (expect (first node-expansion) :to-equal 'progn)
      (expect (first (second node-expansion)) :to-equal 'defstruct)
      (expect (fourth node-expansion)
              :to-equal '(quote coverage-contract-node))
      (expect (first registry-expansion) :to-equal 'progn)
      (expect registry-function-names
              :to-equal
              '(cl-markdown-kit:node-children
                cl-markdown-kit:node-kind
                cl-markdown-kit:node-family
                cl-markdown-kit:node-semantic-attributes
                cl-markdown-kit:node-edit-text-projection)))
    (let ((definitions-before
            (copy-tree cl-markdown-kit::*node-definitions*)))
      (let ((cl-markdown-kit::*node-definitions* nil))
        (let* ((node-form
                 '(cl-markdown-kit::define-node
                    (defstruct node-contract-registration)
                    :kind :contract-registration
                    :family :inline
                    :children nil
                    :attributes ()
                    :edit-text :empty
                    :edit-attributes ()))
               (same-registration
                 (third (macroexpand-1 node-form)))
               (conflict-registration
                 (third
                  (macroexpand-1
                   '(cl-markdown-kit::define-node
                      (defstruct node-contract-registration)
                      :kind :different-registration
                      :family :inline
                      :children nil
                      :attributes ()
                      :edit-text :empty
                      :edit-attributes ()))))
               (expected-definition-count 1))
          (eval same-registration)
          (expect (length cl-markdown-kit::*node-definitions*)
                  :to-equal expected-definition-count)
          (let ((registered-definition
                  (copy-tree (first cl-markdown-kit::*node-definitions*))))
            (eval same-registration)
            (expect cl-markdown-kit::*node-definitions*
                    :to-equal (list registered-definition)))
          (let ((before-conflict
                  (copy-tree cl-markdown-kit::*node-definitions*))
                (condition
                  (handler-case
                      (progn (eval conflict-registration) nil)
                    (simple-error (condition) condition))))
            (expect condition :to-be-truthy)
            (expect (search "Conflicting DEFINE-NODE declarations"
                            (format nil "~A" condition))
                    :to-be-truthy)
            (expect cl-markdown-kit::*node-definitions*
                    :to-equal before-conflict))))
      (expect cl-markdown-kit::*node-definitions*
              :to-equal definitions-before)))
  (it "normalizes typed and inherited child slots"
    (let* ((node-expansion
             (macroexpand-1
              '(cl-markdown-kit::define-node
                 (defstruct (coverage-contract-inherited-node
                             (:include cl-markdown-kit:list-item))
                   (local-value nil :type list))
                 :kind :coverage-contract-inherited
                 :family :block
                 :children (:slot cl-markdown-kit::children)
                 :attributes ()
                 :edit-text :container
                 :edit-attributes ())))
           (registration-call (third (third node-expansion)))
           (definition (second (second registration-call))))
      (expect (first node-expansion) :to-equal 'progn)
      (expect (getf definition :slots) :to-equal '(local-value))
      (expect (getf definition :parent) :to-equal 'cl-markdown-kit:list-item)
      (expect (getf definition :children)
              :to-equal
              '(:slot cl-markdown-kit::children
                :accessor coverage-contract-inherited-node-children))))
  (it "validates node helper inputs and dispatch clauses"
    (progn
      (expect (cl-markdown-kit::%node-form-slot-name 'value) :to-equal 'value)
      (let ((condition
              (handler-case
                  (progn (cl-markdown-kit::%node-defstruct-name 42) nil)
                (simple-error (condition) condition))))
        (expect condition :to-be-truthy)
        (expect (search "requires a DEFSTRUCT form" (format nil "~A" condition))
                :to-be-truthy)))
    (expect (cl-markdown-kit::%node-form-slot-name '(value 0 :type integer))
            :to-equal 'value)
    (expect (cl-markdown-kit::%node-inherited-slot-p
             'cl-markdown-kit:list-item 'cl-markdown-kit::span)
            :to-be-truthy)
    (expect (cl-markdown-kit::%node-inherited-slot-p
             'cl-markdown-kit:list-item 'cl-markdown-kit::unknown-slot)
            :to-equal nil)
    (let ((condition
            (handler-case
                (progn
                  (macroexpand-1
                   '(cl-markdown-kit::define-node
                      (not-a-defstruct)
                      :kind :coverage-contract-invalid
                      :family :inline
                      :children nil
                      :attributes ()
                      :edit-text :empty
                      :edit-attributes ()))
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "requires a DEFSTRUCT form" (format nil "~A" condition))
              :to-be-truthy))
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::%node-attribute-slot 'not-a-keyword)
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "must be keywords" (format nil "~A" condition))
              :to-be-truthy))
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::%node-normalize-children
                   'coverage-contract-invalid-children
                   '(not-a-slot-form))
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "Invalid DEFINE-NODE :CHILDREN value"
                      (format nil "~A" condition))
              :to-be-truthy))
    (dolist (definition (list 42 '(42 (context cl-markdown-kit::node))))
      (let ((condition
              (handler-case
                  (progn
                    (macroexpand-1
                     (list 'cl-markdown-kit::define-node-dispatch
                           definition
                           :operation :coverage-contract
                           :family :inline
                           '(cl-markdown-kit:text nil)))
                    nil)
                (simple-error (condition) condition))))
        (expect condition :to-be-truthy)
        (expect
         (search "requires (function-name lambda-list)" (format nil "~A" condition))
         :to-be-truthy)))
    (let ((condition
            (handler-case
                (progn
                  (macroexpand-1
                   '(cl-markdown-kit::define-node-dispatch
                      (node-contract-non-list-clause
                       (context cl-markdown-kit::node phase))
                      :operation :coverage-contract
                      :family :inline
                      42))
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect
       (search "clauses must contain a type and body" (format nil "~A" condition))
       :to-be-truthy)))
  (it "expands dispatch declarations and validates registration state"
    (let* ((types (cl-markdown-kit::%node-family-types :inline))
           (clauses
             (mapcar (lambda (type) (list type nil)) types))
           (form
             (append
              (list 'cl-markdown-kit::define-node-dispatch
                    '(coverage-contract-dispatch
                      (context cl-markdown-kit::node phase))
                    :operation :coverage-contract
                    :family :inline)
              clauses))
           (expansion (macroexpand-1 form))
           (without-node-form
             (append
              (list 'cl-markdown-kit::define-node-dispatch
                    '(node-contract-without-node (context phase))
                    :operation :coverage-contract
                    :family :inline)
              clauses))
           (without-body-form
             (append
              (list 'cl-markdown-kit::define-node-dispatch
                    '(node-contract-without-body
                      (context cl-markdown-kit::node phase))
                    :operation :coverage-contract
                    :family :inline)
              (cons (list (first types)) (rest clauses))))
           (without-node-condition
             (handler-case
                 (progn (macroexpand-1 without-node-form) nil)
               (simple-error (condition) condition)))
           (without-body-condition
             (handler-case
                 (progn (macroexpand-1 without-body-form) nil)
               (simple-error (condition) condition)))
           (expected-record
             (list (list :coverage-contract :inline)
                   'coverage-contract-dispatch
                   types))
           (registrations-before
             (copy-tree
              cl-markdown-kit::*node-dispatch-registrations*)))
      (expect (first expansion) :to-equal 'progn)
      (expect (first (second expansion)) :to-equal 'defun)
      (expect without-node-condition :to-be-truthy)
      (expect
       (search "lambda list has no node argument"
               (format nil "~A" without-node-condition))
       :to-be-truthy)
      (expect without-body-condition :to-be-truthy)
      (expect
       (search "clauses must contain a type and body"
               (format nil "~A" without-body-condition))
       :to-be-truthy)
      (let ((cl-markdown-kit::*node-dispatch-registrations* nil))
        (let ((record
                (cl-markdown-kit::%register-node-dispatch
                 :coverage-contract :inline
                 'coverage-contract-dispatch types)))
          (expect record :to-equal expected-record)
          (expect cl-markdown-kit::*node-dispatch-registrations*
                  :to-equal (list expected-record))
          (cl-markdown-kit::%register-node-dispatch
           :coverage-contract :inline
           'coverage-contract-dispatch types)
          (expect cl-markdown-kit::*node-dispatch-registrations*
                  :to-equal (list expected-record))
          (progn
  (expect
   (cl-markdown-kit::verify-node-dispatches
    :operation :coverage-contract
    :families (quote (:inline)))
   :to-equal t)
  (expect
   (macroexpand-1
    (quote (cl-markdown-kit::verify-node-dispatches-at-compile-time
            :coverage-contract (:inline))))
   :to-equal nil))
          (let ((before-conflict
                  (copy-tree
                   cl-markdown-kit::*node-dispatch-registrations*))
                (condition
                  (handler-case
                      (progn
                        (cl-markdown-kit::%register-node-dispatch
                         :coverage-contract :inline
                         'coverage-contract-conflicting-dispatch types)
                        nil)
                    (simple-error (condition) condition))))
            (expect condition :to-be-truthy)
            (expect (search "Duplicate dispatch coverage"
                            (format nil "~A" condition))
                    :to-be-truthy)
            (expect cl-markdown-kit::*node-dispatch-registrations*
                    :to-equal before-conflict))))
      (expect cl-markdown-kit::*node-dispatch-registrations*
              :to-equal registrations-before)))
  (it "rejects invalid actions from enter and leave callbacks"
    (let ((root (cl-markdown-kit:make-document)))
      (dolist (invalid-phase '(:enter :leave))
        (expect
         (handler-case
             (progn
               (cl-markdown-kit:walk-node-tree
                root
                (lambda (frame)
                  (if (eq (cl-markdown-kit:node-tree-frame-phase frame)
                          invalid-phase)
                      :invalid
                      :descend)))
               nil)
           (simple-error () t))
         :to-be-truthy))))
  (it "validates default dispatch families and missing registrations"
    (let ((registrations-before
            (copy-tree
             cl-markdown-kit::*node-dispatch-registrations*)))
      (expect
       (cl-markdown-kit::verify-node-dispatches
        :operation :markdown-renderer)
       :to-equal t)
      (expect cl-markdown-kit::*node-dispatch-registrations*
              :to-equal registrations-before))
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::%node-family-types :unknown)
                  nil)
              (simple-error (condition) condition))))
      (expect (and condition
                   (search "Unknown node family" (format nil "~A" condition)))
              :to-be-truthy))
    (let ((cl-markdown-kit::*node-dispatch-registrations* nil))
      (let ((condition
              (handler-case
                  (progn
                    (cl-markdown-kit::verify-node-dispatches
                     :operation :missing
                     :families (list :inline))
                    nil)
                (simple-error (condition) condition))))
        (expect (and condition
                     (search "Missing dispatch coverage"
                             (format nil "~A" condition)))
                :to-be-truthy))))
  (it "resets the registries and restores global state"
    (let ((definitions-before
            (copy-tree cl-markdown-kit::*node-definitions*))
          (registrations-before
            (copy-tree cl-markdown-kit::*node-dispatch-registrations*)))
      (let ((cl-markdown-kit::*node-definitions* '(:temporary))
            (cl-markdown-kit::*node-dispatch-registrations* '(:temporary)))
        (cl-markdown-kit::%reset-node-registries)
        (expect cl-markdown-kit::*node-definitions* :to-equal nil)
        (expect cl-markdown-kit::*node-dispatch-registrations*
                :to-equal nil))
      (expect cl-markdown-kit::*node-definitions* :to-equal definitions-before)
      (expect cl-markdown-kit::*node-dispatch-registrations*
              :to-equal registrations-before)))
  (it "resets registries for the root node declaration"
    (let* ((node-expansion
             (macroexpand-1
              '(cl-markdown-kit::define-node
                 (defstruct (cl-markdown-kit:node
                             (:constructor cl-markdown-kit:make-node))
                   cl-markdown-kit::span)
                 :abstract t
                 :kind nil
                 :family :abstract
                 :children nil
                 :attributes ()
                 :edit-text :empty
                 :edit-attributes ())))
           (registration-call (third (third node-expansion)))
           (definition (second (second registration-call)))
           (stale-definition
             '(:name stale-node :abstract nil :kind :stale :family :inline
               :children nil :attributes nil :edit-attributes nil
               :edit-text :empty :parent nil :slots nil))
           (stale-registration '((:stale :inline) stale-dispatch nil)))
      (expect (first node-expansion) :to-equal 'progn)
      (expect (getf definition :name) :to-equal 'cl-markdown-kit:node)
      (let ((cl-markdown-kit::*node-definitions* (list stale-definition))
            (cl-markdown-kit::*node-dispatch-registrations*
              (list stale-registration)))
        (expect (cl-markdown-kit::%register-node-definition definition)
                :to-equal definition)
        (expect cl-markdown-kit::*node-definitions*
                :to-equal (list definition))
        (expect cl-markdown-kit::*node-dispatch-registrations*
                :to-equal nil))))
  (it "returns child callback results in sibling order"
    (let* ((first (cl-markdown-kit:make-paragraph))
           (second (cl-markdown-kit:make-code-block :body "second"))
           (root (cl-markdown-kit:make-document :children (list first second)))
           (observed nil))
      (cl-markdown-kit:walk-node-tree
       root
       (lambda (frame)
         (if (eq (cl-markdown-kit:node-tree-frame-phase frame) :enter)
             :descend
             (progn
               (if (eq (cl-markdown-kit:node-tree-frame-node frame) root)
                   (setf observed
                         (cl-markdown-kit:node-tree-frame-child-results frame))
                   (setf (cl-markdown-kit:node-tree-frame-result frame)
                         (cl-markdown-kit:node-tree-frame-node frame)))
               nil))))
      (expect observed :to-equal (list first second)))))
