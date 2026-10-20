(in-package #:cl-markdown-kit/test)

(describe "Edit validation API"
  (it "rejects non-source-backed and invalid edit targets"
    (let* ((source (test-source nil "# Heading" "text"))
           (document (cl-markdown-kit:parse-string source))
           (paragraph (second (cl-markdown-kit:document-children document)))
           (inline (first (cl-markdown-kit:heading-children
                           (first (cl-markdown-kit:document-children document)))))
           (foreign-document (cl-markdown-kit:parse-string "foreign"))
           (foreign-node (first (cl-markdown-kit:document-children foreign-document)))
           (foreign-condition
             (test-condition (lambda ()
                (cl-markdown-kit:replace-node document foreign-node "changed"))))
           (inline-condition
             (test-condition (lambda () (cl-markdown-kit:replace-node document inline "changed"))))
           (document-condition
             (test-condition (lambda () (cl-markdown-kit:replace-node document document "changed"))))
           (invalid-edit-condition
             (test-condition (lambda ()
                (cl-markdown-kit::%perform-edit document -1 0 "" nil nil))))
           (noninteger-edit-condition
             (test-condition (lambda ()
                (cl-markdown-kit::%perform-edit document :invalid 0 "" nil nil))))
           (missing-span-document (cl-markdown-kit:parse-string source))
           (missing-span-node
             (second (cl-markdown-kit:document-children missing-span-document)))
           (source-less (cl-markdown-kit:make-document))
           (source-less-condition
             (test-condition (lambda () (cl-markdown-kit:replace-section source-less "" "")))))
      (expect (typep foreign-condition 'simple-error) :to-be-truthy)
      (expect (typep inline-condition 'simple-error) :to-be-truthy)
      (expect (typep document-condition 'simple-error) :to-be-truthy)
      (expect (typep invalid-edit-condition 'simple-error) :to-be-truthy)
      (expect (typep noninteger-edit-condition 'simple-error) :to-be-truthy)
      (expect (typep source-less-condition 'simple-error) :to-be-truthy)
      (setf (cl-markdown-kit:document-source document) nil)
      (expect (cl-markdown-kit::%edit-document-source document) :to-equal source)
      (setf (cl-markdown-kit:node-span paragraph)
            (cl-parser-kit:make-span :source "foreign" :start 0 :end 1))
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:replace-node document paragraph "changed")))
                     'simple-error)
              :to-be-truthy)
      (setf (cl-markdown-kit:node-span missing-span-node) nil)
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:replace-node missing-span-document
                                                      missing-span-node "changed")))
                     'simple-error)
              :to-be-truthy)
      (let* ((bad-document (cl-markdown-kit:parse-string source))
             (bad-node (second (cl-markdown-kit:document-children bad-document))))
        (setf (cl-markdown-kit:node-span bad-node)
              (cl-parser-kit:make-span :source (cl-markdown-kit:document-source bad-document) :start -1 :end 1))
        (expect (typep (test-condition (lambda ()
                          (cl-markdown-kit:replace-node bad-document bad-node "changed")))
                       'simple-error)
                :to-be-truthy))))

  (it "validates task state inputs and supports unchecking"
    (let* ((source (test-source nil "- [x] checked" "- ordinary"))
           (document (cl-markdown-kit:parse-string source))
           (list-node (first (cl-markdown-kit:document-children document)))
           (task (first (cl-markdown-kit:list-node-items list-node)))
           (ordinary (second (cl-markdown-kit:list-node-items list-node)))
           (foreign-task
             (first (cl-markdown-kit:list-node-items
                     (first (cl-markdown-kit:document-children
                             (cl-markdown-kit:parse-string "- [ ] foreign")))))))
      (expect (cl-markdown-kit:set-task-checked document task nil)
              :to-equal (test-source nil "- [ ] checked" "- ordinary"))
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:set-task-checked document ordinary t)))
                     'simple-error)
              :to-be-truthy)
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:set-task-checked document foreign-task t)))
                     'simple-error)
              :to-be-truthy)
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:set-task-checked document task :checked)))
                     'type-error)
              :to-be-truthy)
      (expect (typep (test-condition (lambda ()
                        (cl-markdown-kit:insert-section document "" "bad"
                                                        :position :middle)))
                     'type-error)
              :to-be-truthy)
      (let* ((invalid-marker-document (cl-markdown-kit:parse-string "- [ ] first"))
             (invalid-task
               (first (cl-markdown-kit:list-node-items
                       (first (cl-markdown-kit:document-children invalid-marker-document)))))
             (marker-span
               (cl-markdown-kit:task-list-item-task-marker-span invalid-task)))
        (setf (cl-parser-kit:span-start marker-span) 6
              (cl-parser-kit:span-end marker-span) 9)
        (expect (typep (test-condition (lambda ()
                          (cl-markdown-kit:set-task-checked
                           invalid-marker-document invalid-task t)))
                       'simple-error)
                :to-be-truthy))))

  (it "retries a section insertion after adding required block separation"
    (let* ((source (test-source nil "# A" "body" "" "# B" "other"))
           (document (cl-markdown-kit:parse-string source))
           (inserted (cl-markdown-kit:insert-section
                      document "/B" "new paragraph" :position :before)))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string inserted))
              :to-equal '("A" "B"))
      (expect (search (test-source nil "new paragraph" "" "# B") inserted)
              :to-be-truthy)))

  (it "signals without returning source when a non-target block is consumed"
    (let* ((source (test-source nil "# A" "old" "" "# B" "keep"))
           (document (cl-markdown-kit:parse-string source))
           (condition
             (test-condition (lambda ()
                (cl-markdown-kit:replace-section-body
                 document "/A" (test-source nil "```" "code"))))))
      (expect (typep condition
                     'cl-markdown-kit:markdown-structure-changed-error)
              :to-be-truthy)
      (expect (cl-markdown-kit:markdown-structure-changed-error-target condition)
              :to-be-truthy)
      (let ((difference
              (cl-markdown-kit:markdown-structure-changed-error-difference
               condition)))
        (expect difference :to-be-truthy)
        (expect (< (length difference) 80) :to-be-truthy)
        (expect (search "keep" difference) :to-equal nil))
      (let* ((reference-source
               (test-source nil "[ref]: /old" "" "[label][ref]" "" "target"))
             (reference-document (cl-markdown-kit:parse-string reference-source))
             (definition (first
                          (cl-markdown-kit:document-children reference-document)))
             (reference-condition
               (test-condition (lambda ()
                  (cl-markdown-kit:replace-node
                   reference-document definition "[ref]: /new")))))
        (expect (typep reference-condition
                       'cl-markdown-kit:markdown-structure-changed-error)
                :to-be-truthy))))
  (it "retains :front-matter nil when verifying edits"
      (let* ((source (test-source nil "---" "title: x" "---" ""
                                 "# A" "" "old" ""))
             (document (cl-markdown-kit:parse-string
                        source :front-matter nil))
             (edited (cl-markdown-kit:replace-section-body
                      document "/A" "new")))
        (expect (edit-heading-texts
                 (cl-markdown-kit:parse-string
                  edited :front-matter nil))
                :to-equal (list "title: x" "A"))
        (expect (search "new" edited) :to-be-truthy)))
  (it "normalizes truthy :front-matter values for edits"
      (let* ((source (test-source nil "# A" "" "old" ""))
             (document (cl-markdown-kit:parse-string
                        source :front-matter :enabled))
             (edited (cl-markdown-kit:replace-section-body
                      document "/A" "new")))
        (expect (cl-markdown-kit:document-front-matter-p document)
                :to-be-truthy)
        (expect (search "new" edited) :to-be-truthy)))
  (it "retains :gfm nil when verifying edits"
      (let* ((source (test-source nil "# A" "" "x" "" "# B" ""
                                 "| a |" "| - |" ""))
             (document (cl-markdown-kit:parse-string source :gfm nil))
             (edited (cl-markdown-kit:replace-section-body
                      document "/A" "new")))
        (expect (edit-heading-texts
                 (cl-markdown-kit:parse-string edited :gfm nil))
                :to-equal (list "A" "B"))
        (expect (search "| a |" edited) :to-be-truthy)))
  (it "preserves the prefix before an EOF section insertion"
      (let* ((source (test-source nil "# A" "x" "" "" "" ""))
             (document (cl-markdown-kit:parse-string source))
             (section-span
              (cl-markdown-kit:section-record-span
               (cl-markdown-kit:find-section document "/A")))
             (insert-offset (cl-parser-kit:span-end section-span))
             (suffix-octets (edit-utf8 (subseq source insert-offset)))
             (edited (cl-markdown-kit:insert-section document "/A" "# B"))
             (edited-octets (edit-utf8 edited)))
        (expect (and (<= insert-offset (length edited))
                     (equalp (edit-utf8 (subseq source 0 insert-offset))
                             (edit-utf8 (subseq edited 0 insert-offset))))
                :to-be-truthy)
        (expect (and (<= (length suffix-octets) (length edited-octets))
                     (equalp suffix-octets
                             (subseq edited-octets
                                     (- (length edited-octets)
                                        (length suffix-octets)))))
                :to-be-truthy)
        (expect (edit-heading-texts (cl-markdown-kit:parse-string edited))
                :to-equal (list "A" "B"))))
  (it "preserves the prefix before an EOF section replacement"
      (let* ((source (test-source nil "# Before" "keep" "" "# A" "x" "" "" "" ""))
             (document (cl-markdown-kit:parse-string source))
             (section-span
              (cl-markdown-kit:section-record-span
               (cl-markdown-kit:find-section document "/A")))
             (prefix-end (cl-parser-kit:span-start section-span))
             (suffix-start (cl-parser-kit:span-end section-span))
             (suffix-octets (edit-utf8 (subseq source suffix-start)))
             (edited (cl-markdown-kit:replace-section
                      document "/A" (test-source nil "## New" "changed")))
             (edited-octets (edit-utf8 edited)))
        (expect (and (<= prefix-end (length edited))
                     (equalp (edit-utf8 (subseq source 0 prefix-end))
                             (edit-utf8 (subseq edited 0 prefix-end))))
                :to-be-truthy)
        (expect (and (<= (length suffix-octets) (length edited-octets))
                     (equalp suffix-octets
                             (subseq edited-octets
                                     (- (length edited-octets)
                                        (length suffix-octets)))))
                :to-be-truthy)
        (expect (search "changed" edited) :to-be-truthy)))
  (it "preserves the prefix before deleting an EOF section"
      (let* ((source (test-source nil "# Before" "keep" "" "# A" "x" "" "" "" ""))
             (document (cl-markdown-kit:parse-string source))
             (section-span
              (cl-markdown-kit:section-record-span
               (cl-markdown-kit:find-section document "/A")))
             (prefix-end (cl-parser-kit:span-start section-span))
             (suffix-start (cl-parser-kit:span-end section-span))
             (suffix-octets (edit-utf8 (subseq source suffix-start)))
             (edited (cl-markdown-kit:delete-section document "/A"))
             (edited-octets (edit-utf8 edited)))
        (expect (and (<= prefix-end (length edited))
                     (equalp (edit-utf8 (subseq source 0 prefix-end))
                             (edit-utf8 (subseq edited 0 prefix-end))))
                :to-be-truthy)
        (expect (and (<= (length suffix-octets) (length edited-octets))
                     (equalp suffix-octets
                             (subseq edited-octets
                                     (- (length edited-octets)
                                        (length suffix-octets)))))
                :to-be-truthy)))
  (it "returns the verified edit string"
    (let* ((document (cl-markdown-kit:parse-string "# A"))
           (target (first (cl-markdown-kit:document-children document)))
           (range (edit-node-range target))
           (markdown "# B")
           (edited
             (cl-markdown-kit::%perform-edit
              document (car range) (cdr range) markdown target range)))
      (expect edited :to-equal markdown)))
  (it "signals edit verification conditions directly"
    (let ((document (cl-markdown-kit:parse-string "# A")))
      (let ((range-condition
              (test-condition
               (lambda ()
                 (cl-markdown-kit::%perform-edit
                  document -1 0 "" nil nil)))))
        (expect (typep range-condition 'simple-error) :to-be-truthy)
        (expect (simple-condition-format-control range-condition)
                :to-equal "Invalid edit interval [~S,~S) for source length ~D.")
        (expect (simple-condition-format-arguments range-condition)
                :to-equal '(-1 0 3)))
      (let ((source-condition
              (test-condition
               (lambda ()
                 (cl-markdown-kit::%perform-edit
                  (cl-markdown-kit:make-document) 0 0 "" nil nil)))))
        (expect (typep source-condition 'simple-error) :to-be-truthy)
        (expect (simple-condition-format-control source-condition)
                :to-equal "Cannot edit a document without a source snapshot.")
        (expect (simple-condition-format-arguments source-condition)
                :to-equal nil))
      (let* ((source "text")
             (document (cl-markdown-kit:parse-string
                        source :max-nesting-depth 8))
             (replacement
               (with-output-to-string (output)
                 (loop repeat 9 do (write-string "> " output))
                 (write-string "nested" output)))
             (parse-condition
               (test-condition
                (lambda ()
                  (cl-markdown-kit::%perform-edit
                   document 0 (length source) replacement nil nil)))))
        (expect (typep parse-condition
                       'cl-markdown-kit:markdown-nesting-depth-exceeded)
                :to-be-truthy)
        (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
                 parse-condition)
                :to-equal :nesting-depth)
        (expect (cl-markdown-kit:markdown-resource-limit-error-limit
                 parse-condition)
                :to-equal 8)
        (expect (cl-markdown-kit:markdown-resource-limit-error-actual
                 parse-condition)
                :to-equal 9))))
  (it "rejects an edit candidate above the document input-size limit"
    (let* ((source "# A")
           (document (cl-markdown-kit:parse-string
                      source :max-input-size 5))
           (candidate (make-string 101 :initial-element #\x))
           (condition
             (test-condition
              (lambda ()
                (cl-markdown-kit::%perform-edit
                 document 0 (length source) candidate nil nil)))))
      (expect (length candidate) :to-equal 101)
      (expect (typep condition
                     (quote cl-markdown-kit:markdown-resource-limit-error))
              :to-be-truthy)
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
               condition)
              :to-equal :input-size)
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit condition)
              :to-equal 5)
      (expect (cl-markdown-kit:markdown-resource-limit-error-actual condition)
              :to-equal 6)))
  (it "accepts an edit candidate above the default input-size limit"
    (let* ((default-limit cl-markdown-kit:+default-max-input-size+)
           (raised-limit (1+ default-limit))
           (source "# A")
           (document (cl-markdown-kit:parse-string
                      source :max-input-size raised-limit))
           (replacement
             (make-string (- raised-limit 4) :initial-element #\Space))
           (edited
             (cl-markdown-kit:replace-section-body
              document "/A" replacement)))
      (expect (cl-markdown-kit:document-max-input-size document)
              :to-equal raised-limit)
      (expect (length replacement) :to-equal (- raised-limit 4))
      (expect (length edited) :to-equal raised-limit)
      (expect (> (length edited) default-limit) :to-be-truthy))))
