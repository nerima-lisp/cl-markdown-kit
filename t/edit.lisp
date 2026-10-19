(in-package #:cl-markdown-kit/test)

(defun edit-line-ending (style)
  (case style
    (:lf (string #\Newline))
    (:cr (string #\Return))
    (:crlf (coerce (list #\Return #\Newline) 'string))))

(defun edit-node-range (node)
  (let ((span (cl-markdown-kit:node-span node)))
    (cons (cl-parser-kit:span-start span)
          (cl-parser-kit:span-end span))))

(defun edit-span-range (span)
  (cons (cl-parser-kit:span-start span)
        (cl-parser-kit:span-end span)))

(defun edit-utf8 (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun edit-source-octets (document string)
  (let* ((source-format (cl-markdown-kit:document-source-format document))
         (encoding (cl-markdown-kit:source-format-encoding source-format))
         (bom-character (string (code-char #xfeff)))
         (has-leading-bom
           (and (cl-markdown-kit:source-format-bom-p source-format)
                (plusp (length string))
                (char= (char string 0) (char bom-character 0))))
         (payload (if has-leading-bom (subseq string 1) string))
         (payload-octets (sb-ext:string-to-octets
                          payload :external-format encoding)))
    (if has-leading-bom
        (concatenate '(vector (unsigned-byte 8))
                     (sb-ext:string-to-octets
                      bom-character :external-format encoding)
                     payload-octets)
        payload-octets)))

(defun edit-has-final-newline-p (source)
  (and (plusp (length source))
       (not (null
             (find (char source (1- (length source)))
                   '(#\Return #\Newline))))))

(defun edit-heading-texts (document)
  (mapcar #'cl-markdown-kit:heading-record-plain-text
          (cl-markdown-kit:document-headings document)))

(describe "Format-preserving edit API"
  (it "replaces section bodies while keeping surrounding source exact"
    (let* ((source (test-source nil "# A" "old" "" "# B" "keep"))
           (document (cl-markdown-kit:parse-string source))
           (section (cl-markdown-kit:find-section document "/A"))
           (section-span (cl-markdown-kit:section-record-span section))
           (heading (cl-markdown-kit:heading-record-node
                     (cl-markdown-kit:section-record-heading section)))
           (body-start (cl-parser-kit:span-end
                        (cl-markdown-kit:node-span heading)))
           (body-start (if (char= (char source body-start) #\Newline)
                           (1+ body-start) body-start))
           (section-end (cl-parser-kit:span-end section-span))
           (edited (cl-markdown-kit:replace-section-body
                    document "/A" (test-source nil "new" "line"))))
      (expect (subseq edited 0 body-start)
              :to-equal (subseq source 0 body-start))
      (expect (subseq edited (- (length edited)
                                (- (length source) section-end)))
              :to-equal (subseq source section-end))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string edited))
              :to-equal '("A" "B"))))

  (it "starts a replacement body after an unterminated final heading"
    (let* ((source "# Heading")
           (document (cl-markdown-kit:parse-string source))
           (edited (cl-markdown-kit:replace-section-body
                    document "/Heading" "body"))
           (empty (cl-markdown-kit:replace-section-body
                   document "/Heading" "")))
      (expect edited :to-equal (concatenate 'string source (edit-line-ending :lf)
                                            "body"))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string edited))
              :to-equal '("Heading"))
      (expect empty :to-equal source)))

  (it "preserves bytes outside an edited node for BOM-marked octet input"
    (let* ((source (concatenate 'string
                                (string (code-char #xfeff))
                                "before café" (edit-line-ending :crlf)
                                (edit-line-ending :crlf)
                                "old A" (edit-line-ending :crlf) "old B"
                                (edit-line-ending :crlf)
                                (edit-line-ending :crlf) "tail déjà"))
           (source-octets (edit-utf8 source))
           (document (cl-markdown-kit:parse-octets source-octets))
           (source-format (cl-markdown-kit:document-source-format document))
           (target (second (cl-markdown-kit:document-children document)))
           (edited (cl-markdown-kit:replace-node document target "new text"))
           (edited-octets (edit-source-octets document edited))
           (prefix-octets
             (edit-source-octets
              document (subseq source 0 (car (edit-node-range target)))))
           (suffix-octets
             (edit-source-octets
              document (subseq source (cdr (edit-node-range target)))))
           (bom-octets
             (sb-ext:string-to-octets (string (code-char #xfeff))
                                      :external-format
                                      (cl-markdown-kit:source-format-encoding
                                       source-format))))
      (expect (cl-markdown-kit:source-format-input-kind source-format)
              :to-equal :octets)
      (expect (cl-markdown-kit:source-format-encoding source-format)
              :to-equal :utf-8)
      (expect (cl-markdown-kit:source-format-bom-p source-format)
              :to-be-truthy)
      (expect (edit-source-octets document source) :to-equalp source-octets)
      (expect (subseq edited-octets 0 (length bom-octets))
              :to-equalp bom-octets)
      (expect (subseq edited-octets 0 (length prefix-octets))
              :to-equalp (subseq source-octets 0 (length prefix-octets)))
      (expect (subseq edited-octets (- (length edited-octets)
                                       (length suffix-octets)))
              :to-equalp (subseq source-octets (- (length source-octets)
                                                 (length suffix-octets))))))

  (it "replaces and deletes complete sections"
    (let* ((source (test-source nil "# A" "old" "" "# B" "keep"))
           (document (cl-markdown-kit:parse-string source))
           (section-span
             (cl-markdown-kit:section-record-span
              (cl-markdown-kit:find-section document "/A")))
           (end (cl-parser-kit:span-end section-span))
           (replaced (cl-markdown-kit:replace-section
                      document "/A" (test-source nil "## New" "changed")))
           (deleted (cl-markdown-kit:delete-section document "/A")))
      (expect (subseq replaced (- (length replaced) (- (length source) end)))
              :to-equal (subseq source end))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string replaced))
              :to-equal '("New" "B"))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string deleted))
              :to-equal '("B"))
      (expect deleted :to-equal (test-source nil "# B" "keep"))))

  (it "inserts before, after, and as the last child section"
    (let* ((source (test-source nil "# A" "body" "" "## Child" "child"
                               "" "### Grandchild" "grand"
                               "" "## Second child" "second"
                               "" "# B" "other"))
           (document (cl-markdown-kit:parse-string source))
           (before (cl-markdown-kit:insert-section
                    document "/B" (test-source nil "## Before" "inserted")
                    :position :before))
           (after (cl-markdown-kit:insert-section
                   document "/A/Child" (test-source nil "### After" "inserted")
                   :position :after))
           (last-child (cl-markdown-kit:insert-section
                        document "/A" (test-source nil "## Last" "inserted")
                        :position :last-child))
           (last-child-of-leaf (cl-markdown-kit:insert-section
                                document "/B" "## Leaf"
                                :position :last-child)))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string before))
              :to-equal '("A" "Child" "Grandchild" "Second child"
                          "Before" "B"))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string after))
              :to-equal '("A" "Child" "Grandchild" "After"
                          "Second child" "B"))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string last-child))
              :to-equal '("A" "Child" "Grandchild" "Second child"
                          "Last" "B"))
      (expect (edit-heading-texts
               (cl-markdown-kit:parse-string last-child-of-leaf))
              :to-equal '("A" "Child" "Grandchild" "Second child"
                          "B" "Leaf"))))

  (it "replaces nested blockquote and list blocks with continuation prefixes"
    (let* ((quote-source (test-source nil "> before" ">" "> old" "> continued"
                                     ">" "> after"))
           (quote-document (cl-markdown-kit:parse-string quote-source))
           (quote-node (first (cl-markdown-kit:document-children quote-document)))
           (quote-paragraph (second (cl-markdown-kit:block-quote-children quote-node)))
           (quote-range (edit-node-range quote-paragraph))
           (quote-edited (cl-markdown-kit:replace-node
                          quote-document quote-paragraph
                          (test-source nil "new A" "new B")))
           (list-source (test-source nil "- before" "" "- old" "  continued"
                                    "" "- after"))
           (list-document (cl-markdown-kit:parse-string list-source))
           (list-node (first (cl-markdown-kit:document-children list-document)))
           (list-item (second (cl-markdown-kit:list-node-items list-node)))
           (list-paragraph (first (cl-markdown-kit:list-item-children list-item)))
           (list-range (edit-node-range list-paragraph))
           (list-edited (cl-markdown-kit:replace-node
                         list-document list-paragraph
                         (test-source nil "new A" "new B"))))
      (expect (search (test-source nil "> new A" "> new B") quote-edited)
              :to-be-truthy)
      (expect (search (test-source nil "- new A" "  new B") list-edited)
              :to-be-truthy)
      (expect (subseq quote-edited 0 (car quote-range))
              :to-equal (subseq quote-source 0 (car quote-range)))
      (expect (subseq quote-edited (- (length quote-edited)
                                      (- (length quote-source) (cdr quote-range))))
              :to-equal (subseq quote-source (cdr quote-range)))
      (expect (subseq list-edited 0 (car list-range))
              :to-equal (subseq list-source 0 (car list-range)))
      (expect (subseq list-edited (- (length list-edited)
                                     (- (length list-source) (cdr list-range))))
              :to-equal (subseq list-source (cdr list-range)))
      (expect (search "> after" quote-edited) :to-be-truthy)
      (expect (search "- after" list-edited) :to-be-truthy)))

  (it "changes only a task marker and preserves an unchanged marker spelling"
    (let* ((source (test-source nil "- [ ] first" "- [X] second"))
           (document (cl-markdown-kit:parse-string source))
           (list-node (first (cl-markdown-kit:document-children document)))
           (first-task (first (cl-markdown-kit:list-node-items list-node)))
           (second-task (second (cl-markdown-kit:list-node-items list-node)))
           (marker-range (edit-span-range
                          (cl-markdown-kit:task-list-item-task-marker-span first-task)))
           (checked (cl-markdown-kit:set-task-checked document first-task t))
           (same (cl-markdown-kit:set-task-checked document second-task t)))
      (expect (subseq checked 0 (car marker-range))
              :to-equal (subseq source 0 (car marker-range)))
      (expect (subseq checked (+ (car marker-range) 3))
              :to-equal (subseq source (+ (cdr marker-range))))
      (expect checked :to-equal (test-source nil "- [x] first" "- [X] second"))
      (expect same :to-equal source)))

  (it-each ((:lf nil nil)
          (:lf nil t)
          (:lf t nil)
          (:lf t t)
          (:crlf nil nil)
          (:crlf nil t)
          (:crlf t nil)
          (:crlf t t)
          (:cr nil nil)
          (:cr nil t)
          (:cr t nil)
          (:cr t t)
          (:mixed nil nil)
          (:mixed nil t)
          (:mixed t nil)
          (:mixed t t))
    "preserves source with ~S endings, BOM ~S, and final newline ~S"
    (style bom-p final-newline-p)
  (let* ((outer (if (eq style :mixed)
                    (edit-line-ending :lf)
                    (edit-line-ending style)))
         (inner (if (eq style :mixed)
                    (edit-line-ending :cr)
                    outer))
         (tail (if (eq style :mixed)
                   (edit-line-ending :crlf)
                   outer))
         (final (if (eq style :mixed)
                    (edit-line-ending :cr)
                    outer))
         (bom (if bom-p (string (code-char #xfeff)) ""))
         (source
           (concatenate 'string
                        bom "before café" outer outer
                        "old A" inner "old B" outer outer
                        "tail A" tail "tail B"
                        (if final-newline-p final "")))
         (document (cl-markdown-kit:parse-string source))
         (middle (second (cl-markdown-kit:document-children document)))
         (middle-range (edit-node-range middle))
         (middle-edited (cl-markdown-kit:replace-node
                         document middle (test-source nil "new A" "new B")))
         (middle-prefix (subseq source 0 (car middle-range)))
         (middle-suffix (subseq source (cdr middle-range)))
         (last-node (third (cl-markdown-kit:document-children document)))
         (last-range (edit-node-range last-node))
         (last-edited (cl-markdown-kit:replace-node
                       document last-node (test-source nil "tail C" "tail D"))))
    (expect (subseq middle-edited 0 (length middle-prefix))
            :to-equal middle-prefix)
    (expect (subseq middle-edited (- (length middle-edited)
                                     (length middle-suffix)))
            :to-equal middle-suffix)
    (expect (search (concatenate 'string "new A" inner "new B")
                    middle-edited)
            :to-be-truthy)
    (expect (equalp (edit-utf8
                     (subseq middle-edited 0 (length middle-prefix)))
                    (edit-utf8 middle-prefix))
            :to-be-truthy)
    (expect (equalp
             (edit-utf8
              (subseq middle-edited (- (length middle-edited)
                                       (length middle-suffix))))
             (edit-utf8 middle-suffix))
            :to-be-truthy)
    (expect (edit-has-final-newline-p middle-edited)
            :to-equal final-newline-p)
    (expect (if bom-p
                (= (char-code (char middle-edited 0)) #xfeff)
                (not (and (plusp (length middle-edited))
                          (= (char-code (char middle-edited 0)) #xfeff))))
            :to-be-truthy)
    (expect (subseq last-edited 0 (car last-range))
            :to-equal (subseq source 0 (car last-range)))
    (expect (edit-has-final-newline-p last-edited)
            :to-equal final-newline-p)
    (expect (search (concatenate 'string "tail C" tail "tail D") last-edited)
            :to-be-truthy)
    (expect (cl-markdown-kit:parse-string middle-edited) :to-be-truthy)
    (expect (cl-markdown-kit:parse-string last-edited) :to-be-truthy)))

  (it "uses the nearest preceding line ending before a following one"
    (let* ((source (concatenate 'string "before"
                                (edit-line-ending :cr)
                                (edit-line-ending :cr) "---"
                                (edit-line-ending :lf)
                                (edit-line-ending :lf) "# After"))
           (document (cl-markdown-kit:parse-string source))
           (thematic-break (second (cl-markdown-kit:document-children document)))
           (range (edit-node-range thematic-break))
           (edited (cl-markdown-kit:replace-node
                    document thematic-break (test-source nil "new A" "new B"))))
      (expect (subseq edited 0 (car range))
              :to-equal (subseq source 0 (car range)))
      (expect (search (coerce (list #\Newline) 'string) edited)
              :to-be-truthy)
      (expect (search (concatenate 'string "new A" (edit-line-ending :cr) "new B")
                      edited)
              :to-be-truthy)
      (let* ((following-source
               (concatenate 'string "---"
                            (edit-line-ending :cr)
                            (edit-line-ending :cr) "# After"))
             (following-document (cl-markdown-kit:parse-string following-source))
             (following-break (first
                               (cl-markdown-kit:document-children following-document)))
             (following-edited
               (cl-markdown-kit:replace-node
                following-document following-break
                (test-source nil "next A" "next B"))))
        (expect (search (concatenate 'string "next A" (edit-line-ending :cr) "next B")
                        following-edited)
                :to-be-truthy))))

  (it "covers edit helper boundaries for newline and splice decisions"
    (let* ((lf (edit-line-ending :lf))
           (cr (edit-line-ending :cr))
           (crlf (edit-line-ending :crlf))
           (bom (string (code-char #xfeff))))
      (expect (cl-markdown-kit::%line-ending-at "x" 1) :to-equal nil)
      (expect (cl-markdown-kit::%find-line-ending "text" 0 4) :to-equal nil)
      (expect (cl-markdown-kit::%edit-line-ending "text" 1 3) :to-equal lf)
      (expect (cl-markdown-kit::%normalize-newlines
               (concatenate 'string "a" crlf "b" cr "c" lf "d") lf)
              :to-equal (test-source nil "a" "b" "c" "d"))
      (expect (cl-markdown-kit::%normalize-newlines
               (concatenate 'string "x" cr) lf)
              :to-equal (concatenate 'string "x" lf))
      (expect (cl-markdown-kit::%trailing-line-ending-count "") :to-equal 0)
      (expect (cl-markdown-kit::%trailing-line-ending-count "x") :to-equal 0)
      (expect (cl-markdown-kit::%trailing-line-ending-count lf) :to-equal 1)
      (expect (cl-markdown-kit::%trailing-line-ending-count (concatenate 'string "x" lf))
              :to-equal 1)
      (expect (cl-markdown-kit::%trailing-line-ending-count
               (concatenate 'string "x" crlf crlf))
              :to-equal 2)
      (expect (cl-markdown-kit::%trailing-line-ending-count (concatenate 'string "x" cr))
              :to-equal 1)
      (expect (cl-markdown-kit::%leading-line-ending-count "") :to-equal 0)
      (expect (cl-markdown-kit::%leading-line-ending-count (concatenate 'string crlf cr))
              :to-equal 2)
      (expect (cl-markdown-kit::%leading-line-ending-count (concatenate 'string "x" lf))
              :to-equal 0)
      (expect (cl-markdown-kit::%has-content-p "") :to-equal nil)
      (expect (cl-markdown-kit::%has-content-p
               (concatenate 'string " " lf (string #\Tab)))
              :to-equal nil)
      (expect (cl-markdown-kit::%has-content-p bom) :to-equal nil)
      (expect (cl-markdown-kit::%has-content-p "x") :to-equal t)
      (expect (cl-markdown-kit::%ensure-boundary-separation
               (concatenate 'string "left" lf) "" "right" lf)
              :to-equal lf)
      (expect (cl-markdown-kit::%ensure-boundary-separation
               "left" "new" "right" lf)
              :to-equal (concatenate 'string lf lf "new" lf lf))
      (expect (cl-markdown-kit::%ensure-boundary-separation
               "" "   " "right" lf)
              :to-equal "   ")
      (expect (cl-markdown-kit::%ensure-boundary-separation
               "left" "new" "" lf)
              :to-equal (concatenate 'string lf lf "new"))
      (expect (cl-markdown-kit::%ensure-boundary-separation
               "" "new" "right" lf)
              :to-equal (concatenate 'string "new" lf lf))
      (expect (cl-markdown-kit::%ensure-boundary-separation
               "left" (concatenate 'string "new" lf lf) "right" lf)
              :to-equal (concatenate 'string lf lf "new" lf lf))
      (expect (cl-markdown-kit::%ensure-boundary-separation
               (concatenate 'string "left" lf lf) "" "right" lf)
              :to-equal "")
      (expect (cl-markdown-kit::%set-trailing-newline-state
               (concatenate 'string "text" cr crlf) (concatenate 'string "source" crlf) lf)
              :to-equal (concatenate 'string "text" lf))
      (expect (cl-markdown-kit::%set-trailing-newline-state "text" "source" lf)
              :to-equal "text")
      (expect (cl-markdown-kit::%set-trailing-newline-state "" "" lf)
              :to-equal "")))

  (it "edits the whole document while retaining a BOM and EOF newline state"
    (let* ((lf (edit-line-ending :lf))
           (bom (string (code-char #xfeff)))
           (source (concatenate 'string bom (test-source nil "# A" "body") lf))
           (document (cl-markdown-kit:parse-string source))
           (replaced (cl-markdown-kit:replace-section
                      document "" (test-source nil "## New" "changed")))
           (body-replaced (cl-markdown-kit:replace-section-body
                           document "" (test-source nil "# New" "body")))
           (deleted (cl-markdown-kit:delete-section document ""))
           (inserted-before (cl-markdown-kit:insert-section
                             document "" "## Before" :position :before))
           (inserted-after (cl-markdown-kit:insert-section
                            document "" "## After" :position :after))
           (inserted-last (cl-markdown-kit:insert-section
                           document "" "## Last" :position :last-child))
           (inserted-default (cl-markdown-kit:insert-section
                              document "" "## Default"))
           (plain-document (cl-markdown-kit:parse-string ""))
           (plain-source-document (cl-markdown-kit:parse-string "# Existing"))
           (plain-inserted-before (cl-markdown-kit:insert-section
                                   plain-source-document "" "## Before"
                                   :position :before)))
      (expect replaced :to-equal (concatenate 'string bom
                                               (test-source nil "## New" "changed") lf))
      (expect body-replaced :to-equal (concatenate 'string bom
                                                    (test-source nil "# New" "body") lf))
      (expect deleted :to-equal (concatenate 'string bom lf))
      (expect (char-code (char inserted-before 0)) :to-equal #xfeff)
      (expect (edit-heading-texts (cl-markdown-kit:parse-string inserted-before))
              :to-equal '("Before" "A"))
      (expect (search (concatenate 'string lf "## After") inserted-after)
              :to-be-truthy)
      (expect (search (concatenate 'string lf "## Last") inserted-last)
              :to-be-truthy)
      (expect (edit-heading-texts (cl-markdown-kit:parse-string inserted-default))
              :to-equal '("A" "Default"))
      (expect (cl-markdown-kit:insert-section
               plain-document "" "# New" :position :before)
              :to-equal "# New")
      (expect (cl-markdown-kit:insert-section
               plain-document "" "# New" :position :after)
              :to-equal "# New")
      (expect (edit-heading-texts
               (cl-markdown-kit:parse-string plain-inserted-before))
              :to-equal '("Before" "Existing"))
      (expect (cl-markdown-kit:replace-section-body
               (cl-markdown-kit:parse-string "# Heading") "/Heading" "")
              :to-equal "# Heading")
      (expect (cl-markdown-kit:replace-section-body plain-document "" "")
              :to-equal "")))

  (it "keeps semantic projections for the supported block and inline kinds"
    (let* ((source
             (test-source nil "---" "title: preserved" "---"
                         "# Heading"
                         "`code` <span>html</span> <https://example.com> [link](/url \"title\") ![image](/image \"caption\") [ref][id]"
                         "[id]: /reference \"reference title\""
                         ""
                         "[standalone]: /destination \"definition title\""
                         ""
                         "---"
                         ""
                         "```lang"
                         "code body"
                         "```"
                         ""
                         "<div>"
                         "html body"
                         "</div>"
                         ""
                         "- [x] checked"
                         "- ordinary"
                         ""
                         "| left | right |"
                         "| :--- | ---: |"
                         "| a | b |"
                         ""
                         "[^note]: footnote body"
                         ""
                         "[^note]"
                         ""
                         "target"))
           (document (cl-markdown-kit:parse-string source))
           (target (car (last (cl-markdown-kit:document-children document))))
           (updated (cl-markdown-kit:replace-node document target "changed")))
      (expect target :to-be-truthy)
      (expect (cl-markdown-kit:parse-string updated) :to-be-truthy)
      (expect (search "changed" updated) :to-be-truthy)))

  (it-each ((1 3 5 8 4 20 15 1 3)
            (10 12 5 8 4 20 15 11 13)
            (2 12 5 8 4 20 15 2 13)
            (6 7 5 8 4 20 15 5 9)
            (8 15 10 15 3 13 15 8 13))
      "maps edit intervals through replacement ranges"
      (range-start range-end edit-start edit-end replacement-length
       source-length candidate-length expected-start expected-end)
    (expect (cl-markdown-kit::%map-edit-range
             (cons range-start range-end)
             edit-start edit-end replacement-length source-length candidate-length)
            :to-equal (cons expected-start expected-end)))
  (it-each ((:entry 1 3 5 8 4 1 3)
            (:span 10 12 5 8 4 11 13)
            (:span 2 12 5 8 4 2 13)
            (:span 2 7 5 8 4 2 9)
            (:span 6 12 5 8 4 5 13))
      "maps ~A projection spans"
      (input-kind span-start span-end edit-start edit-end replacement-length
       expected-start expected-end)
    (let ((input-span
            (if (eq input-kind :entry)
                '(1 3 :paragraph "text" nil)
                (list span-start span-end))))
      (expect (multiple-value-list
               (cl-markdown-kit::%map-projection-span
                input-span edit-start edit-end replacement-length))
              :to-equal (list expected-start expected-end))))
  (it-each ((:removed 0 1)
            (:added 0 1)
            (:unchanged 5 6)
            (:changed-text 5 6)
            (:changed-kind 5 6))
      "reports projection differences for ~A cases"
      (difference-kind start end)
    (let* ((entry '(1 3 :paragraph "text" nil))
           (before
             (unless (eq difference-kind :added)
               (list entry)))
           (after
             (case difference-kind
               (:removed nil)
               (:unchanged (list entry))
               (:added (list entry))
               (:changed-text
                (list '(1 3 :paragraph "changed" nil)))
               (:changed-kind
                (list '(1 3 :heading "text" nil)))))
           (difference
             (cl-markdown-kit::%projection-difference
              before after start end 0)))
      (if (eq difference-kind :unchanged)
          (expect difference :to-equal nil)
          (expect difference :to-be-truthy)))))
