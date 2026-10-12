(in-package #:cl-markdown-kit/test)

(defun edit-line-ending (style)
  (case style
    (:lf (string #\Newline))
    (:cr (string #\Return))
    (:crlf (coerce (list #\Return #\Newline) 'string))))

(defun edit-lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

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

(defun edit-condition (function)
  (handler-case
      (progn (funcall function) nil)
    (condition (condition) condition)))

(describe "Format-preserving edit API"
  (it "replaces section bodies while keeping surrounding source exact"
    (let* ((source (edit-lines "# A" "old" "" "# B" "keep"))
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
                    document "/A" (edit-lines "new" "line"))))
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
    (let* ((source (edit-lines "# A" "old" "" "# B" "keep"))
           (document (cl-markdown-kit:parse-string source))
           (section-span
             (cl-markdown-kit:section-record-span
              (cl-markdown-kit:find-section document "/A")))
           (end (cl-parser-kit:span-end section-span))
           (replaced (cl-markdown-kit:replace-section
                      document "/A" (edit-lines "## New" "changed")))
           (deleted (cl-markdown-kit:delete-section document "/A")))
      (expect (subseq replaced (- (length replaced) (- (length source) end)))
              :to-equal (subseq source end))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string replaced))
              :to-equal '("New" "B"))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string deleted))
              :to-equal '("B"))
      (expect deleted :to-equal (edit-lines "# B" "keep"))))

  (it "inserts before, after, and as the last child section"
    (let* ((source (edit-lines "# A" "body" "" "## Child" "child"
                               "" "### Grandchild" "grand"
                               "" "## Second child" "second"
                               "" "# B" "other"))
           (document (cl-markdown-kit:parse-string source))
           (before (cl-markdown-kit:insert-section
                    document "/B" (edit-lines "## Before" "inserted")
                    :position :before))
           (after (cl-markdown-kit:insert-section
                   document "/A/Child" (edit-lines "### After" "inserted")
                   :position :after))
           (last-child (cl-markdown-kit:insert-section
                        document "/A" (edit-lines "## Last" "inserted")
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
    (let* ((quote-source (edit-lines "> before" ">" "> old" "> continued"
                                     ">" "> after"))
           (quote-document (cl-markdown-kit:parse-string quote-source))
           (quote-node (first (cl-markdown-kit:document-children quote-document)))
           (quote-paragraph (second (cl-markdown-kit:block-quote-children quote-node)))
           (quote-range (edit-node-range quote-paragraph))
           (quote-edited (cl-markdown-kit:replace-node
                          quote-document quote-paragraph
                          (edit-lines "new A" "new B")))
           (list-source (edit-lines "- before" "" "- old" "  continued"
                                    "" "- after"))
           (list-document (cl-markdown-kit:parse-string list-source))
           (list-node (first (cl-markdown-kit:document-children list-document)))
           (list-item (second (cl-markdown-kit:list-node-items list-node)))
           (list-paragraph (first (cl-markdown-kit:list-item-children list-item)))
           (list-range (edit-node-range list-paragraph))
           (list-edited (cl-markdown-kit:replace-node
                         list-document list-paragraph
                         (edit-lines "new A" "new B"))))
      (expect (search (edit-lines "> new A" "> new B") quote-edited)
              :to-be-truthy)
      (expect (search (edit-lines "- new A" "  new B") list-edited)
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
    (let* ((source (edit-lines "- [ ] first" "- [X] second"))
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
      (expect checked :to-equal (edit-lines "- [x] first" "- [X] second"))
      (expect same :to-equal source)))

  (it "preserves newline forms, BOM, EOF state, and non-target UTF-8 bytes"
    (let ((case-count 0))
      (dolist (style '(:lf :crlf :cr :mixed))
        (dolist (bom-p '(nil t))
          (dolist (final-newline-p '(nil t))
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
                                   document middle (edit-lines "new A" "new B")))
                   (middle-prefix (subseq source 0 (car middle-range)))
                   (middle-suffix (subseq source (cdr middle-range)))
                   (last-node (third (cl-markdown-kit:document-children document)))
                   (last-range (edit-node-range last-node))
                   (last-edited (cl-markdown-kit:replace-node
                                 document last-node (edit-lines "tail C" "tail D"))))
              (incf case-count 2)
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
              (expect (search (concatenate 'string "tail C" tail "tail D")
                              last-edited)
                      :to-be-truthy)
              (expect (cl-markdown-kit:parse-string middle-edited)
                      :to-be-truthy)
              (expect (cl-markdown-kit:parse-string last-edited)
                      :to-be-truthy)))))
      (expect case-count :to-equal 32)))

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
                    document thematic-break (edit-lines "new A" "new B"))))
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
                (edit-lines "next A" "next B"))))
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
              :to-equal (edit-lines "a" "b" "c" "d"))
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
           (source (concatenate 'string bom (edit-lines "# A" "body") lf))
           (document (cl-markdown-kit:parse-string source))
           (replaced (cl-markdown-kit:replace-section
                      document "" (edit-lines "## New" "changed")))
           (body-replaced (cl-markdown-kit:replace-section-body
                           document "" (edit-lines "# New" "body")))
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
                                               (edit-lines "## New" "changed") lf))
      (expect body-replaced :to-equal (concatenate 'string bom
                                                    (edit-lines "# New" "body") lf))
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
             (edit-lines "---" "title: preserved" "---"
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

  (it "maps source intervals and reports projection additions and removals"
    (let ((entry '(1 3 :paragraph "text" nil)))
      (expect (cl-markdown-kit::%map-edit-range '(1 . 3) 5 8 4 20 15)
              :to-equal '(1 . 3))
      (expect (cl-markdown-kit::%map-edit-range '(10 . 12) 5 8 4 20 15)
              :to-equal '(11 . 13))
      (expect (cl-markdown-kit::%map-edit-range '(2 . 12) 5 8 4 20 15)
              :to-equal '(2 . 13))
      (expect (cl-markdown-kit::%map-edit-range '(6 . 7) 5 8 4 20 15)
              :to-equal '(5 . 9))
      (expect (cl-markdown-kit::%map-edit-range '(8 . 15) 10 15 3 13 15)
              :to-equal '(8 . 13))
      (expect (multiple-value-list
               (cl-markdown-kit::%map-projection-span entry 5 8 4))
              :to-equal '(1 3))
      (expect (multiple-value-list
               (cl-markdown-kit::%map-projection-span '(10 12) 5 8 4))
              :to-equal '(11 13))
      (expect (multiple-value-list
               (cl-markdown-kit::%map-projection-span '(2 12) 5 8 4))
              :to-equal '(2 13))
      (expect (multiple-value-list
               (cl-markdown-kit::%map-projection-span '(2 7) 5 8 4))
              :to-equal '(2 9))
      (expect (multiple-value-list
               (cl-markdown-kit::%map-projection-span '(6 12) 5 8 4))
              :to-equal '(5 13))
      (expect (cl-markdown-kit::%projection-difference
               (list entry) nil 0 1 0)
              :to-be-truthy)
      (expect (cl-markdown-kit::%projection-difference
               nil (list entry) 0 1 0)
              :to-be-truthy)
      (expect (cl-markdown-kit::%projection-difference
               (list entry) (list entry) 5 6 0)
              :to-equal nil)
      (expect (cl-markdown-kit::%projection-difference
               (list entry)
               (list '(1 3 :paragraph "changed" nil))
               5 6 0)
              :to-be-truthy)
      (expect (cl-markdown-kit::%projection-difference
               (list entry)
               (list '(1 3 :heading "text" nil))
               5 6 0)
              :to-be-truthy)))

  (it "rejects non-source-backed and invalid edit targets"
    (let* ((source (edit-lines "# Heading" "text"))
           (document (cl-markdown-kit:parse-string source))
           (paragraph (second (cl-markdown-kit:document-children document)))
           (inline (first (cl-markdown-kit:heading-children
                           (first (cl-markdown-kit:document-children document)))))
           (foreign-document (cl-markdown-kit:parse-string "foreign"))
           (foreign-node (first (cl-markdown-kit:document-children foreign-document)))
           (foreign-condition
             (edit-condition
              (lambda ()
                (cl-markdown-kit:replace-node document foreign-node "changed"))))
           (inline-condition
             (edit-condition
              (lambda () (cl-markdown-kit:replace-node document inline "changed"))))
           (document-condition
             (edit-condition
              (lambda () (cl-markdown-kit:replace-node document document "changed"))))
           (invalid-edit-condition
             (edit-condition
              (lambda ()
                (cl-markdown-kit::%perform-edit document -1 0 "" nil nil))))
           (noninteger-edit-condition
             (edit-condition
              (lambda ()
                (cl-markdown-kit::%perform-edit document :invalid 0 "" nil nil))))
           (missing-span-document (cl-markdown-kit:parse-string source))
           (missing-span-node
             (second (cl-markdown-kit:document-children missing-span-document)))
           (source-less (cl-markdown-kit:make-document))
           (source-less-condition
             (edit-condition
              (lambda () (cl-markdown-kit:replace-section source-less "" "")))))
      (expect (typep foreign-condition 'error) :to-be-truthy)
      (expect (typep inline-condition 'error) :to-be-truthy)
      (expect (typep document-condition 'error) :to-be-truthy)
      (expect (typep invalid-edit-condition 'error) :to-be-truthy)
      (expect (typep noninteger-edit-condition 'error) :to-be-truthy)
      (expect (typep source-less-condition 'error) :to-be-truthy)
      (setf (cl-markdown-kit:document-source document) nil)
      (expect (cl-markdown-kit::%edit-document-source document) :to-equal source)
      (setf (cl-markdown-kit:node-span paragraph)
            (cl-parser-kit:make-span :source "foreign" :start 0 :end 1))
      (expect (typep (edit-condition
                      (lambda ()
                        (cl-markdown-kit:replace-node document paragraph "changed")))
                     'error)
              :to-be-truthy)
      (setf (cl-markdown-kit:node-span missing-span-node) nil)
      (expect (typep (edit-condition
                      (lambda ()
                        (cl-markdown-kit:replace-node missing-span-document
                                                      missing-span-node "changed")))
                     'error)
              :to-be-truthy)
      (let* ((bad-document (cl-markdown-kit:parse-string source))
             (bad-node (second (cl-markdown-kit:document-children bad-document))))
        (setf (cl-markdown-kit:node-span bad-node)
              (cl-parser-kit:make-span :source source :start -1 :end 1))
        (expect (typep (edit-condition
                        (lambda ()
                          (cl-markdown-kit:replace-node bad-document bad-node "changed")))
                       'error)
                :to-be-truthy))))

  (it "validates task state inputs and supports unchecking"
    (let* ((source (edit-lines "- [x] checked" "- ordinary"))
           (document (cl-markdown-kit:parse-string source))
           (list-node (first (cl-markdown-kit:document-children document)))
           (task (first (cl-markdown-kit:list-node-items list-node)))
           (ordinary (second (cl-markdown-kit:list-node-items list-node)))
           (foreign-task
             (first (cl-markdown-kit:list-node-items
                     (first (cl-markdown-kit:document-children
                             (cl-markdown-kit:parse-string "- [ ] foreign")))))))
      (expect (cl-markdown-kit:set-task-checked document task nil)
              :to-equal (edit-lines "- [ ] checked" "- ordinary"))
      (expect (typep (edit-condition
                      (lambda ()
                        (cl-markdown-kit:set-task-checked document ordinary t)))
                     'error)
              :to-be-truthy)
      (expect (typep (edit-condition
                      (lambda ()
                        (cl-markdown-kit:set-task-checked document foreign-task t)))
                     'error)
              :to-be-truthy)
      (expect (typep (edit-condition
                      (lambda ()
                        (cl-markdown-kit:set-task-checked document task :checked)))
                     'type-error)
              :to-be-truthy)
      (expect (typep (edit-condition
                      (lambda ()
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
        (expect (typep (edit-condition
                        (lambda ()
                          (cl-markdown-kit:set-task-checked
                           invalid-marker-document invalid-task t)))
                       'error)
                :to-be-truthy))))

  (it "retries a section insertion after adding required block separation"
    (let* ((source (edit-lines "# A" "body" "" "# B" "other"))
           (document (cl-markdown-kit:parse-string source))
           (inserted (cl-markdown-kit:insert-section
                      document "/B" "new paragraph" :position :before)))
      (expect (edit-heading-texts (cl-markdown-kit:parse-string inserted))
              :to-equal '("A" "B"))
      (expect (search (edit-lines "new paragraph" "" "# B") inserted)
              :to-be-truthy)))

  (it "signals without returning source when a non-target block is consumed"
    (let* ((source (edit-lines "# A" "old" "" "# B" "keep"))
           (document (cl-markdown-kit:parse-string source))
           (condition
             (edit-condition
              (lambda ()
                (cl-markdown-kit:replace-section-body
                 document "/A" (edit-lines "```" "code"))))))
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
               (edit-lines "[ref]: /old" "" "[label][ref]" "" "target"))
             (reference-document (cl-markdown-kit:parse-string reference-source))
             (definition (first
                          (cl-markdown-kit:document-children reference-document)))
             (reference-condition
               (edit-condition
                (lambda ()
                  (cl-markdown-kit:replace-node
                   reference-document definition "[ref]: /new")))))
        (expect (typep reference-condition
                       'cl-markdown-kit:markdown-structure-changed-error)
                :to-be-truthy)))))
