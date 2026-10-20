(in-package #:cl-markdown-kit/test)

(describe "Markdown writer roundtrips"
(it "roundtrips block, inline, GFM, and front matter structure"
    (let ((source
            (format nil
                    "---~%title: Example~%---~%~%# Heading *emphasis*~%~%A [link][ref] and ![image](<image.png> \"caption\").~%~%[ref]: <docs/page.md> \"reference\"~%~%2. first~%3. second~%~%> quoted~%>~%> - nested~%~%```lisp~%```` closes later~%```~%~%| Name | Note |~%| :--- | ---: |~%| pipe | `x|y` |~%~%- [x] done~%- [ ] pending~%~%[^note]: a footnote~%~%Here[^note].~%")))
      (expect (markdown-writer-roundtrips-p source) :to-be-truthy)))

(cl-weave:it-property "preserves ASTs across generated adjacent-list block combinations"
  ((source
    (cl-weave:gen-member
     (loop for (first-marker second-marker) in
             (append
              (loop for first-marker in '("-" "*" "+")
                    append
                    (loop for second-marker in '("-" "*" "+")
                          unless (string= first-marker second-marker)
                            collect (list first-marker second-marker)))
              '(("1." "3)") ("3)" "1."))
              (loop for bullet in '("-" "*" "+")
                    append
                    (list (list bullet "1.")
                          (list bullet "3)")
                          (list "1." bullet)
                          (list "3)" bullet))))
           append
           (loop for layout in '(:lists :before :after :both)
                 collect
                 (format nil "~A~A one~%~%~A two~A"
                         (if (member layout '(:before :both))
                             (format nil "before~%~%")
                             "")
                         first-marker
                         second-marker
                         (if (member layout '(:after :both))
                             (format nil "~%~%after")
                             "")))))))
   (let* ((document (cl-markdown-kit:parse-string source))
          (markdown (cl-markdown-kit:write-markdown document))
          (roundtrip (cl-markdown-kit:parse-string markdown)))
     (expect (markdown-writer-document-signature roundtrip)
             :to-equal (markdown-writer-document-signature document))))

(it "preserves leading spaces through a Markdown HTML round trip"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list
               (cl-markdown-kit:make-paragraph
                :children
                (list (cl-markdown-kit:make-text :value "    x"))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal "&#32;   x")
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t))))

(it "preserves an empty block quote through a Markdown HTML round trip"
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list (cl-markdown-kit:make-block-quote :children nil))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown)))
      (expect markdown :to-equal ">")
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t))))

(it "preserves inline writer context across nested runs"
    (labels ((text (value)
               (cl-markdown-kit:make-text :value value))
             (paragraph (children)
               (cl-markdown-kit:make-paragraph :children children))
             (document (block)
               (cl-markdown-kit:make-document :children (list block)))
             (roundtrip (document)
               (cl-markdown-kit:parse-string
                (cl-markdown-kit:write-markdown document)))
             (html (document)
               (cl-markdown-kit:write-html
                document :raw-html :pass-through :unsafe-urls t))
             (expect-roundtrip (original expected-markdown expected-signature)
               (let ((markdown (cl-markdown-kit:write-markdown original))
                     (parsed (roundtrip original)))
                 (expect markdown :to-equal expected-markdown)
                 (expect (markdown-writer-document-signature parsed)
                         :to-equal expected-signature)
                 (expect (html parsed) :to-equal (html original)))))
      (let* ((strong-heading
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-strong
                        :children (list (text "section #")))
                       (text " follows")))))
             (strong-heading-end
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-strong
                        :children (list (text "section #")))))))
             (emphasis-heading
               (document
                (cl-markdown-kit:make-heading
                 :level 2
                 :children
                 (list (cl-markdown-kit:make-emphasis
                        :children (list (text "section #")))
                       (text " follows")))))
             (safe-mixed-inline
               (document
                (paragraph
                 (list (text "a ~ b")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))))))
             (mixed-delimiter-boundary
               (document
                (paragraph
                 (list (text "left ~ *")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))))))
             (delimiter-boundary
               (document
                (paragraph
                 (list (text "left*")
                       (cl-markdown-kit:make-strong
                        :children (list (text "bold")))
                       (text "*right")))))
             (code-span-boundary
               (document
                (paragraph
                 (list (text "`foo")
                       (cl-markdown-kit:make-code-span
                        :literal-text "bar")))))
             (split-autolink
               (document
                (paragraph
                 (list (text "https")
                       (cl-markdown-kit:make-html-inline :body "")
                       (text "://example.test")))))
             (split-entity
               (document
                (paragraph
                 (list (text "&am")
                       (cl-markdown-kit:make-html-inline :body "")
                       (text "p;"))))))
        (expect-roundtrip
         strong-heading "## **section #** follows"
         '((:heading 2 ((:strong ((:text "section #")))
                       (:text " follows")))))
        (expect-roundtrip
         strong-heading-end "## **section #**"
         '((:heading 2 ((:strong ((:text "section #")))))))
        (expect-roundtrip
         emphasis-heading "## *section #* follows"
         '((:heading 2 ((:emphasis ((:text "section #")))
                       (:text " follows")))))
        (expect-roundtrip
         safe-mixed-inline "a ~ b**bold**"
         '((:paragraph ((:text "a ~ b") (:strong ((:text "bold")))))))
        (expect-roundtrip
         mixed-delimiter-boundary "left ~ \\***bold**"
         '((:paragraph ((:text "left ~ *")
                        (:strong ((:text "bold")))))))
        (expect-roundtrip
         delimiter-boundary "left\\***bold**\\*right"
         '((:paragraph ((:text "left*")
                        (:strong ((:text "bold")))
                        (:text "*right")))))
        (expect-roundtrip
         code-span-boundary "\\`foo`bar`"
         '((:paragraph ((:text "`foo") (:code "bar")))))
        (expect-roundtrip
         split-autolink "https\\://example.test"
         '((:paragraph ((:text "https://example.test")))))
        (expect-roundtrip
         split-entity "\\&amp;"
         '((:paragraph ((:text "&amp;"))))))))

(it "normalizes front matter line endings without changing HTML round trips"
    (let* ((source
             (format nil "---~C~%title: value~Ctail~C---~C~%body"
                     #\Return #\Return #\Return #\Return))
           (document (cl-markdown-kit:parse-string source))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown))
           (stream-markdown
             (with-output-to-string (stream)
               (cl-markdown-kit:write-markdown-to-stream document stream))))
      (expect markdown :to-equal
              (format nil "---~%title: value~%tail~%---~%~%body"))
      (expect stream-markdown :to-equal markdown)
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t))))

(it "preserves rendered HTML across every CommonMark and GFM fixture"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm))
           (exclusions (conformance-read-exclusions))
           (matches 0)
           (failures nil)
           (unexpected-failures nil))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (length examples) :to-equal 682)
      (expect (conformance-exclusions-valid-p examples exclusions)
              :to-be-truthy)
      (dolist (example examples)
        (let ((stage :parse-original)
              (original-html nil)
              (markdown nil)
              (roundtrip-html nil)
              (condition-message nil))
          (handler-case
              (let ((document
                      (cl-markdown-kit:parse-string
                       (spec-example-markdown example))))
                (setf stage :render-original-html
                      original-html
                      (cl-markdown-kit:write-html
                       document :raw-html :pass-through :unsafe-urls t))
                (setf stage :write-markdown
                      markdown (cl-markdown-kit:write-markdown document))
                (setf stage :parse-roundtrip)
                (let ((roundtrip-document
                        (cl-markdown-kit:parse-string markdown)))
                  (setf stage :render-roundtrip-html
                        roundtrip-html
                        (cl-markdown-kit:write-html
                         roundtrip-document
                         :raw-html :pass-through :unsafe-urls t)))
                (setf stage :compare-html))
            (error (condition)
              (setf condition-message (princ-to-string condition))))
          (if (and (null condition-message)
                   (string= original-html roundtrip-html))
              (incf matches)
              (let ((failure
                      (list :id (spec-example-id example)
                            :section (spec-example-section example)
                            :stage stage
                            :condition condition-message
                            :input (spec-example-markdown example)
                            :reference-html (spec-example-html example)
                            :original-html original-html
                            :markdown markdown
                            :roundtrip-html roundtrip-html)))
                (push failure failures)
                (unless (and (eq stage :compare-html)
                             (null condition-message)
                             (member (spec-example-id example)
                                     exclusions :key #'first :test #'string=))
                  (push failure unexpected-failures))))))
      (dolist (failure (reverse failures))
        (format t
                "T-003 mismatch ~A (~A)~%  stage: ~S~%  condition: ~S~%  original matches fixture HTML: ~S~%  input: ~S~%  fixture HTML: ~S~%  original HTML: ~S~%  writer Markdown: ~S~%  round-trip HTML: ~S~%"
                (getf failure :id)
                (getf failure :section)
                (getf failure :stage)
                (getf failure :condition)
                (and (getf failure :original-html)
                     (string= (getf failure :original-html)
                              (getf failure :reference-html)))
                (getf failure :input)
                (getf failure :reference-html)
                (getf failure :original-html)
                (getf failure :markdown)
                (getf failure :roundtrip-html)))
      (format t "T-003 Markdown round trips: ~D CommonMark + ~D GFM; ~D match, ~D documented parser mismatches, ~D declared exclusions.~%"
              (length commonmark) (length gfm) matches
              (- (length failures) (length unexpected-failures))
              (length exclusions))
      (expect unexpected-failures :to-equal nil))))
