(in-package #:cl-markdown-kit/test)

(describe "Block parser HTML blocks"
          (progn
            (cl-weave:it-each
             (("<!-- comment -->" 2 nil) ("<?xml version=\"1.0\"?>" 3 nil)
              ("<script>run()</script>" 1 nil)
              ("<!DOCTYPE html>" 4 nil)
              ("<![CDATA[data]]>" 5 nil)
              ("<div>block</div>" 6 nil)
              ("<custom-element>" 7 "</custom-element>"))
             "recognizes CommonMark HTML block ~A as type ~D with suffix ~A"
             (source-start expected-kind closing-tag)
             (let* ((source
                     (if closing-tag
                         (format nil "~A~%block~%~A" source-start closing-tag)
                         source-start))
                    (html
                     (block-parser-test-only-child
                      (block-parser-test-document
                       (block-parser-test-result source)))))
               (expect
                (and (typep html (quote cl-markdown-kit:html-block))
                     (cl-markdown-kit:html-block-kind html)
                     (= (cl-markdown-kit:html-block-kind html) expected-kind)
                     (string= (cl-markdown-kit:html-block-body html) source)
                     (block-parser-test-span-is html 0 (length source)))
                :to-be-truthy)))
            (it "recognizes all seven distinct CommonMark HTML block types"
                (let* ((examples
                        (list "<!-- comment -->"
                              "<?xml version=\"1.0\"?>"
                              "<script>run()</script>"
                              "<!DOCTYPE html>"
                              "<![CDATA[data]]>"
                              "<div>block</div>"
                              (format nil
                                      "<custom-element>~%block~%</custom-element>")))
                       (kinds
                        (loop for source in examples
                              collect (cl-markdown-kit:html-block-kind
                                       (block-parser-test-only-child
                                        (block-parser-test-document
                                         (block-parser-test-result source)))))))
                  (expect
                   (= (length (remove-duplicates kinds :test (function equal)))
                      7)
                   :to-be-truthy))))
          (it "requires complete HTML block start and end tags"
              (let* ((self-closing
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result
                         "<custom-element data-x=\"1\"/>"))))
                     (inline-tag
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result
                         "<custom-element>inline</custom-element>"))))
                     (bare-declaration
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result "<!"))))
                     (invalid-end-source
                      (format nil "<script>~%before~%</script/>~%after"))
                     (invalid-end
                      (block-parser-test-only-child
                       (block-parser-test-document
                        (block-parser-test-result invalid-end-source))))
                     (valid-end-source
                      (format nil "<script>~%before~%</script   >~%after"))
                     (valid-end-children
                      (cl-markdown-kit:document-children
                       (block-parser-test-document
                        (block-parser-test-result valid-end-source)))))
                (expect
                 (and (typep self-closing 'cl-markdown-kit:html-block)
                      (= (cl-markdown-kit:html-block-kind self-closing) 7)
                      (typep inline-tag 'cl-markdown-kit:paragraph)
                      (typep bare-declaration 'cl-markdown-kit:paragraph)
                      (typep invalid-end 'cl-markdown-kit:html-block)
                      (= (cl-markdown-kit:html-block-kind invalid-end) 1)
                      (string= (cl-markdown-kit:html-block-body invalid-end)
                               invalid-end-source)
                      (= (length valid-end-children) 2)
                      (typep (first valid-end-children)
                             'cl-markdown-kit:html-block)
                      (typep (second valid-end-children)
                             'cl-markdown-kit:paragraph))
                 :to-be-truthy)))
          (it "validates HTML tag attributes and closing-tag boundaries"
              (let* ((valid "<x-tag _id:part=bare title=\"x>y\" data=raw />")
                     (valid-name
                      (multiple-value-list
                       (cl-markdown-kit::%bp-tag-name-at valid 0)))
                     (closing "</x-tag>")
                     (closing-name
                      (multiple-value-list
                       (cl-markdown-kit::%bp-tag-name-at closing 0)))
                     (html-type
                      (multiple-value-list
                       (cl-markdown-kit::%bp-html-block-type
                        (block-parser-test-line valid))))
                     (multiline-destination
                      (cl-markdown-kit::%bp-link-definition
                       (cl-markdown-kit::%bp-join-lines
                        (cl-markdown-kit::%bp-split-lines
                         (format nil "[link]: /target~%continued")))))
                     (empty-tag (cl-markdown-kit::%bp-tag-name-at "<" 0))
                     (invalid-tag (cl-markdown-kit::%bp-tag-name-at "<1tag>" 0))
                     (plain-tag (cl-markdown-kit::%bp-tag-name-at "ordinary" 0)))
                (let ((name-end (second valid-name)))
                  (expect
                   (and (string= (first valid-name) "x-tag")
                        (=
                         (cl-markdown-kit::%bp-html-type-seven-tag-end valid
                                                                       0
                                                                       name-end)
                         (length valid))
                        (string= (first closing-name) "x-tag")
                        (=
                         (cl-markdown-kit::%bp-html-type-seven-tag-end closing
                                                                       0
                                                                       (second
                                                                        closing-name))
                         (length closing))
                        (null
                         (cl-markdown-kit::%bp-html-type-seven-tag-end
                          "</x-tag extra>"
                          0
                          7))
                        (= (first html-type) 7)
                        (string= multiline-destination "link")
                        (null (first empty-tag))
                        (null (first invalid-tag))
                        (null (first plain-tag)))
                   :to-be-truthy))
                (expect
                 (and
                  (cl-markdown-kit::%bp-html-terminator-p
                   (block-parser-test-line "</script >")
                   "</script"
                   1)
                  (not
                   (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "</script/x>")
                    "</script"
                    1))
                  (cl-markdown-kit::%bp-html-terminator-p
                   (block-parser-test-line "data ]]>")
                   nil
                   5)
                  (cl-markdown-kit::%bp-html-terminator-p
                   (block-parser-test-line "anything")
                   ""
                   7)
                  (not
                   (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "</script")
                    "</script"
                    1))
                  (not
                   (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "</script ")
                    "</script"
                    1))
                  (not
                   (cl-markdown-kit::%bp-html-terminator-p
                    (block-parser-test-line "</script x>")
                    "</script"
                    1)))
                 :to-be-truthy)))
          (cl-weave:it-each
           (("<x-tag 1bad=x>") ("<x-tag attr")
            ("<x-tag attr=")
            ("<x-tag attr=>")
            ("<x-tag attr=\"unterminated>")
            ("<x-tag /x>"))
           "rejects malformed HTML tag attributes ~A"
           (malformed-tag)
           (multiple-value-bind (name name-end)
               (cl-markdown-kit::%bp-tag-name-at malformed-tag 0)
             (expect
              (null
               (and name
                    (cl-markdown-kit::%bp-html-type-seven-tag-end malformed-tag
                                                                  0
                                                                  name-end)))
              :to-be-truthy))))
