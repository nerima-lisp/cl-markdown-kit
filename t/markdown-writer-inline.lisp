(in-package #:cl-markdown-kit/test)

(describe "Markdown writer inline"
(it "renders inline node variants and escapes link components"
    (with-mw-writer-state
      (flet ((render (&rest nodes)
               (markdown-writer-render-inline-nodes nodes)))
        (expect (render (cl-markdown-kit:make-code-span :literal-text " a "))
                :to-equal "`  a  `")
        (expect (render (cl-markdown-kit:make-code-span :literal-text "   "))
                :to-equal "`   `")
        (expect (render (cl-markdown-kit:make-code-span :literal-text "a``b"))
                :to-equal "```a``b```")
        (expect (render (cl-markdown-kit:make-code-span
                         :literal-text (format nil "a~%b")))
                :to-equal "`a b`")
        (expect (render (cl-markdown-kit:make-html-inline :body "<i>raw</i>"))
                :to-equal "<i>raw</i>")
        (expect (render (cl-markdown-kit:make-soft-break)
                        (cl-markdown-kit:make-text :value "next")
                        (cl-markdown-kit:make-hard-break)
                        (cl-markdown-kit:make-text :value "last"))
                :to-equal (format nil "~%next\\~%last"))
        (expect (render (cl-markdown-kit:make-emphasis
                         :children (list (cl-markdown-kit:make-text :value "a")))
                        (cl-markdown-kit:make-emphasis
                         :delimiter-character #\_
                         :children (list (cl-markdown-kit:make-text :value "b")))
                        (cl-markdown-kit:make-strong
                         :children (list (cl-markdown-kit:make-text :value "c")))
                        (cl-markdown-kit:make-strong
                         :delimiter-character #\_
                         :children (list (cl-markdown-kit:make-text :value "d")))
                        (cl-markdown-kit:make-strikethrough
                         :children (list (cl-markdown-kit:make-text :value "e"))))
                :to-equal "*a*_b_**c**__d__~~e~~")
        (expect
         (render
          (cl-markdown-kit:make-link
           :destination "a<b>&copy;" :title "a\"b"
           :children (list (cl-markdown-kit:make-text :value "link"))))
         :to-equal
         (format nil "[link](<a~C<b~C>~C&copy;> \"a~C\"b\")"
                 #\\ #\\ #\\ #\\))
        (expect
         (render
          (cl-markdown-kit:make-link
           :destination (format nil "a~C b~%c" #\\)
           :title (format nil "a~C\"b" #\\)
           :children (list (cl-markdown-kit:make-text :value "x"))))
         :to-equal
         (format nil "[x](<a~C~C b&#10;c> \"a~C~C~C\"b\")"
                 #\\ #\\ #\\ #\\ #\\))
        (expect
         (render (cl-markdown-kit:make-image
                  :destination "image.png" :title nil
                  :children (list (cl-markdown-kit:make-text :value "alt"))))
         :to-equal "![alt](<image.png>)")
        (expect
         (render (cl-markdown-kit:make-autolink
                  :destination "https://example.test" :display-text "example"))
         :to-equal "[example](<https://example.test>)")
        (expect
         (markdown-writer-render-inline-nodes (list (cl-markdown-kit:make-text :value "direct")))
         :to-equal "direct")
        (expect
         (render (cl-markdown-kit:make-link
                  :destination "target"
                  :children (list (cl-markdown-kit:make-text :value "link")))
                (cl-markdown-kit:make-text :value "[tail]"))
         :to-equal "[link](<target>)\\[tail\\]")
        (expect
         (render (cl-markdown-kit:make-footnote-reference
                  :normalized-label
                  (format nil "a[b]&~Cc\\d]" #\Newline)))
         :to-equal "[^a\\[b\\]&amp; c\\\\d\\]]")
        (expect
         (handler-case
             (progn
               (markdown-writer-render-inline-nodes (list (cl-markdown-kit:make-node)))
               nil)
           (cl-markdown-kit:markdown-unsupported-node-error () t))
        :to-be-truthy)))
    (let* ((table
             (cl-markdown-kit:make-table
              :alignments '(:left)
              :rows
              (list
               (cl-markdown-kit:make-table-row
                :cells
                (list
                 (cl-markdown-kit:make-table-cell
                  :children
                  (list (cl-markdown-kit:make-text :value "plain|pipe ")
                        (cl-markdown-kit:make-link
                         :destination "/a|b" :title "t|t"
                         :children (list (cl-markdown-kit:make-text
                                          :value "cell"))))))))))
           (output (cl-markdown-kit:write-markdown
                    (cl-markdown-kit:make-document :children (list table)))))
      (expect (search "/a\\|b" output) :to-be-truthy)
      (expect (search "plain\\|pipe" output) :to-be-truthy)
      (expect (search "t\\|t" output) :to-be-truthy))))
