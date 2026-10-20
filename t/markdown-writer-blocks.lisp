(in-package #:cl-markdown-kit/test)

(describe "Markdown writer blocks"
  (it "preserves table alignment, list continuation, and code fence behavior"
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "| :--- | ---: | :---: | --- |")
            :to-equal 4)
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "plain text")
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "| : |")
            :to-equal nil)
    (expect (cl-markdown-kit::%mw-table-alignment-line-p "||")
            :to-equal nil)
    (with-mw-writer-state
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-paragraph
         :children (list (cl-markdown-kit:make-text
                          :value (format nil "a|b~%---|---")))))
       :to-equal (format nil "a|b~%---\\|---")))
    (let* ((document
             (cl-markdown-kit:make-document
              :children
              (list
               (cl-markdown-kit:make-paragraph
                :children
                (list (cl-markdown-kit:make-text
                       :value (format nil "a|b~%---|---")))))))
           (markdown (cl-markdown-kit:write-markdown document))
           (roundtrip (cl-markdown-kit:parse-string markdown :gfm t)))
      (expect markdown :to-equal (format nil "a|b~%---\\|---"))
      (expect (cl-markdown-kit:write-html
               roundtrip :raw-html :pass-through :unsafe-urls t)
              :to-equal
              (cl-markdown-kit:write-html
               document :raw-html :pass-through :unsafe-urls t)))
    (dolist (test-case
              (list (list (format nil "a|~%---:")
                          (format nil "a|~%---\\:"))
                    (list (format nil "|a|~%:---")
                          (format nil "|a|~%\\:---"))
                    (list (format nil "a|~%---")
                          (format nil "a|~%\\---"))))
      (destructuring-bind (text expected-markdown) test-case
        (let* ((document
                 (cl-markdown-kit:make-document
                  :children
                  (list
                   (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-text :value text))))))
               (markdown (cl-markdown-kit:write-markdown document))
               (roundtrip (cl-markdown-kit:parse-string markdown :gfm t)))
          (expect markdown :to-equal expected-markdown)
          (expect (cl-markdown-kit:write-html
                   roundtrip :raw-html :pass-through :unsafe-urls t)
                  :to-equal
                  (cl-markdown-kit:write-html
                   document :raw-html :pass-through :unsafe-urls t)))))
    (with-mw-writer-state
      (expect
       (cl-markdown-kit:write-markdown
        (cl-markdown-kit:make-document
         :children
         (list (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text :value "first")))
               (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text :value "second")))
               (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text :value "third"))))))
       :to-equal (format nil "first~%~%second~%~%third")))
    (expect (cl-markdown-kit::%mw-prefix-lines "" "> ") :to-equal "")
    (expect (cl-markdown-kit::%mw-prefix-lines (format nil "a~%b~%") "> ")
            :to-equal (format nil "> a~%> b~%"))
    (expect (cl-markdown-kit::%mw-prefix-continuation-lines "" "  ")
            :to-equal "")
    (expect (cl-markdown-kit::%mw-prefix-continuation-lines
             (format nil "a~%b~%") "  ")
            :to-equal (format nil "a~%  b~%"))
    (with-mw-writer-state
      (expect
       (cl-markdown-kit::%mw-render-code-block
        (cl-markdown-kit:make-code-block
         :info-string "lisp" :body (format nil "line~%")))
       :to-equal (format nil "``` lisp~%line~%```"))
      (expect
       (cl-markdown-kit::%mw-render-code-block
        (cl-markdown-kit:make-code-block :info-string "l`sp" :body "~~~"))
       :to-equal (format nil "~~~~~~~~ l`sp~%~~~~~~"))
      (expect
       (cl-markdown-kit::%mw-render-code-block
        (cl-markdown-kit:make-code-block :body ""))
       :to-equal (format nil "```~%```")))
    (with-mw-writer-state
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-list-node
         :kind :unordered :tight-p t
         :items
         (list (cl-markdown-kit:make-list-item
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value (format nil "one~%two"))))))
               (cl-markdown-kit:make-task-list-item
                :checked-p t
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value "done")))))
               (cl-markdown-kit:make-task-list-item :checked-p nil)
               (cl-markdown-kit:make-list-item :children nil))))
       :to-equal (format nil "- one~%  two~%- [x] done~%- [ ]~%- "))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-list-node
         :kind :ordered :start-number 5 :tight-p nil
         :items
         (list (cl-markdown-kit:make-task-list-item
                :checked-p nil
                :children
                (list (cl-markdown-kit:make-code-block
                       :body (format nil "code~%"))))
               (cl-markdown-kit:make-list-item
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value "regular"))))))))
       :to-equal (format nil "5. [ ]~%   ```~%   code~%   ```~%~%6. regular"))
      (expect (cl-markdown-kit::%mw-alignment-marker :center) :to-equal ":---:")
      (expect (cl-markdown-kit::%mw-alignment-marker :unknown) :to-equal "---")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-list-node :items nil))
              :to-equal "")
      (let ((loose-task
              (markdown-writer-render-node
               (cl-markdown-kit:make-list-node
                :tight-p nil
                :items
                (list (cl-markdown-kit:make-task-list-item
                       :checked-p nil
                       :children
                       (list (cl-markdown-kit:make-paragraph
                              :children
                              (list (cl-markdown-kit:make-text :value "task"))))))))))
        (expect (search "task" loose-task) :to-be-truthy))
      (let ((tight-task-with-code
              (markdown-writer-render-node
               (cl-markdown-kit:make-list-node
                :tight-p t
                :items
                (list (cl-markdown-kit:make-task-list-item
                       :checked-p t
                       :children
                       (list (cl-markdown-kit:make-code-block :body "code"))))))))
        (expect (search "code" tight-task-with-code) :to-be-truthy))
      (expect
       (markdown-writer-render-node (cl-markdown-kit:make-table :rows nil))
       :to-equal "")))

(it "escapes a dash-only alignment marker in a one-cell paragraph"
    (expect (cl-markdown-kit::%mw-table-alignment-breaker-position
             (format nil "|a|~%---"))
            :to-equal 4)
    (expect
     (cl-markdown-kit:write-markdown
      (cl-markdown-kit:make-document
       :children
       (list
        (cl-markdown-kit:make-paragraph
         :children
         (list (cl-markdown-kit:make-text
                :value (format nil "|a|~%---")))))))
     :to-equal (format nil "|a|~%\\---")))

(it "renders remaining block variants and rejects invalid writer inputs"
    (expect
     (cl-markdown-kit::%mw-normalize-newlines
      (format nil "a~Cb~C~%c~%" #\Return #\Return))
     :to-equal (format nil "a~%b~%c~%"))
    (expect (cl-markdown-kit::%mw-normalize-newlines
             (format nil "end~C" #\Return))
            :to-equal (format nil "end~%"))
    (with-mw-writer-state
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-front-matter
         :raw-text (format nil "a~C~%b~Cc~%" #\Return #\Return)))
       :to-equal (format nil "---~%a~%b~%c~%---"))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-front-matter :raw-text ""))
       :to-equal (format nil "---~%---"))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-front-matter :raw-text "content"))
       :to-equal (format nil "---~%content~%---"))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-heading :level 2 :children nil))
       :to-equal "##")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-thematic-break))
              :to-equal "* * *")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-html-block :body "<hr>"))
              :to-equal "<hr>")
      (expect (markdown-writer-render-node
               (cl-markdown-kit:make-block-quote :children nil))
              :to-equal ">")
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-link-reference-definition
         :raw-label "raw[&]" :normalized-label "" :destination "target"
         :title "say \"hi\""))
       :to-equal (format nil "[raw~C[&amp;~C]]: <target> \"say ~C\"hi~C\"\""
                         #\\ #\\ #\\ #\\))
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-link-reference-definition
         :raw-label "raw" :normalized-label "normalized" :destination "target"))
       :to-equal "[normalized]: <target>")
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-footnote-definition :normalized-label "note"))
       :to-equal "[^note]:")
      (expect
       (markdown-writer-render-node
        (cl-markdown-kit:make-footnote-definition
         :normalized-label "note"
         :children
         (list (cl-markdown-kit:make-paragraph
                :children (list (cl-markdown-kit:make-text
                                 :value (format nil "first~%second")))))))
       :to-equal (format nil "[^note]:~%    first~%    second"))
      (expect
       (handler-case
           (progn
             (markdown-writer-render-node (cl-markdown-kit:make-node))
             nil)
         (cl-markdown-kit:markdown-unsupported-node-error () t))
       :to-be-truthy))
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-markdown :not-a-document) nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-markdown-to-stream
            (cl-markdown-kit:make-document :children nil) "not a stream")
           nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-markdown-to-stream
            (cl-markdown-kit:make-document :children nil)
            (make-string-input-stream "input"))
           nil)
       (type-error () t))
     :to-be-truthy))

(it "writes deeply nested Markdown block and inline trees"
    (let ((inline (cl-markdown-kit:make-text :value "deep")))
      (dotimes (depth 4096)
        (setf inline
              (cl-markdown-kit:make-emphasis
               :children (list inline))))
      (let ((block (cl-markdown-kit:make-paragraph
                    :children (list inline))))
        (dotimes (depth 256)
          (setf block
                (cl-markdown-kit:make-block-quote
                 :children (list block))))
        (let ((markdown
                (cl-markdown-kit:write-markdown
                 (cl-markdown-kit:make-document
                  :children (list block)))))
          (labels ((count-character (character)
                     (loop for candidate across markdown
                           count (char= candidate character))))
            (expect (search "deep" markdown) :to-be-truthy)
            (expect (count-character #\*) :to-equal 8192)
            (expect (count-character #\>) :to-equal 256))))))
  (it "releases a child's result after the parent stores its own result"
      (let* ((child (cl-markdown-kit:make-paragraph))
             (parent (cl-markdown-kit:make-block-quote
                      :children (list child)))
             (grandparent (cl-markdown-kit:make-block-quote
                           :children (list parent)))
             (context (cl-markdown-kit::%mw-make-render-session))
             (results
               (cl-markdown-kit::%mw-render-session-results context)))
        (setf (gethash child results) "retained child"
              (gethash parent
                       (cl-markdown-kit::%mw-render-session-frames context))
              (cl-markdown-kit::%mw-make-render-frame)
              (cl-markdown-kit::%mw-render-session-parent context)
              grandparent)
        (cl-markdown-kit::%mw-render-block-dispatch-event
         context parent :leave :block-quote)
        (expect (gethash parent results) :to-equal "> retained child")
        (expect (nth-value 1 (gethash parent results)) :to-equal t)
        (expect (nth-value 1 (gethash child results)) :to-equal nil))))
