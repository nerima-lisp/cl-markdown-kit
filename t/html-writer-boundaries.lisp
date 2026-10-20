(in-package #:cl-markdown-kit/test)

(describe "HTML writer boundaries"
  (html-writer-it "signals exact conditions for unsupported tree shapes"
    (let* ((node (cl-markdown-kit:make-node))
           (condition
             (test-condition
              (lambda ()
                (cl-markdown-kit:write-html
                 (cl-markdown-kit:make-document :children (list node)))))))
      (expect (typep condition 'cl-markdown-kit:markdown-unsupported-node-error)
              :to-be-truthy)
      (progn
        (expect (eq (cl-markdown-kit:markdown-unsupported-node-error-object condition)
                    node)
                :to-be-truthy)
        (let ((message (cl-markdown-kit:markdown-kit-error-message condition)))
          (expect (search "Unsupported HTML node:" message) :to-be-truthy)
          (expect (search message (princ-to-string condition)) :to-be-truthy))))
    (let* ((child :not-a-node)
           (condition
             (test-condition
              (lambda ()
                (cl-markdown-kit:write-html
                 (html-writer-test-document
                  (cl-markdown-kit:make-paragraph :children (list child))))))))
      (expect (typep condition 'cl-markdown-kit:markdown-unsupported-node-error)
              :to-be-truthy)
      (progn
        (expect (eq (cl-markdown-kit:markdown-unsupported-node-error-object condition)
                    child)
                :to-be-truthy)
        (let ((message (cl-markdown-kit:markdown-kit-error-message condition)))
          (expect (search "Unsupported HTML child:" message) :to-be-truthy)
          (expect (search message (princ-to-string condition)) :to-be-truthy))))
    (let* ((item (cl-markdown-kit:make-list-item))
           (condition
             (test-condition
              (lambda ()
                (cl-markdown-kit:write-html
                 (cl-markdown-kit:make-document :children (list item)))))))
      (expect (typep condition 'cl-markdown-kit:markdown-unsupported-node-error)
              :to-be-truthy)
      (progn
        (expect (eq (cl-markdown-kit:markdown-unsupported-node-error-object condition)
                    item)
                :to-be-truthy)
        (let ((message (cl-markdown-kit:markdown-kit-error-message condition)))
          (expect (search "Unsupported HTML list item outside a list:"
                          message)
                  :to-be-truthy)
          (expect (search message (princ-to-string condition)) :to-be-truthy))
        (let* ((object :writer-fallback-object)
               (fallback-condition
                 (make-condition
                  'cl-markdown-kit:markdown-unsupported-node-error
                  :object object))
               (reported (princ-to-string fallback-condition)))
          (expect (null (cl-markdown-kit:markdown-kit-error-message
                         fallback-condition))
                  :to-be-truthy)
          (expect (eq (cl-markdown-kit:markdown-unsupported-node-error-object
                       fallback-condition)
                      object)
                  :to-be-truthy)
          (expect (search "Unsupported Markdown tree object:" reported)
                  :to-be-truthy)))))

(html-writer-it "rejects unsupported nodes, invalid options, and non-output streams"
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html :not-a-document) nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-html
            (html-writer-test-document
             (cl-markdown-kit:make-block-node)))
           nil)
       (cl-markdown-kit:markdown-unsupported-node-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (html-writer-test-render
            (list (cl-markdown-kit:make-paragraph
                   :children (list (cl-markdown-kit:make-inline-node)))))
           nil)
       (cl-markdown-kit:markdown-unsupported-node-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html (html-writer-test-document)
                                            :raw-html :invalid)
                nil)
       (simple-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html (html-writer-test-document)
                                            :unsafe-urls :invalid)
                nil)
       (simple-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html (html-writer-test-document)
                                            :gfm :invalid)
                nil)
       (simple-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (with-input-from-string (stream "")
           (cl-markdown-kit:write-html-to-stream
            (html-writer-test-document) stream)
           nil)
       (type-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn (cl-markdown-kit:write-html-to-stream
                 (html-writer-test-document) "not-a-stream")
                nil)
       (type-error () t))
     :to-be-truthy))

(html-writer-it "writes deeply nested block and inline trees"
    (let ((inline (cl-markdown-kit:make-text :value "deep")))
      (dotimes (depth 4096)
        (setf inline
              (cl-markdown-kit:make-emphasis
               :children (list inline))))
      (let ((node (cl-markdown-kit:make-paragraph
                   :children (list inline))))
        (dotimes (depth 256)
          (setf node
                (cl-markdown-kit:make-block-quote
                 :children (list node))))
        (let ((html (cl-markdown-kit:write-html
                     (cl-markdown-kit:make-document
                      :children (list node)))))
          (labels ((count-occurrences (needle)
                     (loop with start = 0
                           for position = (search needle html :start2 start)
                           while position
                           count t
                           do (setf start (+ position (length needle))))))
            (expect (count-occurrences "<blockquote>") :to-equal 256)
            (expect (count-occurrences "</blockquote>") :to-equal 256)
            (expect (count-occurrences "<em>") :to-equal 4096)
            (expect (count-occurrences "</em>") :to-equal 4096)
            (expect (search "deep" html) :to-be-truthy)))))))
