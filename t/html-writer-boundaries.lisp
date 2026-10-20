(in-package #:cl-markdown-kit/test)

(describe "HTML writer boundaries"
(html-writer-it "rejects unsupported node values in documents"
    (expect
     (handler-case
         (progn
           (cl-markdown-kit:write-html
            (cl-markdown-kit:make-document
             :children (list (cl-markdown-kit:make-node))))
           nil)
       (simple-error () t))
     :to-be-truthy))

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
       (simple-error () t))
     :to-be-truthy)
    (expect
     (handler-case
         (progn
           (html-writer-test-render
            (list (cl-markdown-kit:make-paragraph
                   :children (list (cl-markdown-kit:make-inline-node)))))
           nil)
       (simple-error () t))
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
