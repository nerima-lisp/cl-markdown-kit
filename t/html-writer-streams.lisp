(in-package #:cl-markdown-kit/test)

(describe "HTML writer streams"
  (html-writer-it "writes equivalent output to a string or character stream"
    (let* ((document
             (html-writer-test-document
              (cl-markdown-kit:make-paragraph
               :children (list (cl-markdown-kit:make-text
                                :value "stream output")))))
           (string-output (cl-markdown-kit:write-html document))
           (stream-output
             (with-output-to-string (stream)
               (cl-markdown-kit:write-html-to-stream document stream))))
      (expect stream-output :to-equal string-output)))
  (html-writer-it "returns the supplied stream from the stream API"
    (let ((document
            (html-writer-test-document
             (cl-markdown-kit:make-paragraph
              :children (list (cl-markdown-kit:make-text
                               :value "stream output")))))
          (stream (make-string-output-stream)))
      (expect (eq (cl-markdown-kit:write-html-to-stream document stream)
                  stream)
              :to-equal t)))
  (html-writer-it "preserves partial output when rendering signals an error"
      (let* ((document
               (cl-markdown-kit:make-document
                :children
                (list (cl-markdown-kit:make-paragraph
                       :children (list (cl-markdown-kit:make-text
                                        :value "written")))
                      (cl-markdown-kit:make-node))))
             (stream (make-string-output-stream))
             (condition
               (handler-case
                   (cl-markdown-kit:write-html-to-stream document stream)
                 (condition (condition) condition))))
        (expect (typep condition 'simple-error) :to-be-truthy)
        (expect (get-output-stream-string stream)
                :to-equal (format nil "<p>written</p>~%")))))
