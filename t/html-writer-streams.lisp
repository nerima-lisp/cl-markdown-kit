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
              :to-equal t))))
