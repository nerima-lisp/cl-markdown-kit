(in-package #:cl-markdown-kit/test)

(describe "Markdown writer streams"
  (it "writes Markdown to a character stream"
    (let* ((document (cl-markdown-kit:parse-string "# Heading"))
           (written (with-output-to-string (stream)
                      (cl-markdown-kit:write-markdown-to-stream
                       document stream))))
      (expect written :to-equal "# Heading")))
  (it "returns the supplied stream from the stream API"
    (let ((document (cl-markdown-kit:parse-string "# Heading"))
          (stream (make-string-output-stream)))
      (expect (eq (cl-markdown-kit:write-markdown-to-stream document stream)
                  stream)
              :to-equal t)))
  (it "preserves partial output when rendering signals an error"
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
                   (cl-markdown-kit:write-markdown-to-stream document stream)
                 (condition (condition) condition))))
        (expect (typep condition 'simple-error) :to-be-truthy)
        (expect (get-output-stream-string stream) :to-equal "written"))))
