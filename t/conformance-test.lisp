(in-package #:cl-markdown-kit/test)

(describe "Markdown conformance fixtures"
  (it "reads the pinned CommonMark and GFM extension examples"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm)))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (every #'spec-example-valid-p examples) :to-be-truthy)
      (expect (conformance-exclusions-valid-p
               examples (conformance-read-exclusions))
              :to-be-truthy)
      (format t "CommonMark 0.31.2 examples: ~D~%" (length commonmark))
      (format t "cmark-gfm extension examples: ~D~%" (length gfm))))

  (it "reports parser conditions with and without custom messages"
    (let* ((base-condition
             (make-condition 'cl-markdown-kit:markdown-kit-error))
           (base-message-condition
             (make-condition 'cl-markdown-kit:markdown-kit-error
                             :message "custom parser error"))
           (resource-default-condition
             (make-condition 'cl-markdown-kit:markdown-resource-limit-error))
           (resource-condition
             (make-condition 'cl-markdown-kit:markdown-resource-limit-error
                             :limit-name :nesting-depth
                             :limit 64
                             :actual 65
                             :context "document"))
           (resource-message-condition
             (make-condition 'cl-markdown-kit:markdown-resource-limit-error
                             :limit-name :nesting-depth
                             :limit 64
                             :actual 65
                             :context "document"
                             :message "maximum nesting depth exceeded")))
      (expect (princ-to-string base-condition) :to-equal "Markdown error")
      (expect (princ-to-string base-message-condition)
              :to-equal "custom parser error")
      (expect (cl-markdown-kit:markdown-kit-error-message base-condition)
              :to-equal nil)
      (expect (cl-markdown-kit:markdown-kit-error-message
               base-message-condition)
              :to-equal "custom parser error")
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
               resource-default-condition)
              :to-equal "resource")
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit
               resource-default-condition)
              :to-equal nil)
      (expect (cl-markdown-kit:markdown-resource-limit-error-actual
               resource-default-condition)
              :to-equal nil)
      (expect (cl-markdown-kit:markdown-resource-limit-error-context
               resource-default-condition)
              :to-equal "Markdown input")
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
               resource-condition)
              :to-equal :nesting-depth)
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit
               resource-condition)
              :to-equal 64)
      (expect (cl-markdown-kit:markdown-resource-limit-error-actual
               resource-condition)
              :to-equal 65)
      (expect (cl-markdown-kit:markdown-resource-limit-error-context
               resource-condition)
              :to-equal "document")
      (expect (search "Markdown resource limit exceeded in document"
                      (princ-to-string resource-condition))
              :to-be-truthy)
      (expect (search "; maximum nesting depth exceeded"
                      (princ-to-string resource-message-condition))
              :to-be-truthy))))
