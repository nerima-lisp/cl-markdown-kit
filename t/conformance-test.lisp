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
      (format t "cmark-gfm extension examples: ~D~%" (length gfm)))))
