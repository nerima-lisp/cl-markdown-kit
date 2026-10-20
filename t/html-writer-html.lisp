(in-package #:cl-markdown-kit/test)

(describe "HTML writer html"
(html-writer-it "omits raw script blocks by default"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body (format nil "<script>alert(1)</script>~%"))))))
      (expect (search "<script" output) :to-equal nil)
      (expect (search "alert(1)" output) :to-equal nil)
      (expect (search "<!-- raw HTML omitted -->" output)
              :to-be-truthy)))

(html-writer-it "filters dangerous link and image schemes by default"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children
                    (list (cl-markdown-kit:make-link
                           :destination "javascript:alert(1)"
                           :children (list (cl-markdown-kit:make-text
                                            :value "javascript")))
                          (cl-markdown-kit:make-text :value " ")
                          (cl-markdown-kit:make-link
                           :destination "JaVaScRiPt:alert(1)"
                           :children (list (cl-markdown-kit:make-text
                                            :value "mixed case")))
                          (cl-markdown-kit:make-text :value " ")
                          (cl-markdown-kit:make-image
                           :destination "data:text/html,<script>alert(1)</script>"
                           :children (list (cl-markdown-kit:make-text
                                            :value "unsafe image")))))))))
      (expect (html-writer-test-count-substring "href=\"\"" output)
              :to-equal 2)
      (expect (search "src=\"\"" output) :to-be-truthy)
      (expect (search "javascript:" output) :to-equal nil)
      (expect (search "data:text/html" output) :to-equal nil)))

(html-writer-it "escapes raw HTML when the escape mode is selected"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline
                              :body "<em>safe</em><script>x</script>"))))
      :raw-html :escape)
     :to-match-snapshot
     "html/escaped-raw-inline-html"))

(html-writer-it "passes ordinary tags through and applies the GFM tagfilter"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-html-inline
                                     :body "<em>safe</em><script>x</script>"))))
             :raw-html :pass-through)))
      (expect (search "<em>safe</em>" output) :to-be-truthy)
      (expect (search "&lt;script>" output) :to-be-truthy)
      (expect (search "&lt;/script>" output) :to-be-truthy)
      (expect (search "<script>" output) :to-equal nil)))

(html-writer-it "renders raw block HTML in escape mode and filters every tagfilter boundary"
    (let ((escaped
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body "<b>raw</b>"))
             :raw-html :escape))
          (filtered
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body (concatenate
                           'string
                           "<script>x</script> <scriptx>safe</scriptx> "
                           "<script/> <style type=\"text/css\">x</style> plain<")))
             :raw-html :pass-through :gfm t)))
      (expect escaped :to-match-snapshot "html/escaped-raw-block-html")
      (expect filtered
              :to-match-snapshot
              "html/gfm-tagfilter-boundaries")))

(html-writer-it "omits inline HTML and filters pass-through text without tag openers"
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline :body "<i>raw</i>")))))
     :to-match-snapshot "html/omitted-inline-html")
    (expect
     (html-writer-test-render
      (list (cl-markdown-kit:make-paragraph
             :children (list (cl-markdown-kit:make-html-inline :body "plain text"))))
      :raw-html :pass-through)
     :to-match-snapshot "html/passthrough-inline-html"))

(html-writer-it "passes raw CommonMark HTML through without the GFM tagfilter"
    (let* ((inline-body
             "<script>x</script><style>y</style><textarea>z</textarea><pre>w</pre>")
           (block-body
             (format nil
                     "<script>x</script>~%<style>y</style>~%<textarea>z</textarea>~%<pre>w</pre>~%"))
           (inline-document
             (html-writer-test-document
              (cl-markdown-kit:make-paragraph
               :children (list (cl-markdown-kit:make-html-inline
                                :body inline-body)))))
           (block-document
             (html-writer-test-document
              (cl-markdown-kit:make-html-block :body block-body)))
           (stream-output
             (with-output-to-string (stream)
               (cl-markdown-kit:write-html-to-stream
                inline-document stream :raw-html :pass-through :gfm nil))))
      (expect (cl-markdown-kit:write-html
               inline-document :raw-html :pass-through :gfm nil)
              :to-match-snapshot "html/passthrough-commonmark-inline")
      (expect stream-output :to-match-snapshot
              "html/passthrough-commonmark-inline-stream")
      (expect (cl-markdown-kit:write-html
               block-document :raw-html :pass-through :gfm nil)
              :to-match-snapshot "html/passthrough-commonmark-block")))

(html-writer-it "keeps the GFM tagfilter enabled for inline and block HTML"
    (let ((inline-output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children (list (cl-markdown-kit:make-html-inline
                                     :body "<textarea>x</textarea>"))))
             :raw-html :pass-through :gfm t))
          (block-output
            (html-writer-test-render
             (list (cl-markdown-kit:make-html-block
                    :body "<textarea>x</textarea>"))
             :raw-html :pass-through :gfm t)))
      (expect inline-output :to-match-snapshot
              "html/gfm-inline-tagfilter")
      (expect block-output :to-match-snapshot
              "html/gfm-block-tagfilter")))

(html-writer-it "allows dangerous URL schemes only when unsafe URLs are enabled"
    (let ((output
            (html-writer-test-render
             (list (cl-markdown-kit:make-paragraph
                    :children
                    (list (cl-markdown-kit:make-link
                           :destination "javascript:alert(1)"
                           :children (list (cl-markdown-kit:make-text
                                            :value "unsafe"))))))
             :unsafe-urls t)))
      (expect (search "href=\"javascript:alert(1)\"" output) :to-be-truthy))))
