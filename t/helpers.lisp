(in-package #:cl-markdown-kit/test)

(defun run-tests ()
  (run-all :reporter :spec :pass-with-no-tests nil))

(defun test-resource-limit-fields (condition)
  (expect
   (typep condition
          (quote cl-markdown-kit:markdown-resource-limit-error))
   :to-be-truthy)
  (when (typep condition
               (quote cl-markdown-kit:markdown-resource-limit-error))
    (list
     (cl-markdown-kit:markdown-resource-limit-error-limit-name condition)
     (cl-markdown-kit:markdown-resource-limit-error-limit condition)
     (cl-markdown-kit:markdown-resource-limit-error-actual condition)
     (cl-markdown-kit:markdown-resource-limit-error-context condition))))

(defun markdown-writer-render-inline-nodes (nodes)
  (cl-markdown-kit:write-markdown
   (cl-markdown-kit:make-document
    :children
    (list (cl-markdown-kit:make-paragraph :children nodes)))))

(defun markdown-writer-render-node (node)
  (cl-markdown-kit:write-markdown
   (cl-markdown-kit:make-document :children (list node))))
