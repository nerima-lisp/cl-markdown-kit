(in-package #:cl-markdown-kit/test)

(defun run-tests ()
  (run-all :reporter :spec :pass-with-no-tests nil))

(defun markdown-writer-render-inline-nodes (nodes)
  (cl-markdown-kit:write-markdown
   (cl-markdown-kit:make-document
    :children
    (list (cl-markdown-kit:make-paragraph :children nodes)))))

(defun markdown-writer-render-node (node)
  (cl-markdown-kit:write-markdown
   (cl-markdown-kit:make-document :children (list node))))
