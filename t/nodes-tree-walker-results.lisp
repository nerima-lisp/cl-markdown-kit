(in-package #:cl-markdown-kit/test)

(describe "AST node contract"
  (it "returns child callback results in sibling order"
    (let* ((first (cl-markdown-kit:make-paragraph))
           (second (cl-markdown-kit:make-code-block :body "second"))
           (root (cl-markdown-kit:make-document :children (list first second)))
           (observed nil))
      (cl-markdown-kit:walk-node-tree
       root
       (lambda (frame)
         (if (eq (cl-markdown-kit:node-tree-frame-phase frame) :enter)
             :descend
             (progn
               (if (eq (cl-markdown-kit:node-tree-frame-node frame) root)
                   (setf observed
                         (cl-markdown-kit:node-tree-frame-child-results frame))
                   (setf (cl-markdown-kit:node-tree-frame-result frame)
                         (cl-markdown-kit:node-tree-frame-node frame)))
               nil))))
      (expect observed :to-equal (list first second)))))
