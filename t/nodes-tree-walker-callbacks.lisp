(in-package #:cl-markdown-kit/test)

(describe "AST node contract"
  (it "rejects invalid actions from enter and leave callbacks"
    (let ((root (cl-markdown-kit:make-document)))
      (dolist (invalid-phase '(:enter :leave))
        (expect
         (handler-case
             (progn
               (cl-markdown-kit:walk-node-tree
                root
                (lambda (frame)
                  (if (eq (cl-markdown-kit:node-tree-frame-phase frame)
                          invalid-phase)
                      :invalid
                      :descend)))
               nil)
           (simple-error () t))
         :to-be-truthy)))))
