(in-package #:cl-markdown-kit/test)

(describe "AST node contract"
  (it "walks the tree pre-order, depth-first, preserving sibling order"
    (let* ((first-leaf (cl-markdown-kit:make-text :value "first"))
           (nested-leaf (cl-markdown-kit:make-text :value "nested"))
           (last-leaf (cl-markdown-kit:make-text :value "last"))
           (emphasis (cl-markdown-kit:make-emphasis
                      :children (list nested-leaf)))
           (paragraph (cl-markdown-kit:make-paragraph
                       :children (list first-leaf emphasis)))
           (code (cl-markdown-kit:make-code-block :body "code"))
           (last-paragraph (cl-markdown-kit:make-paragraph
                            :children (list last-leaf)))
           (root (cl-markdown-kit:make-document
                  :children (list paragraph code last-paragraph)))
           (visited nil)
           (result (cl-markdown-kit:walk-node-tree root
            (lambda (frame)
              (let ((node (cl-markdown-kit:node-tree-frame-node frame))
                    (phase (cl-markdown-kit:node-tree-frame-phase frame)))
                (when (eq phase :enter)
                  (push node visited))
                :descend))))
           (expected (list root paragraph first-leaf emphasis nested-leaf
                           code last-paragraph last-leaf)))
      (expect (eq result root) :to-be-truthy)
      (expect (node-contract-same-order-p (nreverse visited) expected)
              :to-be-truthy)))
  (it "walks node trees with enter and leave context"
    (let* ((first-leaf (cl-markdown-kit:make-text :value "first"))
           (nested-inline
             (cl-markdown-kit:make-emphasis :children (list first-leaf)))
           (last-leaf (cl-markdown-kit:make-text :value "last"))
           (paragraph
             (cl-markdown-kit:make-paragraph
              :children (list nested-inline last-leaf)))
           (sibling (cl-markdown-kit:make-code-block :body "code"))
           (root
             (cl-markdown-kit:make-document
              :children (list paragraph sibling)))
           (events nil)
           (result
             (cl-markdown-kit:walk-node-tree root
              (lambda (frame)
                (push (list (cl-markdown-kit:node-tree-frame-node frame)
                            (cl-markdown-kit:node-tree-frame-phase frame)
                            (cl-markdown-kit:node-tree-frame-parent frame)
                            (cl-markdown-kit:node-tree-frame-grandparent frame)
                            (cl-markdown-kit:node-tree-frame-previous-sibling frame)
                            (cl-markdown-kit:node-tree-frame-next-sibling frame)
                            (cl-markdown-kit:node-tree-frame-child-index frame)
                            (cl-markdown-kit:node-tree-frame-last-child-p frame))
                      events)
                (if (eq (cl-markdown-kit:node-tree-frame-phase frame) :leave)
                    :stop
                    :descend)))))
      (setf events (nreverse events))
      (expect (eq result root) :to-be-truthy)
      (expect (mapcar (lambda (event) (subseq event 0 2)) events)
              :to-equal
              (list (list root :enter)
                    (list paragraph :enter)
                    (list nested-inline :enter)
                    (list first-leaf :enter)
                    (list first-leaf :leave)
                    (list nested-inline :leave)
                    (list last-leaf :enter)
                    (list last-leaf :leave)
                    (list paragraph :leave)
                    (list sibling :enter)
                    (list sibling :leave)
                    (list root :leave)))
      (expect (subseq (find root events :key (function first)) 2)
              :to-equal (list nil nil nil nil 0 t))
      (expect (subseq (find paragraph events :key (function first)) 2)
              :to-equal (list root nil nil sibling 0 nil))
      (expect (subseq (find sibling events :key (function first)) 2)
              :to-equal (list root nil paragraph nil 1 t))
      (expect (subseq (find last-leaf events :key (function first)) 2)
              :to-equal (list paragraph root nested-inline nil 1 t))
      (expect (subseq (find first-leaf events :key (function first)) 2)
              :to-equal (list nested-inline paragraph nil nil 0 t))))
  (it-each
    ((:skip) (:stop))
    "honors the ~A walker continuation"
    (decision)
    (let* ((leaf (cl-markdown-kit:make-text :value "hidden"))
           (inner
             (cl-markdown-kit:make-paragraph :children (list leaf)))
           (branch
             (cl-markdown-kit:make-block-quote :children (list inner)))
           (sibling (cl-markdown-kit:make-paragraph))
           (root
             (cl-markdown-kit:make-document
              :children (list branch sibling)))
           (events nil)
           (result
             (cl-markdown-kit:walk-node-tree root
              (lambda (frame)
                (let ((node (cl-markdown-kit:node-tree-frame-node frame))
                      (phase (cl-markdown-kit:node-tree-frame-phase frame)))
                  (push (list node phase) events)
                  (if (and (eq node branch) (eq phase :enter))
                      decision
                      :descend))))))
      (setf events (nreverse events))
      (expect (eq result root) :to-be-truthy)
      (expect events
              :to-equal
              (case decision
                (:skip
                 (list (list root :enter)
                       (list branch :enter)
                       (list branch :leave)
                       (list sibling :enter)
                       (list sibling :leave)
                       (list root :leave)))
                (:stop
                 (list (list root :enter)
                       (list branch :enter))))))))
