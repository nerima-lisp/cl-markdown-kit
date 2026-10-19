(in-package #:cl-markdown-kit)

(defun %mw-render-document-tree (document)
  (let ((context (%mw-make-render-session)))
    (walk-node-tree document
                    (lambda (frame)
                      (let* ((node (node-tree-frame-node frame))
                             (phase (node-tree-frame-phase frame))
                             (parent (node-tree-frame-parent frame))
                             (previous-sibling
                              (node-tree-frame-previous-sibling frame))
                             (next-sibling (node-tree-frame-next-sibling frame)))
                        (setf (%mw-render-session-parent context) parent
                              (%mw-render-session-previous-sibling context) previous-sibling
                              (%mw-render-session-next-sibling context) next-sibling)
                        (when (eq phase :enter)
                          (setf (gethash node
                                         (%mw-render-session-frames context)) (%mw-make-node-render-frame
                                                                               context
                                                                               node)))
                        (let ((event-result
                               (cond
                                 ((inline-node-p node)
                                  (%mw-render-inline-event context node phase))
                                 ((block-node-p node)
                                  (%mw-render-block-event context node phase))
                                 (t
                                  (error "Unsupported Markdown node: ~S" node)))))
                          (when
                              (and (eq phase :leave)
                                   parent
                                   (%mw-inline-children-parent-p parent))
                            (let ((parent-frame
                                   (gethash parent
                                            (%mw-render-session-frames context))))
                              (when parent-frame
                                (%mw-advance-render-frame parent-frame
                                                          (or
                                                           (gethash node
                                                                    (%mw-render-session-results
                                                                     context))
                                                           "")))))
                          (if (eq phase :enter)
                              event-result
                              nil)))))
    (gethash document (%mw-render-session-results context))))

(defun %mw-render-document-markdown (document)
  (let ((*mw-line-start-p* t)
        (*mw-line-indent* 0)
        (*mw-table-cell-p* nil)
        (*mw-text-autolink-breakpoints* nil)
        (*mw-heading-p* nil)
        (*mw-autolink-label-p* nil)
        (*mw-escape-trailing-bang-p* nil)
        (*mw-following-block-p* nil)
        (*mw-minimize-inline-escapes-p* nil)
        (*mw-heading-closing-hash-allowed-p* nil))
    (%mw-render-document-tree document)))

(defun write-markdown-to-stream (document stream)
  (check-type document document)
  (unless (output-stream-p stream)
    (error 'type-error
           :datum
           stream
           :expected-type
           '(satisfies output-stream-p)))
  (write-string (%mw-render-document-markdown document) stream))

(defun write-markdown (document)
  (with-output-to-string (stream)
    (write-markdown-to-stream document stream)))
