(in-package #:cl-markdown-kit)

(defstruct
    (%mw-render-frame
     (:constructor %mw-make-render-frame
                   (&key (line-start-p t)
                         (line-indent 0)
                         table-cell-p
                         heading-p
                         autolink-label-p
                         escape-trailing-bang-p
                         following-block-p
                         minimize-inline-escapes-p
                         heading-closing-hash-allowed-p
                         escape-terminal-backslash-p
                         prefix)))
  (line-start-p t)
  (line-indent 0)
  table-cell-p
  heading-p
  autolink-label-p
  escape-trailing-bang-p
  following-block-p
  minimize-inline-escapes-p
  heading-closing-hash-allowed-p
  escape-terminal-backslash-p
  (prefix "" :type string))

(defstruct
    (%mw-render-session
     (:constructor %mw-make-render-session
                   (&key (frames (make-hash-table :test #'eq))
                         (results (make-hash-table :test #'eq))
                         output-stream)))
  frames
  results
  output-stream
  child-index
  parent
  previous-sibling
  next-sibling)

(defun %mw-inline-children-parent-p (node)
  (or (paragraph-p node)
      (heading-p node)
      (table-cell-p node)
      (emphasis-p node)
      (strong-p node)
      (link-p node)
      (image-p node)
      (strikethrough-p node)))

(defun %mw-inline-wrapper-p (node)
  (or (emphasis-p node)
      (strong-p node)
      (link-p node)
      (image-p node)
      (strikethrough-p node)))

(defun %mw-copy-render-frame (frame)
  (%mw-make-render-frame :line-start-p
                         (%mw-render-frame-line-start-p frame)
                         :line-indent
                         (%mw-render-frame-line-indent frame)
                         :table-cell-p
                         (%mw-render-frame-table-cell-p frame)
                         :heading-p
                         (%mw-render-frame-heading-p frame)
                         :autolink-label-p
                         (%mw-render-frame-autolink-label-p frame)
                         :escape-trailing-bang-p
                         (%mw-render-frame-escape-trailing-bang-p frame)
                         :following-block-p
                         (%mw-render-frame-following-block-p frame)
                         :minimize-inline-escapes-p
                         (%mw-render-frame-minimize-inline-escapes-p frame)
                         :heading-closing-hash-allowed-p
                         (%mw-render-frame-heading-closing-hash-allowed-p frame)
                         :escape-terminal-backslash-p
                         (%mw-render-frame-escape-terminal-backslash-p frame)))

(defun %mw-make-node-render-frame (session node)
  (let* ((parent (%mw-render-session-parent session))
         (parent-frame
          (and parent (gethash parent (%mw-render-session-frames session))))
         (next (%mw-render-session-next-sibling session))
         (following
          (or
           (and parent-frame (%mw-render-frame-following-block-p parent-frame))
           next)))
    (cond
      ((and parent-frame (%mw-inline-children-parent-p parent))
       (let ((frame (%mw-copy-render-frame parent-frame)))
         (when
             (or (%mw-inline-wrapper-p parent)
                 (not (%mw-inline-text-run-node-p node)))
           (setf (%mw-render-frame-heading-closing-hash-allowed-p frame) nil))
         (when (%mw-inline-wrapper-p parent)
           (setf (%mw-render-frame-minimize-inline-escapes-p frame) nil
                 (%mw-render-frame-escape-terminal-backslash-p frame) t))
         frame))
      ((paragraph-p node)
       (%mw-make-render-frame :following-block-p
                              following
                              :minimize-inline-escapes-p
                              t))
      ((heading-p node)
       (%mw-make-render-frame :following-block-p
                              following
                              :minimize-inline-escapes-p
                              t
                              :heading-p
                              t
                              :heading-closing-hash-allowed-p
                              t))
      ((table-cell-p node)
       (%mw-make-render-frame :following-block-p
                              following
                              :table-cell-p
                              t
                              :minimize-inline-escapes-p
                              t))
      (t (%mw-make-render-frame :following-block-p following)))))

(defun %mw-capture-render-frame (frame function)
  (let ((*mw-line-start-p* (%mw-render-frame-line-start-p frame))
        (*mw-line-indent* (%mw-render-frame-line-indent frame))
        (*mw-table-cell-p* (%mw-render-frame-table-cell-p frame))
        (*mw-heading-p* (%mw-render-frame-heading-p frame))
        (*mw-autolink-label-p* (%mw-render-frame-autolink-label-p frame))
        (*mw-escape-trailing-bang-p*
         (%mw-render-frame-escape-trailing-bang-p frame))
        (*mw-following-block-p* (%mw-render-frame-following-block-p frame))
        (*mw-minimize-inline-escapes-p*
         (%mw-render-frame-minimize-inline-escapes-p frame))
        (*mw-heading-closing-hash-allowed-p*
         (%mw-render-frame-heading-closing-hash-allowed-p frame)))
    (let ((result
           (with-output-to-string (stream)
             (funcall function stream))))
      (setf (%mw-render-frame-line-start-p frame) *mw-line-start-p*
            (%mw-render-frame-line-indent frame) *mw-line-indent*)
      result)))

(defun %mw-advance-render-frame (frame text)
  (%mw-capture-render-frame frame
                            (lambda (stream)
                              (%mw-write-string text stream))))

(defun %mw-release-render-results (context children)
  (let ((results (%mw-render-session-results context)))
    (dolist (child children)
      (remhash child results))))

(defun %mw-store-render-result (context node children result)
  (setf (gethash node (%mw-render-session-results context)) result)
  (%mw-release-render-results context children)
  result)

(defun %mw-render-child-results (children results)
  (with-output-to-string (stream)
    (dolist (child children)
      (write-string (or (gethash child results) "") stream))))
