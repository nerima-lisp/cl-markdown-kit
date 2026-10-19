(in-package #:cl-markdown-kit)
  (progn
  (defun %hw-verify-node-dispatches ()
    (verify-node-dispatches
     :operation :html-renderer
     :families '(:block :inline)))
  (verify-node-dispatches-at-compile-time
   :html-renderer
   (:block :inline)))
  (defun %hw-render-node-tree (context root)
    (walk-node-tree root
     (lambda (frame)
       (let ((node (node-tree-frame-node frame))
             (phase (node-tree-frame-phase frame))
             (parent (node-tree-frame-parent frame))
             (grandparent (node-tree-frame-grandparent frame))
             (child-index (node-tree-frame-child-index frame))
             (last-child-p (node-tree-frame-last-child-p frame)))
         (setf (%html-writer-context-tree-parent context) parent
               (%html-writer-context-tree-grandparent context) grandparent
               (%html-writer-context-tree-child-index context) child-index
               (%html-writer-context-tree-last-child-p context) last-child-p)
         (let ((action
                 (cond
                   ((inline-node-p node)
                    (%hw-render-inline-event context node phase))
                   ((block-node-p node)
                    (%hw-render-block-event context node phase))
                   (t
                    (error "Unsupported HTML node: ~S" node)))))
           (if (eq phase :enter) action nil))))))

(defun write-html-to-stream (document stream &key (raw-html :omit) unsafe-urls
                                                  (gfm t))
  "Write DOCUMENT as HTML to STREAM and return STREAM."
  (check-type document document)
  (check-type stream stream)
  (unless (output-stream-p stream)
    (error "STREAM is not open for output"))
  (unless (member raw-html '(:omit :escape :pass-through))
    (error "RAW-HTML must be :OMIT, :ESCAPE, or :PASS-THROUGH"))
  (unless (typep unsafe-urls 'boolean)
    (error "UNSAFE-URLS must be a boolean"))
  (unless (typep gfm 'boolean)
    (error "GFM must be a boolean"))
  (multiple-value-bind (definitions numbers reference-counts order
                        footnote-ids footnote-reference-ids)
      (%hw-build-footnote-context document)
    (let ((context (%make-html-writer-context
                    :stream stream
                    :raw-html raw-html
                    :unsafe-urls unsafe-urls
                    :gfm gfm
                    :footnote-definitions definitions
                    :footnote-numbers numbers
                    :footnote-reference-counts reference-counts
                    :footnote-reference-indices (make-hash-table :test #'equal)
                    :footnote-order order
                    :footnote-ids footnote-ids
                    :footnote-reference-ids footnote-reference-ids)))
      (%hw-render-node-tree context document)
      (%hw-write-footnotes context)))
  stream)

(defun write-html (document &key (raw-html :omit) unsafe-urls (gfm t))
  "Return DOCUMENT rendered as HTML."
  (with-output-to-string (stream)
    (write-html-to-stream document stream
                          :raw-html raw-html
                          :unsafe-urls unsafe-urls
                          :gfm gfm)))
