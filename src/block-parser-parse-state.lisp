(in-package #:cl-markdown-kit)

(defstruct (%bp-open-frame
             (:constructor %bp-make-open-frame
                 (&key kind node context child-context start-line start-position
                       end-line marker-kind marker-delimiter marker-columns
                       content-indent children items blank-seen has-content)))
  kind node context child-context start-line start-position end-line
  marker-kind marker-delimiter marker-columns content-indent
  (children nil) (items nil) (blank-seen nil) (has-content nil))

(defstruct (%bp-open-leaf
             (:constructor %bp-make-open-leaf
                 (&key kind context start-line start-position lines end-line
                       fence-character fence-length indentation info-string
                       html-kind html-terminator pending-lines table-node
                       table-alignments table-rows)))
  kind context start-line start-position end-line
  (lines nil) fence-character fence-length indentation info-string
  html-kind html-terminator (pending-lines nil) table-node table-alignments
  (table-rows nil))

(defstruct (%bp-parser-state
            (:constructor %bp-make-parser-state
                (&key source-buffer lines context depth limit gfm root stack)))
  source-buffer
  lines
  context
  depth
  limit
  gfm
  root
  stack
  leaf
  (cursor 0))

(defun %bp-new-parser-state (source-buffer lines context depth limit gfm)
  (let* ((lines (if (vectorp lines) lines (coerce lines 'vector)))
         (root (%bp-make-open-frame :kind :root :node nil
                                    :context context :child-context context)))
    (%bp-make-parser-state
     :source-buffer source-buffer :lines lines :context context
     :depth depth :limit limit :gfm gfm :root root :stack (list root))))

(defun %bp-state-current-frame (state)
  (car (%bp-parser-state-stack state)))

(defun %bp-state-current-context (state)
  (%bp-frame-context (%bp-state-current-frame state)))

(defun %bp-state-mark-container-content (state)
  (dolist (frame (%bp-parser-state-stack state))
    (when (member (%bp-open-frame-kind frame) '(:item :footnote))
      (setf (%bp-open-frame-has-content frame) t))))

(defun %bp-state-append-node (state node)
  (%bp-frame-add-node (%bp-state-current-frame state) node))

(defun %bp-state-push-frame (state frame)
  (push frame (%bp-parser-state-stack state))
  frame)

(defun %bp-state-nesting-level (state)
  (count-if (lambda (frame)
              (member (%bp-open-frame-kind frame) +bp-nesting-frame-kinds+))
            (%bp-parser-state-stack state)))

(defun %bp-state-close-leaf (state)
  (let ((leaf (%bp-parser-state-leaf state)))
    (when leaf
      (let* ((kind (%bp-open-leaf-kind leaf))
             (content-lines (nreverse (%bp-open-leaf-lines leaf)))
             (end-line (%bp-open-leaf-end-line leaf))
             (context (%bp-open-leaf-context leaf))
             (source-buffer (%bp-parser-state-source-buffer state))
             (node
               (case kind
                 (:paragraph
                  (%bp-make-paragraph source-buffer content-lines context))
                 (:fenced
                  (make-code-block
                   :span (source-buffer-span
                          source-buffer
                          (%bp-line-boundary
                           (%bp-open-leaf-start-line leaf)
                           (%bp-open-leaf-start-position leaf))
                          (%bp-line-boundary
                           end-line (length (%block-line-text end-line))))
                   :continuation-prefix context :style :fenced
                   :body (%bp-body-from-lines content-lines)
                   :fence-character (%bp-open-leaf-fence-character leaf)
                   :fence-length (%bp-open-leaf-fence-length leaf)
                   :indentation (%bp-open-leaf-indentation leaf)
                   :info-string (%bp-open-leaf-info-string leaf)))
                 (:indented
                  (make-code-block
                   :span (source-buffer-span
                          source-buffer
                          (%bp-line-boundary
                           (%bp-open-leaf-start-line leaf)
                           (%bp-open-leaf-start-position leaf))
                          (%bp-line-boundary
                           end-line (length (%block-line-text end-line))))
                   :continuation-prefix context :style :indented
                   :body (%bp-body-from-lines content-lines)
                   :fence-character nil :fence-length 0
                   :indentation 4 :info-string nil))
                 (:html
                  (make-html-block
                   :span (source-buffer-span
                          source-buffer
                          (%bp-line-boundary
                           (%bp-open-leaf-start-line leaf)
                           (%bp-open-leaf-start-position leaf))
                          (%bp-line-boundary
                           end-line (length (%block-line-text end-line))))
                   :continuation-prefix context
                   :body (%bp-body-from-lines content-lines)
                   :kind (%bp-open-leaf-html-kind leaf)))
                 (:table
                  (let ((table (%bp-open-leaf-table-node leaf)))
                    (setf (node-span table)
                          (source-buffer-span
                           source-buffer
                           (%bp-line-boundary
                            (%bp-open-leaf-start-line leaf)
                            (%bp-open-leaf-start-position leaf))
                           (%bp-line-boundary
                            end-line (length (%block-line-text end-line))))
                          (table-rows table)
                          (nreverse (%bp-open-leaf-table-rows leaf)))
                    table)))))
        (setf (%bp-parser-state-leaf state) nil)
        (%bp-state-append-node state node)
        node))))

(defun %bp-state-close-top-frame (state)
  (let* ((frame (pop (%bp-parser-state-stack state)))
         (parent (%bp-state-current-frame state))
         (end-line (%bp-open-frame-end-line frame))
         (node (%bp-finish-frame
                (%bp-parser-state-source-buffer state) frame)))
    (%bp-frame-add-node parent node)
    (setf (%bp-open-frame-end-line parent) end-line)
    node))

(defun %bp-state-close-through (state target)
  (loop while (not (eq (%bp-state-current-frame state) target))
        do (%bp-state-close-top-frame state))
  (%bp-state-close-top-frame state))

(defun %bp-state-frame-parent (state frame)
  (loop for tail on (%bp-parser-state-stack state)
        when (eq (car tail) frame)
          return (cadr tail)))

(defun %bp-state-finish (state)
  (%bp-state-close-leaf state)
  (loop while (cdr (%bp-parser-state-stack state))
        do (%bp-state-close-top-frame state))
  (nreverse
   (%bp-open-frame-children (%bp-parser-state-root state))))
