(in-package #:cl-markdown-kit)

(defun %hw-render-block-document-stage (phase) (when (eq phase :enter) :descend))

(defun %hw-render-block-footnote-definition-stage (context) (if (null (%html-writer-context-tree-parent context))
         :descend
         :skip))

(defun %hw-render-block-quote-stage (context phase) (case phase
       (:enter
        (%hw-newline context)
        (%hw-write context "<blockquote>")
        (%hw-write-char context #\Newline)
        :descend)
       (:leave
        (%hw-newline context)
        (%hw-write context "</blockquote>")
        (%hw-write-char context #\Newline))))

(defun %hw-render-block-list-stage (context node phase) (case phase
       (:enter
        (%hw-write-list-open context node)
        :descend)
       (:leave
        (%hw-write-list-close context node))))

(defun %hw-render-block-list-item-stage (context node phase) (case phase
       (:enter
        (%hw-write-list-item-open context node)
        :descend)
       (:leave
        (%hw-write-list-item-close context))))

(defun %hw-render-block-paragraph-stage (context phase) (let ((parent (%html-writer-context-tree-parent context)))
       (case phase
         (:enter
          (unless (%hw-tight-list-paragraph-p context)
            (%hw-newline context)
            (%hw-write context "<p>"))
          :descend)
         (:leave
          (when (and (footnote-definition-p parent)
                     (%html-writer-context-tree-last-child-p context))
            (%hw-write-char context #\Space)
            (%hw-write-footnote-backrefs
             context (footnote-definition-normalized-label parent)))
          (unless (%hw-tight-list-paragraph-p context)
            (%hw-write context "</p>")
            (%hw-write-char context #\Newline))))))

(defun %hw-render-block-heading-stage (context node phase) (case phase
       (:enter
        (%hw-newline context)
        (%hw-write context (format nil "<h~D>" (heading-level node)))
        :descend)
       (:leave
        (%hw-write context (format nil "</h~D>" (heading-level node)))
        (%hw-write-char context #\Newline))))

(defun %hw-render-block-thematic-break-stage (context phase) (when (eq phase :enter)
       (%hw-newline context)
       (%hw-write context "<hr />")
       (%hw-write-char context #\Newline)))

(defun %hw-render-block-code-stage (context node phase) (when (eq phase :enter)
       (%hw-write-code-block context node)
       :descend))

(defun %hw-render-block-html-stage (context node phase) (when (eq phase :enter)
       (%hw-newline context)
       (case (%html-writer-context-raw-html context)
         (:omit (%hw-write context "<!-- raw HTML omitted -->"))
         (:escape (%hw-escaped-text context (html-block-body node)))
         (:pass-through (%hw-write-raw-html context (html-block-body node))))
       (%hw-newline context)
       :descend))

(defun %hw-render-block-table-stage (context phase) (case phase
       (:enter
        (%hw-write-table-open context)
        :descend)
       (:leave
        (%hw-write-table-close context))))

(defun %hw-render-block-table-row-stage (context node phase) (case phase
       (:enter
        (%hw-write-table-row-open context node)
        :descend)
       (:leave
        (%hw-write-table-row-close context node))))

(defun %hw-render-block-table-cell-stage (context node phase) (case phase
       (:enter
        (%hw-write-table-cell-open context node)
        :descend)
       (:leave
        (%hw-write-table-cell-close context))))

(define-node-dispatch (%hw-render-block-event (context node phase))
      :operation :html-renderer
      :family :block
    (document
     (%hw-render-block-document-stage phase))
    (front-matter :skip)
    (link-reference-definition :skip)
    (footnote-definition
     (%hw-render-block-footnote-definition-stage context))
    (block-quote
     (%hw-render-block-quote-stage context phase))
    (list-node
     (%hw-render-block-list-stage context node phase))
    (task-list-item
     (%hw-render-block-list-item-stage context node phase))
    (list-item
     (%hw-render-block-list-item-stage context node phase))
    (paragraph
     (%hw-render-block-paragraph-stage context phase))
    (heading
     (%hw-render-block-heading-stage context node phase))
    (thematic-break
     (%hw-render-block-thematic-break-stage context phase)
     (when (eq phase :enter) :descend))
    (code-block
     (%hw-render-block-code-stage context node phase))
    (html-block
     (%hw-render-block-html-stage context node phase))
    (table
     (%hw-render-block-table-stage context phase))
    (table-row
     (%hw-render-block-table-row-stage context node phase))
    (table-cell
     (%hw-render-block-table-cell-stage context node phase)))
