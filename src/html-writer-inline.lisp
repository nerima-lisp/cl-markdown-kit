(in-package #:cl-markdown-kit)

(defun %hw-inline-plain-alt-p (context) (%html-writer-context-plain-alt-root context))

(defun %hw-render-inline-text-content (context node) (if (%hw-inline-plain-alt-p context)
         (%hw-escaped-attribute context (text-value node))
         (%hw-escaped-text context (text-value node))))

(defun %hw-render-inline-code-span-content (context node) (if (%hw-inline-plain-alt-p context)
         (%hw-escaped-attribute context (code-span-literal-text node))
         (progn
           (%hw-write context "<code>")
           (%hw-escaped-text context (code-span-literal-text node))
           (%hw-write context "</code>"))))

(defun %hw-render-inline-html-content (context node) (if (%hw-inline-plain-alt-p context)
         (%hw-escaped-attribute context (html-inline-body node))
         (case (%html-writer-context-raw-html context)
           (:omit (%hw-write context "<!-- raw HTML omitted -->"))
           (:escape (%hw-escaped-text context (html-inline-body node)))
           (:pass-through (%hw-write-raw-html context (html-inline-body node))))))

(defun %hw-render-inline-soft-break-content (context) (%hw-write-char context
                     (if (%hw-inline-plain-alt-p context)
                         #\Space
                         #\Newline)))

(defun %hw-render-inline-hard-break-content (context) (if (%hw-inline-plain-alt-p context)
         (%hw-write-char context #\Space)
         (progn
           (%hw-write context "<br />")
           (%hw-write-char context #\Newline))))

(defun %hw-render-inline-emphasis-stage (context phase) (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "<em>"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "</em>")))))

(defun %hw-render-inline-strong-stage (context phase) (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "<strong>"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "</strong>")))))

(defun %hw-render-inline-strikethrough-stage (context phase) (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "<del>"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "</del>")))))

(defun %hw-render-inline-link-stage (context node phase) (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "<a href=\"")
        (%hw-href context (link-destination node))
        (when (link-title node)
          (%hw-write context "\" title=\"")
          (%hw-escaped-attribute context (link-title node)))
        (%hw-write context "\">"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context)
        (%hw-write context "</a>")))))

(defun %hw-render-inline-image-stage (context node phase) (cond
     ((and (eq phase :leave)
           (eq node (%html-writer-context-plain-alt-root context)))
      (setf (%html-writer-context-plain-alt-root context) nil)
      (when (image-title node)
        (%hw-write context "\" title=\"")
        (%hw-escaped-attribute context (image-title node)))
      (%hw-write context "\" />"))
     ((%hw-inline-plain-alt-p context)
      (when (eq phase :enter) :descend))
     (t
      (%hw-write context "<img src=\"")
      (%hw-href context (image-destination node))
      (%hw-write context "\" alt=\"")
      (setf (%html-writer-context-plain-alt-root context) node)
      :descend)))

(defun %hw-render-inline-autolink-content (context node) (if (%hw-inline-plain-alt-p context)
         (%hw-escaped-attribute context (autolink-display-text node))
         (progn
           (%hw-write context "<a href=\"")
           (%hw-href context (autolink-destination node))
           (%hw-write context "\">")
           (%hw-escaped-text context (autolink-display-text node))
           (%hw-write context "</a>"))))

(defun %hw-render-inline-footnote-reference-stage (context node phase) (when (and (eq phase :enter)
              (not (%hw-inline-plain-alt-p context)))
     (%hw-write-footnote-reference context node)))

(define-node-dispatch (%hw-render-inline-event (context node phase))
    :operation :html-renderer
    :family :inline
  (text
   (when (eq phase :enter)
     (%hw-render-inline-text-content context node))
   (when (eq phase :enter) :descend))
  (code-span
   (when (eq phase :enter)
     (%hw-render-inline-code-span-content context node))
   (when (eq phase :enter) :descend))
  (html-inline
   (when (eq phase :enter)
     (%hw-render-inline-html-content context node))
   (when (eq phase :enter) :descend))
  (soft-break
   (when (eq phase :enter)
     (%hw-render-inline-soft-break-content context))
   (when (eq phase :enter) :descend))
  (hard-break
   (when (eq phase :enter)
     (%hw-render-inline-hard-break-content context))
   (when (eq phase :enter) :descend))
  (emphasis
   (%hw-render-inline-emphasis-stage context phase))
  (strong
   (%hw-render-inline-strong-stage context phase))
  (strikethrough
   (%hw-render-inline-strikethrough-stage context phase))
  (link
   (%hw-render-inline-link-stage context node phase))
  (image
   (%hw-render-inline-image-stage context node phase))
  (autolink
   (when (eq phase :enter)
     (%hw-render-inline-autolink-content context node))
   (when (eq phase :enter) :descend))
  (footnote-reference
   (%hw-render-inline-footnote-reference-stage context node phase)
   (when (eq phase :enter) :descend)))
