;;;; src/nodes.lisp
(in-package #:cl-markdown-kit)

(defstruct (node (:constructor make-node (&key span)))
  span)

(defstruct (block-node
             (:include node)
             (:constructor make-block-node (&key span continuation-prefix)))
  (continuation-prefix "" :type string))

(defstruct (inline-node
             (:include node)
             (:constructor make-inline-node (&key span)))
  )

(defstruct (document
             (:include block-node)
             (:constructor make-document
                 (&key span continuation-prefix children source source-format
                       reference-index front-matter-p gfm-p max-nesting-depth
                       max-inline-delimiter-depth)))
  (children nil :type list)
  (source nil :type (or null string))
  (source-format nil :type (or null source-format))
  (reference-index nil :type (or null hash-table))
  (front-matter-p t :type boolean)
  (gfm-p t :type boolean)
  (max-nesting-depth 256 :type (integer 1 *))
  (max-inline-delimiter-depth 4096 :type (integer 1 *)))

(defstruct (front-matter
             (:include block-node)
             (:constructor make-front-matter
                 (&key span continuation-prefix raw-text opening-delimiter
                       closing-delimiter)))
  (raw-text "" :type string)
  (opening-delimiter "---" :type string)
  (closing-delimiter "---" :type string))

(defstruct (block-quote
             (:include block-node)
             (:constructor make-block-quote (&key span continuation-prefix children)))
  (children nil :type list))

(defstruct (list-node
             (:include block-node)
             (:constructor make-list-node
                 (&key span continuation-prefix kind tight-p start-number
                       marker-character delimiter-character items)))
  (kind :unordered :type keyword)
  (tight-p nil :type boolean)
  (start-number 1 :type (integer 0 *))
  (marker-character nil :type (or null character))
  (delimiter-character nil :type (or null character))
  (items nil :type list))

(defstruct (list-item
             (:include block-node)
             (:constructor make-list-item (&key span continuation-prefix children)))
  (children nil :type list))

(defstruct (task-list-item
             (:include list-item)
             (:constructor make-task-list-item
                 (&key span continuation-prefix children checked-p
                       task-marker-span original-marker-character)))
  (checked-p nil :type boolean)
  task-marker-span
  (original-marker-character #\Space :type character))

(defstruct (paragraph
             (:include block-node)
             (:constructor make-paragraph (&key span continuation-prefix children)))
  (children nil :type list))

(defstruct (heading
             (:include block-node)
             (:constructor make-heading
                 (&key span continuation-prefix level style marker-character
                       opening-marker-length closing-marker-length children)))
  (level 1 :type (integer 1 6))
  (style :atx :type keyword)
  (marker-character #\# :type (or null character))
  (opening-marker-length 1 :type (integer 0 *))
  (closing-marker-length 0 :type (integer 0 *))
  (children nil :type list))

(defstruct (thematic-break
             (:include block-node)
             (:constructor make-thematic-break
                 (&key span continuation-prefix marker-character marker-count)))
  (marker-character #\- :type character)
  (marker-count 3 :type (integer 1 *)))

(defstruct (code-block
             (:include block-node)
             (:constructor make-code-block
                 (&key span continuation-prefix style body fence-character
                       fence-length indentation info-string)))
  (style :fenced :type keyword)
  (body "" :type string)
  (fence-character nil :type (or null character))
  (fence-length 0 :type (integer 0 *))
  (indentation 0 :type (integer 0 *))
  (info-string nil :type (or null string)))

(defstruct (html-block
             (:include block-node)
             (:constructor make-html-block
                 (&key span continuation-prefix body kind)))
  (body "" :type string)
  (kind nil :type (or null integer keyword)))

(defstruct (link-reference-definition
             (:include block-node)
             (:constructor make-link-reference-definition
                 (&key span continuation-prefix raw-label normalized-label
                       destination title eligible-p)))
  (raw-label "" :type string)
  (normalized-label "" :type string)
  (destination "" :type string)
  (title nil :type (or null string))
  (eligible-p nil :type boolean))

(defstruct (table
             (:include block-node)
             (:constructor make-table (&key span continuation-prefix alignments rows)))
  (alignments nil :type list)
  (rows nil :type list))

(defstruct (table-row
             (:include block-node)
             (:constructor make-table-row
                 (&key span continuation-prefix header-row-p cells)))
  (header-row-p nil :type boolean)
  (cells nil :type list))

(defstruct (table-cell
             (:include block-node)
             (:constructor make-table-cell
                 (&key span continuation-prefix alignment children)))
  alignment
  (children nil :type list))

(defstruct (footnote-definition
             (:include block-node)
             (:constructor make-footnote-definition
                 (&key span continuation-prefix raw-label normalized-label children)))
  (raw-label "" :type string)
  (normalized-label "" :type string)
  (children nil :type list))

(defstruct (text
             (:include inline-node)
             (:constructor make-text (&key span value)))
  (value "" :type string))

(defstruct (code-span
             (:include inline-node)
             (:constructor make-code-span (&key span literal-text backtick-run-length)))
  (literal-text "" :type string)
  (backtick-run-length 1 :type (integer 1 *)))

(defstruct (html-inline
             (:include inline-node)
             (:constructor make-html-inline (&key span body)))
  (body "" :type string))

(defstruct (soft-break (:include inline-node)
                       (:constructor make-soft-break (&key span))))

(defstruct (hard-break (:include inline-node)
                       (:constructor make-hard-break (&key span))))

(defstruct (emphasis
             (:include inline-node)
             (:constructor make-emphasis (&key span delimiter-character children)))
  (delimiter-character nil :type (or null character))
  (children nil :type list))

(defstruct (strong
             (:include inline-node)
             (:constructor make-strong (&key span delimiter-character children)))
  (delimiter-character nil :type (or null character))
  (children nil :type list))

(defstruct (link
             (:include inline-node)
             (:constructor make-link
                 (&key span destination title reference-kind reference-label children)))
  (destination "" :type string)
  (title nil :type (or null string))
  (reference-kind :inline :type keyword)
  (reference-label nil :type (or null string))
  (children nil :type list))

(defstruct (image
             (:include inline-node)
             (:constructor make-image
                 (&key span destination title reference-kind reference-label children)))
  (destination "" :type string)
  (title nil :type (or null string))
  (reference-kind :inline :type keyword)
  (reference-label nil :type (or null string))
  (children nil :type list))

(defstruct (strikethrough
             (:include inline-node)
             (:constructor make-strikethrough
                 (&key span delimiter-character children)))
  (delimiter-character #\~ :type (or null character))
  (children nil :type list))

(defstruct (autolink
             (:include inline-node)
             (:constructor make-autolink (&key span destination display-text kind)))
  (destination "" :type string)
  (display-text "" :type string)
  (kind :angle-uri :type keyword))

(defstruct (footnote-reference
             (:include inline-node)
             (:constructor make-footnote-reference
                 (&key span raw-label normalized-label)))
  (raw-label "" :type string)
  (normalized-label "" :type string))

(defun node-children (node)
  "Return NODE's children in source order, or NIL for a leaf."
  (check-type node node)
  (typecase node
    (document (document-children node))
    (block-quote (block-quote-children node))
    (list-node (list-node-items node))
    (task-list-item (list-item-children node))
    (list-item (list-item-children node))
    (paragraph (paragraph-children node))
    (heading (heading-children node))
    (table (table-rows node))
    (table-row (table-row-cells node))
    (table-cell (table-cell-children node))
    (footnote-definition (footnote-definition-children node))
    (emphasis (emphasis-children node))
    (strong (strong-children node))
    (link (link-children node))
    (image (image-children node))
    (strikethrough (strikethrough-children node))
    (node nil)))

(defun walk-nodes (function root)
  "Call FUNCTION on ROOT and its descendants in pre-order, depth-first order.

FUNCTION receives one node. The tree structure is returned after the walk."
  (check-type function function)
  (check-type root node)
  (let ((pending (list root)))
    (loop while pending
          for current = (pop pending)
          do (funcall function current)
             (dolist (child (reverse (node-children current)))
               (push child pending))))
  root)
