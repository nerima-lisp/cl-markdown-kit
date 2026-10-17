;;;; src/package.lisp
(defpackage #:cl-markdown-kit
  (:use #:cl)
  (:export
   ;; Conditions
   #:markdown-kit-error
   #:markdown-kit-error-message
   #:markdown-resource-limit-error
   #:markdown-resource-limit-error-limit-name
   #:markdown-resource-limit-error-limit
   #:markdown-resource-limit-error-actual
   #:markdown-resource-limit-error-context
   #:markdown-no-match-error
   #:markdown-no-match-error-address
   #:markdown-ambiguous-match-error
   #:markdown-ambiguous-match-error-address
   #:markdown-ambiguous-match-error-candidates
   #:markdown-invalid-section-address-error
   #:markdown-invalid-section-address-error-address
   #:markdown-invalid-section-address-error-reason
   #:markdown-nesting-depth-exceeded
   #:markdown-inline-delimiter-depth-exceeded
   #:markdown-structure-changed-error
   #:markdown-structure-changed-error-target
   #:markdown-structure-changed-error-difference
   ;; Source buffers and locations
   #:source-format
   #:source-format-p
   #:make-source-format
   #:source-format-input-kind
   #:source-format-encoding
   #:source-format-bom-p
   #:source-format-newline-style
   #:source-format-newline-sequence
   #:source-buffer
   #:source-buffer-p
   #:make-source-buffer
   #:source-buffer-source
   #:source-buffer-normalized-text
   #:source-buffer-format
   #:source-buffer-span
   #:source-buffer-document-span
   ;; AST metadata and traversal
   #:node-children
   #:walk-node-tree-cps
   #:node-kind
   #:node-family
   #:node-semantic-attributes
   #:node-edit-text-projection
   #:define-node
   #:define-node-dispatch
   #:verify-node-dispatches
   ;; Block parser results and deferred inline fragments
   #:inline-fragment
   #:inline-fragment-p
   #:make-inline-fragment
   #:inline-fragment-node
   #:inline-fragment-text
   #:inline-fragment-boundary-map
   #:block-parse-result
   #:block-parse-result-p
   #:make-block-parse-result
   #:block-parse-result-document
   #:block-parse-result-inline-fragments
   #:block-parse-result-reference-index
   ;; Parser facade
   #:parse-blocks
   #:parse
   #:parse-string
   #:parse-stream
   #:parse-octets
   #:write-html
   #:write-html-to-stream
   ;; Query records and functions
   #:heading-record
   #:heading-record-p
   #:make-heading-record
   #:heading-record-node
   #:heading-record-level
   #:heading-record-plain-text
   #:heading-record-source-form
   #:heading-record-section-span
   #:section-record
   #:section-record-p
   #:make-section-record
   #:section-record-heading
   #:section-record-span
   #:section-record-address
   #:document-headings
   #:find-section
   #:extract
   ;; Format-preserving edits
   #:replace-section-body
   #:replace-section
   #:delete-section
   #:insert-section
   #:replace-node
   #:set-task-checked
   ;; Markdown writer
   #:write-markdown
   #:write-markdown-to-stream))
