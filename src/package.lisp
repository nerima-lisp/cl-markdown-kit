;;;; src/package.lisp
(defpackage #:cl-markdown-kit
  (:use #:cl)
  (:export
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
   #:markdown-structure-changed-error-difference))
