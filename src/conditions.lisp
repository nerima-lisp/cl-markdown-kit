;;;; src/conditions.lisp
(in-package #:cl-markdown-kit)

(define-condition markdown-kit-error (error)
  ((message :initarg :message :initform nil :reader markdown-kit-error-message))
  (:report (lambda (condition stream)
             (if (markdown-kit-error-message condition)
                 (write-string (markdown-kit-error-message condition) stream)
                 (write-string "Markdown error" stream)))))

(define-condition markdown-resource-limit-error (markdown-kit-error)
  ((limit-name :initarg :limit-name :initform "resource"
               :reader markdown-resource-limit-error-limit-name)
   (limit :initarg :limit :initform nil
          :reader markdown-resource-limit-error-limit)
   (actual :initarg :actual :initform nil
           :reader markdown-resource-limit-error-actual)
   (context :initarg :context :initform "Markdown input"
            :reader markdown-resource-limit-error-context))
  (:report (lambda (condition stream)
             (format stream "Markdown resource limit exceeded in ~A (~S limit ~S, actual ~S)"
                     (markdown-resource-limit-error-context condition)
                     (markdown-resource-limit-error-limit-name condition)
                     (markdown-resource-limit-error-limit condition)
                     (markdown-resource-limit-error-actual condition))
             (when (markdown-kit-error-message condition)
               (format stream "; ~A" (markdown-kit-error-message condition))))))

(define-condition markdown-no-match-error (markdown-kit-error)
  ((address :initarg :address :reader markdown-no-match-error-address)))

(define-condition markdown-ambiguous-match-error (markdown-kit-error)
  ((address :initarg :address :reader markdown-ambiguous-match-error-address)
   (candidates :initarg :candidates
               :reader markdown-ambiguous-match-error-candidates)))

(define-condition markdown-invalid-section-address-error (markdown-kit-error)
  ((address :initarg :address
            :reader markdown-invalid-section-address-error-address)
   (reason :initarg :reason
           :reader markdown-invalid-section-address-error-reason)))

(define-condition markdown-nesting-depth-exceeded
    (markdown-resource-limit-error)
  ())

(define-condition markdown-inline-delimiter-depth-exceeded
    (markdown-resource-limit-error)
  ())

(define-condition markdown-structure-changed-error (markdown-kit-error)
  ((target :initarg :target
           :reader markdown-structure-changed-error-target)
   (difference :initarg :difference
               :reader markdown-structure-changed-error-difference)))
(define-condition markdown-invalid-edit-error (markdown-kit-error)
  ((object :initarg :object
           :reader markdown-invalid-edit-error-object))
  (:report (lambda (condition stream)
             (if (markdown-kit-error-message condition)
                 (write-string (markdown-kit-error-message condition) stream)
                 (format stream "Invalid Markdown edit involving ~S"
                         (markdown-invalid-edit-error-object condition))))))
(define-condition markdown-unsupported-node-error (markdown-kit-error)
  ((object :initarg :object
           :reader markdown-unsupported-node-error-object))
  (:report (lambda (condition stream)
             (if (markdown-kit-error-message condition)
                 (write-string (markdown-kit-error-message condition) stream)
                 (format stream "Unsupported Markdown tree object: ~S"
                         (markdown-unsupported-node-error-object condition))))))
