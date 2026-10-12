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
