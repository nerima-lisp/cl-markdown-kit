(in-package #:cl-markdown-kit/test)

(describe "AST node contract"
  (it "validates default dispatch families and missing registrations"
    (let ((registrations-before
            (copy-tree
             cl-markdown-kit::*node-dispatch-registrations*)))
      (expect
       (cl-markdown-kit::verify-node-dispatches
        :operation :markdown-renderer)
       :to-equal t)
      (expect cl-markdown-kit::*node-dispatch-registrations*
              :to-equal registrations-before))
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::%node-family-types :unknown)
                  nil)
              (simple-error (condition) condition))))
      (expect (and condition
                   (search "Unknown node family" (format nil "~A" condition)))
              :to-be-truthy))
    (let ((cl-markdown-kit::*node-dispatch-registrations* nil))
      (let ((condition
              (handler-case
                  (progn
                    (cl-markdown-kit::verify-node-dispatches
                     :operation :missing
                     :families (list :inline))
                    nil)
                (simple-error (condition) condition))))
        (expect (and condition
                     (search "Missing dispatch coverage"
                             (format nil "~A" condition)))
                :to-be-truthy))))
  (it "resets the registries and restores global state"
    (let ((definitions-before
            (copy-tree cl-markdown-kit::*node-definitions*))
          (registrations-before
            (copy-tree cl-markdown-kit::*node-dispatch-registrations*)))
      (let ((cl-markdown-kit::*node-definitions* '(:temporary))
            (cl-markdown-kit::*node-dispatch-registrations* '(:temporary)))
        (cl-markdown-kit::%reset-node-registries)
        (expect cl-markdown-kit::*node-definitions* :to-equal nil)
        (expect cl-markdown-kit::*node-dispatch-registrations*
                :to-equal nil))
      (expect cl-markdown-kit::*node-definitions* :to-equal definitions-before)
      (expect cl-markdown-kit::*node-dispatch-registrations*
              :to-equal registrations-before)))
  (it "resets registries for the root node declaration"
    (let* ((node-expansion
             (macroexpand-1
              '(cl-markdown-kit::define-node
                 (defstruct (cl-markdown-kit:node
                             (:constructor cl-markdown-kit:make-node))
                   cl-markdown-kit::span)
                 :abstract t
                 :kind nil
                 :family :abstract
                 :children nil
                 :attributes ()
                 :edit-text :empty
                 :edit-attributes ())))
           (registration-call (third (third node-expansion)))
           (definition (second (second registration-call)))
           (stale-definition
             '(:name stale-node :abstract nil :kind :stale :family :inline
               :children nil :attributes nil :edit-attributes nil
               :edit-text :empty :parent nil :slots nil))
           (stale-registration '((:stale :inline) stale-dispatch nil)))
      (expect (first node-expansion) :to-equal 'progn)
      (expect (getf definition :name) :to-equal 'cl-markdown-kit:node)
      (let ((cl-markdown-kit::*node-definitions* (list stale-definition))
            (cl-markdown-kit::*node-dispatch-registrations*
              (list stale-registration)))
        (expect (cl-markdown-kit::%register-node-definition definition)
                :to-equal definition)
        (expect cl-markdown-kit::*node-definitions*
                :to-equal (list definition))
        (expect cl-markdown-kit::*node-dispatch-registrations*
                :to-equal nil)))))
