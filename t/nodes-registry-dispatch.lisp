(in-package #:cl-markdown-kit/test)

(describe "AST node contract"
  (it-each
    ((:malformed-metadata-plist
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-plist)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes)
      "metadata must be a property list")
     (:duplicate-metadata-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-metadata)
        :kind :contract :kind :other :family :inline :children nil
        :attributes nil :edit-text :empty :edit-attributes ())
      "metadata contains duplicate keys")
     (:unknown-metadata-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-unknown-metadata)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes () :unknown t)
      "Unknown DEFINE-NODE metadata key")
     (:missing-metadata-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-missing-metadata)
        :kind :contract :family :inline :children nil :edit-text :empty
        :edit-attributes ())
      "is missing required")
     (:invalid-kind
      (cl-markdown-kit::define-node
        (defstruct node-contract-invalid-kind)
        :kind "contract" :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE kind/family")
     (:invalid-family
      (cl-markdown-kit::define-node
        (defstruct (node-contract-invalid-family-node
                    (:constructor make-node-contract-invalid-family-node ())))
        :kind :invalid :family :unknown :children nil :attributes nil
        :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE kind/family")
     (:abstract-family-mismatch
      (cl-markdown-kit::define-node
        (defstruct node-contract-abstract-family)
        :abstract t :kind nil :family :inline :children nil :attributes nil
        :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE kind/family")
     (:non-keyword-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-keyword-attribute value)
        :kind :contract :family :inline :children nil :attributes (value)
        :edit-text :empty :edit-attributes ())
      "attributes must be distinct keyword names")
     (:duplicate-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-attribute value)
        :kind :contract :family :inline :children nil
        :attributes (:value :value) :edit-text :empty :edit-attributes ())
      "attributes must be distinct keyword names")
     (:attribute-without-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-attribute-without-slot)
        :kind :contract :family :inline :children nil
        :attributes (:missing) :edit-text :empty :edit-attributes ())
      "has no slot")
     (:malformed-edit-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-edit-attribute cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((:value)))
      "edit attributes must map declared attribute slots")
     (:undeclared-edit-attribute
      (cl-markdown-kit::define-node
        (defstruct node-contract-undeclared-edit-attribute value)
        :kind :contract :family :inline :children nil :attributes ()
        :edit-text :empty :edit-attributes ((:value :before)))
      "edit attributes must map declared attribute slots")
     (:duplicate-edit-attribute-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-edit-slot cl-markdown-kit::value cl-markdown-kit::other)
        :kind :contract :family :inline :children nil
        :attributes (:value :other) :edit-text :empty
        :edit-attributes ((:value :before) (:value :after)))
      "edit attributes contain a duplicate slot")
     (:duplicate-edit-difference-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-duplicate-edit-key cl-markdown-kit::value cl-markdown-kit::other)
        :kind :contract :family :inline :children nil
        :attributes (:value :other) :edit-text :empty
        :edit-attributes ((:value :same) (:other :same)))
      "edit attributes contain a duplicate difference key")
     (:malformed-children
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-children value)
        :kind :contract :family :inline :children (:unknown value)
        :attributes nil :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE :CHILDREN value")
     (:child-without-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-child-without-slot value)
        :kind :contract :family :inline :children missing
        :attributes nil :edit-text :empty :edit-attributes ())
      "child slot")
     (:malformed-edit-text
      (cl-markdown-kit::define-node
        (defstruct node-contract-malformed-edit-text value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:slot value :extra) :edit-attributes ())
      "invalid or missing edit text projection")
     (:edit-text-without-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-edit-text-without-slot value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:slot missing) :edit-attributes ())
      "invalid or missing edit text projection")
     (:malformed-dispatch-definition
      (cl-markdown-kit::define-node-dispatch
        (node-contract-invalid-dispatch)
        :operation :contract :family :inline
        (cl-markdown-kit:text nil))
      "requires (function-name lambda-list)")
     (:malformed-dispatch-options
      (cl-markdown-kit::define-node-dispatch
        (node-contract-missing-dispatch-options (context cl-markdown-kit::node phase))
        :operation :contract :family :inline)
      "requires :OPERATION and :FAMILY before clauses")
     (:missing-dispatch-type
      (cl-markdown-kit::define-node-dispatch
        (node-contract-incomplete-dispatch (context cl-markdown-kit::node phase))
        :operation :html-renderer :family :inline
        (cl-markdown-kit:text nil))
      "Missing node type clause")
     (:duplicate-dispatch-type
      (cl-markdown-kit::define-node-dispatch
        (node-contract-duplicate-dispatch (context cl-markdown-kit::node phase))
        :operation :html-renderer :family :inline
        (cl-markdown-kit:text nil)
        (cl-markdown-kit:text nil))
      "Duplicate node type clause")
     (:extra-dispatch-type
      (cl-markdown-kit::define-node-dispatch
        (node-contract-extra-dispatch (context cl-markdown-kit::node phase))
        :operation :html-renderer :family :inline
        (cl-markdown-kit:paragraph nil))
      "Extra node type clause")
     (:non-list-edit-attributes
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-list-edit-attributes cl-markdown-kit::value)
        :kind :contract :family :inline :children nil
        :attributes (:value) :edit-text :empty :edit-attributes 42)
      "edit attributes must map declared attribute slots")
     (:non-keyword-edit-attribute-name
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-keyword-edit-attribute cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((value :before)))
      "edit attributes must map declared attribute slots")
     (:non-keyword-edit-difference-key
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-keyword-edit-key cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((:value before)))
      "edit attributes must map declared attribute slots")
     (:extra-edit-attribute-elements
      (cl-markdown-kit::define-node
        (defstruct node-contract-extra-edit-attribute-elements cl-markdown-kit::value)
        :kind :contract :family :inline :children nil :attributes (:value)
        :edit-text :empty :edit-attributes ((:value :before :extra)))
      "edit attributes must map declared attribute slots")
     (:child-with-non-symbol-accessor
      (cl-markdown-kit::define-node
        (defstruct node-contract-child-invalid-accessor value)
        :kind :contract :family :inline
        :children (:slot value :accessor 42)
        :attributes nil :edit-text :empty :edit-attributes ())
      "child slot")
     (:edit-text-with-invalid-head
      (cl-markdown-kit::define-node
        (defstruct node-contract-edit-text-invalid-head value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:other value) :edit-attributes ())
      "invalid or missing edit text projection")
     (:edit-text-with-non-symbol-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-edit-text-non-symbol-slot value)
        :kind :contract :family :inline :children nil :attributes nil
        :edit-text (:slot 42) :edit-attributes ())
      "invalid or missing edit text projection")
     (:child-with-non-symbol-slot
      (cl-markdown-kit::define-node
        (defstruct node-contract-child-non-symbol-slot)
        :kind :contract :family :inline :children (:slot 42)
        :attributes nil :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE :CHILDREN value")
     (:non-list-child-spec
      (cl-markdown-kit::define-node
        (defstruct node-contract-non-list-child-spec)
        :kind :contract :family :inline :children 42
        :attributes nil :edit-text :empty :edit-attributes ())
      "Invalid DEFINE-NODE :CHILDREN value"))
    "rejects invalid node and dispatch declarations during macro expansion: ~A"
    (failure form expected-message)
    (let ((condition
            (handler-case (progn (macroexpand-1 form) nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (let ((condition-message (format nil "~A" condition)))
        (unless (search expected-message condition-message)
          (error "Expected condition message containing ~S, got ~S."
                 expected-message condition-message)))))
  (it "expands node declarations, projects the registry, and registers idempotently"
    (let* ((node-form
             '(cl-markdown-kit::define-node
                (defstruct (coverage-contract-node
                            (:constructor make-coverage-contract-node ()))
                  value)
                :kind :coverage-contract
                :family :inline
                :children value
                :attributes ()
                :edit-text (:slot value)
                :edit-attributes ()))
           (node-expansion (macroexpand-1 node-form))
           (registry-expansion
             (macroexpand-1 '(cl-markdown-kit::define-node-registry)))
           (registry-function-names
             (mapcar #'second (rest registry-expansion))))
      (expect (first node-expansion) :to-equal 'progn)
      (expect (first (second node-expansion)) :to-equal 'defstruct)
      (expect (fourth node-expansion)
              :to-equal '(quote coverage-contract-node))
      (expect (first registry-expansion) :to-equal 'progn)
      (expect registry-function-names
              :to-equal
              '(cl-markdown-kit:node-children
                cl-markdown-kit:node-kind
                cl-markdown-kit:node-family
                cl-markdown-kit:node-semantic-attributes
                cl-markdown-kit:node-edit-text-projection)))
    (let ((definitions-before
            (copy-tree cl-markdown-kit::*node-definitions*)))
      (let ((cl-markdown-kit::*node-definitions* nil))
        (let* ((node-form
                 '(cl-markdown-kit::define-node
                    (defstruct node-contract-registration)
                    :kind :contract-registration
                    :family :inline
                    :children nil
                    :attributes ()
                    :edit-text :empty
                    :edit-attributes ()))
               (same-registration
                 (third (macroexpand-1 node-form)))
               (conflict-registration
                 (third
                  (macroexpand-1
                   '(cl-markdown-kit::define-node
                      (defstruct node-contract-registration)
                      :kind :different-registration
                      :family :inline
                      :children nil
                      :attributes ()
                      :edit-text :empty
                      :edit-attributes ()))))
               (expected-definition-count 1))
          (eval same-registration)
          (expect (length cl-markdown-kit::*node-definitions*)
                  :to-equal expected-definition-count)
          (let ((registered-definition
                  (copy-tree (first cl-markdown-kit::*node-definitions*))))
            (eval same-registration)
            (expect cl-markdown-kit::*node-definitions*
                    :to-equal (list registered-definition)))
          (let ((before-conflict
                  (copy-tree cl-markdown-kit::*node-definitions*))
                (condition
                  (handler-case
                      (progn (eval conflict-registration) nil)
                    (simple-error (condition) condition))))
            (expect condition :to-be-truthy)
            (expect (search "Conflicting DEFINE-NODE declarations"
                            (format nil "~A" condition))
                    :to-be-truthy)
            (expect cl-markdown-kit::*node-definitions*
                    :to-equal before-conflict))))
      (expect cl-markdown-kit::*node-definitions*
              :to-equal definitions-before)))
  (it "normalizes typed and inherited child slots"
    (let* ((node-expansion
             (macroexpand-1
              '(cl-markdown-kit::define-node
                 (defstruct (coverage-contract-inherited-node
                             (:include cl-markdown-kit:list-item))
                   (local-value nil :type list))
                 :kind :coverage-contract-inherited
                 :family :block
                 :children (:slot cl-markdown-kit::children)
                 :attributes ()
                 :edit-text :container
                 :edit-attributes ())))
           (registration-call (third (third node-expansion)))
           (definition (second (second registration-call))))
      (expect (first node-expansion) :to-equal 'progn)
      (expect (getf definition :slots) :to-equal '(local-value))
      (expect (getf definition :parent) :to-equal 'cl-markdown-kit:list-item)
      (expect (getf definition :children)
              :to-equal
              '(:slot cl-markdown-kit::children
                :accessor coverage-contract-inherited-node-children))))
  (it "validates node helper inputs and dispatch clauses"
    (progn
      (expect (cl-markdown-kit::%node-form-slot-name 'value) :to-equal 'value)
      (let ((condition
              (handler-case
                  (progn (cl-markdown-kit::%node-defstruct-name 42) nil)
                (simple-error (condition) condition))))
        (expect condition :to-be-truthy)
        (expect (search "requires a DEFSTRUCT form" (format nil "~A" condition))
                :to-be-truthy)))
    (expect (cl-markdown-kit::%node-form-slot-name '(value 0 :type integer))
            :to-equal 'value)
    (expect (cl-markdown-kit::%node-inherited-slot-p
             'cl-markdown-kit:list-item 'cl-markdown-kit::span)
            :to-be-truthy)
    (expect (cl-markdown-kit::%node-inherited-slot-p
             'cl-markdown-kit:list-item 'cl-markdown-kit::unknown-slot)
            :to-equal nil)
    (let ((condition
            (handler-case
                (progn
                  (macroexpand-1
                   '(cl-markdown-kit::define-node
                      (not-a-defstruct)
                      :kind :coverage-contract-invalid
                      :family :inline
                      :children nil
                      :attributes ()
                      :edit-text :empty
                      :edit-attributes ()))
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "requires a DEFSTRUCT form" (format nil "~A" condition))
              :to-be-truthy))
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::%node-attribute-slot 'not-a-keyword)
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "must be keywords" (format nil "~A" condition))
              :to-be-truthy))
    (let ((condition
            (handler-case
                (progn
                  (cl-markdown-kit::%node-normalize-children
                   'coverage-contract-invalid-children
                   '(not-a-slot-form))
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "Invalid DEFINE-NODE :CHILDREN value"
                      (format nil "~A" condition))
              :to-be-truthy))
    (dolist (definition (list 42 '(42 (context cl-markdown-kit::node))))
      (let ((condition
              (handler-case
                  (progn
                    (macroexpand-1
                     (list 'cl-markdown-kit::define-node-dispatch
                           definition
                           :operation :coverage-contract
                           :family :inline
                           '(cl-markdown-kit:text nil)))
                    nil)
                (simple-error (condition) condition))))
        (expect condition :to-be-truthy)
        (expect
         (search "requires (function-name lambda-list)" (format nil "~A" condition))
         :to-be-truthy)))
    (let ((condition
            (handler-case
                (progn
                  (macroexpand-1
                   '(cl-markdown-kit::define-node-dispatch
                      (node-contract-non-list-clause
                       (context cl-markdown-kit::node phase))
                      :operation :coverage-contract
                      :family :inline
                      42))
                  nil)
              (simple-error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect
       (search "clauses must contain a type and body" (format nil "~A" condition))
       :to-be-truthy)))
  (it "expands dispatch declarations and validates registration state"
    (let* ((types (cl-markdown-kit::%node-family-types :inline))
           (clauses
             (mapcar (lambda (type) (list type nil)) types))
           (form
             (append
              (list 'cl-markdown-kit::define-node-dispatch
                    '(coverage-contract-dispatch
                      (context cl-markdown-kit::node phase))
                    :operation :coverage-contract
                    :family :inline)
              clauses))
           (expansion (macroexpand-1 form))
           (without-node-form
             (append
              (list 'cl-markdown-kit::define-node-dispatch
                    '(node-contract-without-node (context phase))
                    :operation :coverage-contract
                    :family :inline)
              clauses))
           (without-body-form
             (append
              (list 'cl-markdown-kit::define-node-dispatch
                    '(node-contract-without-body
                      (context cl-markdown-kit::node phase))
                    :operation :coverage-contract
                    :family :inline)
              (cons (list (first types)) (rest clauses))))
           (without-node-condition
             (handler-case
                 (progn (macroexpand-1 without-node-form) nil)
               (simple-error (condition) condition)))
           (without-body-condition
             (handler-case
                 (progn (macroexpand-1 without-body-form) nil)
               (simple-error (condition) condition)))
           (expected-record
             (list (list :coverage-contract :inline)
                   'coverage-contract-dispatch
                   types))
           (registrations-before
             (copy-tree
              cl-markdown-kit::*node-dispatch-registrations*)))
      (expect (first expansion) :to-equal 'progn)
      (expect (first (second expansion)) :to-equal 'defun)
      (expect without-node-condition :to-be-truthy)
      (expect
       (search "lambda list has no node argument"
               (format nil "~A" without-node-condition))
       :to-be-truthy)
      (expect without-body-condition :to-be-truthy)
      (expect
       (search "clauses must contain a type and body"
               (format nil "~A" without-body-condition))
       :to-be-truthy)
      (let ((cl-markdown-kit::*node-dispatch-registrations* nil))
        (let ((record
                (cl-markdown-kit::%register-node-dispatch
                 :coverage-contract :inline
                 'coverage-contract-dispatch types)))
          (expect record :to-equal expected-record)
          (expect cl-markdown-kit::*node-dispatch-registrations*
                  :to-equal (list expected-record))
          (cl-markdown-kit::%register-node-dispatch
           :coverage-contract :inline
           'coverage-contract-dispatch types)
          (expect cl-markdown-kit::*node-dispatch-registrations*
                  :to-equal (list expected-record))
          (progn
  (expect
   (cl-markdown-kit::verify-node-dispatches
    :operation :coverage-contract
    :families (quote (:inline)))
   :to-equal t)
  (expect
   (macroexpand-1
    (quote (cl-markdown-kit::verify-node-dispatches-at-compile-time
            :coverage-contract (:inline))))
   :to-equal nil))
          (let ((before-conflict
                  (copy-tree
                   cl-markdown-kit::*node-dispatch-registrations*))
                (condition
                  (handler-case
                      (progn
                        (cl-markdown-kit::%register-node-dispatch
                         :coverage-contract :inline
                         'coverage-contract-conflicting-dispatch types)
                        nil)
                    (simple-error (condition) condition))))
            (expect condition :to-be-truthy)
            (expect (search "Duplicate dispatch coverage"
                            (format nil "~A" condition))
                    :to-be-truthy)
            (expect cl-markdown-kit::*node-dispatch-registrations*
                    :to-equal before-conflict))))
      (expect cl-markdown-kit::*node-dispatch-registrations*
              :to-equal registrations-before))))
