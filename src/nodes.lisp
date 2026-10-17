;;;; src/nodes.lisp
(in-package #:cl-markdown-kit)

(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *node-definitions* nil)
    (defvar *node-dispatch-registrations* nil)

    (defun %node-form-slot-name (slot-form)
      (if (consp slot-form) (car slot-form) slot-form))

    (defun %node-defstruct-name (form)
      (unless (and (consp form) (eq (car form) 'defstruct))
        (error "DEFINE-NODE requires a DEFSTRUCT form, got ~S." form))
      (let ((name-form (cadr form)))
        (if (consp name-form) (car name-form) name-form)))

    (defun %node-defstruct-options (form)
      (let ((name-form (cadr form)))
        (if (consp name-form) (cdr name-form) nil)))

    (defun %node-defstruct-slots (form)
      (cddr form))

    (defun %node-defstruct-parent (form)
      (let ((include-option (find :include (%node-defstruct-options form)
                                  :key #'car)))
        (when include-option (cadr include-option))))

    (defun %node-symbol (name suffix) (intern (string-upcase (format nil "~A-~A" (symbol-name name) suffix)) (symbol-package name)))

    (defun %node-constructor (name form) (let* ((options (%node-defstruct-options form)) (constructor-option (find :constructor options :key (lambda (option) (car option)))) ) (or (and constructor-option (cadr constructor-option)) (%node-symbol name "make"))))

    (defun %node-slot-names (form)
      (mapcar #'%node-form-slot-name (%node-defstruct-slots form)))

    (defun %node-accessor (name slot)
      (%node-symbol name slot))

    (defun %node-inherited-slot-p (parent slot)
  (loop for definition = (find parent *node-definitions*
                               :key (lambda (item) (getf item :name)))
        then (find (getf definition :parent) *node-definitions*
                   :key (lambda (item) (getf item :name)))
        while definition
        thereis (member slot (getf definition :slots) :test #'eq)))

    (defun %node-attribute-slot (attribute) (unless (keywordp attribute) (error "Node semantic attributes must be keywords, got ~S." attribute)) (intern (string-upcase (symbol-name attribute)) (find-package "CL-MARKDOWN-KIT")))

    (defun %node-normalize-children (name children)
      (cond
        ((null children) nil)
        ((symbolp children)
         (list :slot children :accessor (%node-accessor name children)))
        ((and (consp children)
              (eq (first children) :slot)
              (symbolp (second children)))
         (let ((accessor-position (position :accessor children)))
           (list :slot (second children)
                 :accessor (if accessor-position
                               (nth (1+ accessor-position) children)
                               (%node-accessor name (second children))))))
        (t (error "Invalid DEFINE-NODE :CHILDREN value ~S." children))))

    (defun %node-definition (name form metadata)
  (let ((allowed '(:abstract :kind :family :children :attributes :edit-text))
        (keys (loop for key in metadata by #'cddr collect key)))
    (unless (and (listp metadata) (evenp (length metadata)))
      (error "DEFINE-NODE metadata must be a property list, got ~S." metadata))
    (unless (= (length keys) (length (remove-duplicates keys :test #'eq)))
      (error "DEFINE-NODE metadata contains duplicate keys for ~S." name))
    (dolist (key keys)
      (unless (member key allowed)
        (error "Unknown DEFINE-NODE metadata key ~S." key)))
    (dolist (key '(:kind :family :children :attributes :edit-text))
      (unless (member key metadata)
        (error "DEFINE-NODE for ~S is missing required ~S metadata." name key)))
    (let* ((abstract-p (getf metadata :abstract))
           (kind (getf metadata :kind))
           (family (getf metadata :family))
           (children (%node-normalize-children name (getf metadata :children)))
           (attributes (getf metadata :attributes))
           (edit-text (getf metadata :edit-text))
           (slots (%node-slot-names form))
           (parent (%node-defstruct-parent form)))
      (unless (or (and abstract-p (eq family :abstract))
                  (and (not abstract-p)
                       (keywordp kind)
                       (member family '(:block :inline))))
        (error "Invalid DEFINE-NODE kind/family ~S/~S for ~S." kind family name))
      (unless (and (listp attributes)
                   (every #'keywordp attributes)
                   (= (length attributes)
                      (length (remove-duplicates attributes :test #'eq))))
        (error "DEFINE-NODE attributes must be distinct keyword names, got ~S."
               attributes))
      (dolist (attribute attributes)
        (unless (or (member (%node-attribute-slot attribute) slots :test #'eq)
                    (%node-inherited-slot-p parent (%node-attribute-slot attribute)))
          (error "DEFINE-NODE attribute ~S has no slot on ~S." attribute name)))
      (when children
        (unless (and (symbolp (getf children :accessor))
                     (or (member (getf children :slot) slots :test #'eq)
                         (%node-inherited-slot-p parent (getf children :slot))))
          (error "DEFINE-NODE child slot ~S has no accessor on ~S."
                 (getf children :slot) name)))
      (unless (or (member edit-text '(:empty :container))
                  (and (consp edit-text)
                       (eq (first edit-text) :slot)
                       (symbolp (second edit-text))
                       (or (member (second edit-text) slots :test #'eq)
                           (%node-inherited-slot-p parent (second edit-text)))
                       (= (length edit-text) 2))
                  (and (consp edit-text)
                       (eq (first edit-text) :literal)
                       (= (length edit-text) 2)))
        (error "DEFINE-NODE for ~S has invalid or missing edit text projection ~S."
               name edit-text))
      (list :name name :abstract abstract-p :kind kind :family family
            :children children :attributes attributes :edit-text edit-text
            :parent parent :slots slots))))

    (defun %register-node-definition (definition)
      (let* ((name (getf definition :name))
             (previous (find name *node-definitions* :key (lambda (item)
                                                           (getf item :name)))))
        (cond
          ((null previous) (push definition *node-definitions*))
          ((equal previous definition) nil)
          (t (error "Conflicting DEFINE-NODE declarations for ~S." name)))
      definition))

    (defun %node-definitions-for-family (family)
      (remove-if-not (lambda (definition)
                       (and (not (getf definition :abstract))
                            (eq (getf definition :family) family)))
                     *node-definitions*))

    (defun %node-definition-depth (definition) (loop with current = definition with depth = 0 do (let* ((parent (getf current :parent)) (parent-definition (find parent *node-definitions* :key (lambda (item) (getf item :name))))) (unless parent-definition (return depth)) (incf depth) (setf current parent-definition))))

    (defun %sorted-node-definitions (&optional family)
      (stable-sort (if family (%node-definitions-for-family family)
                        (remove-if (lambda (definition)
                                     (getf definition :abstract))
                                   *node-definitions*))
                   #'> :key #'%node-definition-depth))

    (defun %node-family-types (family)
      (unless (member family '(:block :inline))
        (error "Unknown node family ~S." family))
      (mapcar (lambda (definition) (getf definition :name))
              (%sorted-node-definitions family)))

    (defun %node-ensure-exact-coverage (operation family types)
      (let ((expected (%node-family-types family)))
        (unless (= (length types) (length (remove-duplicates types :test #'eq)))
          (error "Duplicate node type clause for ~S / ~S." operation family))
        (dolist (type types)
          (unless (member type expected :test #'eq)
            (error "Extra node type clause ~S for ~S / ~S." type operation family)))
        (dolist (type expected)
          (unless (member type types :test #'eq)
            (error "Missing node type clause ~S for ~S / ~S." type operation family)))
        expected))

    (defun %register-node-dispatch (operation family function-name types)
      (let* ((expected (%node-ensure-exact-coverage operation family types))
             (key (list operation family))
             (previous (assoc key *node-dispatch-registrations* :test #'equal))
             (record (list key function-name expected)))
        (cond
          ((null previous) (push record *node-dispatch-registrations*))
          ((and (eq (second previous) function-name)
                (equal (third previous) expected)) nil)
          (t (error "Duplicate dispatch coverage for ~S / ~S." operation family)))
        record))

    (defmacro define-node (defstruct-form &rest metadata)
  (let* ((name (%node-defstruct-name defstruct-form))
         (definition (%node-definition name defstruct-form metadata))
         (constructor (%node-constructor name defstruct-form))
         (accessors (mapcar (lambda (slot)
                             (%node-accessor name (%node-form-slot-name slot)))
                           (%node-defstruct-slots defstruct-form)))
         (exports (remove-duplicates
                   (append (list name constructor)
                           (when (eq name 'front-matter)
                             (list (%node-symbol name "p")))
                           accessors)
                   :test #'eq)))
    `(progn
       ,defstruct-form
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%register-node-definition ',definition)
         (export ',exports))
       ',name)))

    (defmacro define-node-registry ()
      (let ((definitions (%sorted-node-definitions)))
        (let ((children-clauses
                (mapcar (lambda (definition)
                          (let ((name (getf definition :name))
                                (children (getf definition :children)))
                            (if children
                                `(,name (,(getf children :accessor) node))
                                `(,name nil))))
                        definitions))
              (kind-clauses
                (mapcar (lambda (definition)
                          `(,(getf definition :name) ,(getf definition :kind)))
                        definitions))
              (family-clauses
                (mapcar (lambda (definition)
                          `(,(getf definition :name) ,(getf definition :family)))
                        definitions))
              (attribute-clauses
                (mapcar (lambda (definition)
                          (let ((name (getf definition :name)))
                            `(,name
                              (list
                               ,@(loop for attribute in (getf definition :attributes)
                                       append (list `',attribute
                                                    `(,(%node-accessor
                                                        name
                                                        (%node-attribute-slot
                                                         attribute))
                                                      node)))))))
                        definitions))
              (text-clauses
                (mapcar (lambda (definition)
                          (let* ((name (getf definition :name))
                                 (projection (getf definition :edit-text)))
                            `(,name
                              ,(cond
                                 ((eq projection :empty) "")
                                 ((eq projection :container) nil)
                                 ((eq (first projection) :slot)
                                  `(,(%node-accessor name (second projection)) node))
                                 ((eq (first projection) :literal)
                                  (let ((value (second projection)))
                                    (if (characterp value) `(string ,value) value)))))))
                        definitions))) `(progn
             (defun node-children (node)
               (check-type node node)
               (typecase node
                 ,@children-clauses
                 (node nil)))
             (defun node-kind (node)
               (check-type node node)
               (typecase node
                 ,@kind-clauses
                 (node (error "Abstract node ~S has no concrete kind." node))))
             (defun node-family (node)
               (check-type node node)
               (typecase node
                 ,@family-clauses
                 (node :abstract)))
             (defun node-semantic-attributes (node)
               (check-type node node)
               (typecase node
                 ,@attribute-clauses
                 (node nil)))
             (defun node-edit-text-projection (node)
               (check-type node node)
               (typecase node
                 ,@text-clauses
                 (node "")))))))
(defmacro define-node-dispatch (definition &rest options-and-clauses)
  (unless (and (consp definition) (symbolp (first definition))
               (consp (second definition)))
    (error "DEFINE-NODE-DISPATCH requires (function-name lambda-list)."))
  (unless (and (>= (length options-and-clauses) 5)
               (eq (first options-and-clauses) :operation)
               (eq (third options-and-clauses) :family))
    (error "DEFINE-NODE-DISPATCH requires :OPERATION and :FAMILY before clauses."))
  (let* ((function-name (first definition))
         (lambda-list (second definition))
         (operation (second options-and-clauses))
         (family (fourth options-and-clauses))
         (clauses (nthcdr 4 options-and-clauses))
         (node-variable (find 'node lambda-list :test #'eq))
         (types (mapcar #'first clauses))
         (expected (%node-ensure-exact-coverage operation family types))
         (ordered-clauses
          (mapcar (lambda (type)
                    (cons type (cdr (assoc type clauses :test #'eq))))
                  expected)))
    (unless (and node-variable (symbolp node-variable))
      (error "DEFINE-NODE-DISPATCH lambda list has no node argument."))
    (unless (every (lambda (clause) (and (consp clause) (cdr clause))) clauses)
      (error "DEFINE-NODE-DISPATCH clauses must contain a type and body."))
    `(progn
       (defun ,function-name ,lambda-list
         (check-type ,node-variable node)
         (typecase ,node-variable
           ,@ordered-clauses
           (node
            (error "No ~S dispatch clause for node type ~S."
                   ',operation (type-of ,node-variable)))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%register-node-dispatch ',operation ',family ',function-name ',expected))
       ',function-name)))

    (defun verify-node-dispatches (&key operation (families '(:block :inline)))
      (dolist (family families)
        (let* ((key (list operation family))
               (record (assoc key *node-dispatch-registrations* :test #'equal)))
          (unless record
            (error "Missing dispatch coverage for ~S / ~S." operation family))
          (%node-ensure-exact-coverage operation family (third record))))
      t))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *node-definitions* nil
          *node-dispatch-registrations* nil))

  (define-node (defstruct (node (:constructor make-node (&key span))) span)
    :abstract t :kind nil :family :abstract :children nil :attributes ()
    :edit-text :empty))

(define-node (defstruct (block-node (:include node) (:constructor make-block-node (&key span continuation-prefix))) (continuation-prefix "" :type string)) :abstract t :kind nil :family :abstract :children nil :attributes () :edit-text :empty)

(define-node (defstruct (inline-node (:include node) (:constructor make-inline-node (&key span)))) :abstract t :kind nil :family :abstract :children nil :attributes () :edit-text :empty)

(define-node (defstruct (document
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
  (max-inline-delimiter-depth 4096 :type (integer 1 *))) :kind :document :family :block :children children :attributes (:front-matter-p :gfm-p) :edit-text :container)

(define-node (defstruct (front-matter
             (:include block-node)
             (:constructor make-front-matter
                 (&key span continuation-prefix raw-text opening-delimiter
                       closing-delimiter)))
  (raw-text "" :type string)
  (opening-delimiter "---" :type string)
  (closing-delimiter "---" :type string)) :kind :front-matter :family :block :children nil :attributes (:opening-delimiter :closing-delimiter) :edit-text (:slot raw-text))

(define-node (defstruct (block-quote
             (:include block-node)
             (:constructor make-block-quote (&key span continuation-prefix children)))
  (children nil :type list)) :kind :block-quote :family :block :children children :attributes () :edit-text :container)

(define-node (defstruct (list-node
             (:include block-node)
             (:constructor make-list-node
                 (&key span continuation-prefix kind tight-p start-number
                       marker-character delimiter-character items)))
  (kind :unordered :type keyword)
  (tight-p nil :type boolean)
  (start-number 1 :type (integer 0 *))
  (marker-character nil :type (or null character))
  (delimiter-character nil :type (or null character))
  (items nil :type list)) :kind :list :family :block :children items :attributes (:kind :tight-p :start-number :marker-character :delimiter-character) :edit-text :container)

(define-node (defstruct (list-item
             (:include block-node)
             (:constructor make-list-item (&key span continuation-prefix children)))
  (children nil :type list)) :kind :list-item :family :block :children children :attributes () :edit-text :container)

(define-node (defstruct (task-list-item
             (:include list-item)
             (:constructor make-task-list-item
                 (&key span continuation-prefix children checked-p
                       task-marker-span original-marker-character)))
  (checked-p nil :type boolean)
  task-marker-span
  (original-marker-character #\Space :type character)) :kind :task-list-item :family :block :children (:slot children :accessor list-item-children) :attributes (:checked-p :task-marker-span :original-marker-character) :edit-text :container)

(define-node (defstruct (paragraph
             (:include block-node)
             (:constructor make-paragraph (&key span continuation-prefix children)))
  (children nil :type list)) :kind :paragraph :family :block :children children :attributes () :edit-text :container)

(define-node (defstruct (heading
             (:include block-node)
             (:constructor make-heading
                 (&key span continuation-prefix level style marker-character
                       opening-marker-length closing-marker-length children)))
  (level 1 :type (integer 1 6))
  (style :atx :type keyword)
  (marker-character #\# :type (or null character))
  (opening-marker-length 1 :type (integer 0 *))
  (closing-marker-length 0 :type (integer 0 *))
  (children nil :type list)) :kind :heading :family :block :children children :attributes (:level :style :marker-character :opening-marker-length :closing-marker-length) :edit-text :container)

(define-node (defstruct (thematic-break
             (:include block-node)
             (:constructor make-thematic-break
                 (&key span continuation-prefix marker-character marker-count)))
  (marker-character #\- :type character)
  (marker-count 3 :type (integer 1 *))) :kind :thematic-break :family :block :children nil :attributes (:marker-character :marker-count) :edit-text :empty)

(define-node (defstruct (code-block
             (:include block-node)
             (:constructor make-code-block
                 (&key span continuation-prefix style body fence-character
                       fence-length indentation info-string)))
  (style :fenced :type keyword)
  (body "" :type string)
  (fence-character nil :type (or null character))
  (fence-length 0 :type (integer 0 *))
  (indentation 0 :type (integer 0 *))
  (info-string nil :type (or null string))) :kind :code-block :family :block :children nil :attributes (:style :fence-character :fence-length :indentation :info-string) :edit-text (:slot body))

(define-node (defstruct (html-block
             (:include block-node)
             (:constructor make-html-block
                 (&key span continuation-prefix body kind)))
  (body "" :type string)
  (kind nil :type (or null integer keyword))) :kind :html-block :family :block :children nil :attributes (:kind) :edit-text (:slot body))

(define-node (defstruct (link-reference-definition
             (:include block-node)
             (:constructor make-link-reference-definition
                 (&key span continuation-prefix raw-label normalized-label
                       destination title eligible-p)))
  (raw-label "" :type string)
  (normalized-label "" :type string)
  (destination "" :type string)
  (title nil :type (or null string))
  (eligible-p nil :type boolean)) :kind :link-reference-definition :family :block :children nil :attributes (:raw-label :normalized-label :destination :title :eligible-p) :edit-text :empty)

(define-node (defstruct (table
             (:include block-node)
             (:constructor make-table (&key span continuation-prefix alignments rows)))
  (alignments nil :type list)
  (rows nil :type list)) :kind :table :family :block :children rows :attributes (:alignments) :edit-text :container)

(define-node (defstruct (table-row
             (:include block-node)
             (:constructor make-table-row
                 (&key span continuation-prefix header-row-p cells)))
  (header-row-p nil :type boolean)
  (cells nil :type list)) :kind :table-row :family :block :children cells :attributes (:header-row-p) :edit-text :container)

(define-node (defstruct (table-cell
             (:include block-node)
             (:constructor make-table-cell
                 (&key span continuation-prefix alignment children)))
  alignment
  (children nil :type list)) :kind :table-cell :family :block :children children :attributes (:alignment) :edit-text :container)

(define-node (defstruct (footnote-definition
             (:include block-node)
             (:constructor make-footnote-definition
                 (&key span continuation-prefix raw-label normalized-label children)))
  (raw-label "" :type string)
  (normalized-label "" :type string)
  (children nil :type list)) :kind :footnote-definition :family :block :children children :attributes (:raw-label :normalized-label) :edit-text :container)

(define-node (defstruct (text
             (:include inline-node)
             (:constructor make-text (&key span value)))
  (value "" :type string)) :kind :text :family :inline :children nil :attributes (:value) :edit-text (:slot value))

(define-node (defstruct (code-span
             (:include inline-node)
             (:constructor make-code-span (&key span literal-text backtick-run-length)))
  (literal-text "" :type string)
  (backtick-run-length 1 :type (integer 1 *))) :kind :code-span :family :inline :children nil :attributes (:backtick-run-length) :edit-text (:slot literal-text))

(define-node (defstruct (html-inline
             (:include inline-node)
             (:constructor make-html-inline (&key span body)))
  (body "" :type string)) :kind :html-inline :family :inline :children nil :attributes () :edit-text (:slot body))

(define-node (defstruct (soft-break (:include inline-node) (:constructor make-soft-break (&key span)))) :kind :soft-break :family :inline :children nil :attributes () :edit-text :empty)

(define-node (defstruct (hard-break (:include inline-node) (:constructor make-hard-break (&key span)))) :kind :hard-break :family :inline :children nil :attributes () :edit-text :empty)

(define-node (defstruct (emphasis
             (:include inline-node)
             (:constructor make-emphasis (&key span delimiter-character children)))
  (delimiter-character nil :type (or null character))
  (children nil :type list)) :kind :emphasis :family :inline :children children :attributes (:delimiter-character) :edit-text :container)

(define-node (defstruct (strong
             (:include inline-node)
             (:constructor make-strong (&key span delimiter-character children)))
  (delimiter-character nil :type (or null character))
  (children nil :type list)) :kind :strong :family :inline :children children :attributes (:delimiter-character) :edit-text :container)

(define-node (defstruct (link
             (:include inline-node)
             (:constructor make-link
                 (&key span destination title reference-kind reference-label children)))
  (destination "" :type string)
  (title nil :type (or null string))
  (reference-kind :inline :type keyword)
  (reference-label nil :type (or null string))
  (children nil :type list)) :kind :link :family :inline :children children :attributes (:destination :title :reference-kind :reference-label) :edit-text :container)

(define-node (defstruct (image
             (:include inline-node)
             (:constructor make-image
                 (&key span destination title reference-kind reference-label children)))
  (destination "" :type string)
  (title nil :type (or null string))
  (reference-kind :inline :type keyword)
  (reference-label nil :type (or null string))
  (children nil :type list)) :kind :image :family :inline :children children :attributes (:destination :title :reference-kind :reference-label) :edit-text :container)

(define-node (defstruct (strikethrough
             (:include inline-node)
             (:constructor make-strikethrough
                 (&key span delimiter-character children)))
  (delimiter-character #\~ :type (or null character))
  (children nil :type list)) :kind :strikethrough :family :inline :children children :attributes (:delimiter-character) :edit-text :container)

(define-node (defstruct (autolink
             (:include inline-node)
             (:constructor make-autolink (&key span destination display-text kind)))
  (destination "" :type string)
  (display-text "" :type string)
  (kind :angle-uri :type keyword)) :kind :autolink :family :inline :children nil :attributes (:destination :kind) :edit-text (:slot display-text))

(define-node (defstruct (footnote-reference (:include inline-node) (:constructor make-footnote-reference (&key span raw-label normalized-label))) (raw-label "" :type string) (normalized-label "" :type string)) :kind :footnote-reference :family :inline :children nil :attributes (:raw-label :normalized-label) :edit-text :empty)

(define-node-registry)
