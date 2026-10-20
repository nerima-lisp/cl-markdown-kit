;;;; src/nodes.lisp
(in-package #:cl-markdown-kit)

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

    (progn
      (defun %node-definition-check-metadata (name metadata)
        (let ((allowed '(:abstract :kind :family :children :attributes :edit-attributes :edit-text))
              (keys (loop for key in metadata by #'cddr collect key)))
          (unless (evenp (length metadata))
            (error "DEFINE-NODE metadata must be a property list, got ~S." metadata))
          (unless (= (length keys) (length (remove-duplicates keys :test #'eq)))
            (error "DEFINE-NODE metadata contains duplicate keys for ~S." name))
          (dolist (key keys)
            (unless (member key allowed)
              (error "Unknown DEFINE-NODE metadata key ~S." key)))
          (dolist (key '(:kind :family :children :attributes :edit-attributes :edit-text))
            (unless (member key metadata)
              (error "DEFINE-NODE for ~S is missing required ~S metadata." name key)))))
      (defun %node-definition-check-kind (name abstract-p kind family)
        (unless (or (and abstract-p (eq family :abstract))
                    (and (not abstract-p)
                         (keywordp kind)
                         (member family '(:block :inline))))
          (error "Invalid DEFINE-NODE kind/family ~S/~S for ~S." kind family name)))
      (defun %node-definition-check-attributes (name parent slots attributes)
        (unless (and (listp attributes)
                     (every #'keywordp attributes)
                     (= (length attributes)
                        (length (remove-duplicates attributes :test #'eq))))
          (error "DEFINE-NODE attributes must be distinct keyword names, got ~S."
                 attributes))
        (dolist (attribute attributes)
          (unless (or (member (%node-attribute-slot attribute) slots :test #'eq)
                      (%node-inherited-slot-p parent (%node-attribute-slot attribute)))
            (error "DEFINE-NODE attribute ~S has no slot on ~S." attribute name))))
      (defun %node-definition-check-edit-attributes (name attributes edit-attributes)
        (unless (and (listp edit-attributes)
                     (every (lambda (mapping)
                              (and (consp mapping)
                                   (keywordp (first mapping))
                                   (consp (rest mapping))
                                   (keywordp (second mapping))
                                   (null (cddr mapping))
                                   (member (first mapping) attributes :test #'eq)))
                            edit-attributes))
          (error "DEFINE-NODE edit attributes must map declared attribute slots to keyword difference keys, got ~S."
                 edit-attributes))
        (unless (= (length edit-attributes)
                   (length (remove-duplicates edit-attributes :key #'first :test #'eq)))
          (error "DEFINE-NODE edit attributes contain a duplicate slot for ~S." name))
        (unless (= (length edit-attributes)
                   (length (remove-duplicates edit-attributes :key #'second :test #'eq)))
          (error "DEFINE-NODE edit attributes contain a duplicate difference key for ~S." name)))
      (defun %node-definition-check-children (name parent slots children)
        (when children
          (unless (and (symbolp (getf children :accessor))
                       (or (member (getf children :slot) slots :test #'eq)
                           (%node-inherited-slot-p parent (getf children :slot))))
            (error "DEFINE-NODE child slot ~S has no accessor on ~S."
                   (getf children :slot) name))))
      (defun %node-definition-check-edit-text (name parent slots edit-text)
        (unless (or (member edit-text '(:empty :container))
                    (and (consp edit-text)
                         (eq (first edit-text) :slot)
                         (symbolp (second edit-text))
                         (or (member (second edit-text) slots :test #'eq)
                             (%node-inherited-slot-p parent (second edit-text)))
                         (= (length edit-text) 2)))
          (error "DEFINE-NODE for ~S has invalid or missing edit text projection ~S."
                 name edit-text)))
      (defun %node-definition (name form metadata)
        (%node-definition-check-metadata name metadata)
        (let* ((abstract-p (getf metadata :abstract))
               (kind (getf metadata :kind))
               (family (getf metadata :family))
               (children (%node-normalize-children name (getf metadata :children)))
               (attributes (getf metadata :attributes))
               (edit-attributes (getf metadata :edit-attributes))
               (edit-text (getf metadata :edit-text))
               (slots (%node-slot-names form))
               (parent (%node-defstruct-parent form)))
          (%node-definition-check-kind name abstract-p kind family)
          (%node-definition-check-attributes name parent slots attributes)
          (%node-definition-check-edit-attributes name attributes edit-attributes)
          (%node-definition-check-children name parent slots children)
          (%node-definition-check-edit-text name parent slots edit-text)
          (list :name name :abstract abstract-p :kind kind :family family
                :children children :attributes attributes
                :edit-attributes edit-attributes :edit-text edit-text
                :parent parent :slots slots))))

    (defun %register-node-definition (definition)
      (let ((name (getf definition :name)))
        (when (eq name 'node)
          (%reset-node-registries))
        (let ((previous (find name *node-definitions*
                              :key (lambda (item)
                                     (getf item :name)))))
          (cond
           ((null previous) (push definition *node-definitions*))
           ((equal previous definition) nil)
           (t (error "Conflicting DEFINE-NODE declarations for ~S." name)))
          definition)))

    (defun %node-definitions-for-family (family)
      (remove-if-not (lambda (definition)
                       (and (not (getf definition :abstract))
                            (eq (getf definition :family) family)))
                     *node-definitions*))

    (defun %node-definition-depth (definition)
      (loop with current = definition
            with depth = 0
            do (let* ((parent (getf current :parent))
                      (parent-definition
                        (find parent *node-definitions*
                              :key (lambda (item)
                                     (getf item :name)))))
                 (unless parent-definition
                   (return depth))
                 (incf depth)
                 (setf current parent-definition))))

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
         (definition (%node-definition name defstruct-form metadata)))
    `(progn
       ,defstruct-form
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%register-node-definition ',definition))
       ',name)))

    (progn
      (defun %node-registry-children-clauses (definitions)
        (mapcar (lambda (definition)
                  (let ((name (getf definition :name))
                        (children (getf definition :children)))
                    (if children
                        `(,name (,(getf children :accessor) node))
                        `(,name nil))))
                definitions))
      (defun %node-registry-kind-clauses (definitions)
        (mapcar (lambda (definition)
                  `(,(getf definition :name) ,(getf definition :kind)))
                definitions))
      (defun %node-registry-family-clauses (definitions)
        (mapcar (lambda (definition)
                  `(,(getf definition :name) ,(getf definition :family)))
                definitions))
      (defun %node-registry-attribute-clauses (definitions)
        (mapcar (lambda (definition)
                  (let ((name (getf definition :name)))
                    `(,name
                      (list
                       ,@(loop for attribute in (getf definition :attributes)
                               append (list `',attribute
                                            `(,(%node-accessor
                                                name
                                                (%node-attribute-slot attribute))
                                              node)))))))
                definitions))
      (defun %node-registry-text-clauses (definitions)
        (mapcar (lambda (definition)
                  (let* ((name (getf definition :name))
                         (projection (getf definition :edit-text)))
                    `(,name
                      ,(cond
                        ((eq projection :empty) "")
                        ((eq projection :container) nil)
                        (t `(,(%node-accessor name (second projection)) node))))))
                definitions))
      (defun %node-registry-expansion (definitions)
        (let ((children-clauses (%node-registry-children-clauses definitions))
              (kind-clauses (%node-registry-kind-clauses definitions))
              (family-clauses (%node-registry-family-clauses definitions))
              (attribute-clauses (%node-registry-attribute-clauses definitions))
              (text-clauses (%node-registry-text-clauses definitions)))
          `(progn
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
                 (node ""))))))
      (defmacro define-node-registry ()
        (%node-registry-expansion (%sorted-node-definitions))))
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
         (node-variable (find 'node lambda-list :test #'eq)))
    (unless (and node-variable (symbolp node-variable))
      (error "DEFINE-NODE-DISPATCH lambda list has no node argument."))
    (unless (every #'consp clauses)
      (error "DEFINE-NODE-DISPATCH clauses must contain a type and body."))
    (let* ((types (mapcar #'first clauses))
           (expected (%node-ensure-exact-coverage operation family types))
           (ordered-clauses
             (mapcar (lambda (type)
                       (cons type (cdr (assoc type clauses :test #'eq))))
                     expected)))
      (unless (every (lambda (clause) (cdr clause)) clauses)
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
           (%register-node-dispatch ',operation ',family
                                    ',function-name ',expected))
         ',function-name))))


    (progn
      (defun verify-node-dispatches (&key operation
                                          (families '(:block :inline)))
        (dolist (family families)
          (let* ((key (list operation family))
                 (record (assoc key *node-dispatch-registrations* :test #'equal)))
            (unless record
              (error "Missing dispatch coverage for ~S / ~S." operation family))
            (%node-ensure-exact-coverage operation family (third record))))
        t)
      (defmacro verify-node-dispatches-at-compile-time (operation families)
        (verify-node-dispatches :operation operation :families families)
        nil)
      (defun %reset-node-registries ()
        (setf *node-definitions* nil
              *node-dispatch-registrations* nil))))

  (define-node (defstruct (node (:constructor make-node)) span)
    :abstract t :kind nil :family :abstract :children nil :attributes ()
    :edit-text :empty :edit-attributes ())

(define-node (defstruct (block-node (:include node) (:constructor make-block-node)) (continuation-prefix "" :type string)) :abstract t :kind nil :family :abstract :children nil :attributes () :edit-text :empty :edit-attributes ())

(define-node (defstruct (inline-node (:include node) (:constructor make-inline-node))) :abstract t :kind nil :family :abstract :children nil :attributes () :edit-text :empty :edit-attributes ())

(define-node
  (defstruct (document (:include block-node) (:constructor make-document))
    (children nil :type list)
    (source nil :type (or null string))
    (source-format nil :type (or null source-format))
    (reference-index nil :type (or null hash-table))
    (front-matter-p t :type boolean)
    (gfm-p t :type boolean)
    (max-nesting-depth +default-max-nesting-depth+ :type (integer 1 *))
    (max-inline-delimiter-depth +default-max-inline-delimiter-depth+
                                :type (integer 1 *))
    (max-input-size +default-max-input-size+ :type (integer 1 *)))
  :kind :document
  :family :block
  :children children
  :attributes (:front-matter-p :gfm-p :max-input-size)
  :edit-text :container
  :edit-attributes ())

(define-node (defstruct (front-matter (:include block-node) (:constructor make-front-matter)) (raw-text "" :type string)
  (opening-delimiter "---" :type string)
  (closing-delimiter "---" :type string)) :kind :front-matter :family :block :children nil :attributes (:opening-delimiter :closing-delimiter) :edit-text (:slot raw-text) :edit-attributes ((:opening-delimiter :opening) (:closing-delimiter :closing)))

(define-node (defstruct (block-quote (:include block-node) (:constructor make-block-quote)) (children nil :type list)) :kind :block-quote :family :block :children children :attributes () :edit-text :container :edit-attributes ())

(define-node (defstruct (list-node (:include block-node) (:constructor make-list-node)) (kind :unordered :type keyword)
  (tight-p nil :type boolean)
  (start-number 1 :type (integer 0 *))
  (marker-character nil :type (or null character))
  (delimiter-character nil :type (or null character))
  (items nil :type list)) :kind :list :family :block :children items :attributes (:kind :tight-p :start-number :marker-character :delimiter-character) :edit-text :container :edit-attributes ((:kind :kind) (:start-number :start-number) (:delimiter-character :delimiter) (:tight-p :tight-p)))

(define-node (defstruct (list-item (:include block-node) (:constructor make-list-item)) (children nil :type list)) :kind :list-item :family :block :children children :attributes () :edit-text :container :edit-attributes ())

(define-node (defstruct (task-list-item (:include list-item) (:constructor make-task-list-item)) (checked-p nil :type boolean)
  task-marker-span
  (original-marker-character #\Space :type character)) :kind :task-list-item :family :block :children (:slot children :accessor list-item-children) :attributes (:checked-p :task-marker-span :original-marker-character) :edit-text :container :edit-attributes ((:checked-p :checked-p)))

(define-node (defstruct (paragraph (:include block-node) (:constructor make-paragraph)) (children nil :type list)) :kind :paragraph :family :block :children children :attributes () :edit-text :container :edit-attributes ())

(define-node (defstruct (heading (:include block-node) (:constructor make-heading)) (level 1 :type (integer 1 6))
  (style :atx :type keyword)
  (marker-character #\# :type (or null character))
  (opening-marker-length 1 :type (integer 0 *))
  (closing-marker-length 0 :type (integer 0 *))
  (children nil :type list)) :kind :heading :family :block :children children :attributes (:level :style :marker-character :opening-marker-length :closing-marker-length) :edit-text :container :edit-attributes ((:level :level)))

(define-node (defstruct (thematic-break (:include block-node) (:constructor make-thematic-break)) (marker-character #\- :type character)
  (marker-count 3 :type (integer 1 *))) :kind :thematic-break :family :block :children nil :attributes (:marker-character :marker-count) :edit-text :empty :edit-attributes ())

(define-node (defstruct (code-block (:include block-node) (:constructor make-code-block)) (style :fenced :type keyword)
  (body "" :type string)
  (fence-character nil :type (or null character))
  (fence-length 0 :type (integer 0 *))
  (indentation 0 :type (integer 0 *))
  (info-string nil :type (or null string))) :kind :code-block :family :block :children nil :attributes (:style :fence-character :fence-length :indentation :info-string) :edit-text (:slot body) :edit-attributes ((:info-string :info-string)))

(define-node (defstruct (html-block (:include block-node) (:constructor make-html-block)) (body "" :type string)
  (kind nil :type (or null integer keyword))) :kind :html-block :family :block :children nil :attributes (:kind) :edit-text (:slot body) :edit-attributes ((:kind :kind)))

(define-node (defstruct (link-reference-definition (:include block-node) (:constructor make-link-reference-definition)) (raw-label "" :type string)
  (normalized-label "" :type string)
  (destination "" :type string)
  (title nil :type (or null string))
  (eligible-p nil :type boolean)) :kind :link-reference-definition :family :block :children nil :attributes (:raw-label :normalized-label :destination :title :eligible-p) :edit-text :empty :edit-attributes ((:raw-label :label) (:normalized-label :normalized-label) (:destination :destination) (:title :title) (:eligible-p :eligible-p)))

(define-node (defstruct (table (:include block-node) (:constructor make-table)) (alignments nil :type list)
  (rows nil :type list)) :kind :table :family :block :children rows :attributes (:alignments) :edit-text :container :edit-attributes ((:alignments :alignments)))

(define-node (defstruct (table-row (:include block-node) (:constructor make-table-row)) (header-row-p nil :type boolean)
  (cells nil :type list)) :kind :table-row :family :block :children cells :attributes (:header-row-p) :edit-text :container :edit-attributes ((:header-row-p :header-row-p)))

(define-node (defstruct (table-cell (:include block-node) (:constructor make-table-cell)) alignment
  (children nil :type list)) :kind :table-cell :family :block :children children :attributes (:alignment) :edit-text :container :edit-attributes ((:alignment :alignment)))

(define-node (defstruct (footnote-definition (:include block-node) (:constructor make-footnote-definition)) (raw-label "" :type string)
  (normalized-label "" :type string)
  (children nil :type list)) :kind :footnote-definition :family :block :children children :attributes (:raw-label :normalized-label) :edit-text :container :edit-attributes ((:raw-label :label) (:normalized-label :normalized-label)))

(define-node (defstruct (text (:include inline-node) (:constructor make-text)) (value "" :type string)) :kind :text :family :inline :children nil :attributes (:value) :edit-text (:slot value) :edit-attributes ())

(define-node (defstruct (code-span (:include inline-node) (:constructor make-code-span)) (literal-text "" :type string)
  (backtick-run-length 1 :type (integer 1 *))) :kind :code-span :family :inline :children nil :attributes (:backtick-run-length) :edit-text (:slot literal-text) :edit-attributes ())

(define-node (defstruct (html-inline (:include inline-node) (:constructor make-html-inline)) (body "" :type string)) :kind :html-inline :family :inline :children nil :attributes () :edit-text (:slot body) :edit-attributes ())

(define-node (defstruct (soft-break (:include inline-node) (:constructor make-soft-break))) :kind :soft-break :family :inline :children nil :attributes () :edit-text :empty :edit-attributes ())

(define-node (defstruct (hard-break (:include inline-node) (:constructor make-hard-break))) :kind :hard-break :family :inline :children nil :attributes () :edit-text :empty :edit-attributes ())

(define-node (defstruct (emphasis (:include inline-node) (:constructor make-emphasis)) (delimiter-character nil :type (or null character))
  (children nil :type list)) :kind :emphasis :family :inline :children children :attributes (:delimiter-character) :edit-text :container :edit-attributes ())

(define-node (defstruct (strong (:include inline-node) (:constructor make-strong)) (delimiter-character nil :type (or null character))
  (children nil :type list)) :kind :strong :family :inline :children children :attributes (:delimiter-character) :edit-text :container :edit-attributes ())

(define-node (defstruct (link (:include inline-node) (:constructor make-link)) (destination "" :type string)
  (title nil :type (or null string))
  (reference-kind :inline :type keyword)
  (reference-label nil :type (or null string))
  (children nil :type list)) :kind :link :family :inline :children children :attributes (:destination :title :reference-kind :reference-label) :edit-text :container :edit-attributes ((:destination :destination) (:title :title) (:reference-kind :reference-kind) (:reference-label :reference-label)))

(define-node (defstruct (image (:include inline-node) (:constructor make-image)) (destination "" :type string)
  (title nil :type (or null string))
  (reference-kind :inline :type keyword)
  (reference-label nil :type (or null string))
  (children nil :type list)) :kind :image :family :inline :children children :attributes (:destination :title :reference-kind :reference-label) :edit-text :container :edit-attributes ((:destination :destination) (:title :title) (:reference-kind :reference-kind) (:reference-label :reference-label)))

(define-node (defstruct (strikethrough (:include inline-node) (:constructor make-strikethrough)) (delimiter-character #\~ :type (or null character))
  (children nil :type list)) :kind :strikethrough :family :inline :children children :attributes (:delimiter-character) :edit-text :container :edit-attributes ())

(define-node (defstruct (autolink (:include inline-node) (:constructor make-autolink)) (destination "" :type string)
  (display-text "" :type string)
  (kind :angle-uri :type keyword)) :kind :autolink :family :inline :children nil :attributes (:destination :kind) :edit-text (:slot display-text) :edit-attributes ((:destination :destination)))

(define-node (defstruct (footnote-reference (:include inline-node) (:constructor make-footnote-reference)) (raw-label "" :type string) (normalized-label "" :type string)) :kind :footnote-reference :family :inline :children nil :attributes (:raw-label :normalized-label) :edit-text :empty :edit-attributes ((:raw-label :label) (:normalized-label :normalized-label)))

(define-node-registry) (defun node-edit-attributes (node)
  (check-type node node)
  (let* ((definition
           (find (type-of node) *node-definitions*
                 :key (lambda (item) (getf item :name))))
         (attributes (node-semantic-attributes node)))
    (loop for (slot-key difference-key)
            in (getf definition :edit-attributes)
          append (list difference-key
                       (getf attributes slot-key)))))
