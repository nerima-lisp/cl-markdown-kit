;;;; src/query.lisp
(in-package #:cl-markdown-kit)

(defstruct (heading-record
             (:constructor make-heading-record
                 (&key node level plain-text source-form section-span)))
  (node nil :type heading)
  (level 1 :type (integer 1 6))
  (plain-text "" :type string)
  (source-form :atx :type keyword)
  section-span)

(defstruct (section-record
             (:constructor make-section-record (&key heading span address)))
  (heading nil :type (or null heading-record))
  (span nil :type (or null cl-parser-kit:span))
  address)

(define-node-dispatch (%write-inline-plain-text-node (node stream))
      :operation :query
      :family :inline
    (text
     (write-string (text-value node) stream))
    (code-span
     (write-string (code-span-literal-text node) stream))
    (html-inline nil)
    (soft-break
     (write-char #\Space stream))
    (hard-break
     (write-char #\Space stream))
    (emphasis nil)
    (strong nil)
    (link nil)
    (image nil)
    (strikethrough nil)
    (autolink
     (write-string (autolink-display-text node) stream))
    (footnote-reference
     (write-string (footnote-reference-raw-label node) stream)))
  (progn
    (defun %query-verify-node-dispatches ()
      (verify-node-dispatches
       :operation :query
       :families '(:inline)))
    (verify-node-dispatches-at-compile-time :query (:inline)))
  (defun %write-inline-plain-text (node stream)
    (walk-node-tree node
     (lambda (frame)
       (let ((node (node-tree-frame-node frame))
             (phase (node-tree-frame-phase frame)))
         (when (and (eq phase :enter) (inline-node-p node))
           (%write-inline-plain-text-node node stream))
         :descend))))

(defun %collapse-heading-whitespace (string)
  (let ((pending-space nil)
        (wrote-character nil))
    (with-output-to-string (output)
      (loop for char across string
            do (if (unicode-whitespace-p char)
                   (when wrote-character
                     (setf pending-space t))
                   (progn
                     (when pending-space
                       (write-char #\Space output))
                     (write-char char output)
                     (setf pending-space nil
                           wrote-character t)))))))

(defun %heading-plain-text (heading)
  (%collapse-heading-whitespace
   (with-output-to-string (output)
     (dolist (child (heading-children heading))
       (%write-inline-plain-text child output)))))

(defun %span-starts-before-or-at-level-p (record level)
  (<= (heading-record-level record) level))

(defun %source-end-location (source)
  (let ((line 1)
        (column 1)
        (index 0)
        (length (length source)))
    (loop while (< index length)
          for char = (char source index)
          do (cond
               ((char= char #\Return)
                (when (and (< (1+ index) length)
                           (char= (char source (1+ index)) #\Newline))
                  (incf index))
                (incf line)
                (setf column 1))
               ((char= char #\Newline)
                (incf line)
                (setf column 1))
               (t (incf column)))
             (incf index))
    (values line column)))

(defun %section-endpoint (record later-records document-span source)
  (let* ((level (heading-record-level record))
         (next (find-if (lambda (candidate)
                          (%span-starts-before-or-at-level-p candidate level))
                        later-records)))
    (cond
      (next
       (let ((span (node-span (heading-record-node next))))
         (values (cl-parser-kit:span-start span)
                 (cl-parser-kit:span-start-line span)
                 (cl-parser-kit:span-start-column span))))
      (document-span
       (values (cl-parser-kit:span-end document-span)
               (cl-parser-kit:span-end-line document-span)
               (cl-parser-kit:span-end-column document-span)))
      (t
       (multiple-value-bind (end-line end-column)
           (%source-end-location source)
         (values (length source) end-line end-column))))))

(defun %make-section-span (record later-records document-span source)
  (let* ((heading-span (node-span (heading-record-node record)))
         (start (cl-parser-kit:span-start heading-span))
         (start-line (cl-parser-kit:span-start-line heading-span))
         (start-column (cl-parser-kit:span-start-column heading-span)))
    (multiple-value-bind (end end-line end-column)
        (%section-endpoint record later-records document-span source)
      (cl-parser-kit:make-span
       :source source
       :start start :end end
       :start-line start-line :start-column start-column
       :end-line end-line :end-column end-column))))

(defun document-headings (document)
  (check-type document document)
  (let ((records nil)
        (direct-headings (make-hash-table :test #'eq))
        (source (or (document-source document)
                    (let ((span (node-span document)))
                      (and span (cl-parser-kit:span-source span)))
                    ""))
        (document-span (node-span document)))
    (dolist (child (document-children document))
      (when (heading-p child)
        (setf (gethash child direct-headings) t)))
    (walk-node-tree document
     (lambda (frame)
       (let ((node (node-tree-frame-node frame))
             (phase (node-tree-frame-phase frame)))
         (when (and (eq phase :enter) (heading-p node))
           (push (make-heading-record
                  :node node
                  :level (heading-level node)
                  :plain-text (%heading-plain-text node)
                  :source-form (heading-style node))
                 records))
         :descend)))
    (setf records (nreverse records))
    (let ((section-headings
            (remove-if-not (lambda (record)
                             (gethash (heading-record-node record)
                                      direct-headings))
                           records)))
      (loop for tail on section-headings
            for record = (first tail)
            do (setf (heading-record-section-span record)
                     (%make-section-span record (rest tail) document-span source))))
    records))

(defun %invalid-section-address (address reason)
  (error 'markdown-invalid-section-address-error
         :address address :reason reason))

(defun %decode-pointer-component (component address)
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length component))
          for char = (char component index)
          do (if (char= char #\~)
                 (progn
                   (when (= (1+ index) (length component))
                     (%invalid-section-address address
                                               "A trailing tilde is not a valid escape."))
                   (case (char component (1+ index))
                     (#\0 (write-char #\~ output))
                     (#\1 (write-char #\/ output))
                     (otherwise
                      (%invalid-section-address
                       address "Only ~0 and ~1 are valid pointer escapes.")))
                   (incf index 2))
                 (progn
                   (write-char char output)
                   (incf index))))))

(defun %pointer-components (address)
  (unless (char= (char address 0) #\/)
    (%invalid-section-address address
                              "A nonempty section address must begin with /."))
  (let ((components nil)
        (start 1))
    (loop for slash = (position #\/ address :start start)
          do (push (%decode-pointer-component
                    (subseq address start slash) address)
                   components)
             (if slash
                 (setf start (1+ slash))
                 (return)))
    (nreverse components)))

(defun %section-heading-tree (records)
  (let ((roots nil)
        (children (make-hash-table :test #'eq))
        (parents (make-hash-table :test #'eq))
        (stack nil))
    (dolist (record records)
      (loop while (and stack
                       (>= (heading-record-level (first stack))
                           (heading-record-level record)))
            do (pop stack))
      (let ((parent (first stack)))
        (if parent
            (progn
              (push record (gethash parent children))
              (setf (gethash record parents) parent))
            (push record roots)))
      (push record stack))
    (maphash (lambda (parent records)
               (setf (gethash parent children) (nreverse records)))
             children)
    (values (nreverse roots) children parents)))

(defun %matching-children (records component)
  (remove-if-not (lambda (record)
                   (string= component (heading-record-plain-text record)))
                 records))

(defun %matching-path (roots children components)
  (let ((matches (%matching-children roots (first components))))
    (dolist (component (rest components))
      (setf matches
            (loop for parent in matches
                  append (%matching-children (gethash parent children) component))))
    matches))

(defun %encode-pointer-component (component)
  (with-output-to-string (output)
    (loop for char across component
          do (case char
               (#\~ (write-string "~0" output))
               (#\/ (write-string "~1" output))
               (otherwise (write-char char output))))))

(defun %record-address (record parents)
  (let ((components nil)
        (cursor record))
    (loop while cursor
          do (push (%encode-pointer-component
                    (heading-record-plain-text cursor))
                   components)
             (setf cursor (gethash cursor parents)))
    (with-output-to-string (output)
      (dolist (component components)
        (write-char #\/ output)
        (write-string component output)))))

(defun %whole-document-section (document address)
  (make-section-record :heading nil :span (node-span document) :address address))

(defun %heading-line (record)
  (let ((span (node-span (heading-record-node record))))
    (and span (cl-parser-kit:span-start-line span))))

(defun %candidate-description (record)
  (list :heading record :line (%heading-line record)))

(defun %resolve-section-candidates (address candidates parents)
  (cond
    ((null candidates)
     (error 'markdown-no-match-error :address address))
    ((cdr candidates)
     (error 'markdown-ambiguous-match-error
            :address address
            :candidates (mapcar #'%candidate-description candidates)))
    (t
     (let ((record (first candidates)))
       (make-section-record
        :heading record
        :span (heading-record-section-span record)
        :address (%record-address record parents))))))

(defun find-section (document address)
  (check-type document document)
  (let ((section-records
          (remove-if-not (function heading-record-section-span)
                         (document-headings document))))
    (multiple-value-bind (roots children parents)
        (%section-heading-tree section-records)
      (cond
        ((stringp address)
         (if (zerop (length address))
             (%whole-document-section document address)
             (%resolve-section-candidates
              address
              (%matching-path roots children (%pointer-components address))
              parents)))
        ((and (integerp address) (plusp address))
         (%resolve-section-candidates
          address
          (remove-if-not (lambda (record)
                           (eql (%heading-line record) address))
                         section-records)
          parents))
        ((integerp address)
         (%invalid-section-address address
                                   "Heading line numbers must be positive."))
        (t
         (%invalid-section-address
          address
          "A section address must be a JSON pointer or heading line number."))))))

(defun %document-source-string (document)
  (or (document-source document)
      (let ((span (node-span document)))
        (and span (cl-parser-kit:span-source span)))
      ""))

(defun %section-scope (document section)
  (cond
    ((null section) (node-span document))
    ((section-record-p section)
     (let* ((span (section-record-span section))
            (source (and span (cl-parser-kit:span-source span)))
            (document-span (node-span document)))
       (unless (and span document-span
                    (string= source (%document-source-string document))
                    (<= (cl-parser-kit:span-start document-span)
                        (cl-parser-kit:span-start span)
                        (cl-parser-kit:span-end span)
                        (cl-parser-kit:span-end document-span)))
         (%invalid-section-address
          (section-record-address section)
          "The section record does not belong to this document."))
       span))
    ((or (stringp section) (integerp section))
     (section-record-span (find-section document section)))
    (t
     (%invalid-section-address section
                               "A section must be an address or section record."))))

(defun %span-in-scope-p (span scope)
  (and span scope
       (string= (cl-parser-kit:span-source span)
                (cl-parser-kit:span-source scope))
       (<= (cl-parser-kit:span-start scope)
           (cl-parser-kit:span-start span)
           (cl-parser-kit:span-end span)
           (cl-parser-kit:span-end scope))))

(defun %span-source-slice (span)
  (let ((source (cl-parser-kit:span-source span))
        (start (cl-parser-kit:span-start span))
        (end (cl-parser-kit:span-end span)))
    (unless (and (stringp source) (<= 0 start end (length source)))
      (error "Invalid source span [~S,~S) for query result." start end))
    (subseq source start end)))

(defun %plain-node-text (node)
  (%collapse-heading-whitespace
   (with-output-to-string (output)
     (%write-inline-plain-text node output))))

(defun %first-info-token (info-string)
  (when info-string
    (let ((start (position-if-not #'unicode-whitespace-p info-string)))
      (when start
        (let ((end (position-if #'unicode-whitespace-p info-string :start start)))
          (subseq info-string start end))))))

(defun %table-values (table)
  (let* ((rows (table-rows table))
         (header (find-if #'table-row-header-row-p rows)))
    (list :header (when header
                    (mapcar (lambda (cell) (%plain-node-text cell))
                            (table-row-cells header)))
          :rows (loop for row in rows
                      unless (table-row-header-row-p row)
                        collect (mapcar (lambda (cell) (%plain-node-text cell))
                                        (table-row-cells row)))
          :alignments (table-alignments table))))

(defun %extract-result (kind node heading-record include-source-slice)
  (let* ((span (node-span node))
         (values
           (case kind
             (:code-block
              (list :node node :span span
                    :body (code-block-body node)
                    :info-string (code-block-info-string node)))
             (:link
              (if (autolink-p node)
                  (list :node node :span span
                        :destination (autolink-destination node)
                        :title nil
                        :display-text (autolink-display-text node))
                  (list :node node :span span
                        :destination (link-destination node)
                        :title (link-title node)
                        :display-text (%plain-node-text node))))
             (:image
              (list :node node :span span
                    :destination (image-destination node)
                    :title (image-title node)
                    :alt-text (%plain-node-text node)))
             (:table
              (append (list :node node :span span) (%table-values node)))
             (:task-item
              (list :node node :span span
                    :checked-p (task-list-item-checked-p node)))
             (:heading
              (list :node node :span span
                    :level (heading-record-level heading-record)
                    :plain-text (heading-record-plain-text heading-record)
                    :source-form (heading-record-source-form heading-record)
                    :section-span (heading-record-section-span heading-record)))
             (:front-matter
              (list :raw-text (front-matter-raw-text node) :span span)))))
    (when include-source-slice
      (setf values (append values (list :source-slice (%span-source-slice span)))))
    values))

(defun %extract-node-p (kind node)
  (case kind
    (:code-block (code-block-p node))
    (:link (or (link-p node) (autolink-p node)))
    (:image (image-p node))
    (:table (table-p node))
    (:task-item (task-list-item-p node))
    (:heading (heading-p node))
    (:front-matter (front-matter-p node))))

(defun extract (document kind &key section info-string language include-source-slice)
  (check-type document document)
  (check-type kind (member :code-block :link :image :table :task-item
                           :heading :front-matter))
  (when info-string
    (check-type info-string string))
  (when language
    (check-type language string))
  (let* ((scope (%section-scope document section))
         (heading-records (when (eq kind :heading)
                            (document-headings document)))
         (records-by-node (when heading-records
                            (let ((table (make-hash-table :test #'eq)))
                              (dolist (record heading-records table)
                                (setf (gethash (heading-record-node record) table)
                                      record)))))
         (results nil))
    (walk-node-tree document
     (lambda (frame)
       (let ((node (node-tree-frame-node frame))
             (phase (node-tree-frame-phase frame)))
         (when (and (eq phase :enter)
                    (%extract-node-p kind node)
                    (%span-in-scope-p (node-span node) scope)
                    (or (not (eq kind :code-block))
                        (and (or (null info-string)
                                 (equal info-string
                                        (code-block-info-string node)))
                             (or (null language)
                                 (equal language
                                        (%first-info-token
                                         (code-block-info-string node))))))
                    (or (not (eq kind :heading))
                        (gethash node records-by-node)))
           (push (%extract-result kind node
                                  (and records-by-node
                                       (gethash node records-by-node))
                                  include-source-slice)
                 results))
         :descend)))
    (nreverse results)))
