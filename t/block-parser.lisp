(in-package #:cl-markdown-kit/test)

(defun block-parser-test-result (source &key (front-matter t) (gfm t))
  (cl-markdown-kit::parse-blocks
   (cl-markdown-kit:make-source-buffer source)
   :front-matter front-matter
   :gfm gfm))

(defun block-parser-test-line (text)
  (aref (cl-markdown-kit::%bp-split-lines text) 0))

(defun block-parser-test-document (result)
  (cl-markdown-kit::block-parse-result-document result))

(defun block-parser-test-span-is (node start end)
  (let ((span (cl-markdown-kit:node-span node)))
    (and span
         (= (cl-parser-kit:span-start span) start)
         (= (cl-parser-kit:span-end span) end))))

(defun block-parser-test-ranges-valid-p (node source)
  (let* ((span (cl-markdown-kit:node-span node))
         (start (and span (cl-parser-kit:span-start span)))
         (end (and span (cl-parser-kit:span-end span)))
         (children (cl-markdown-kit:node-children node)))
    (and span
         (string= (cl-parser-kit:span-source span) source)
         (<= 0 start end (length source))
         (loop with previous-end = start
               for child in children
               for child-span = (cl-markdown-kit:node-span child)
               for child-start = (and child-span
                                      (cl-parser-kit:span-start child-span))
               for child-end = (and child-span
                                    (cl-parser-kit:span-end child-span))
               do (unless (and child-span
                               (<= start child-start child-end end)
                               (<= previous-end child-start)
                               (block-parser-test-ranges-valid-p child source))
                    (return-from block-parser-test-ranges-valid-p nil))
                  (setf previous-end child-end)
               finally (return t)))))

(defun block-parser-test-only-child (node)
  (let ((children (cl-markdown-kit:node-children node)))
    (and (= (length children) 1) (first children))))

(defun block-parser-test-gfm-example (id)
  (or (find id (gfm-extension-examples)
            :key #'spec-example-id :test #'string=)
      (error "Missing GFM fixture ~A" id)))

(defun block-parser-test-commonmark-example (id)
  (or (find id (commonmark-spec-examples)
            :key #'spec-example-id :test #'string=)
      (error "Missing CommonMark fixture ~A" id)))

(defun block-parser-test-example-source (example)
  (substitute #\Tab #\→ (spec-example-markdown example)))

(defun block-parser-test-gfm-result (id)
  (block-parser-test-result
   (spec-example-markdown (block-parser-test-gfm-example id))))

(defun block-parser-test-nodes-of-type (root predicate)
  (let ((nodes nil))
    (cl-markdown-kit:walk-node-tree root
     (lambda (frame)
       (let ((node (cl-markdown-kit:node-tree-frame-node frame))
             (phase (cl-markdown-kit:node-tree-frame-phase frame)))
         (when (and (eq phase :enter) (funcall predicate node))
           (push node nodes))
         :descend)))
    (nreverse nodes)))

(defun block-parser-test-fragments-for (result node)
  (remove-if-not
   (lambda (fragment)
     (eq (cl-markdown-kit::inline-fragment-node fragment) node))
   (cl-markdown-kit::block-parse-result-inline-fragments result)))

(defun block-parser-test-fragment-texts (result)
  (mapcar #'cl-markdown-kit::inline-fragment-text
          (cl-markdown-kit::block-parse-result-inline-fragments result)))

(defun block-parser-test-span-text (span)
  (subseq (cl-parser-kit:span-source span)
          (cl-parser-kit:span-start span)
          (cl-parser-kit:span-end span)))

(defun block-parser-test-substring-count (needle text)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count position
        do (setf start (+ position (length needle)))))

(defun block-parser-test-data-path (name)
  (merge-pathnames (format nil "t/data/~A" name)
                   (asdf:system-source-directory :cl-markdown-kit)))

(defun block-parser-test-read-data (name)
  (with-open-file (stream (block-parser-test-data-path name)
                          :direction :input :external-format :utf-8)
    (let* ((*read-eval* nil)
           (eof (gensym "EOF"))
           (data (read stream nil eof)))
      (when (eq data eof)
        (error "The block structure data file is empty: ~A" name))
      (unless (eq (read stream nil eof) eof)
        (error "The block structure data file has multiple forms: ~A" name))
      data)))

(declaim (ftype function block-parser-test-node-sequence-signature
                  block-parser-test-node-signature))

(defun block-parser-test-node-sequence-signature (nodes)
  (let* ((nodes (remove-if (lambda (node)
                             (typep node
                                    'cl-markdown-kit:link-reference-definition))
                           nodes))
         (count (length nodes)))
    (loop for node in nodes
          for index from 0
          collect
          (block-parser-test-node-signature
           node
           (cond ((= count 1) :only)
                 ((zerop index) :first)
                 ((= index (1- count)) :last)
                 (t :middle))))))

(defun block-parser-test-node-form (kind attributes children)
  (append (list kind attributes) children))

(defun block-parser-test-container-node-signature (node)
  (typecase node
    (cl-markdown-kit:document
     (block-parser-test-node-form
      :document nil
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:document-children node))))
    (cl-markdown-kit:block-quote
     (block-parser-test-node-form
      :block-quote nil
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:block-quote-children node))))
    (cl-markdown-kit:list-node
     (block-parser-test-node-form
      :list
      (list :kind (cl-markdown-kit:list-node-kind node)
            :start (cl-markdown-kit:list-node-start-number node)
            :tight (cl-markdown-kit:list-node-tight-p node)
            :delimiter
            (let ((delimiter (cl-markdown-kit:list-node-delimiter-character node)))
              (cond ((null delimiter) nil)
                    ((char= delimiter #\.) :period)
                    ((char= delimiter #\)) :paren)
                    (t delimiter))))
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:list-node-items node))))
    (cl-markdown-kit:task-list-item
     (block-parser-test-node-form
      :item (list :task-checked (cl-markdown-kit:task-list-item-checked-p node))
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:list-item-children node))))
    (cl-markdown-kit:list-item
     (block-parser-test-node-form
      :item nil
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:list-item-children node))))
    (cl-markdown-kit:footnote-definition
     (block-parser-test-node-form
      :footnote-definition
      (list :label (cl-markdown-kit:footnote-definition-raw-label node))
      (block-parser-test-node-sequence-signature
       (cl-markdown-kit:footnote-definition-children node))))
    (otherwise nil)))

(defun block-parser-test-table-node-signature (node)
  (typecase node
    (cl-markdown-kit:table
     (let* ((rows (cl-markdown-kit:table-rows node))
            (alignments (cl-markdown-kit:table-alignments node)))
       (block-parser-test-node-form
        :table
        (list :columns (length alignments)
              :alignments alignments
              :row-cells (mapcar
                          (lambda (row)
                            (length (cl-markdown-kit:table-row-cells row)))
                          rows))
        (mapcar #'block-parser-test-node-signature rows))))
    (cl-markdown-kit:table-row
     (block-parser-test-node-form
      :row (list :header (cl-markdown-kit:table-row-header-row-p node))
      (mapcar #'block-parser-test-node-signature
              (cl-markdown-kit:table-row-cells node))))
    (cl-markdown-kit:table-cell
     (block-parser-test-node-form :cell nil nil))
    (otherwise nil)))

(defun block-parser-test-leaf-node-signature (node placement)
  (typecase node
    (cl-markdown-kit:paragraph
     (block-parser-test-node-form :paragraph (list :placement placement) nil))
    (cl-markdown-kit:heading
     (block-parser-test-node-form
      :heading (list :level (cl-markdown-kit:heading-level node)) nil))
    (cl-markdown-kit:thematic-break
     (block-parser-test-node-form :thematic-break nil nil))
    (cl-markdown-kit:code-block
     (block-parser-test-node-form
      :code-block
      (list :info (or (cl-markdown-kit:code-block-info-string node) "")
            :literal (cl-markdown-kit:code-block-body node))
      nil))
    (cl-markdown-kit:html-block
     (block-parser-test-node-form
      :html-block (list :literal (cl-markdown-kit:html-block-body node)) nil))
    (cl-markdown-kit:front-matter
     (block-parser-test-node-form :front-matter nil nil))
    (cl-markdown-kit:link-reference-definition
     (block-parser-test-node-form
      :link-reference-definition
      (list :label (cl-markdown-kit:link-reference-definition-raw-label node)
            :destination (cl-markdown-kit:link-reference-definition-destination node)
            :title (cl-markdown-kit:link-reference-definition-title node))
      nil))
    (otherwise nil)))

(defun block-parser-test-node-signature (node &optional placement)
  (or (block-parser-test-container-node-signature node)
      (block-parser-test-table-node-signature node)
      (block-parser-test-leaf-node-signature node placement)
      (error "Unsupported block node in structural signature: ~S" node)))

(defun block-parser-test-short-value (value)
  (if (and (stringp value) (> (length value) 120))
      (list :string-length (length value)
            :prefix (subseq value 0 80)
            :suffix (subseq value (- (length value) 24)))
      value))

(defun block-parser-test-first-difference (expected actual &optional path)
  (cond
    ((equal expected actual) nil)
    ((and (consp expected) (consp actual))
     (loop for left in expected
           for right in actual
           for index from 0
           for difference =
             (block-parser-test-first-difference
              left right (cons index path))
           when difference do (return-from block-parser-test-first-difference
                                difference))
     (unless (= (length expected) (length actual))
       (list (reverse path)
             (list :length (length expected))
             (list :length (length actual)))))
    (t
     (list (reverse path)
           (block-parser-test-short-value expected)
           (block-parser-test-short-value actual)))))

(defun block-parser-test-structure-signature (example)
  (handler-case
      (values
       (block-parser-test-node-signature
        (block-parser-test-document
         (block-parser-test-result (spec-example-markdown example)
                                   :front-matter nil)))
       nil)
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun block-parser-test-footnote-node-p (signature)
  (and (consp signature)
       (eq (first signature) :footnote-definition)))

(defun block-parser-test-footnote-label (signature)
  (getf (second signature) :label))

(defun block-parser-test-document-without-footnotes (signature)
  (destructuring-bind (kind attributes &rest children) signature
    (block-parser-test-node-form
     kind attributes
     (remove-if #'block-parser-test-footnote-node-p children))))

(defun block-parser-test-gfm-footnote-difference (expected actual)
  ;; cmark XML orders used definitions by reference and omits unused definitions.
  (let* ((expected-footnotes
           (remove-if-not #'block-parser-test-footnote-node-p (cddr expected)))
         (actual-footnotes
           (remove-if-not #'block-parser-test-footnote-node-p (cddr actual)))
         (document-difference
           (block-parser-test-first-difference
            (block-parser-test-document-without-footnotes expected)
            (block-parser-test-document-without-footnotes actual))))
    (or document-difference
        (loop for expected-footnote in expected-footnotes
              do (let* ((label
                          (block-parser-test-footnote-label expected-footnote))
                        (actual-footnote
                          (find label actual-footnotes
                                :key #'block-parser-test-footnote-label
                                :test #'string=)))
                   (unless actual-footnote
                     (return (list :missing-footnote label)))
                   (let ((difference
                           (block-parser-test-first-difference
                            expected-footnote actual-footnote)))
                     (when difference
                       (return (list :footnote label difference)))))))))

(defun block-parser-test-structure-difference
    (expected actual error-message &optional example-id)
  (cond
    (error-message (list :parse-error error-message))
    ((equal example-id "gfm-0023")
     (block-parser-test-gfm-footnote-difference expected actual))
    (t (block-parser-test-first-difference expected actual))))
