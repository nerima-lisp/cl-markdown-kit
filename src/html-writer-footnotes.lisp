(in-package #:cl-markdown-kit)

(defun %hw-footnote-id-candidate (prefix label)
  (format nil "~A~A" prefix (normalize-uri-percent-encoding label)))

(defun %hw-footnote-reference-id-candidate (label reference-index)
  (let ((base (%hw-footnote-id-candidate "fnref-" label)))
    (if (= reference-index 1)
        base
        (format nil "~A-~D" base reference-index))))

(defun %hw-allocate-footnote-ids (candidates)
  (let ((reserved (make-hash-table :test #'equal))
        (allocated (make-hash-table :test #'equal)))
    (dolist (candidate candidates)
      (setf (gethash candidate reserved) t))
    (mapcar (lambda (candidate)
              (if (gethash candidate allocated)
                  (loop for suffix from 2
                        for identifier = (format nil "~A-~D" candidate suffix)
                        unless (or (gethash identifier reserved)
                                   (gethash identifier allocated))
                          do (setf (gethash identifier allocated) t)
                             (return identifier))
                  (progn
                    (setf (gethash candidate allocated) t)
                    candidate)))
            candidates)))

(defun %hw-build-footnote-context (document)
  (let ((definitions (make-hash-table :test #'equal))
        (numbers (make-hash-table :test #'equal))
        (reference-counts (make-hash-table :test #'equal))
        (reference-identifiers nil)
        (order nil)
        (next-number 0))
    (walk-node-tree document
     (lambda (frame)
       (let* ((node (node-tree-frame-node frame))
              (phase (node-tree-frame-phase frame)))
         (when (eq phase :enter)
           (dolist (child (node-children node))
             (unless (node-p child)
               (error 'markdown-unsupported-node-error
                      :object child
                      :message (format nil "Unsupported HTML child: ~S"
                                       child)))))
         (when (and (eq phase :enter)
                    (footnote-definition-p node))
           (setf (gethash (footnote-definition-normalized-label node)
                          definitions)
                 node)))
       :descend))
    (walk-node-tree document
     (lambda (frame)
       (let ((node (node-tree-frame-node frame)))
         (when (eq (node-tree-frame-phase frame) :enter)
           (when (footnote-reference-p node)
             (let ((label (footnote-reference-normalized-label node)))
               (when (gethash label definitions)
                 (unless (gethash label numbers)
                   (incf next-number)
                   (setf (gethash label numbers) next-number)
                   (push label order))
                 (let ((reference-index
                         (incf (gethash label reference-counts 0))))
                   (push (list label reference-index
                               (%hw-footnote-reference-id-candidate
                                label reference-index))
                         reference-identifiers)))))))
       :descend))
    (let* ((order (nreverse order))
           (reference-identifiers (nreverse reference-identifiers))
           (footnote-ids (make-hash-table :test #'equal))
           (footnote-reference-ids (make-hash-table :test #'equal))
           (allocated-footnote-ids
             (%hw-allocate-footnote-ids
              (mapcar (lambda (label)
                        (%hw-footnote-id-candidate "fn-" label))
                      order)))
           (allocated-reference-ids
             (%hw-allocate-footnote-ids
              (mapcar #'third reference-identifiers))))
      (loop for label in order
            for identifier in allocated-footnote-ids
            do (setf (gethash label footnote-ids) identifier))
      (loop for entry in reference-identifiers
            for identifier in allocated-reference-ids
            do (setf (gethash (list (first entry) (second entry))
                              footnote-reference-ids)
                     identifier))
      (values definitions numbers reference-counts order
              footnote-ids footnote-reference-ids))))

(defun %hw-footnote-reference-index (context label)
  (incf (gethash label
                 (%html-writer-context-footnote-reference-indices context)
                 0)))

(defun %hw-write-footnote-reference (context node)
  (let* ((label (footnote-reference-normalized-label node))
         (number (gethash label (%html-writer-context-footnote-numbers context)))
         (reference-index (%hw-footnote-reference-index context label))
         (footnote-id (gethash label
                               (%html-writer-context-footnote-ids context)))
         (reference-id
           (gethash (list label reference-index)
                    (%html-writer-context-footnote-reference-ids context))))
    (when number
      (%hw-write context "<sup class=\"footnote-ref\"><a href=\"#")
      (%hw-escaped-attribute context footnote-id)
      (%hw-write context "\" id=\"")
      (%hw-escaped-attribute context reference-id)
      (%hw-write context "\" data-footnote-ref>")
      (%hw-write context (format nil "~D" number))
      (%hw-write context "</a></sup>"))))

(defun %hw-write-footnote-backrefs (context label)
  (let ((number (gethash label (%html-writer-context-footnote-numbers context)))
        (count (gethash label (%html-writer-context-footnote-reference-counts
                               context))))
    (when number
      (loop for reference-index from 1 to count
            do (when (> reference-index 1)
                 (%hw-write context " "))
               (%hw-write context "<a href=\"#")
               (%hw-escaped-attribute
                context
                (gethash (list label reference-index)
                         (%html-writer-context-footnote-reference-ids
                          context)))
               (%hw-write context
                          (format nil
                                  "\" class=\"footnote-backref\" data-footnote-backref data-footnote-backref-idx=\"~D~A\" aria-label=\"Back to reference ~D~A\">↩"
                                  number
                                  (if (> reference-index 1)
                                      (format nil "-~D" reference-index)
                                      "")
                                  number
                                  (if (> reference-index 1)
                                      (format nil "-~D" reference-index)
                                      "")))
               (when (> reference-index 1)
                 (%hw-write context (format nil "<sup class=\"footnote-ref\">~D</sup>"
                                            reference-index)))
               (%hw-write context "</a>")))))

(defun %hw-write-footnote-definition (context node)
  (let ((label (footnote-definition-normalized-label node))
        (children (footnote-definition-children node)))
    (%hw-newline context)
    (%hw-write context "<li id=\"")
    (%hw-escaped-attribute
     context
     (gethash label (%html-writer-context-footnote-ids context)))
    (%hw-write context "\">")
    (%hw-write-char context #\Newline)
    (%hw-render-node-tree context node)
    (unless (and children (paragraph-p (car (last children))))
      (%hw-newline context)
      (%hw-write-footnote-backrefs context label)
      (%hw-write-char context #\Newline))
    (%hw-write context "</li>")
    (%hw-write-char context #\Newline)))

(defun %hw-write-footnotes (context)
  (let ((order (%html-writer-context-footnote-order context))
        (definitions (%html-writer-context-footnote-definitions context)))
    (when order
      (%hw-newline context)
      (%hw-write context "<section class=\"footnotes\" data-footnotes>")
      (%hw-write-char context #\Newline)
      (%hw-write context "<ol>")
      (%hw-write-char context #\Newline)
      (dolist (label order)
        (%hw-write-footnote-definition
         context (gethash label definitions)))
      (%hw-write context "</ol>")
      (%hw-write-char context #\Newline)
      (%hw-write context "</section>")
      (%hw-write-char context #\Newline))))
