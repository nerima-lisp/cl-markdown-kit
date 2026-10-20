;;;; src/block-parser.lisp
(in-package #:cl-markdown-kit)

(declaim (ftype function %bp-parse-lines %pc-check-depth
                make-inline-fragment make-block-parse-result))
(declaim (ftype function parse-inline))

(defun %bp-parse-lines (source-buffer lines context depth limit gfm)
  (let ((state (%bp-new-parser-state
                source-buffer lines context depth limit gfm)))
    (%bp-state-run state)
    (%bp-state-finish state)))

(defun %bp-front-matter (source-buffer lines)
  (when (and (plusp (length lines))
             (string= (%block-line-text (aref lines 0)) "---"))
    (let ((closing (loop for index from 1 below (length lines)
                         when (string= (%block-line-text (aref lines index)) "---")
                           return index))
          (source (source-buffer-source source-buffer)))
      (when closing
        (let* ((closing-index closing)
               (opening-line (aref lines 0))
               (closing-line (aref lines closing-index))
               (inner-start (%block-line-after opening-line))
               (inner-end (%block-line-raw-start closing-line))
               (start (%bp-line-boundary opening-line 0))
               (end (%bp-line-boundary closing-line
                                       (length (%block-line-text closing-line))))
               (raw-start
                 (source-boundary-offset
                  (%bp-source-boundary-at source-buffer inner-start)))
               (raw-end
                 (source-boundary-offset
                  (%bp-source-boundary-at source-buffer inner-end)))
               (node (make-front-matter
                      :span (source-buffer-span source-buffer start end)
                      :continuation-prefix ""
                      :raw-text (subseq source raw-start raw-end)
                      :opening-delimiter "---"
                      :closing-delimiter "---")))
          (values node (1+ closing-index)))))))

(defun parse-blocks
    (source-buffer
     &key (front-matter t)
          (gfm t)
          (max-nesting-depth +default-max-nesting-depth+))
  "Parse block structure and defer paragraph, heading, and table-cell inlines."
  (check-type source-buffer source-buffer)
  (check-type gfm boolean)
  (unless (and (integerp max-nesting-depth) (plusp max-nesting-depth))
    (error 'type-error :datum max-nesting-depth
           :expected-type '(integer 1 *)))
  (let* ((*block-inline-fragments* nil)
         (*block-reference-index* (make-hash-table :test 'equal))
         (lines (%bp-split-lines (source-buffer-normalized-text source-buffer)))
         (children nil))
    (when front-matter
      (multiple-value-bind (front-matter-node next)
          (%bp-front-matter source-buffer lines)
        (when front-matter-node
          (push front-matter-node children)
          (setf lines (subseq lines next)))))
    (setf children
          (nconc (nreverse children)
                 (%bp-parse-lines source-buffer lines "" 0 max-nesting-depth
                                  gfm)))
    (let* ((reference-index *block-reference-index*)
           (document (make-document
                      :span (source-buffer-document-span source-buffer)
                      :continuation-prefix ""
                      :children children
                      :source (source-buffer-source source-buffer)
                      :source-format (source-buffer-format source-buffer)
                      :reference-index reference-index
                      :max-nesting-depth max-nesting-depth)))
      (make-block-parse-result
       :document document
       :inline-fragments (nreverse *block-inline-fragments*)
       :reference-index reference-index))))
