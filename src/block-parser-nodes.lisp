(in-package #:cl-markdown-kit)

(defun %bp-body-from-lines (lines)
  (let ((lines (if (listp lines) (coerce lines 'vector) lines)))
  (with-output-to-string (output)
    (loop for index from 0 below (length lines)
          for line = (aref lines index)
          do (write-string (%block-line-text line) output)
             (when (or (< index (1- (length lines)))
                       (> (%block-line-after line)
                          (%block-line-raw-end line)))
               (write-char #\Newline output))))))

(defun %bp-frame-context (frame)
  (%bp-open-frame-child-context frame))

(defun %bp-frame-add-node (frame node)
  (if (eq (%bp-open-frame-kind frame) :list)
      (push node (%bp-open-frame-items frame))
      (push node (%bp-open-frame-children frame)))
  node)

(defun %bp-frame-span (source-buffer frame)
  (source-buffer-span
   source-buffer
   (%bp-line-boundary (%bp-open-frame-start-line frame)
                      (%bp-open-frame-start-position frame))
   (%bp-line-boundary
    (%bp-open-frame-end-line frame)
    (length (%block-line-text (%bp-open-frame-end-line frame))))))

(defun %bp-finish-frame (source-buffer frame)
  (let ((node (%bp-open-frame-node frame)))
    (setf (node-span node) (%bp-frame-span source-buffer frame))
    (case (%bp-open-frame-kind frame)
      (:quote
       (setf (block-quote-children node)
             (nreverse (%bp-open-frame-children frame)))
       node)
      (:list
       (let ((items (nreverse (%bp-open-frame-items frame))))
         (setf (list-node-items node) items
               (list-node-tight-p node) (not (%bp-list-items-loose-p items))))
       node)
      (:item
       (setf (list-item-children node)
             (nreverse (%bp-open-frame-children frame)))
       node)
      (:footnote
       (setf (footnote-definition-children node)
             (nreverse (%bp-open-frame-children frame)))
       node)
      (otherwise node))))

(defun %bp-container-transform-line (line stack on-match on-fail &optional record-p)
  (let ((content line)
        (mismatch nil))
    (dolist (frame (reverse (butlast stack)))
      (unless mismatch
        (case (%bp-open-frame-kind frame)
          (:quote
           (multiple-value-bind (marker-start quote-content prefix)
               (%bp-quote-content content)
             (declare (ignore prefix))
             (if marker-start
                 (progn
                   (setf content quote-content)
                   (when record-p
                     (setf (%bp-open-frame-end-line frame) line)))
                 (setf mismatch frame))))
          ((:item :footnote)
           (cond
             ((%bp-blank-line-p content)
              (when record-p
                (setf (%bp-open-frame-blank-seen frame) t)))
             ((and (eq (%bp-open-frame-kind frame) :item)
                   (%bp-open-frame-blank-seen frame)
                   (not (%bp-open-frame-has-content frame)))
              (setf mismatch frame))
             ((>= (%bp-line-indent-columns content)
                  (%bp-open-frame-content-indent frame))
              (setf content
                    (%bp-strip-columns
                     content (%bp-open-frame-content-indent frame)))
              (when record-p
                (setf (%bp-open-frame-end-line frame) line)))
             (t
              (setf mismatch frame)))))))
    (if mismatch
        (funcall on-fail content mismatch)
        (funcall on-match content))))

(defun %bp-line-gap-p (left right)
  (> (- (cl-parser-kit:span-start-line (node-span right))
        (cl-parser-kit:span-end-line (node-span left)))
     1))

(defun %bp-node-sequence-loose-p (nodes)
  (loop with previous = nil
        for node in nodes
        thereis (prog1 (and previous (%bp-line-gap-p previous node))
                  (setf previous node))))

(defun %bp-list-items-loose-p (items)
  (or (loop for item in items
            thereis (%bp-node-sequence-loose-p (node-children item)))
      (%bp-node-sequence-loose-p items)))

(defun %bp-task-marker (line)
  (let ((text (%block-line-text line)))
    (when (and (>= (length text) 3)
               (char= (char text 0) #\[)
               (find (char text 1) +bp-task-marker-states+)
               (char= (char text 2) #\])
               (or (= (length text) 3)
                   (%pc-space-p (char text 3))))
      (values t (not (char= (char text 1) #\Space))
              (if (and (> (length text) 3)
                       (%pc-space-p (char text 3)))
                  (%bp-line-slice line 4)
                  (%bp-line-slice line 3))))))

(defun %bp-make-inline-node (source-buffer lines context start-position end-position)
  (let ((node (make-paragraph
               :span (%bp-lines-span source-buffer (coerce lines 'vector)
                                     start-position end-position)
               :continuation-prefix context
               :children nil)))
    (%bp-add-fragment source-buffer node lines)
    node))

(defun %bp-prepared-paragraph-lines (lines)
  (loop for line in lines
        for first = t then nil
        collect (if first
                    (%bp-strip-up-to-three line)
                    (%bp-strip-columns line (%bp-line-indent-columns line)))))

(defun %bp-trim-line-end (line)
  (let ((end (length (%block-line-text line))))
    (loop while (and (plusp end)
                     (%pc-space-p (char (%block-line-text line) (1- end))))
          do (decf end))
    (%bp-line-slice line 0 end)))

(defun %bp-make-paragraph (source-buffer lines context)
  (let* ((prepared (%bp-prepared-paragraph-lines lines))
         (node (%bp-make-inline-node
                source-buffer prepared context 0
                (length (%block-line-text (car (last prepared)))))))
    node))
