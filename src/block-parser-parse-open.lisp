(in-package #:cl-markdown-kit)

(defun %bp-state-open-item (state physical-line content record)
  (destructuring-bind (kind start-number delimiter indent marker-end
                       content-end content-indent prefix marker-columns)
      record
    (declare (ignore start-number delimiter marker-end prefix marker-columns))
    (%pc-check-depth
     (+ (%bp-parser-state-depth state) (%bp-state-nesting-level state) 1)
     (%bp-parser-state-limit state) :block)
    (let* ((list-frame (%bp-state-current-frame state))
           (item-context (%bp-open-frame-context list-frame))
           (first-content
             (let* ((line (%bp-line-slice content content-end))
                    (content-column
                      (+ (%block-line-column-start content) content-indent))
                    (virtual-indent
                      (max 0 (- (%block-line-column-start line)
                                content-column))))
               (if (plusp virtual-indent)
                   (%bp-line-prefix-spaces line virtual-indent)
                   line)))
           (task-values
             (multiple-value-list
              (if (%bp-parser-state-gfm state)
                  (%bp-task-marker first-content)
                  (values nil nil nil))))
           (task-p (first task-values))
           (checked (second task-values))
           (task-content (third task-values))
           (item
             (if task-p
                 (make-task-list-item
                  :span nil :continuation-prefix item-context :children nil
                  :checked-p checked
                  :task-marker-span
                  (source-buffer-span
                   (%bp-parser-state-source-buffer state)
                   (%bp-line-boundary first-content 0)
                   (%bp-line-boundary first-content 3))
                  :original-marker-character
                  (char (%block-line-text first-content) 1))
                 (make-list-item
                  :span nil :continuation-prefix item-context :children nil)))
           (item-frame
             (%bp-make-open-frame
              :kind :item :node item :context item-context
              :child-context
              (concatenate 'string item-context
                           (make-string content-indent :initial-element #\Space))
              :start-line content :start-position indent
              :end-line physical-line :content-indent content-indent
              :has-content task-p)))
      (%bp-state-push-frame state item-frame)
      (values (if task-p task-content first-content) kind))))

(defun %bp-state-open-list-and-item (state physical-line content record)
  (destructuring-bind (kind start-number delimiter indent marker-end
                       content-end content-indent prefix marker-columns)
      record
    (declare (ignore marker-end content-end content-indent prefix))
    (%pc-check-depth
     (+ (%bp-parser-state-depth state) (%bp-state-nesting-level state) 1)
     (%bp-parser-state-limit state) :block)
    (let* ((list-context (%bp-state-current-context state))
           (list-node
             (make-list-node
              :span nil :continuation-prefix list-context
              :kind kind :tight-p t :start-number start-number
              :marker-character (if (eq kind :unordered) delimiter nil)
              :delimiter-character (if (eq kind :ordered) delimiter nil)
              :items nil))
           (list-frame
             (%bp-make-open-frame
              :kind :list :node list-node :context list-context
              :child-context list-context :start-line content
              :start-position indent :end-line physical-line
              :marker-kind kind :marker-delimiter delimiter
              :marker-columns marker-columns)))
      (%bp-state-push-frame state list-frame)
      (nth-value 0
                 (%bp-state-open-item state physical-line content record)))))

(defun %bp-state-sibling-record (content list-frame)
  (let ((record (%bp-list-marker-record content)))
    (and record
         (not (nth-value 0 (%bp-thematic-break content)))
         (eq (first record) (%bp-open-frame-marker-kind list-frame))
         (char= (third record)
                (%bp-open-frame-marker-delimiter list-frame))
         (<= (ninth record)
             (+ (%bp-open-frame-marker-columns list-frame) 3))
         record)))

(defun %bp-state-list-delimiter-change-p (content list-frame)
  (let ((record (%bp-list-marker-record content)))
    (and record
         (eq (first record) (%bp-open-frame-marker-kind list-frame))
         (not (char= (third record)
                     (%bp-open-frame-marker-delimiter list-frame)))
         (<= (ninth record)
             (+ (%bp-open-frame-marker-columns list-frame) 3)))))

(defun %bp-state-reference-candidate (state content)
  (let ((candidate-lines (list content))
        (source-index (1+ (%bp-parser-state-cursor state)))
        (lines (%bp-parser-state-lines state))
        (stack (%bp-parser-state-stack state))
        (stopped nil))
    (loop while (and (< source-index (length lines)) (not stopped))
          do (multiple-value-bind (next-content mismatch)
                 (%bp-container-transform-line
                  (aref lines source-index) stack)
               (if (or mismatch (%bp-blank-line-p next-content))
                   (setf stopped t)
                   (push next-content candidate-lines)))
             (unless stopped
               (incf source-index)))
    (let* ((candidate (coerce (nreverse candidate-lines) 'vector))
           (result (multiple-value-list
                    (%bp-link-definition-at candidate 0))))
      (when (first result)
        (values candidate (first result) (second result) (third result))))))

(defun %bp-state-open-quote (state physical-line content
                               marker-start quote-content prefix)
  (%pc-check-depth
   (+ (%bp-parser-state-depth state) (%bp-state-nesting-level state) 1)
   (%bp-parser-state-limit state) :block)
  (let* ((quote-context (%bp-state-current-context state))
         (node (make-block-quote
                :span nil :continuation-prefix quote-context :children nil)))
    (%bp-state-push-frame
     state
     (%bp-make-open-frame
      :kind :quote :node node :context quote-context
      :child-context (concatenate 'string quote-context prefix)
      :start-line content :start-position marker-start
      :end-line physical-line))
    quote-content))

(defun %bp-state-open-footnote (state physical-line content raw-label
                                  normalized content-start indent)
  (%pc-check-depth
   (+ (%bp-parser-state-depth state) (%bp-state-nesting-level state) 1)
   (%bp-parser-state-limit state) :block)
  (let* ((footnote-context (%bp-state-current-context state))
         (node (make-footnote-definition
                :span nil :continuation-prefix footnote-context
                :raw-label raw-label :normalized-label normalized
                :children nil))
         (frame
           (%bp-make-open-frame
            :kind :footnote :node node :context footnote-context
            :child-context
            (concatenate 'string footnote-context
                         (make-string 4 :initial-element #\Space))
            :start-line content :start-position indent
            :end-line physical-line :content-indent 4)))
    (%bp-state-push-frame state frame)
    (unless (nth-value 1
                       (gethash (%bp-footnote-index-key normalized)
                                *block-reference-index*))
      (setf (gethash (%bp-footnote-index-key normalized)
                     *block-reference-index*) node))
    (%bp-line-slice content content-start)))

(defun %bp-state-open-reference-definition (state line parsed next)
  (let ((node (%bp-parse-reference-definition
               (%bp-parser-state-source-buffer state) line
               (%bp-state-current-context state) parsed))
        (cursor (%bp-parser-state-cursor state))
        (lines (%bp-parser-state-lines state))
        (stack (%bp-parser-state-stack state)))
    (%bp-state-mark-container-content state)
    (%bp-state-append-node state node)
    (loop for offset from 1 below next
          do (%bp-container-transform-line
              (aref lines (+ cursor offset)) stack t))
    next))

(defun %bp-state-open-container (state physical-line content)
  (if (%bp-blank-line-p content)
      (values content nil 1 t)
      (multiple-value-bind (raw-label normalized content-start indent)
          (if (%bp-parser-state-gfm state)
              (%bp-footnote-start content)
              (values nil nil nil nil))
        (cond
          (raw-label
           (%bp-state-mark-container-content state)
           (values (%bp-state-open-footnote
                    state physical-line content raw-label normalized
                    content-start indent)
                   t 1 t))
          ((%bp-link-definition-start-p content)
           (multiple-value-bind (candidate line parsed next)
               (%bp-state-reference-candidate state content)
             (if candidate
                 (values content nil
                         (%bp-state-open-reference-definition
                          state line parsed next)
                         t)
                 (values content nil 1 nil))))
          (t
           (multiple-value-bind (marker-start quote-content prefix)
               (%bp-quote-content content)
             (if marker-start
                 (progn
                   (%bp-state-mark-container-content state)
                   (values (%bp-state-open-quote
                            state physical-line content marker-start
                            quote-content prefix)
                           t 1 t))
                 (values content nil 1 nil))))))))

(defun %bp-state-open-heading (state content)
  (multiple-value-bind (marker count start)
      (%bp-thematic-break content)
    (if marker
        (progn
          (%bp-state-append-node
           state
           (%bp-make-thematic-break
            (%bp-parser-state-source-buffer state) content
            (%bp-state-current-context state) marker count start))
          t)
        (multiple-value-bind (level indent content-start content-end opening closing)
            (%bp-atx-heading content)
          (when level
            (let* ((content-line (%bp-line-slice content content-start content-end))
                   (node
                     (make-heading
                      :span (%bp-line-span
                             (%bp-parser-state-source-buffer state) content indent
                             (length (%block-line-text content)))
                      :continuation-prefix (%bp-state-current-context state)
                      :level level :style :atx
                      :marker-character +bp-atx-heading-marker+
                      :opening-marker-length opening :closing-marker-length closing
                      :children nil)))
              (%bp-add-fragment
               (%bp-parser-state-source-buffer state) node (list content-line))
              (%bp-state-append-node state node))
            t)))))

(defun %bp-state-open-fence-or-html (state physical-line content)
  (multiple-value-bind (character fence-length indentation fence-end
                                 info-string fence-start)
      (%bp-fence-start content)
    (declare (ignore fence-end))
    (if character
        (progn
          (setf (%bp-parser-state-leaf state)
                (%bp-make-open-leaf
                 :kind :fenced :context (%bp-state-current-context state)
                 :start-line content :start-position fence-start
                 :end-line physical-line :lines nil
                 :fence-character character :fence-length fence-length
                 :indentation indentation
                 :info-string
                 (let ((trimmed (%pc-trimmed-string info-string)))
                   (unless (zerop (length trimmed))
                     (%pc-unescape trimmed)))))
          t)
        (multiple-value-bind (kind terminator name)
            (%bp-html-block-type content)
          (declare (ignore name))
          (when kind
            (setf (%bp-parser-state-leaf state)
                  (%bp-make-open-leaf
                   :kind :html :context (%bp-state-current-context state)
                   :start-line content
                   :start-position (nth-value 0 (%bp-line-leading-indent content 3))
                   :end-line physical-line :lines (list content)
                   :html-kind kind :html-terminator terminator))
            (when (and (<= kind 5)
                       (%bp-html-terminator-p content terminator kind))
              (%bp-state-close-leaf state))
            t)))))

(defun %bp-state-open-list-or-paragraph (state physical-line content)
  (let ((record (%bp-list-marker-record content)))
    (cond
      (record
       (multiple-value-bind (item-content ignored-kind)
           (%bp-state-open-list-and-item state physical-line content record)
         (declare (ignore ignored-kind))
         (values item-content t)))
      ((>= (%bp-line-indent-columns content) 4)
       (setf (%bp-parser-state-leaf state)
             (%bp-make-open-leaf
              :kind :indented :context (%bp-state-current-context state)
              :start-line content :start-position 0 :end-line physical-line
              :lines (list (%bp-strip-columns content 4))))
       (values content nil))
      (t
       (setf (%bp-parser-state-leaf state)
             (%bp-make-open-leaf
              :kind :paragraph :context (%bp-state-current-context state)
              :start-line content :start-position 0
              :end-line physical-line :lines (list content)))
       (values content nil)))))

(defun %bp-state-open-new-block (state physical-line initial-content)
  (let ((content initial-content)
        (consumed 1)
        (again t))
    (loop while again
          do (multiple-value-bind (container-content repeat-p line-count handled-p)
                 (%bp-state-open-container state physical-line content)
               (if handled-p
                   (setf content container-content
                         consumed line-count
                         again repeat-p)
                   (progn
                     (setf again nil)
                     (%bp-state-mark-container-content state)
                     (unless (%bp-state-open-heading state content)
                       (unless (%bp-state-open-fence-or-html
                                state physical-line content)
                         (multiple-value-bind (new-content repeat-p)
                             (%bp-state-open-list-or-paragraph
                              state physical-line content)
                           (setf content new-content
                                 again repeat-p))))))))
    consumed))
