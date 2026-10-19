(in-package #:cl-markdown-kit)

(defun %bp-state-table-header-followed-by-separator-p (state content)
  (let* ((next-index (1+ (%bp-parser-state-cursor state)))
         (lines (%bp-parser-state-lines state)))
    (and (< next-index (length lines))
         (multiple-value-bind (next-content mismatch)
             (%bp-container-transform-line
              (aref lines next-index) (%bp-parser-state-stack state)
              (lambda (next-content) (values next-content nil))
              (lambda (next-content mismatch)
                (values next-content mismatch)))
           (let ((alignments (%bp-table-alignments next-content)))
             (and (not mismatch)
                  alignments
                  (= (length alignments)
                     (length (%pc-table-cells (%block-line-text content))))
                  (find #\| (%block-line-text content))))))))

(defun %bp-state-promote-paragraph-to-table (state physical-line content)
  (let* ((leaf (%bp-parser-state-leaf state))
         (paragraph-lines-reversed (%bp-open-leaf-lines leaf))
         (only-paragraph-line
           (and (null (cdr paragraph-lines-reversed))
                (car paragraph-lines-reversed)))
         (alignments (and (%bp-parser-state-gfm state)
                          (%bp-table-alignments content))))
    (when (and (%bp-parser-state-gfm state)
               only-paragraph-line
               alignments
               (= (length alignments)
                  (length (%pc-table-cells
                           (%block-line-text only-paragraph-line))))
               (or (find #\| (%block-line-text only-paragraph-line))
                   (find #\| (%block-line-text content))))
      (let* ((header only-paragraph-line)
             (table-context (%bp-open-leaf-context leaf))
             (node
               (make-table
                :span nil :continuation-prefix table-context
                :alignments alignments
                :rows (list (%bp-table-row
                             (%bp-parser-state-source-buffer state)
                             header alignments t table-context)))))
        (setf (%bp-parser-state-leaf state)
              (%bp-make-open-leaf
               :kind :table :context table-context
               :start-line header
               :start-position (nth-value 0 (%bp-line-leading-indent header 3))
               :end-line physical-line :table-node node
               :table-alignments alignments :table-rows (table-rows node)))
        t))))

(defun %bp-state-promote-paragraph-to-setext (state content)
  (multiple-value-bind (level marker count indent) (%bp-setext-level content)
    (declare (ignore indent))
    (when level
      (let* ((leaf (%bp-parser-state-leaf state))
             (paragraph-lines
               (reverse (%bp-open-leaf-lines leaf)))
             (prepared (%bp-prepared-paragraph-lines paragraph-lines))
             (setext-prepared
               (append (butlast prepared)
                       (list (%bp-trim-line-end (car (last prepared))))))
             (span-lines
               (concatenate 'vector
                            (coerce setext-prepared 'vector)
                            (vector content)))
             (node
               (make-heading
                :span (%bp-lines-span
                       (%bp-parser-state-source-buffer state)
                       span-lines 0
                       (length (%block-line-text content)))
                :continuation-prefix (%bp-open-leaf-context leaf)
                :level level :style :setext :marker-character marker
                :opening-marker-length 0 :closing-marker-length count
                :children nil)))
        (%bp-add-fragment
         (%bp-parser-state-source-buffer state) node setext-prepared)
        (setf (%bp-parser-state-leaf state) nil)
        (%bp-state-append-node state node)
        t))))

(defun %bp-state-process-paragraph-line (state physical-line content)
  (cond
    ((%bp-state-promote-paragraph-to-table state physical-line content) 1)
    ((%bp-state-table-header-followed-by-separator-p state content)
     (%bp-state-close-leaf state)
     0)
    ((%bp-state-promote-paragraph-to-setext state content) 1)
    ((%bp-blank-line-p content)
     (%bp-state-close-leaf state)
     0)
    ((%bp-interrupts-paragraph-p
      content (%bp-parser-state-gfm state))
     (%bp-state-close-leaf state)
     0)
    (t
     (push content
           (%bp-open-leaf-lines (%bp-parser-state-leaf state)))
     (setf (%bp-open-leaf-end-line (%bp-parser-state-leaf state))
           physical-line)
     1)))

(defun %bp-state-process-fenced-line (state physical-line content)
  (let ((leaf (%bp-parser-state-leaf state)))
    (if (%bp-fence-close-p
         content (%bp-open-leaf-fence-character leaf)
         (%bp-open-leaf-fence-length leaf))
        (progn
          (setf (%bp-open-leaf-end-line leaf) physical-line)
          (%bp-state-close-leaf state)
          1)
        (progn
          (push (%bp-strip-columns content
                                   (%bp-open-leaf-indentation leaf))
                (%bp-open-leaf-lines leaf))
          (setf (%bp-open-leaf-end-line leaf) physical-line)
          1))))

(defun %bp-state-process-html-line (state physical-line content)
  (let ((leaf (%bp-parser-state-leaf state)))
    (if (and (>= (%bp-open-leaf-html-kind leaf) 6)
             (%bp-blank-line-p content))
        (progn
          (%bp-state-close-leaf state)
          0)
        (progn
          (push content (%bp-open-leaf-lines leaf))
          (setf (%bp-open-leaf-end-line leaf) physical-line)
          (when (and (<= (%bp-open-leaf-html-kind leaf) 5)
                     (%bp-html-terminator-p
                      content (%bp-open-leaf-html-terminator leaf)
                      (%bp-open-leaf-html-kind leaf)))
            (%bp-state-close-leaf state))
          1))))

(defun %bp-state-process-indented-line (state physical-line content)
  (let ((leaf (%bp-parser-state-leaf state)))
    (cond
      ((%bp-blank-line-p content)
       (push (%bp-strip-columns content 4)
             (%bp-open-leaf-pending-lines leaf))
       1)
      ((>= (%bp-line-indent-columns content) 4)
       (setf (%bp-open-leaf-lines leaf)
             (append (list (%bp-strip-columns content 4))
                     (%bp-open-leaf-pending-lines leaf)
                     (%bp-open-leaf-lines leaf))
             (%bp-open-leaf-pending-lines leaf) nil
             (%bp-open-leaf-end-line leaf) physical-line)
       1)
      (t
       (%bp-state-close-leaf state)
       0))))

(defun %bp-state-process-table-line (state physical-line content)
  (let ((leaf (%bp-parser-state-leaf state)))
    (if (and (not (%bp-blank-line-p content))
             (or (find #\| (%block-line-text content))
                 (= (length (%bp-open-leaf-table-alignments leaf)) 1)))
        (progn
          (push (%bp-table-row
                 (%bp-parser-state-source-buffer state) content
                 (%bp-open-leaf-table-alignments leaf) nil
                 (%bp-open-leaf-context leaf))
                (%bp-open-leaf-table-rows leaf))
          (setf (%bp-open-leaf-end-line leaf) physical-line)
          1)
        (progn
          (%bp-state-close-leaf state)
          0))))

(defun %bp-state-process-leaf-line (state physical-line content)
  (case (%bp-open-leaf-kind (%bp-parser-state-leaf state))
    (:paragraph
     (%bp-state-process-paragraph-line state physical-line content))
    (:fenced
     (%bp-state-process-fenced-line state physical-line content))
    (:html
     (%bp-state-process-html-line state physical-line content))
    (:indented
     (%bp-state-process-indented-line state physical-line content))
    (:table
     (%bp-state-process-table-line state physical-line content))))

(defun %bp-state-lazy-paragraph-continuation-p (state content mismatch)
  (let* ((parent (and (eq (%bp-open-frame-kind mismatch) :item)
                      (%bp-state-frame-parent state mismatch)))
         (sibling (and parent
                       (eq (%bp-open-frame-kind parent) :list)
                       (%bp-state-sibling-record content parent)))
         (delimiter-change
           (and parent
                (eq (%bp-open-frame-kind parent) :list)
                (%bp-state-list-delimiter-change-p content parent))))
    (and mismatch
         (%bp-parser-state-leaf state)
         (eq (%bp-open-leaf-kind (%bp-parser-state-leaf state)) :paragraph)
         (not sibling)
         (not delimiter-change)
         (not (%bp-blank-line-p content))
         (not (%bp-interrupts-paragraph-p
               content (%bp-parser-state-gfm state))))))

(defun %bp-state-record-lazy-continuation (state physical-line)
  (dolist (frame (%bp-parser-state-stack state))
    (setf (%bp-open-frame-end-line frame) physical-line)))

(defun %bp-state-handle-mismatch (state physical-line content mismatch)
  (if (%bp-state-lazy-paragraph-continuation-p state content mismatch)
      (progn
        (%bp-state-record-lazy-continuation state physical-line)
        (%bp-state-mark-container-content state)
        (push content
              (%bp-open-leaf-lines (%bp-parser-state-leaf state)))
        (setf (%bp-open-leaf-end-line (%bp-parser-state-leaf state))
              physical-line)
        1)
      (progn
        (%bp-state-close-leaf state)
        (let* ((mismatch-kind (%bp-open-frame-kind mismatch))
               (parent (%bp-state-frame-parent state mismatch))
               (record (and (eq mismatch-kind :item)
                            parent
                            (eq (%bp-open-frame-kind parent) :list)
                            (%bp-state-sibling-record content parent))))
          (if record
              (progn
                (%bp-state-close-through state mismatch)
                (setf content
                      (%bp-state-open-item state physical-line content record))
                (%bp-state-open-new-block state physical-line content))
              (progn
                (%bp-state-close-through state mismatch)
                (when (eq mismatch-kind :item)
                  (%bp-state-close-top-frame state))
                0))))))

(defun %bp-state-parse-current-line (state physical-line)
  (%bp-container-transform-line
   physical-line (%bp-parser-state-stack state)
   (lambda (content)
     (if (%bp-parser-state-leaf state)
         (%bp-state-process-leaf-line state physical-line content)
         (%bp-state-open-new-block state physical-line content)))
   (lambda (content mismatch)
     (%bp-state-handle-mismatch state physical-line content mismatch))
   t))

(defun %bp-state-run (state)
  (let ((lines (%bp-parser-state-lines state)))
    (loop while (< (%bp-parser-state-cursor state) (length lines))
          for physical-line =
            (aref lines (%bp-parser-state-cursor state))
          do (incf (%bp-parser-state-cursor state)
                   (%bp-state-parse-current-line state physical-line)))))
