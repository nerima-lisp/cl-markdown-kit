(in-package #:cl-markdown-kit)

(defun %mw-table-alignment-line-p (line)
  (let ((cells (%pc-table-cells line)))
    (when
        (and cells
             (every
              (lambda (range)
                (let* ((cell (%pc-trimmed-string (subseq line (car range) (cdr range))))
                       (length (length cell))
                       (start
                        (if (and (plusp length) (char= (char cell 0) #\:))
                            1
                            0))
                       (end
                        (if (and (> length start) (char= (char cell (1- length)) #\:))
                            (1- length)
                            length)))
                  (and (< start end)
                       (loop for index from start below
                             end
                             always (char= (char cell index) #\-)))))
              cells))
      (length cells))))

(defun %mw-table-alignment-breaker-position (text)
  (let ((lines nil)
        (start 0)
        (alignment-breaker-position nil))
    (loop for
          end = (position #\Newline text :start start)
          do (push (cons start (or end (length text))) lines)
          while
          end
          do (setf start (1+ end)))
    (setf lines (nreverse lines))
    (loop for tail on lines
          for line = (car tail)
          for next = (cadr tail)
          while next
          do (let* ((next-start (car next))
                    (next-end (cdr next))
                    (first-non-space
                     (or
                      (position-if-not #'%pc-space-p text :start next-start :end next-end)
                      next-end))
                    (header-has-pipe
                     (position #\| text :start (car line) :end (cdr line))))
               (when
                   (and header-has-pipe
                        (= (length (%pc-table-cells (subseq text (car line) (cdr line))))
                           (or
                            (%mw-table-alignment-line-p (subseq text next-start next-end))
                            0))
                        (< first-non-space next-end))
                 (setf alignment-breaker-position (or
                                                   (position #\|
                                                             text
                                                             :start
                                                             next-start
                                                             :end
                                                             next-end)
                                                   (position #\:
                                                             text
                                                             :start
                                                             next-start
                                                             :end
                                                             next-end)
                                                   (position #\-
                                                             text
                                                             :start
                                                             next-start
                                                             :end
                                                             next-end)))
                 (return))))
    alignment-breaker-position))

(defun %mw-render-paragraph (node results)
  (let* ((children (paragraph-children node))
         (text (%mw-render-child-results children results))
         (alignment-breaker-position (%mw-table-alignment-breaker-position text)))
    (if alignment-breaker-position
        (concatenate 'string
                     (subseq text 0 alignment-breaker-position)
                     "\\"
                     (subseq text alignment-breaker-position))
        text)))

(defun %mw-render-blocks (nodes results &optional (separator (format nil "~%~%")))
  (with-output-to-string (stream)
    (loop with first-node-p = t
          for node in nodes
          do (unless first-node-p
               (write-string separator stream))
             (setf first-node-p nil)
             (write-string (or (gethash node results) "") stream))))

(defun %mw-prefix-lines (text prefix)
  (if (zerop (length text))
      text
      (with-output-to-string (stream)
        (write-string prefix stream)
        (loop for index from 0 below (length text)
              for character = (char text index)
              do (write-char character stream) (when
                                                   (and (char= character #\Newline)
                                                        (< (1+ index) (length text)))
                                                 (write-string prefix stream))))))

(defun %mw-prefix-continuation-lines (text prefix)
  (with-output-to-string (stream)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (write-char character stream) (when
                                               (and (char= character #\Newline)
                                                    (< (1+ index) (length text))
                                                    (not
                                                     (char= (char text (1+ index))
                                                            #\Newline)))
                                             (write-string prefix stream)))))

(defun %mw-render-code-block (node)
  (let* ((body (code-block-body node))
         (info (or (code-block-info-string node) ""))
         (fence-character
          (if (find #\` info)
              #\~
              #\`))
         (fence-length (max 3 (1+ (%mw-longest-run body fence-character))))
         (fence (make-string fence-length :initial-element fence-character)))
    (with-output-to-string (stream)
      (write-string fence stream)
      (unless (zerop (length info))
        (write-char #\Space stream)
        (write-string info stream))
      (write-char #\Newline stream)
      (write-string body stream)
      (when
          (or (zerop (length body))
              (char= (char body (1- (length body))) #\Newline)
              *mw-following-block-p*)
        (unless
            (or (zerop (length body)) (char= (char body (1- (length body))) #\Newline))
          (write-char #\Newline stream))
        (write-string fence stream)))))

(defun %mw-list-marker (list-node index)
  (if (eq (list-node-kind list-node) :unordered)
      (format nil "~C " (or (list-node-marker-character list-node) #\-))
      (format nil "~D~C "
              (min 999999999 (+ (list-node-start-number list-node) index))
              (or (list-node-delimiter-character list-node) #\.))))

(defun %mw-trailing-newline-count (text)
  (loop for index downfrom (1- (length text)) to 0
        while (char= (char text index) #\Newline)
        count t))

(defun %mw-render-list-item-content (item results child-separator continuation-prefix)
  (let* ((children (node-children item))
         (task-p (task-list-item-p item))
         (child-markdown
           (when children
             (%mw-render-blocks children results child-separator))))
    (cond
      ((and task-p children (paragraph-p (first children)))
       (%mw-prefix-continuation-lines child-markdown continuation-prefix))
      (task-p
       (if children
           (concatenate 'string
                        (format nil "~%")
                        (%mw-prefix-lines child-markdown continuation-prefix))
           ""))
      (children
       (%mw-prefix-continuation-lines child-markdown continuation-prefix))
      (t ""))))

(defun %mw-write-list-item-marker (stream marker item task-marker)
  (write-string marker stream)
  (when task-marker
    (let ((children (node-children item)))
      (write-string
       (if (and children (paragraph-p (first children)))
           task-marker
           (string-right-trim (list #\Space) task-marker))
       stream))))

(defun %mw-write-list-item-separator (stream separator-count previous-content)
  (loop repeat
        (max 0
             (- separator-count
                (%mw-trailing-newline-count previous-content)))
        do (write-char #\Newline stream)))

(defun %mw-render-list-item-result (context item results)
  (let* ((list-node (%mw-render-session-parent context))
         (separator
           (if (list-node-tight-p list-node)
               (format nil "~%")
               (format nil "~%~%")))
         (marker
           (%mw-list-marker list-node
                            (%mw-render-session-child-index context)))
         (continuation-prefix
           (make-string (length marker) :initial-element #\Space)))
    (%mw-render-list-item-content item results separator continuation-prefix)))

(defun %mw-render-list (node results)
  (let* ((items (list-node-items node))
         (separator-count (if (list-node-tight-p node) 1 2)))
    (with-output-to-string (stream)
      (let ((previous-content ""))
        (loop for item in items
              for index from 0
              for marker = (%mw-list-marker node index)
              for continuation-prefix = (make-string
                                         (length marker)
                                         :initial-element #\Space)
              for task-marker = (when (task-list-item-p item)
                                  (if (task-list-item-checked-p item)
                                      "[x] "
                                      "[ ] "))
              for content = (or (gethash item results) "")
              do (when (plusp index)
                   (%mw-write-list-item-separator
                    stream
                    separator-count
                    previous-content))
                 (%mw-write-list-item-marker
                  stream
                  marker
                  item
                  task-marker)
                 (write-string content stream)
                 (setf previous-content content))))))

(defun %mw-render-table-row (row results)
  (with-output-to-string (stream)
    (write-char #\| stream)
    (dolist (cell (table-row-cells row))
      (write-char #\Space stream)
      (write-string (or (gethash cell results) "") stream)
      (write-string " |" stream))))

(defun %mw-alignment-marker (alignment)
  (case alignment
    (:left ":---")
    (:right "---:")
    (:center ":---:")
    (otherwise "---")))

(defun %mw-render-table (node results)
  (let ((rows (table-rows node))
        (alignments (table-alignments node)))
    (if (null rows)
        ""
        (with-output-to-string (stream)
          (write-string (or (gethash (first rows) results) "") stream)
          (write-char #\Newline stream)
          (write-char #\| stream)
          (loop for cell in (table-row-cells (first rows))
                for alignment in alignments
                do (write-char #\Space stream)
                   (write-string (%mw-alignment-marker alignment) stream)
                   (write-string " |" stream))
          (dolist (row (rest rows))
            (write-char #\Newline stream)
            (write-string (or (gethash row results) "") stream))))))

(defun %mw-normalize-newlines (text)
  (with-output-to-string (stream)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (unless
                    (and (< (1+ index) (length text))
                         (char= (char text (1+ index)) #\Newline))
                  (write-char #\Newline stream)))
               (t (write-char character stream))))))

(defun %mw-render-block-result (context node frame kind)
  (let ((results (%mw-render-session-results context)))
    (ecase kind
      (:document nil)
      (:front-matter
       (let ((raw-text (%mw-normalize-newlines (front-matter-raw-text node))))
         (with-output-to-string (stream)
           (write-string "---" stream)
           (write-char #\Newline stream)
           (write-string raw-text stream)
           (unless
               (or (zerop (length raw-text))
                   (char= (char raw-text (1- (length raw-text))) #\Newline))
             (write-char #\Newline stream))
           (write-string "---" stream))))
      (:block-quote
       (let ((children (block-quote-children node)))
         (if (null children)
             ">"
             (%mw-prefix-lines (%mw-render-blocks children results) "> "))))
      (:list-node (%mw-render-list node results))
      ((:list-item :task-list-item) (%mw-render-list-item-result context node results))
      (:paragraph (%mw-render-paragraph node results))
      (:heading
       (concatenate 'string
                    (%mw-render-frame-prefix frame)
                    (%mw-render-child-results (heading-children node) results)))
      (:thematic-break "* * *")
      (:code-block
       (%mw-capture-render-frame frame
                                 (lambda (stream)
                                   (%mw-write-string (%mw-render-code-block node) stream))))
      (:html-block (html-block-body node))
      (:link-reference-definition
       (%mw-capture-render-frame frame
                                 (lambda (stream)
                                   (%mw-write-character #\[ stream)
                                   (%mw-write-string
                                    (%mw-escape-label
                                     (let ((normalized
                                            (link-reference-definition-normalized-label
                                             node)))
                                       (if (plusp (length normalized))
                                           normalized
                                           (link-reference-definition-raw-label node))))
                                    stream)
                                   (%mw-write-string "]: " stream)
                                   (%mw-write-link-destination
                                    (link-reference-definition-destination node)
                                    stream)
                                   (%mw-write-link-title
                                    (link-reference-definition-title node)
                                    stream))))
      (:table (%mw-render-table node results))
      (:table-row (%mw-render-table-row node results))
      (:table-cell (%mw-render-child-results (table-cell-children node) results))
      (:footnote-definition
       (with-output-to-string (stream)
         (write-string "[^" stream)
         (write-string (%mw-escape-label (footnote-definition-normalized-label node))
                       stream)
         (write-string "]:" stream)
         (unless (null (footnote-definition-children node))
           (write-char #\Newline stream)
           (write-string
            (%mw-prefix-lines
             (%mw-render-blocks (footnote-definition-children node) results)
             "    ")
            stream)))))))

(defun %mw-render-block-dispatch-event (context node phase kind)
  (let ((frame (gethash node (%mw-render-session-frames context))))
    (case phase
      (:enter
       (when (heading-p node)
         (let ((prefix
                 (concatenate 'string
                              (make-string (heading-level node) :initial-element #\#)
                              (if (null (heading-children node))
                                  ""
                                  " "))))
           (setf (%mw-render-frame-prefix frame) prefix)
           (%mw-advance-render-frame frame prefix)))
       :descend)
      (:leave
       (let* ((result
                (%mw-render-block-result context node frame kind))
              (parent (%mw-render-session-parent context)))
         (cond
           ((document-p node) nil)
           ((and parent (document-p parent))
            (let ((stream (%mw-render-session-output-stream context)))
              (when (%mw-render-session-previous-sibling context)
                (write-char #\Newline stream)
                (write-char #\Newline stream))
              (write-string (or result "") stream))
            (%mw-release-render-results context (node-children node)))
           (t
            (%mw-store-render-result
             context node (node-children node) result)))
         nil)))))

(define-node-dispatch (%mw-render-block-event (context node phase))
                      :operation
                      :markdown-renderer
                      :family
                      :block
                      (document
                       (%mw-render-block-dispatch-event context node phase :document))
                      (front-matter
                       (%mw-render-block-dispatch-event context node phase :front-matter))
                      (block-quote
                       (%mw-render-block-dispatch-event context node phase :block-quote))
                      (list-node
                       (%mw-render-block-dispatch-event context node phase :list-node))
                      (task-list-item
                       (%mw-render-block-dispatch-event context
                                                        node
                                                        phase
                                                        :task-list-item))
                      (list-item
                       (%mw-render-block-dispatch-event context node phase :list-item))
                      (paragraph
                       (%mw-render-block-dispatch-event context node phase :paragraph))
                      (heading
                       (%mw-render-block-dispatch-event context node phase :heading))
                      (thematic-break
                       (%mw-render-block-dispatch-event context
                                                        node
                                                        phase
                                                        :thematic-break))
                      (code-block
                       (%mw-render-block-dispatch-event context node phase :code-block))
                      (html-block
                       (%mw-render-block-dispatch-event context node phase :html-block))
                      (link-reference-definition
                       (%mw-render-block-dispatch-event context
                                                        node
                                                        phase
                                                        :link-reference-definition))
                      (table (%mw-render-block-dispatch-event context node phase :table))
                      (table-row
                       (%mw-render-block-dispatch-event context node phase :table-row))
                      (table-cell
                       (%mw-render-block-dispatch-event context node phase :table-cell))
                      (footnote-definition
                       (%mw-render-block-dispatch-event context
                                                        node
                                                        phase
                                                        :footnote-definition)))

(progn
  (defun %mw-verify-node-dispatches ()
    (verify-node-dispatches
     :operation :markdown-renderer
     :families '(:block :inline)))
  (verify-node-dispatches-at-compile-time
   :markdown-renderer
   (:block :inline)))
