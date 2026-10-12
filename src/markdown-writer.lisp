;;;; src/markdown-writer.lisp
(in-package #:cl-markdown-kit)

(defvar *mw-line-start-p* t)
(defvar *mw-line-indent* 0)
(defvar *mw-table-cell-p* nil)
(defvar *mw-escape-table-pipes-p* nil)
(defvar *mw-text-autolink-breakpoints* nil)

(defun %mw-write-string (string stream)
  (write-string string stream)
  (loop for character across string
        do (cond
             ((char= character #\Newline)
              (setf *mw-line-start-p* t
                    *mw-line-indent* 0))
             ((and *mw-line-start-p*
                   (char= character #\Space))
              (incf *mw-line-indent*))
             ((and *mw-line-start-p*
                   (char= character #\Tab))
              (incf *mw-line-indent*
                    (- 4 (mod *mw-line-indent* 4))))
             (t
              (setf *mw-line-start-p* nil
                    *mw-line-indent* 0))))
  string)

(defun %mw-write-character (character stream)
  (%mw-write-string (string character) stream))

(defun %mw-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun %mw-line-end (text start)
  (or (position #\Newline text :start start)
      (length text)))

(defun %mw-longest-run (text character)
  (loop with longest = 0
        with index = 0
        while (< index (length text))
        do (if (char= (char text index) character)
               (let ((run-end
                       (loop for cursor from index below (length text)
                             while (char= (char text cursor) character)
                             finally (return cursor))))
                 (setf longest (max longest (- run-end index))
                       index run-end))
               (incf index))
        finally (return longest)))

(defun %mw-heading-marker-p (text start end)
  (let ((cursor start))
    (loop while (and (< cursor end) (char= (char text cursor) #\#))
          do (incf cursor))
    (and (> cursor start)
         (<= (- cursor start) 6)
         (or (= cursor end)
             (%mw-horizontal-space-p (char text cursor))))))

(defun %mw-list-marker-p (text start end)
  (when (< start end)
    (let ((character (char text start)))
      (or (and (find character '(#\- #\+ #\*))
               (or (= (1+ start) end)
                   (%mw-horizontal-space-p (char text (1+ start)))))
          (and (<= (char-code #\0) (char-code character)
                   (char-code #\9))
               (let ((cursor start))
                 (loop while (and (< cursor end)
                                  (<= (char-code #\0)
                                      (char-code (char text cursor))
                                      (char-code #\9)))
                       do (incf cursor))
                 (and (<= (- cursor start) 9)
                      (< cursor end)
                      (find (char text cursor) '(#\. #\)))
                      (or (= (1+ cursor) end)
                          (%mw-horizontal-space-p (char text (1+ cursor)))))))))))

(defun %mw-marker-line-p (text start end markers minimum)
  (let ((marker (and (< start end) (char text start)))
        (cursor start)
        (count 0))
    (when (and marker (find marker markers))
      (loop while (< cursor end)
            do (cond
                 ((char= (char text cursor) marker)
                  (incf count)
                  (incf cursor))
                 ((%mw-horizontal-space-p (char text cursor))
                  (incf cursor))
                 (t (return))))
      (and (>= count minimum) (= cursor end)))))

(defun %mw-setext-marker-line-p (text start end)
  (let ((cursor start)
        (count 0)
        (marker (and (< start end) (char text start))))
    (when (find marker '(#\= #\-))
      (loop while (< cursor end)
            do (cond
                 ((char= (char text cursor) marker)
                  (incf count)
                  (incf cursor))
                 ((%mw-horizontal-space-p (char text cursor))
                  (incf cursor))
                 (t (return))))
      (and (plusp count) (= cursor end)))))

(defun %mw-line-prefix-hazard-p (text start end)
  (or (%mw-heading-marker-p text start end)
      (%mw-list-marker-p text start end)
      (and (< start end)
           (char= (char text start) #\>))
      (%mw-marker-line-p text start end '(#\* #\_ #\-) 3)
      (%mw-setext-marker-line-p text start end)))

(defun %mw-entity-at-p (text index)
  (nth-value 0 (%ip-entity-at text index (length text))))

(defun %mw-autolink-breakpoints (text)
  (let ((breakpoints (make-hash-table :test #'eql))
        (index 0)
        (end (length text)))
    (loop while (< index end)
          do (let ((candidate (%ip-bare-candidate-at text index end)))
               (if candidate
                   (let* ((candidate-end (%ip-candidate-end candidate))
                          (breakpoint
                            (cond
                              ((%ip-ascii-prefix-p text index "www.")
                               (+ index 3))
                              ((or (%ip-ascii-prefix-p text index "http://")
                                   (%ip-ascii-prefix-p text index "https://")
                                   (%ip-ascii-prefix-p text index "mailto:")
                                   (%ip-ascii-prefix-p text index "xmpp:"))
                               (position #\: text :start index :end candidate-end))
                              (t
                               (position #\@ text :start index
                                         :end candidate-end)))))
                     (when breakpoint
                       (setf (gethash breakpoint breakpoints) t))
                     (setf index candidate-end))
                   (incf index))))
    breakpoints))

(defun %mw-write-character-reference (character stream)
  (%mw-write-string (format nil "&#~D;" (char-code character)) stream))

(defun %mw-escape-text-character-p (text index)
  (let ((character (char text index)))
    (or (find character '(#\\ #\` #\* #\_ #\[ #\] #\< #\~))
        (and (or *mw-table-cell-p* *mw-escape-table-pipes-p*)
             (char= character #\|))
        (and (char= character #\&)
             (%mw-entity-at-p text index))
        (and *mw-text-autolink-breakpoints*
             (gethash index *mw-text-autolink-breakpoints*))
        (and *mw-line-start-p*
             (<= *mw-line-indent* 3)
             (%mw-line-prefix-hazard-p
              text index (%mw-line-end text index))))))

(defun %mw-write-text (text stream)
  (let* ((end (length text))
         (index 0)
         (*mw-text-autolink-breakpoints* (%mw-autolink-breakpoints text)))
    (loop while (< index end)
          for character = (char text index)
          do (cond
               ((char= character #\Newline)
                (%mw-write-character character stream)
                (incf index))
               ((and (%mw-horizontal-space-p character)
                     (let ((line-end (%mw-line-end text index)))
                       (null (position-if-not #'%mw-horizontal-space-p text
                                              :start index :end line-end))))
                (let ((line-end (%mw-line-end text index)))
                  (loop while (< index line-end)
                        for current = (char text index)
                        do (%mw-write-string
                            (if (char= current #\Tab) "&#9;" "&#32;")
                            stream)
                           (incf index))))
               ((and *mw-line-start-p*
                     (%mw-horizontal-space-p character)
                     (let ((next-indent
                             (if (char= character #\Tab)
                                 (+ *mw-line-indent*
                                    (- 4 (mod *mw-line-indent* 4)))
                                 (1+ *mw-line-indent*))))
                       (>= next-indent 4)))
                (%mw-write-string
                 (if (char= character #\Tab) "&#9;" "&#32;")
                 stream)
                (incf index))
               ((%mw-escape-text-character-p text index)
                (cond
                  ((and *mw-line-start-p*
                        (digit-char-p character 10)
                        (<= *mw-line-indent* 3)
                        (%mw-line-prefix-hazard-p
                         text index (%mw-line-end text index)))
                   (%mw-write-character-reference character stream))
                  ((and (char= character #\\)
                         (< (1+ index) end)
                         (ascii-punctuation-p (char text (1+ index))))
                   (progn
                     (%mw-write-string "\\\\" stream)
                     (%mw-write-character character stream)))
                  (t
                   (%mw-write-character #\\ stream)
                   (%mw-write-character character stream)))
                (incf index))
               (t
                (%mw-write-character character stream)
                (incf index)))))
  text)

(defun %mw-render-inline-children (children stream)
  (dolist (child children)
    (%mw-render-inline-node child stream)))

(defun %mw-escape-label (label)
  (with-output-to-string (stream)
    (loop for character across label
          do (cond
               ((find character '(#\\ #\[ #\]))
                (write-char #\\ stream)
                (write-char character stream))
               ((char= character #\&)
                (write-string "&amp;" stream))
               ((char= character #\Newline)
                (write-char #\Space stream))
               (t
                (write-char character stream))))))

(defun %mw-write-link-string (text stream &key title-p)
  (loop with end = (length text)
        for index from 0 below end
        for character = (char text index)
        do (cond
             ((char= character #\\)
              (%mw-write-string "\\\\" stream))
             ((and title-p (char= character #\"))
              (%mw-write-string "\\\"" stream))
             ((and (not title-p) (find character '(#\< #\>)))
              (%mw-write-character #\\ stream)
              (%mw-write-character character stream))
             ((and (or *mw-table-cell-p* *mw-escape-table-pipes-p*)
                   (char= character #\|))
              (%mw-write-string "\\|" stream))
             ((and (char= character #\&)
                   (%mw-entity-at-p text index))
              (%mw-write-string "\\&" stream))
             ((and (not title-p) (char= character #\Newline))
              (%mw-write-string "&#10;" stream))
             (t
              (%mw-write-character character stream)))))

(defun %mw-write-link-destination (destination stream)
  (%mw-write-character #\< stream)
  (%mw-write-link-string destination stream)
  (%mw-write-character #\> stream))

(defun %mw-write-link-title (title stream)
  (when title
    (%mw-write-string " \"" stream)
    (%mw-write-link-string title stream :title-p t)
    (%mw-write-character #\" stream)))

(defun %mw-render-inline-node (node stream)
  (typecase node
    (text
     (%mw-write-text (text-value node) stream))
    (code-span
     (let* ((literal (substitute #\Space #\Newline
                                 (code-span-literal-text node)))
            (fence-length (1+ (%mw-longest-run literal #\`)))
            (fence (make-string (max 1 fence-length) :initial-element #\`))
            (pad-p
              (and (plusp (length literal))
                   (char= (char literal 0) #\Space)
                   (char= (char literal (1- (length literal))) #\Space)
                   (find-if-not (lambda (character)
                                  (char= character #\Space))
                                literal))))
       (%mw-write-string fence stream)
       (when pad-p (%mw-write-character #\Space stream))
       (%mw-write-string literal stream)
       (when pad-p (%mw-write-character #\Space stream))
       (%mw-write-string fence stream)))
    (html-inline
     (%mw-write-string (html-inline-body node) stream))
    (soft-break
     (%mw-write-character #\Newline stream))
    (hard-break
     (%mw-write-string (format nil "\\~%") stream))
    (emphasis
     (let ((delimiter (or (emphasis-delimiter-character node) #\*)))
       (%mw-write-character delimiter stream)
       (%mw-render-inline-children (emphasis-children node) stream)
       (%mw-write-character delimiter stream)))
    (strong
     (let ((delimiter (or (strong-delimiter-character node) #\*)))
       (%mw-write-character delimiter stream)
       (%mw-write-character delimiter stream)
       (%mw-render-inline-children (strong-children node) stream)
       (%mw-write-character delimiter stream)
       (%mw-write-character delimiter stream)))
    (strikethrough
     (%mw-write-string "~~" stream)
     (%mw-render-inline-children (strikethrough-children node) stream)
     (%mw-write-string "~~" stream))
    (link
     (%mw-write-character #\[ stream)
     (%mw-render-inline-children (link-children node) stream)
     (%mw-write-string "](" stream)
     (%mw-write-link-destination (link-destination node) stream)
     (%mw-write-link-title (link-title node) stream)
     (%mw-write-character #\) stream))
    (image
     (%mw-write-string "![" stream)
     (%mw-render-inline-children (image-children node) stream)
     (%mw-write-string "](" stream)
     (%mw-write-link-destination (image-destination node) stream)
     (%mw-write-link-title (image-title node) stream)
     (%mw-write-character #\) stream))
    (autolink
     (%mw-write-character #\[ stream)
     (%mw-write-text (autolink-display-text node) stream)
     (%mw-write-string "](" stream)
     (%mw-write-link-destination (autolink-destination node) stream)
     (%mw-write-character #\) stream))
    (footnote-reference
     (%mw-write-string "[^" stream)
     (%mw-write-string
      (%mw-escape-label (footnote-reference-normalized-label node))
      stream)
     (%mw-write-character #\] stream))
    (t
     (error "Unsupported inline node ~S." (type-of node)))))

(defun %mw-render-inline-string (children &key table-cell-p escape-table-pipes-p)
  (let ((*mw-line-start-p* t)
        (*mw-line-indent* 0)
        (*mw-table-cell-p* table-cell-p)
        (*mw-escape-table-pipes-p* escape-table-pipes-p))
    (with-output-to-string (stream)
      (%mw-render-inline-children children stream))))

(defun %mw-table-cell-ranges (line)
  (%bp-table-cells (%make-block-line line #() 0 0 0 0)))

(defun %mw-table-alignment-line-p (line)
  (let ((cells (%mw-table-cell-ranges line)))
    (when (and cells
               (every (lambda (range)
                        (let* ((cell (%bp-trimmed-string
                                      (subseq line (car range) (cdr range))))
                               (length (length cell))
                               (start (if (and (plusp length)
                                               (char= (char cell 0) #\:))
                                          1 0))
                               (end (if (and (> length start)
                                             (char= (char cell (1- length)) #\:))
                                        (1- length)
                                        length)))
                          (and (< start end)
                               (loop for index from start below end
                                     always (char= (char cell index) #\-)))))
                      cells))
      (length cells))))

(defun %mw-table-start-follows-p (text)
  (let ((lines nil)
        (start 0)
        (table-start-p nil))
    (loop for end = (position #\Newline text :start start)
          do (push (cons start (or end (length text))) lines)
          while end
          do (setf start (1+ end)))
    (setf lines (nreverse lines))
    (loop for tail on lines
          for line = (car tail)
          for next = (cadr tail)
          while next
          do (let* ((next-start (car next))
                    (next-end (cdr next))
                    (first-non-space
                      (or (position-if-not #'%mw-horizontal-space-p
                                           text :start next-start
                                           :end next-end)
                          next-end))
                    (header-has-pipe
                      (position #\| text :start (car line) :end (cdr line))))
               (when (and header-has-pipe
                          (= (length (%mw-table-cell-ranges
                                      (subseq text (car line) (cdr line))))
                             (or (%mw-table-alignment-line-p
                                  (subseq text next-start next-end))
                                 0))
                          (< first-non-space next-end))
                 (setf table-start-p t))))
    table-start-p))

(defun %mw-render-paragraph (node)
  (let* ((children (paragraph-children node))
         (text (%mw-render-inline-string children)))
    (if (%mw-table-start-follows-p text)
        (%mw-render-inline-string children :escape-table-pipes-p t)
        text)))

(defun %mw-render-blocks (nodes &optional (separator (format nil "~%~%")))
  (with-output-to-string (stream)
    (loop for node in nodes
          for first-p = t then nil
          do (unless first-p (write-string separator stream))
             (write-string (%mw-render-block node) stream))))

(defun %mw-prefix-lines (text prefix)
  (if (zerop (length text))
      text
      (with-output-to-string (stream)
        (write-string prefix stream)
        (loop for index from 0 below (length text)
              for character = (char text index)
              do (write-char character stream)
                 (when (and (char= character #\Newline)
                            (< (1+ index) (length text)))
                   (write-string prefix stream))))))

(defun %mw-prefix-continuation-lines (text prefix)
  (with-output-to-string (stream)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (write-char character stream)
             (when (and (char= character #\Newline)
                        (< (1+ index) (length text)))
               (write-string prefix stream)))))

(defun %mw-render-code-block (node)
  (let* ((body (code-block-body node))
         (info (or (code-block-info-string node) ""))
         (fence-character (if (find #\` info) #\~ #\`))
         (fence-length
           (max 3
                (1+ (%mw-longest-run body fence-character))))
         (fence (make-string fence-length :initial-element fence-character)))
    (with-output-to-string (stream)
      (write-string fence stream)
      (unless (zerop (length info))
        (write-char #\Space stream)
        (write-string info stream))
      (write-char #\Newline stream)
      (write-string body stream)
      (when (or (zerop (length body))
                (char= (char body (1- (length body))) #\Newline))
        (write-string fence stream)))))

(defun %mw-list-marker (list-node index)
  (if (eq (list-node-kind list-node) :unordered)
      "- "
      (format nil "~D. "
              (min 999999999
                   (+ (list-node-start-number list-node) index)))))

(defun %mw-render-list (node)
  (let ((items (list-node-items node))
        (tight-p (list-node-tight-p node)))
    (with-output-to-string (stream)
      (loop for item in items
            for index from 0
            for marker = (%mw-list-marker node index)
            for children = (node-children item)
            for task-p = (task-list-item-p item)
            for task-marker = (and task-p
                                   (if (task-list-item-checked-p item)
                                       "[x] "
                                       "[ ] "))
            do (when (plusp index)
                 (write-string (if tight-p (format nil "~%")
                                   (format nil "~%~%"))
                               stream))
               (write-string marker stream)
               (cond
                 ((and task-p children
                       (paragraph-p (first children)))
                  (write-string task-marker stream)
                  (write-string
                   (%mw-prefix-continuation-lines
                    (%mw-render-blocks
                     children
                     (if tight-p (format nil "~%")
                         (format nil "~%~%")))
                    (make-string (length marker) :initial-element #\Space))
                   stream))
                 (task-p
                  (write-string (string-right-trim '(#\Space) task-marker)
                                stream)
                  (when children
                    (write-char #\Newline stream)
                    (write-string
                     (%mw-prefix-lines
                      (%mw-render-blocks
                       children
                       (if tight-p (format nil "~%")
                           (format nil "~%~%")))
                      (make-string (length marker) :initial-element #\Space))
                     stream)))
                 (children
                  (write-string
                   (%mw-prefix-continuation-lines
                    (%mw-render-blocks
                     children
                     (if tight-p (format nil "~%")
                         (format nil "~%~%")))
                    (make-string (length marker) :initial-element #\Space))
                   stream)))))))

(defun %mw-render-table-row (row)
  (with-output-to-string (stream)
    (write-char #\| stream)
    (dolist (cell (table-row-cells row))
      (write-char #\Space stream)
      (write-string (%mw-render-inline-string (table-cell-children cell)
                                               :table-cell-p t)
                    stream)
      (write-string " |" stream))))

(defun %mw-alignment-marker (alignment)
  (case alignment
    (:left ":---")
    (:right "---:")
    (:center ":---:")
    (otherwise "---")))

(defun %mw-render-table (node)
  (let ((rows (table-rows node))
        (alignments (table-alignments node)))
    (if (null rows)
        ""
        (with-output-to-string (stream)
          (write-string (%mw-render-table-row (first rows)) stream)
          (write-char #\Newline stream)
          (write-char #\| stream)
          (loop for cell in (table-row-cells (first rows))
                for alignment in alignments
                do (write-char #\Space stream)
                   (write-string (%mw-alignment-marker alignment) stream)
                   (write-string " |" stream))
          (dolist (row (rest rows))
            (write-char #\Newline stream)
            (write-string (%mw-render-table-row row) stream))))))

(defun %mw-normalize-newlines (text)
  (with-output-to-string (stream)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (unless (and (< (1+ index) (length text))
                             (char= (char text (1+ index)) #\Newline))
                  (write-char #\Newline stream)))
               (t
                (write-char character stream))))))

(defun %mw-render-block (node)
  (let ((*mw-line-start-p* t)
        (*mw-line-indent* 0)
        (*mw-table-cell-p* nil))
    (with-output-to-string (stream)
      (typecase node
        (paragraph
         (write-string (%mw-render-paragraph node) stream))
        (heading
         (write-string (make-string (heading-level node)
                                    :initial-element #\#)
                       stream)
         (unless (null (heading-children node))
           (write-char #\Space stream)
           (%mw-render-inline-children (heading-children node) stream)))
        (thematic-break
         (write-string "---" stream))
        (code-block
         (write-string (%mw-render-code-block node) stream))
        (html-block
         (write-string (html-block-body node) stream))
        (block-quote
         (write-string
          (%mw-prefix-lines (%mw-render-blocks (block-quote-children node))
                            "> ")
          stream))
        (list-node
         (write-string (%mw-render-list node) stream))
        (table
         (write-string (%mw-render-table node) stream))
        (link-reference-definition
         (write-char #\[ stream)
         (write-string
          (%mw-escape-label
           (let ((normalized
                   (link-reference-definition-normalized-label node)))
             (if (plusp (length normalized))
                 normalized
                 (link-reference-definition-raw-label node))))
          stream)
         (write-string "]: " stream)
         (%mw-write-link-destination
          (link-reference-definition-destination node) stream)
         (%mw-write-link-title (link-reference-definition-title node) stream))
        (footnote-definition
         (write-string "[^" stream)
         (write-string
          (%mw-escape-label
           (footnote-definition-normalized-label node))
          stream)
         (write-string "]:" stream)
         (unless (null (footnote-definition-children node))
           (write-char #\Newline stream)
           (write-string
            (%mw-prefix-lines
             (%mw-render-blocks (footnote-definition-children node))
             "    ")
            stream)))
        (front-matter
         (let ((raw-text (%mw-normalize-newlines
                          (front-matter-raw-text node))))
           (write-string "---" stream)
           (write-char #\Newline stream)
           (write-string raw-text stream)
           (unless (or (zerop (length raw-text))
                       (char= (char raw-text (1- (length raw-text)))
                              #\Newline))
             (write-char #\Newline stream))
           (write-string "---" stream)))
        (t
         (error "Unsupported block node ~S." (type-of node)))))))

(defun write-markdown-to-stream (document stream)
  (check-type document document)
  (unless (output-stream-p stream)
    (error 'type-error :datum stream
           :expected-type '(satisfies output-stream-p)))
  (write-string (%mw-render-blocks (document-children document)) stream))

(defun write-markdown (document)
  (with-output-to-string (stream)
    (write-markdown-to-stream document stream)))
