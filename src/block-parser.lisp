;;;; src/block-parser.lisp
(in-package #:cl-markdown-kit)

(defstruct (%block-line
             (:constructor %make-block-line
                 (text boundaries raw-start raw-end after column-start)))
  (text "" :type string)
  (boundaries #() :type vector)
  (raw-start 0 :type (integer 0 *))
  (raw-end 0 :type (integer 0 *))
  (after 0 :type (integer 0 *))
  (column-start 0 :type (integer 0 *)))

(defstruct (%bp-open-frame
             (:constructor %bp-make-open-frame
                 (&key kind node context child-context start-line start-position
                       end-line marker-kind marker-delimiter marker-columns
                       content-indent children items blank-seen has-content)))
  kind node context child-context start-line start-position end-line
  marker-kind marker-delimiter marker-columns content-indent
  (children nil) (items nil) (blank-seen nil) (has-content nil))

(defstruct (%bp-open-leaf
             (:constructor %bp-make-open-leaf
                 (&key kind context start-line start-position lines end-line
                       fence-character fence-length indentation info-string
                       html-kind html-terminator pending-lines table-node
                       table-alignments table-rows)))
  kind context start-line start-position end-line
  (lines nil) fence-character fence-length indentation info-string
  html-kind html-terminator (pending-lines nil) table-node table-alignments
  (table-rows nil))

(declaim (special *block-inline-fragments* *block-reference-index*))
(declaim (ftype function %bp-parse-lines %bp-check-depth))
(declaim (ftype function parse-inline))

(defun %bp-space-p (character)
  (or (char= character #\Space) (char= character #\Tab)))

(defun %bp-line-boundary (line index)
  (aref (%block-line-boundaries line) index))

(defun %bp-array-slice-view (array start end)
  (let ((base array)
        (offset start))
    (loop
      (multiple-value-bind (displaced-to displacement-offset)
          (array-displacement base)
        (unless displaced-to (return))
        (setf base displaced-to
              offset (+ offset displacement-offset))))
    (make-array (- end start)
                :element-type (array-element-type base)
                :displaced-to base
                :displaced-index-offset offset)))

(defun %bp-line-column-at (line index)
  (loop with column = (%block-line-column-start line)
        for offset from 0 below index
        for character = (char (%block-line-text line) offset)
        do (incf column
                  (if (char= character #\Tab)
                      (- 4 (mod column 4))
                      1))
        finally (return column)))

(defun %bp-line-slice (line start &optional (end (length (%block-line-text line))))
  (%make-block-line
   (%bp-array-slice-view (%block-line-text line) start end)
   (%bp-array-slice-view (%block-line-boundaries line) start (1+ end))
   (%block-line-raw-start line)
   (%block-line-raw-end line)
   (%block-line-after line)
   (%bp-line-column-at line start)))

(defun %bp-split-lines (text)
  (let ((lines nil)
        (start 0)
        (length (length text)))
    (loop for index from 0 below length
          when (char= (char text index) #\Newline)
            do (let* ((boundaries
                        (make-array (1+ (- index start))))
                      (line-text (subseq text start index)))
                 (loop for offset from start to index
                       for boundary-index from 0
                       do (setf (aref boundaries boundary-index) offset))
                 (push (%make-block-line line-text boundaries start index
                                         (1+ index) 0)
                       lines)
                 (setf start (1+ index))))
    (let* ((boundaries (make-array (1+ (- length start))))
           (line-text (subseq text start length)))
      (loop for offset from start to length
            for boundary-index from 0
            do (setf (aref boundaries boundary-index) offset))
      (push (%make-block-line line-text boundaries start length length 0) lines))
    (coerce (nreverse lines) 'vector)))

(defun %bp-leading-indent (text &optional (limit 3) (initial-column 0))
  (let ((index 0)
        (column initial-column))
    (loop while (< index (length text))
          for character = (char text index)
          while (%bp-space-p character)
          for next-column =
            (if (char= character #\Tab)
                (+ column (- 4 (mod column 4)))
                (1+ column))
          while (<= (- next-column initial-column) limit)
          do (setf column next-column)
             (incf index))
    (values index (- column initial-column))))

(defun %bp-line-leading-indent (line &optional (limit 3))
  (%bp-leading-indent (%block-line-text line) limit
                      (%block-line-column-start line)))

(defun %bp-indent-columns (text &optional (end (length text)) (initial-column 0))
  (loop with column = initial-column
        for index from 0 below end
        for character = (char text index)
        while (%bp-space-p character)
        do (setf column
                 (if (char= character #\Tab)
                     (+ column (- 4 (mod column 4)))
                     (1+ column)))
        finally (return (- column initial-column))))

(defun %bp-line-indent-columns (line)
  (%bp-indent-columns (%block-line-text line) (length (%block-line-text line))
                      (%block-line-column-start line)))

(defun %bp-whitespace-width (text start end initial-column)
  (loop with column = initial-column
        for index from start below end
        for character = (char text index)
        do (setf column
                 (+ column (if (char= character #\Tab)
                               (- 4 (mod column 4))
                               1)))
        finally (return (- column initial-column))))

(defun %bp-strip-columns (line columns)
  "Remove COLUMNS visual indentation columns while preserving source mapping."
  (let* ((text (%block-line-text line))
         (index 0)
         (column (%block-line-column-start line))
         (partial-spaces 0))
    (loop while (and (< index (length text))
                     (< (- column (%block-line-column-start line)) columns)
                     (%bp-space-p (char text index)))
          for character = (char text index)
          for width = (if (char= character #\Tab)
                          (- 4 (mod column 4))
                          1)
          do (incf index)
             (if (> (- (+ column width) (%block-line-column-start line))
                    columns)
                 (setf partial-spaces
                       (- (+ column width)
                          (%block-line-column-start line) columns)
                       column (+ (%block-line-column-start line) columns))
                 (incf column width)))
    (if (zerop partial-spaces)
        (%bp-line-slice line index)
        (let* ((remaining (subseq text index))
               (new-text (concatenate 'string
                                      (make-string partial-spaces
                                                   :initial-element #\Space)
                                      remaining))
               (new-boundaries (make-array (1+ (length new-text))))
               (tab-start (%bp-line-boundary line (1- index)))
               (tab-end (%bp-line-boundary line index)))
          (setf (aref new-boundaries 0) tab-start)
          (loop for offset from 1 to partial-spaces
                do (setf (aref new-boundaries offset)
                         (if (= offset partial-spaces) tab-end tab-start)))
          (loop for offset from 1 to (length remaining)
                do (setf (aref new-boundaries (+ partial-spaces offset))
                         (%bp-line-boundary line (+ index offset))))
          (%make-block-line new-text new-boundaries
                            (%block-line-raw-start line)
                            (%block-line-raw-end line)
                            (%block-line-after line)
                            (+ (%block-line-column-start line) columns))))))

(defun %bp-strip-up-to-three (line)
  (multiple-value-bind (index columns)
      (%bp-leading-indent (%block-line-text line) 3
                          (%block-line-column-start line))
    (declare (ignore columns))
    (%bp-line-slice line index)))

(defun %bp-blank-line-p (line)
  (every #'%bp-space-p (%block-line-text line)))

(defun %bp-trim-indices (text start end)
  (loop while (and (< start end) (%bp-space-p (char text start)))
        do (incf start))
  (loop while (and (< start end) (%bp-space-p (char text (1- end))))
        do (decf end))
  (values start end))

(defun %bp-trimmed-string (text)
  (multiple-value-bind (start end) (%bp-trim-indices text 0 (length text))
    (subseq text start end)))

(defun %bp-line-span (source-buffer line start end)
  (source-buffer-span source-buffer
                      (%bp-line-boundary line start)
                      (%bp-line-boundary line end)))

(defun %bp-lines-span (source-buffer lines start-position end-position)
  (source-buffer-span
   source-buffer
   (%bp-line-boundary (aref lines 0) start-position)
   (%bp-line-boundary (aref lines (1- (length lines))) end-position)))

(defun %bp-source-boundary-at (source-buffer normalized-index)
  (boundary-map-boundary (source-buffer-boundary-map source-buffer)
                         normalized-index))

(defun %bp-add-fragment (source-buffer owner lines)
  (let* ((characters (make-array 0 :element-type 'character
                                 :adjustable t :fill-pointer 0))
         (boundaries (make-array 0 :adjustable t :fill-pointer 0))
         (source (source-buffer-source source-buffer)))
    (declare (ignore source))
    (when lines
      (vector-push-extend
       (%bp-source-boundary-at source-buffer
                               (%bp-line-boundary (first lines) 0))
       boundaries)
      (loop for tail on lines
            for line = (car tail)
            for text = (%block-line-text line)
            do (loop for index from 0 below (length text)
                     do (vector-push-extend (char text index) characters)
                        (vector-push-extend
                         (%bp-source-boundary-at
                          source-buffer (%bp-line-boundary line (1+ index)))
                         boundaries))
               (when (cdr tail)
                 (vector-push-extend #\Newline characters)
                 (vector-push-extend
                  (%bp-source-boundary-at
                   source-buffer (%bp-line-boundary (cadr tail) 0))
                  boundaries)))
      (let* ((text (coerce characters 'string))
             (boundary-map
               (make-boundary-map (source-buffer-source source-buffer)
                                  boundaries))
             (fragment
               (funcall (symbol-function 'make-inline-fragment)
                        :node owner :text text :boundary-map boundary-map)))
        (push fragment *block-inline-fragments*)))
    owner))

(defun %bp-footnote-index-key (normalized-label)
  (cons :footnote normalized-label))

(defun %bp-prefix-p (string prefix &optional (start 0) (case-insensitive nil))
  (and (<= (+ start (length prefix)) (length string))
       (loop for offset from 0 below (length prefix)
             for left = (char string (+ start offset))
             for right = (char prefix offset)
             always (if case-insensitive
                        (char-equal left right)
                        (char= left right)))))

(defun %bp-skip-whitespace (text index)
  (loop while (and (< index (length text))
                   (%bp-space-p (char text index)))
        do (incf index)
        finally (return index)))

(defun %bp-link-whitespace-p (character)
  (unicode-whitespace-p character))

(defun %bp-link-skip-whitespace (text index)
  (loop while (and (< index (length text))
                   (%bp-link-whitespace-p (char text index)))
        do (incf index)
        finally (return index)))

(defun %bp-join-lines (lines)
  (let ((characters (make-array 0 :element-type 'character
                                :adjustable t :fill-pointer 0))
        (boundaries (make-array 0 :adjustable t :fill-pointer 0)))
    (when lines
      (vector-push-extend (%bp-line-boundary (aref lines 0) 0) boundaries)
      (loop for index from 0 below (length lines)
            for line = (aref lines index)
            for text = (%block-line-text line)
            do (loop for offset from 0 below (length text)
                     do (vector-push-extend (char text offset) characters)
                        (vector-push-extend (%bp-line-boundary line (1+ offset))
                                            boundaries))
               (when (< index (1- (length lines)))
                 (vector-push-extend #\Newline characters)
                 (vector-push-extend
                  (%bp-line-boundary (aref lines (1+ index)) 0)
                  boundaries)))
      (%make-block-line (coerce characters 'string)
                        boundaries
                        (%block-line-raw-start (aref lines 0))
                        (%block-line-raw-end (aref lines (1- (length lines))))
                        (%block-line-after (aref lines (1- (length lines))))
                        (%block-line-column-start (aref lines 0))))))

(defun %bp-unescape-punctuation (text)
  (with-output-to-string (output)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (if (and (char= character #\\)
                      (< (1+ index) (length text))
                      (ascii-punctuation-p (char text (1+ index))))
                 (progn (write-char (char text (1+ index)) output)
                        (incf index))
                 (write-char character output)))))

(defun %bp-decode-character-references (text)
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length text))
          do (if (char= (char text index) #\&)
                 (let ((semicolon (position #\; text :start (1+ index))))
                   (if semicolon
                       (let* ((reference (subseq text (1+ index) semicolon))
                              (decoded
                                (cond
                                  ((and (> (length reference) 1)
                                        (char= (char reference 0) #\#))
                                   (if (and (> (length reference) 2)
                                            (find (char reference 1) '(#\x #\X)))
                                       (decode-numeric-character-reference
                                        (subseq reference 2) 16)
                                       (decode-numeric-character-reference
                                        (subseq reference 1) 10)))
                                  ((plusp (length reference))
                                   (lookup-html5-entity
                                    (concatenate 'string reference ";"))))))
                         (if decoded
                             (progn (write-string decoded output)
                                    (setf index (1+ semicolon)))
                             (progn (write-char #\& output)
                                    (incf index))))
                       (progn (write-char #\& output)
                              (incf index))))
                 (progn (write-char (char text index) output)
                        (incf index))))))

(defun %bp-parse-destination (text index)
  (let ((start index)
        (depth 0)
        (escaped nil))
    (cond
      ((and (< index (length text)) (char= (char text index) #\<))
       (incf index)
       (setf start index)
       (loop while (< index (length text))
             for character = (char text index)
             do (cond
                  (escaped (setf escaped nil))
                  ((char= character #\\)
                   (setf escaped
                         (and (< (1+ index) (length text))
                              (ascii-punctuation-p (char text (1+ index))))))
                  ((char= character #\<) (return-from %bp-parse-destination nil))
                  ((char= character #\>)
                   (return-from %bp-parse-destination
                     (values (%bp-unescape-punctuation
                              (subseq text start index))
                             (1+ index))))
                  ((find character '(#\Newline #\Return))
                   (return-from %bp-parse-destination nil)))
                (incf index))
       nil)
      (t
       (setf start index)
       (loop while (< index (length text))
             for character = (char text index)
             do (cond
                  (escaped (setf escaped nil))
                  ((char= character #\\)
                   (setf escaped
                         (and (< (1+ index) (length text))
                              (ascii-punctuation-p (char text (1+ index))))))
                  ((char= character #\() (incf depth))
                  ((char= character #\))
                   (if (zerop depth)
                       (return)
                       (decf depth)))
                  ((and (zerop depth)
                        (or (%bp-link-whitespace-p character)
                            (find character '(#\< #\>))))
                   (return)))
                (incf index))
       (when (or escaped (plusp depth) (= start index))
         (return-from %bp-parse-destination nil))
       (values (%bp-unescape-punctuation (subseq text start index)) index)))))

(defun %bp-parse-title (text index)
  (when (< index (length text))
    (let* ((opening (char text index))
           (closing (case opening (#\( #\)) (otherwise opening))))
      (when (find opening '(#\" #\' #\())
        (let ((start (1+ index))
              (escaped nil))
          (loop for cursor from (1+ index) below (length text)
                for character = (char text cursor)
                do (cond
                     (escaped (setf escaped nil))
                     ((char= character #\\) (setf escaped t))
                     ((char= character closing)
                      (return-from %bp-parse-title
                        (values (%bp-unescape-punctuation
                                 (subseq text start cursor))
                                (1+ cursor))))))
          nil)))))

(defun %bp-link-definition (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (declare (ignore columns))
      (when (and (< indent length) (char= (char text indent) #\[)
                 (not (%bp-prefix-p text "[^" indent)))
        (let ((index (1+ indent))
              (label-start (1+ indent))
              (escaped nil))
          (loop while (< index length)
                for character = (char text index)
                do (cond
                     (escaped (setf escaped nil))
                     ((char= character #\\) (setf escaped t))
                     ((char= character #\[)
                      (return-from %bp-link-definition nil))
                     ((char= character #\])
                      (return)))
                  (incf index))
          (when (or (>= index length) (= index label-start)
                    (>= (1+ index) length)
                    (not (char= (char text (1+ index)) #\:))
                    (> (- index label-start) 999))
            (return-from %bp-link-definition nil))
          (let* ((raw-label (subseq text label-start index))
                 (normalized
                   (normalize-link-label
                    (%bp-decode-character-references
                     (%bp-unescape-punctuation raw-label))))
                 (cursor (%bp-link-skip-whitespace text (+ index 2))))
            (when (zerop (length normalized))
              (return-from %bp-link-definition nil))
            (multiple-value-bind (destination destination-end)
                (%bp-parse-destination text cursor)
              (when destination
                (let* ((after-space (%bp-link-skip-whitespace
                                     text destination-end))
                       (destination-line-end
                         (or (position #\Newline text :start destination-end)
                             length))
                       (title nil)
                       (end destination-end))
                  (when (and (< after-space length)
                             (= after-space destination-end))
                    (return-from %bp-link-definition nil))
                  (when (< after-space length)
                    (if (find (char text after-space) '(#\" #\' #\())
                        (progn
                          (multiple-value-bind (parsed-title title-end)
                              (%bp-parse-title text after-space)
                            (unless parsed-title
                              (return-from %bp-link-definition
                                (values nil nil nil nil nil
                                        (list
                                         :incomplete-title
                                         (case (char text after-space)
                                           (#\( #\))
                                           (otherwise
                                            (char text after-space)))))))
                            (let ((title-line-end
                                    (or (position #\Newline text :start title-end)
                                        length)))
                              (unless (every #'%bp-link-whitespace-p
                                             (subseq text title-end title-line-end))
                                (return-from %bp-link-definition nil)))
                            (setf title parsed-title end title-end)))
                        (progn
                          (unless (every #'%bp-link-whitespace-p
                                         (subseq text destination-end
                                                 destination-line-end))
                            (return-from %bp-link-definition nil)))))
                  (values raw-label normalized
                          (%bp-decode-character-references destination)
                          (and title (%bp-decode-character-references title))
                          end))))))))))

(defun %bp-link-label-final-line (lines index)
  (let* ((start (1+ (nth-value 0
                               (%bp-line-leading-indent
                                (aref lines index) 3))))
         (escaped nil))
    (loop for line-index from index below (length lines)
          for line = (aref lines line-index)
          for text = (%block-line-text line)
          do (when (%bp-blank-line-p line)
               (return-from %bp-link-label-final-line nil))
             (loop for cursor from start below (length text)
                   for character = (char text cursor)
                   do (cond
                        (escaped (setf escaped nil))
                        ((char= character #\\) (setf escaped t))
                        ((char= character #\[)
                         (return-from %bp-link-label-final-line nil))
                        ((char= character #\])
                         (return-from %bp-link-label-final-line
                           (and (< (1+ cursor) (length text))
                                (char= (char text (1+ cursor)) #\:)
                                line-index)))))
             (setf escaped nil start 0))
    nil))

(defun %bp-link-title-opener (line)
  (let* ((text (%block-line-text line))
         (index (%bp-skip-whitespace text 0)))
    (when (and (< index (length text))
               (find (char text index) '(#\" #\' #\()))
      (values (char text index) (1+ index)))))

(defun %bp-link-title-close-line (lines start-line start-index closing)
  (loop for line-index from start-line below (length lines)
        for line = (aref lines line-index)
        for text = (%block-line-text line)
        do (when (%bp-blank-line-p line)
             (return-from %bp-link-title-close-line nil))
           (let ((escaped nil))
             (loop for index from (if (= line-index start-line) start-index 0)
                     below (length text)
                   for character = (char text index)
                   do (cond
                        (escaped (setf escaped nil))
                        ((char= character #\\) (setf escaped t))
                        ((char= character closing)
                         (return-from %bp-link-title-close-line line-index))))))
  nil)

(defun %bp-link-definition-result (line parsed-values start-index)
  (let ((parsed-end (fifth parsed-values)))
    (values line
            (subseq parsed-values 0 5)
            (+ start-index
               1
               (count #\Newline (%block-line-text line) :end parsed-end)))))

(defun %bp-link-definition-at (lines index)
  (let* ((first-line (aref lines index))
         (first-text (%block-line-text first-line))
         (indent (nth-value 0 (%bp-line-leading-indent first-line 3))))
    (unless (and (< indent (length first-text))
                 (char= (char first-text indent) #\[)
                 (not (%bp-prefix-p first-text "[^" indent)))
      (return-from %bp-link-definition-at nil))
    (let ((label-end (%bp-link-label-final-line lines index)))
      (unless label-end (return-from %bp-link-definition-at nil))
      (let* ((end (1+ label-end))
             (candidate-line (%bp-join-lines (subseq lines index end)))
             (parsed-values (multiple-value-list
                             (%bp-link-definition candidate-line))))
        (when (and (not (first parsed-values))
                   (< end (length lines))
                   (not (%bp-blank-line-p (aref lines end))))
          (incf end)
          (setf candidate-line (%bp-join-lines (subseq lines index end))
                parsed-values
                (multiple-value-list (%bp-link-definition candidate-line))))
        (when (and (first parsed-values) (null (fourth parsed-values))
                   (< end (length lines))
                   (not (%bp-blank-line-p (aref lines end))))
          (multiple-value-bind (opener title-start)
              (%bp-link-title-opener (aref lines end))
            (when opener
              (let ((close-line
                      (%bp-link-title-close-line
                       lines end title-start
                       (case opener (#\( #\)) (otherwise opener)))))
                (when close-line
                  (setf end (1+ close-line)
                        candidate-line (%bp-join-lines (subseq lines index end))
                        parsed-values
                        (multiple-value-list
                         (%bp-link-definition candidate-line))))))))
        (when (first parsed-values)
          (return-from %bp-link-definition-at
            (%bp-link-definition-result candidate-line parsed-values index)))
        (let ((incomplete-title (sixth parsed-values)))
          (when (and incomplete-title
                     (eq (first incomplete-title) :incomplete-title))
            (let ((close-line
                    (%bp-link-title-close-line
                     lines end 0 (second incomplete-title))))
              (when close-line
                (setf end (1+ close-line)
                      candidate-line (%bp-join-lines (subseq lines index end))
                      parsed-values
                      (multiple-value-list
                       (%bp-link-definition candidate-line)))
                (when (first parsed-values)
                  (return-from %bp-link-definition-at
                    (%bp-link-definition-result candidate-line
                                                parsed-values index)))))))))))

(defun %bp-footnote-start (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (declare (ignore columns))
      (when (and (%bp-prefix-p text "[^" indent))
        (let ((label-start (+ indent 2))
              (close nil)
              (escaped nil))
          (loop for index from label-start below length
                for character = (char text index)
                do (cond
                     (escaped (setf escaped nil))
                     ((char= character #\\) (setf escaped t))
                     ((char= character #\])
                      (setf close index)
                      (return))))
          (when (and close (> close label-start)
                     (< (1+ close) length)
                     (char= (char text (1+ close)) #\:))
            (let* ((raw-label (subseq text label-start close))
                   (normalized
                     (normalize-link-label
                      (%bp-decode-character-references
                       (%bp-unescape-punctuation raw-label))))
                   (cursor (%bp-skip-whitespace text (+ close 2))))
              (values raw-label normalized cursor
                      indent))))))))

(defun %bp-atx-heading (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (declare (ignore columns))
      (when (and (< indent length) (char= (char text indent) #\#))
        (let ((marker-end indent))
          (loop while (and (< marker-end length)
                           (char= (char text marker-end) #\#))
                do (incf marker-end))
          (let ((level (- marker-end indent)))
            (when (and (<= level 6)
                       (or (= marker-end length)
                           (%bp-space-p (char text marker-end))))
              (let* ((content-start (%bp-skip-whitespace text marker-end))
                     (content-end length)
                     (close-count 0))
                (multiple-value-setq (content-start content-end)
                  (%bp-trim-indices text content-start content-end))
                (when (> content-end content-start)
                  (let ((cursor content-end))
                    (loop while (and (> cursor content-start)
                                     (char= (char text (1- cursor)) #\#))
                          do (decf cursor))
                    (when (and (< cursor content-end)
                               (> cursor content-start)
                               (%bp-space-p (char text (1- cursor))))
                      (setf close-count (- content-end cursor)
                            content-end (1- cursor))
                      (multiple-value-setq (content-start content-end)
                        (%bp-trim-indices text content-start content-end)))))
                (values level indent content-start content-end
                        (- marker-end indent) close-count)))))))))

(defun %bp-setext-level (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns)
        (%bp-line-leading-indent line most-positive-fixnum)
      (when (<= columns 3)
        (let* ((start indent)
               (end length))
        (multiple-value-setq (start end) (%bp-trim-indices text start end))
        (when (< start end)
          (let ((marker (char text start)))
            (when (find marker '(#\= #\-))
              (loop for index from start below end
                    always (char= (char text index) marker)
                    finally (return-from %bp-setext-level
                              (values (if (char= marker #\=) 1 2)
                                      marker (- end start) indent)))))))))))

(defun %bp-thematic-break (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns)
        (%bp-line-leading-indent line most-positive-fixnum)
      (when (<= columns 3)
        (let ((marker nil)
              (count 0))
        (loop for index from indent below length
              for character = (char text index)
              do (cond
                   ((%bp-space-p character))
                   ((find character '(#\- #\_ #\*))
                    (if marker
                        (unless (char= marker character)
                          (return-from %bp-thematic-break nil))
                        (setf marker character))
                    (incf count))
                   (t (return-from %bp-thematic-break nil)))
              finally (return (when (and marker (>= count 3))
                                (values marker count indent)))))))))

(defun %bp-fence-start (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (when (and (< indent length)
                 (find (char text indent) '(#\` #\~)))
        (let* ((character (char text indent))
               (end indent))
          (loop while (and (< end length) (char= (char text end) character))
                do (incf end))
          (when (>= (- end indent) 3)
            (let ((info (string-left-trim '(#\Space #\Tab)
                                          (subseq text end))))
              (when (or (char/= character #\`)
                        (not (find #\` info)))
                (values character (- end indent) columns end info indent)))))))))

(defun %bp-fence-close-p (line character minimum-length)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (declare (ignore columns))
      (when (and (< indent length) (char= (char text indent) character))
        (let ((end indent))
          (loop while (and (< end length) (char= (char text end) character))
                do (incf end))
          (and (>= (- end indent) minimum-length)
               (every #'%bp-space-p (subseq text end length))))))))

(defun %bp-ascii-alpha-p (character)
  (or (<= (char-code #\A) (char-code character) (char-code #\Z))
      (<= (char-code #\a) (char-code character) (char-code #\z))))

(defun %bp-ascii-alphanumeric-p (character)
  (or (%bp-ascii-alpha-p character)
      (<= (char-code #\0) (char-code character) (char-code #\9))))

(defun %bp-tag-name-at (text index)
  (let ((length (length text))
        (cursor index))
    (when (and (< cursor length) (char= (char text cursor) #\<))
      (incf cursor)
      (when (and (< cursor length) (char= (char text cursor) #\/))
        (incf cursor))
      (when (and (< cursor length) (%bp-ascii-alpha-p (char text cursor)))
        (let ((start cursor))
          (loop while (and (< cursor length)
                           (or (%bp-ascii-alphanumeric-p (char text cursor))
                               (char= (char text cursor) #\-)))
                do (incf cursor))
          (let ((name (string-downcase (subseq text start cursor))))
            (when (or (= cursor length)
                      (%bp-space-p (char text cursor))
                      (char= (char text cursor) #\>))
              (values name cursor))))))))

(defun %bp-html-attribute-name-start-p (character)
  (or (%bp-ascii-alpha-p character)
      (find character '(#\_ #\:))))

(defun %bp-html-attribute-name-character-p (character)
  (or (%bp-ascii-alphanumeric-p character)
      (find character '(#\_ #\: #\. #\-))))

(defun %bp-html-type-seven-tag-end (text index name-end)
  (let* ((length (length text))
         (closing-p (and (< (1+ index) length)
                         (char= (char text (1+ index)) #\/)))
         (cursor name-end))
    (when closing-p
      (setf cursor (%bp-skip-whitespace text cursor))
      (return-from %bp-html-type-seven-tag-end
        (and (< cursor length)
             (char= (char text cursor) #\>)
             (1+ cursor))))
    (loop
      (setf cursor (%bp-skip-whitespace text cursor))
      (when (>= cursor length)
        (return nil))
      (when (char= (char text cursor) #\>)
        (return (1+ cursor)))
      (when (char= (char text cursor) #\/)
        (incf cursor)
        (return (and (< cursor length)
                     (char= (char text cursor) #\>)
                     (1+ cursor))))
      (unless (%bp-html-attribute-name-start-p (char text cursor))
        (return nil))
      (incf cursor)
      (loop while (and (< cursor length)
                       (%bp-html-attribute-name-character-p
                        (char text cursor)))
            do (incf cursor))
      (setf cursor (%bp-skip-whitespace text cursor))
      (when (and (< cursor length) (char= (char text cursor) #\=))
        (incf cursor)
        (setf cursor (%bp-skip-whitespace text cursor))
        (when (>= cursor length)
          (return nil))
        (let ((quote (char text cursor)))
          (if (find quote '(#\" #\'))
              (let ((end (position quote text :start (1+ cursor))))
                (unless end (return nil))
                (setf cursor (1+ end)))
              (let ((start cursor))
                (loop while (and (< cursor length)
                                 (not (%bp-space-p (char text cursor)))
                                 (not (find (char text cursor)
                                            '(#\" #\' #\= #\< #\> #\`))))
                      do (incf cursor))
                (when (= start cursor)
                  (return nil))))))
      (when (and (< cursor length)
                 (not (%bp-space-p (char text cursor)))
                 (not (find (char text cursor) '(#\> #\/))))
        (return nil)))))

(defun %bp-html-block-type (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (declare (ignore columns))
      (cond
        ((%bp-prefix-p text "<!--" indent) (values 2 "-->" nil))
        ((%bp-prefix-p text "<?" indent) (values 3 "?>" nil))
        ((%bp-prefix-p text "<![CDATA[" indent) (values 5 "]]>" nil))
        ((and (< (+ indent 2) length)
              (%bp-prefix-p text "<!" indent)
              (<= (char-code #\A)
                  (char-code (char text (+ indent 2)))
                  (char-code #\Z)))
         (values 4 ">" nil))
        (t
         (multiple-value-bind (name name-end) (%bp-tag-name-at text indent)
           (when name
             (cond
               ((and (not (char= (char text (1+ indent)) #\/))
                     (member name '("script" "pre" "style" "textarea")
                             :test #'string=))
                (values 1 (concatenate 'string "</" name) name))
               ((member name '("address" "article" "aside" "base" "basefont"
                               "blockquote" "body" "caption" "center" "col"
                               "colgroup" "dd" "details" "dialog" "dir" "div"
                               "dl" "dt" "fieldset" "figcaption" "figure"
                               "footer" "form" "frame" "frameset" "h1" "h2"
                               "h3" "h4" "h5" "h6" "head" "header" "hr"
                               "html" "iframe" "legend" "li" "link" "main"
                               "menu" "menuitem" "nav" "noframes" "ol"
                               "optgroup" "option" "p" "param" "search"
                               "section" "summary" "table" "tbody" "td"
                               "tfoot" "th" "thead" "title" "tr" "track" "ul")
                          :test #'string=)
                (values 6 nil name))
               ((and (not (member name '("pre" "script" "style" "textarea")
                                  :test #'string=))
                     (let ((tag-end (%bp-html-type-seven-tag-end
                                     text indent name-end)))
                       (and tag-end
                            (every #'%bp-space-p
                                   (subseq text tag-end length)))))
                (values 7 nil name))))))))))

(defun %bp-list-marker (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (when (< indent length)
        (let* ((marker-start indent)
               (character (char text marker-start))
               (kind nil)
               (start-number 1)
               (delimiter nil)
               (marker-end nil))
          (cond
            ((find character '(#\- #\+ #\*))
             (setf kind :unordered delimiter character marker-end (1+ indent)))
            ((<= (char-code #\0) (char-code character) (char-code #\9))
             (let ((cursor indent))
               (loop while (and (< cursor length)
                                (<= (char-code #\0)
                                    (char-code (char text cursor))
                                    (char-code #\9)))
                     do (incf cursor))
               (when (and (<= (- cursor indent) 9)
                          (< cursor length)
                          (find (char text cursor) '(#\. #\))))
                 (setf kind :ordered
                       start-number (parse-integer text :start indent :end cursor)
                       delimiter (char text cursor)
                       marker-end (1+ cursor))))))
          (when marker-end
            (when (or (= marker-end length)
                      (%bp-space-p (char text marker-end)))
                     (let* ((marker-column
                              (+ (%block-line-column-start line) columns
                                 (- marker-end indent)))
                     (marker-columns (+ columns (- marker-end indent)))
                     (space-end (%bp-skip-whitespace text marker-end))
                     (space-columns
                       (if (= space-end marker-end)
                           1
                           (%bp-whitespace-width text marker-end space-end
                                                 marker-column)))
                     (padding (if (or (= space-end length)
                                     (> space-columns 4))
                                 1
                                 space-columns))
                     (content-end
                       (if (= space-end marker-end)
                           marker-end
                           (if (> space-columns 4)
                               (1+ marker-end)
                               space-end)))
                     (content-indent (+ marker-columns padding)))
                (values kind start-number delimiter indent marker-end
                        content-end content-indent
                        (subseq text 0 content-end) columns)))))))))

(defun %bp-list-interrupts-paragraph-p (line)
  (multiple-value-bind (kind start-number delimiter indent marker-end content-end)
      (%bp-list-marker line)
    (declare (ignore delimiter indent marker-end))
    (and kind
         (< content-end (length (%block-line-text line)))
         (or (eq kind :unordered) (= start-number 1)))))

(defun %bp-link-definition-start-p (line)
  (let* ((text (%block-line-text line))
         (indent (nth-value 0 (%bp-line-leading-indent line 3))))
    (and (< indent (length text))
         (char= (char text indent) #\[)
         (not (%bp-prefix-p text "[^" indent)))))

(defun %bp-interrupts-paragraph-p (line)
  (or (nth-value 0 (%bp-atx-heading line))
      (nth-value 0 (%bp-thematic-break line))
      (nth-value 0 (%bp-fence-start line))
      (%bp-list-interrupts-paragraph-p line)
      (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
        (declare (ignore columns))
        (%bp-prefix-p (%block-line-text line) ">" indent))
      (multiple-value-bind (type) (%bp-html-block-type line)
        (and type (<= type 6)))
      (nth-value 0 (%bp-footnote-start line))))

(defun %bp-code-indent-line (line columns)
  (%bp-strip-columns line columns))

(defun %bp-default-body-trailing-newline-p ()
  t)

(defun %bp-body-from-lines
    (lines &key (trailing-newline (%bp-default-body-trailing-newline-p)))
  (let ((lines (if (listp lines) (coerce lines 'vector) lines)))
  (with-output-to-string (output)
    (loop for index from 0 below (length lines)
          for line = (aref lines index)
          do (write-string (%block-line-text line) output)
             (when (or (< index (1- (length lines)))
                       (and trailing-newline
                            (> (%block-line-after line)
                               (%block-line-raw-end line))))
               (write-char #\Newline output))))))

(defun %bp-html-terminator-p (line terminator kind)
  (let ((text (%block-line-text line)))
    (cond
      ((= kind 1)
       (let ((cursor 0))
         (loop for found = (search terminator text :start2 cursor
                                         :test #'char-equal)
               while found
               do (let ((end (+ found (length terminator))))
                    (when (< end (length text))
                      (let ((closing-end
                              (if (%bp-space-p (char text end))
                                  (%bp-skip-whitespace text end)
                                  end)))
                        (when (and (< closing-end (length text))
                                   (char= (char text closing-end) #\>))
                          (return-from %bp-html-terminator-p t))))
                    (setf cursor (1+ found))))
         nil))
      ((= kind 5) (search "]]>" text))
      (terminator (search terminator text))
      (t nil))))

(defun %bp-backtick-run-end (text index end)
  (loop while (and (< index end) (char= (char text index) #\`))
        do (incf index)
        finally (return index)))

(defun %bp-backtick-matching-run-ends (text start end)
  (let ((unmatched (make-hash-table :test #'eql))
        (matches (make-hash-table :test #'eql))
        (index start)
        (escaped nil))
    (loop while (< index end)
          for character = (char text index)
          do (cond
               ((char= character #\\)
                (setf escaped (not escaped))
                (incf index))
               (escaped
                (setf escaped nil)
                (incf index))
               ((char= character #\`)
                (let* ((run-end (%bp-backtick-run-end text index end))
                       (run-length (- run-end index)))
                  (multiple-value-bind (opening present-p)
                      (gethash run-length unmatched)
                    (if present-p
                        (progn
                          (setf (gethash opening matches) run-end)
                          (remhash run-length unmatched))
                        (setf (gethash run-length unmatched) index)))
                  (setf index run-end)))
               (t
                (setf escaped nil)
                (incf index))))
    matches))

(defun %bp-table-cells (line)
  (let* ((text (%block-line-text line))
         (length (length text))
         (start 0)
         (end length)
         (cells nil)
         (index 0)
         (backtick-matches (%bp-backtick-matching-run-ends text start end))
         (escaped nil))
    (multiple-value-setq (start end) (%bp-trim-indices text start end))
    (when (and (< start end) (char= (char text start) #\|))
      (incf start))
    (setf index start)
    (let ((cell-start start))
      (loop while (< index end)
            for character = (char text index)
            do (cond
                 (escaped (setf escaped nil))
                 ((char= character #\\) (setf escaped t))
                 ((char= character #\`)
                  (let ((run-end index))
                    (loop while (and (< run-end end)
                                     (char= (char text run-end) #\`))
                          do (incf run-end))
                    (setf index
                          (1- (or (gethash index backtick-matches)
                                  run-end)))))
                 ((char= character #\|)
                  (push (cons cell-start index) cells)
                  (setf cell-start (1+ index))))
              (incf index))
      (push (cons cell-start end) cells)
      (when (and (> (length cells) 1)
                 (= cell-start end)
                 (char= (char text (1- end)) #\|))
        (pop cells)))
    (nreverse cells)))

(defun %bp-table-alignments (line)
  (let ((cells (%bp-table-cells line)))
    (when cells
      (let ((alignments nil))
        (dolist (cell cells)
          (let* ((part (%bp-trimmed-string
                        (subseq (%block-line-text line) (car cell) (cdr cell))))
                 (length (length part))
                 (start (if (and (plusp length) (char= (char part 0) #\:)) 1 0))
                 (end (if (and (> length start)
                               (char= (char part (1- length)) #\:))
                          (1- length)
                          length)))
            (unless (and (plusp (- end start))
                         (loop for index from start below end
                               always (char= (char part index) #\-)))
              (return-from %bp-table-alignments nil))
            (push (cond ((and (plusp start) (< end length)) :center)
                        ((plusp start) :left)
                        ((< end length) :right)
                        (t nil))
                  alignments)))
        (nreverse alignments)))))

(defun %bp-table-cell-node (source-buffer line range alignment context)
  (multiple-value-bind (start end)
      (%bp-trim-indices (%block-line-text line) (car range) (cdr range))
    (let* ((text (subseq (%block-line-text line) start end))
           (cell-line (%bp-line-slice line start end))
           (cell (make-table-cell
                  :span (%bp-line-span source-buffer line start end)
                  :continuation-prefix context
                  :alignment alignment
                  :children nil)))
      (%bp-add-fragment source-buffer cell (list cell-line))
      (values cell text))))

(defun %bp-table-row (source-buffer line alignments header-p context)
  (let* ((ranges (coerce (%bp-table-cells line) 'vector))
         (cells nil))
    (loop for alignment in alignments
          for index from 0
          for range = (if (< index (length ranges))
                          (aref ranges index)
                          (cons (length (%block-line-text line))
                                (length (%block-line-text line))))
          do (push (nth-value 0
                             (%bp-table-cell-node source-buffer line range
                                                  alignment context))
                   cells))
    (make-table-row
     :span (%bp-line-span source-buffer line
                          (nth-value 0 (%bp-line-leading-indent line 3))
                          (length (%block-line-text line)))
     :continuation-prefix context
     :header-row-p header-p
     :cells (nreverse cells))))

(defun %bp-make-theta (source-buffer line context marker count start)
  (make-thematic-break
   :span (%bp-line-span source-buffer line start
                        (length (%block-line-text line)))
   :continuation-prefix context
   :marker-character marker
   :marker-count count))

(defun %bp-list-marker-record (line)
  (multiple-value-bind (kind start-number delimiter indent marker-end content-end
                             content-indent prefix marker-columns)
      (%bp-list-marker line)
    (when kind
      (list kind start-number delimiter indent marker-end content-end
            content-indent prefix marker-columns))))

(defun %bp-quote-content (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-line-leading-indent line 3)
      (when (and (< indent length) (char= (char text indent) #\>))
        (let* ((marker-end (1+ indent))
               (prefix-end marker-end)
               (content (%bp-line-slice line marker-end)))
          (when (and (< marker-end length)
                     (%bp-space-p (char text marker-end)))
            (setf prefix-end (1+ marker-end)
                  content (%bp-strip-columns
                           (%bp-line-slice line marker-end) 1)))
          (values indent content (subseq text 0 prefix-end) columns))))))

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

(defun %bp-container-transform-line (line stack &optional record-p)
  (let ((content line))
    (dolist (frame (reverse (butlast stack)))
      (case (%bp-open-frame-kind frame)
        (:quote
         (multiple-value-bind (marker-start quote-content prefix)
             (%bp-quote-content content)
           (declare (ignore prefix))
           (unless marker-start
             (return-from %bp-container-transform-line
               (values content frame)))
           (setf content quote-content)
           (when record-p
             (setf (%bp-open-frame-end-line frame) line))))
        ((:item :footnote)
         (cond
           ((%bp-blank-line-p content)
            (when record-p
              (setf (%bp-open-frame-blank-seen frame) t)))
           ((and (eq (%bp-open-frame-kind frame) :item)
                 (%bp-open-frame-blank-seen frame)
                 (not (%bp-open-frame-has-content frame)))
            (return-from %bp-container-transform-line
              (values content frame)))
           ((>= (%bp-line-indent-columns content)
                (%bp-open-frame-content-indent frame))
            (setf content
                  (%bp-strip-columns
                   content (%bp-open-frame-content-indent frame)))
            (when record-p
              (setf (%bp-open-frame-end-line frame) line)))
           (t
            (return-from %bp-container-transform-line
              (values content frame)))))))
    (values content nil)))

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
               (find (char text 1) '(#\Space #\x #\X))
               (char= (char text 2) #\])
               (or (= (length text) 3)
                   (%bp-space-p (char text 3))))
      (values t (not (char= (char text 1) #\Space))
              (if (and (> (length text) 3)
                       (%bp-space-p (char text 3)))
                  (%bp-line-slice line 4)
                  (%bp-line-slice line 3))))))

(defun %bp-check-depth (depth limit)
  (when (> depth limit)
    (error 'markdown-nesting-depth-exceeded
           :limit-name :nesting-depth
           :limit limit
           :actual depth
           :context "block parsing"
           :message "maximum block nesting depth exceeded")))

(defun %bp-make-inline-node (source-buffer lines context start-position end-position)
  (let ((node (make-paragraph
               :span (%bp-lines-span source-buffer (coerce lines 'vector)
                                     start-position end-position)
               :continuation-prefix context
               :children nil)))
    (%bp-add-fragment source-buffer node lines)
    node))

(defun %bp-prepared-paragraph-lines (lines)
  (mapcar #'%bp-strip-up-to-three lines))

(defun %bp-make-paragraph (source-buffer lines context)
  (let* ((prepared (%bp-prepared-paragraph-lines lines))
         (node (%bp-make-inline-node
                source-buffer prepared context 0
                (length (%block-line-text (car (last prepared)))))))
    node))

(defun %bp-reference-node (source-buffer line context raw-label normalized
                           destination title eligible end)
  (make-link-reference-definition
   :span (%bp-line-span source-buffer line
                        (nth-value 0 (%bp-line-leading-indent line 3))
                        end)
   :continuation-prefix context
   :raw-label raw-label
   :normalized-label normalized
   :destination destination
   :title title
   :eligible-p eligible))

(defun %bp-parse-reference-definition (source-buffer line context parsed-values)
  (destructuring-bind (raw-label normalized destination title end) parsed-values
    (when raw-label
      (let* ((eligible (not (nth-value 1
                                       (gethash normalized
                                                *block-reference-index*))))
             (node (%bp-reference-node source-buffer line context raw-label
                                       normalized destination title eligible end)))
        (when eligible
          (setf (gethash normalized *block-reference-index*) node))
        node))))

(defun %bp-parse-lines (source-buffer lines context depth limit)
  (let* ((lines (if (vectorp lines) lines (coerce lines 'vector)))
         (root (%bp-make-open-frame :kind :root :node nil
                                    :context context :child-context context))
         (stack (list root))
         (leaf nil)
         (cursor 0))
    (labels ((current-frame ()
               (car stack))
             (current-context ()
               (%bp-frame-context (current-frame)))
             (mark-container-content ()
               (dolist (frame stack)
                 (when (member (%bp-open-frame-kind frame) '(:item :footnote))
                   (setf (%bp-open-frame-has-content frame) t))))
             (append-to-current (node)
               (%bp-frame-add-node (current-frame) node))
             (push-frame (frame)
               (push frame stack)
               frame)
             (close-leaf ()
               (when leaf
                 (let* ((kind (%bp-open-leaf-kind leaf))
                        (content-lines (nreverse (%bp-open-leaf-lines leaf)))
                        (end-line (%bp-open-leaf-end-line leaf))
                        (context (%bp-open-leaf-context leaf))
                        (node
                          (case kind
                            (:paragraph
                             (%bp-make-paragraph source-buffer content-lines context))
                            (:fenced
                             (make-code-block
                              :span (source-buffer-span
                                     source-buffer
                                     (%bp-line-boundary
                                      (%bp-open-leaf-start-line leaf)
                                      (%bp-open-leaf-start-position leaf))
                                     (%bp-line-boundary
                                      end-line
                                      (length (%block-line-text end-line))))
                              :continuation-prefix context
                              :style :fenced
                              :body (%bp-body-from-lines content-lines)
                              :fence-character
                              (%bp-open-leaf-fence-character leaf)
                              :fence-length (%bp-open-leaf-fence-length leaf)
                              :indentation (%bp-open-leaf-indentation leaf)
                              :info-string (%bp-open-leaf-info-string leaf)))
                            (:indented
                             (make-code-block
                              :span (source-buffer-span
                                     source-buffer
                                     (%bp-line-boundary
                                      (%bp-open-leaf-start-line leaf)
                                      (%bp-open-leaf-start-position leaf))
                                     (%bp-line-boundary
                                      end-line
                                      (length (%block-line-text end-line))))
                              :continuation-prefix context
                              :style :indented
                              :body (%bp-body-from-lines content-lines)
                              :fence-character nil :fence-length 0
                              :indentation 4 :info-string nil))
                            (:html
                             (make-html-block
                              :span (source-buffer-span
                                     source-buffer
                                     (%bp-line-boundary
                                      (%bp-open-leaf-start-line leaf)
                                      (%bp-open-leaf-start-position leaf))
                                     (%bp-line-boundary
                                      end-line
                                      (length (%block-line-text end-line))))
                              :continuation-prefix context
                              :body (%bp-body-from-lines content-lines)
                              :kind (%bp-open-leaf-html-kind leaf)))
                            (:table
                             (let ((table (%bp-open-leaf-table-node leaf)))
                               (setf (node-span table)
                                     (source-buffer-span
                                      source-buffer
                                      (%bp-line-boundary
                                       (%bp-open-leaf-start-line leaf)
                                       (%bp-open-leaf-start-position leaf))
                                      (%bp-line-boundary
                                       end-line
                                       (length (%block-line-text end-line))))
                                     (table-rows table)
                                     (nreverse (%bp-open-leaf-table-rows leaf)))
                               table)))))
                   (setf leaf nil)
                   (append-to-current node))))
             (close-top-frame ()
               (let* ((frame (pop stack))
                      (parent (current-frame))
                      (end-line (%bp-open-frame-end-line frame))
                      (node (%bp-finish-frame source-buffer frame)))
                 (%bp-frame-add-node parent node)
                 (setf (%bp-open-frame-end-line parent) end-line)
                 node))
             (close-through (target)
               (loop while (not (eq (current-frame) target))
                     do (close-top-frame))
               (close-top-frame))
             (frame-parent (frame)
               (loop for tail on stack
                     when (eq (car tail) frame)
                       return (cadr tail)))
             (open-item (physical-line content record)
               (destructuring-bind (kind start-number delimiter indent marker-end
                                    content-end content-indent prefix marker-columns)
                   record
                 (declare (ignore start-number delimiter marker-end prefix
                                  marker-columns))
                 (%bp-check-depth (+ depth
                                     (loop for frame in stack
                                           sum (case (%bp-open-frame-kind frame)
                                                 ((:quote :footnote) 1)
                                                 (:list 1)
                                                 (:item 1)
                                                 (otherwise 0)))
                                     1)
                                  limit)
                 (let* ((list-frame (current-frame))
                        (item-context (%bp-open-frame-context list-frame))
                        (first-content (%bp-line-slice content content-end))
                        (task-values (multiple-value-list
                                      (%bp-task-marker first-content)))
                        (task-p (first task-values))
                        (checked (second task-values))
                        (task-content (third task-values))
                        (item (if task-p
                                  (make-task-list-item
                                   :span nil :continuation-prefix item-context
                                   :children nil :checked-p checked
                                   :task-marker-span
                                   (source-buffer-span
                                    source-buffer
                                    (%bp-line-boundary first-content 0)
                                    (%bp-line-boundary first-content 3))
                                   :original-marker-character
                                   (char (%block-line-text first-content) 1))
                                  (make-list-item
                                   :span nil :continuation-prefix item-context
                                   :children nil)))
                        (item-frame
                          (%bp-make-open-frame
                           :kind :item :node item :context item-context
                           :child-context
                           (concatenate 'string item-context
                                        (make-string content-indent
                                                     :initial-element #\Space))
                           :start-line content :start-position indent
                           :end-line physical-line :content-indent content-indent
                           :has-content task-p)))
                   (push-frame item-frame)
                   (values (if task-p task-content first-content) kind))))
             (open-list-and-item (physical-line content record)
               (destructuring-bind (kind start-number delimiter indent marker-end
                                    content-end content-indent prefix marker-columns)
                   record
                 (declare (ignore marker-end content-end content-indent prefix))
                 (%bp-check-depth
                  (+ depth
                     (loop for frame in stack
                           sum (case (%bp-open-frame-kind frame)
                                 ((:quote :footnote) 1)
                                 (:list 1)
                                 (:item 1)
                                 (otherwise 0)))
                     1)
                  limit)
                 (let* ((list-context (current-context))
                        (list-node (make-list-node
                                    :span nil :continuation-prefix list-context
                                    :kind kind :tight-p t
                                    :start-number start-number
                                    :marker-character
                                    (if (eq kind :unordered) delimiter nil)
                                    :delimiter-character
                                    (if (eq kind :ordered) delimiter nil)
                                    :items nil))
                        (list-frame
                          (%bp-make-open-frame
                           :kind :list :node list-node :context list-context
                           :child-context list-context :start-line content
                           :start-position indent :end-line physical-line
                           :marker-kind kind :marker-delimiter delimiter
                           :marker-columns marker-columns)))
                   (push-frame list-frame)
                   (multiple-value-bind (item-content ignored-kind)
                       (open-item physical-line content record)
                     (declare (ignore ignored-kind))
                     item-content))))
             (sibling-record-p (content list-frame)
               (let ((record (%bp-list-marker-record content)))
                 (and record
                      (not (nth-value 0 (%bp-thematic-break content)))
                      (eq (first record) (%bp-open-frame-marker-kind list-frame))
                      (char= (third record)
                             (%bp-open-frame-marker-delimiter list-frame))
                      (<= (ninth record)
                          (+ (%bp-open-frame-marker-columns list-frame) 3))
                      record)))
             (list-delimiter-change-p (content list-frame)
               (let ((record (%bp-list-marker-record content)))
                 (and record
                      (eq (first record)
                          (%bp-open-frame-marker-kind list-frame))
                      (not (char= (third record)
                                  (%bp-open-frame-marker-delimiter list-frame)))
                      (<= (ninth record)
                          (+ (%bp-open-frame-marker-columns list-frame) 3)))))
             (reference-candidate (content)
               (let ((candidate-lines (list content))
                     (source-index (1+ cursor)))
                 (loop while (< source-index (length lines))
                       do (multiple-value-bind (next-content mismatch)
                              (%bp-container-transform-line
                               (aref lines source-index) stack nil)
                            (when mismatch
                              (return))
                            (when (%bp-blank-line-p next-content)
                              (return))
                            (push next-content candidate-lines))
                          (incf source-index))
                 (let* ((candidate (coerce (nreverse candidate-lines) 'vector))
                        (result (multiple-value-list
                                 (%bp-link-definition-at candidate 0))))
                   (when (first result)
                     (values candidate (first result) (second result)
                             (third result))))))
             (open-quote (physical-line content marker-start quote-content prefix)
               (%bp-check-depth
                (+ depth
                   (loop for frame in stack
                         sum (case (%bp-open-frame-kind frame)
                               ((:quote :footnote) 1)
                               (:list 1)
                               (:item 1)
                               (otherwise 0)))
                   1)
                limit)
               (let* ((quote-context (current-context))
                      (node (make-block-quote
                             :span nil :continuation-prefix quote-context
                             :children nil)))
                 (push-frame
                  (%bp-make-open-frame
                   :kind :quote :node node :context quote-context
                   :child-context (concatenate 'string quote-context prefix)
                   :start-line content :start-position marker-start
                   :end-line physical-line))
                 quote-content))
             (open-footnote (physical-line content raw-label normalized
                                           content-start indent)
               (%bp-check-depth
                (+ depth
                   (loop for frame in stack
                         sum (case (%bp-open-frame-kind frame)
                               ((:quote :footnote) 1)
                               (:list 1)
                               (:item 1)
                               (otherwise 0)))
                   1)
                limit)
               (let* ((footnote-context (current-context))
                      (node (make-footnote-definition
                             :span nil :continuation-prefix footnote-context
                             :raw-label raw-label
                             :normalized-label normalized :children nil))
                      (frame (%bp-make-open-frame
                              :kind :footnote :node node
                              :context footnote-context
                              :child-context
                              (concatenate 'string footnote-context
                                           (make-string 4 :initial-element #\Space))
                              :start-line content :start-position indent
                              :end-line physical-line :content-indent 4)))
                 (push-frame frame)
                 (unless (nth-value 1
                                    (gethash (%bp-footnote-index-key normalized)
                                             *block-reference-index*))
                   (setf (gethash (%bp-footnote-index-key normalized)
                                  *block-reference-index*) node))
                 (%bp-line-slice content content-start)))
             (open-new-block (physical-line initial-content)
               (let ((content initial-content)
                     (again t))
                 (loop while again
                       do (setf again nil)
                          (cond
                            ((%bp-blank-line-p content)
                             nil)
                            ((nth-value 0 (%bp-footnote-start content))
                             (mark-container-content)
                             (multiple-value-bind (raw-label normalized start indent)
                                 (%bp-footnote-start content)
                               (setf content
                                     (open-footnote physical-line content raw-label
                                                    normalized start indent)
                                     again t)))
                            ((and (%bp-link-definition-start-p content)
                                  (first (multiple-value-list
                                          (reference-candidate content))))
                             (multiple-value-bind (candidate line parsed next)
                                 (reference-candidate content)
                               (declare (ignore candidate))
                               (mark-container-content)
                               (let ((node
                                       (%bp-parse-reference-definition
                                        source-buffer line (current-context) parsed)))
                                 (append-to-current node)
                                 (loop for offset from 1 below next
                                       do (%bp-container-transform-line
                                           (aref lines (+ cursor offset))
                                           stack t))
                                 (setf cursor (+ cursor next -1)))))
                            ((multiple-value-bind (start quote-content prefix)
                                 (%bp-quote-content content)
                               (when start
                                 (mark-container-content)
                                 (setf content
                                       (open-quote physical-line content start
                                                   quote-content prefix))
                                 (setf again t)))
                             nil)
                            ((nth-value 0 (%bp-thematic-break content))
                             (mark-container-content)
                             (multiple-value-bind (marker count start)
                                 (%bp-thematic-break content)
                               (append-to-current
                                (%bp-make-theta source-buffer content
                                                (current-context) marker count start))))
                            ((nth-value 0 (%bp-atx-heading content))
                             (mark-container-content)
                             (multiple-value-bind (level indent content-start content-end
                                                         opening closing)
                                 (%bp-atx-heading content)
                               (let* ((content-line
                                        (%bp-line-slice content content-start content-end))
                                      (node (make-heading
                                             :span (%bp-line-span
                                                    source-buffer content indent
                                                    (length (%block-line-text content)))
                                             :continuation-prefix (current-context)
                                             :level level :style :atx
                                             :marker-character #\#
                                             :opening-marker-length opening
                                             :closing-marker-length closing
                                             :children nil)))
                                 (%bp-add-fragment source-buffer node
                                                   (list content-line))
                                 (append-to-current node))))
                            ((nth-value 0 (%bp-fence-start content))
                             (mark-container-content)
                             (multiple-value-bind (character fence-length indentation
                                                           fence-end info-string fence-start)
                                 (%bp-fence-start content)
                               (declare (ignore fence-end))
                               (setf leaf
                                     (%bp-make-open-leaf
                                      :kind :fenced :context (current-context)
                                      :start-line content :start-position fence-start
                                      :end-line physical-line :lines nil
                                      :fence-character character
                                      :fence-length fence-length
                                      :indentation indentation
                                      :info-string
                                      (let ((trimmed (%bp-trimmed-string info-string)))
                                        (unless (zerop (length trimmed))
                                          (%bp-decode-character-references
                                           (%bp-unescape-punctuation trimmed))))))))
                            ((nth-value 0 (%bp-html-block-type content))
                             (mark-container-content)
                             (multiple-value-bind (kind terminator name)
                                 (%bp-html-block-type content)
                               (declare (ignore name))
                               (setf leaf
                                     (%bp-make-open-leaf
                                      :kind :html :context (current-context)
                                      :start-line content
                                      :start-position
                                      (nth-value 0 (%bp-line-leading-indent content 3))
                                      :end-line physical-line
                                      :lines (list content)
                                      :html-kind kind
                                      :html-terminator terminator))
                               (when (and (<= kind 5)
                                          (%bp-html-terminator-p
                                           content terminator kind))
                                 (close-leaf))))
                            ((%bp-list-marker-record content)
                             (mark-container-content)
                             (let ((record (%bp-list-marker-record content)))
                               (setf content
                                     (open-list-and-item
                                      physical-line content record)
                                     again t)))
                            ((>= (%bp-line-indent-columns content) 4)
                             (mark-container-content)
                             (setf leaf
                                   (%bp-make-open-leaf
                                    :kind :indented :context (current-context)
                                    :start-line content :start-position 0
                                    :end-line physical-line
                                    :lines (list (%bp-strip-columns content 4)))))
                            (t
                             (mark-container-content)
                             (setf leaf
                                   (%bp-make-open-leaf
                                    :kind :paragraph :context (current-context)
                                    :start-line content :start-position 0
                                    :end-line physical-line :lines (list content))))))))
             (process-leaf-line (physical-line content)
               (block process-leaf-line
                 (case (%bp-open-leaf-kind leaf)
                 (:paragraph
                  (let* ((paragraph-lines
                           (reverse (%bp-open-leaf-lines leaf)))
                         (alignments (%bp-table-alignments content)))
                    (when (and (= (length paragraph-lines) 1)
                               alignments
                               (= (length alignments)
                                  (length (%bp-table-cells
                                           (first paragraph-lines))))
                               (or (find #\| (%block-line-text
                                              (first paragraph-lines)))
                                   (find #\| (%block-line-text content))))
                      (let* ((header (first paragraph-lines))
                             (table-context (%bp-open-leaf-context leaf))
                             (node (make-table
                                    :span nil :continuation-prefix table-context
                                    :alignments alignments
                                    :rows (list (%bp-table-row
                                                 source-buffer header alignments t
                                                 table-context)))))
                        (setf leaf
                              (%bp-make-open-leaf
                               :kind :table :context table-context
                               :start-line header
                               :start-position
                               (nth-value 0 (%bp-line-leading-indent header 3))
                               :end-line physical-line
                               :table-node node :table-alignments alignments
                               :table-rows (table-rows node)))
                        (return-from process-leaf-line :consumed)))
                    (when (table-header-followed-by-separator-p content)
                      (close-leaf)
                      (return-from process-leaf-line nil))
                    (multiple-value-bind (level marker count indent)
                        (%bp-setext-level content)
                      (declare (ignore indent))
                      (when level
                        (let* ((prepared
                                 (%bp-prepared-paragraph-lines paragraph-lines))
                               (span-lines
                                 (concatenate 'vector (coerce prepared 'vector)
                                              (vector content)))
                               (node
                                 (make-heading
                                  :span (%bp-lines-span
                                         source-buffer span-lines 0
                                         (length (%block-line-text content)))
                                  :continuation-prefix
                                  (%bp-open-leaf-context leaf)
                                  :level level :style :setext
                                  :marker-character marker
                                  :opening-marker-length 0
                                  :closing-marker-length count
                                  :children nil)))
                          (%bp-add-fragment source-buffer node prepared)
                          (setf leaf nil)
                          (append-to-current node)
                          (return-from process-leaf-line :consumed))))
                    (cond
                      ((%bp-blank-line-p content)
                       (close-leaf)
                       nil)
                      ((%bp-interrupts-paragraph-p content)
                       (close-leaf)
                       nil)
                      (t
                       (push content (%bp-open-leaf-lines leaf))
                       (setf (%bp-open-leaf-end-line leaf) physical-line)
                       :consumed))))
                 (:fenced
                  (if (%bp-fence-close-p
                       content (%bp-open-leaf-fence-character leaf)
                       (%bp-open-leaf-fence-length leaf))
                      (progn
                        (setf (%bp-open-leaf-end-line leaf) physical-line)
                        (close-leaf)
                        :consumed)
                      (progn
                        (push (%bp-code-indent-line
                               content (%bp-open-leaf-indentation leaf))
                              (%bp-open-leaf-lines leaf))
                        (setf (%bp-open-leaf-end-line leaf) physical-line)
                        :consumed)))
                 (:html
                  (if (and (>= (%bp-open-leaf-html-kind leaf) 6)
                           (%bp-blank-line-p content))
                      (progn (close-leaf) nil)
                      (progn
                        (push content (%bp-open-leaf-lines leaf))
                        (setf (%bp-open-leaf-end-line leaf) physical-line)
                        (when (and (<= (%bp-open-leaf-html-kind leaf) 5)
                                   (%bp-html-terminator-p
                                    content (%bp-open-leaf-html-terminator leaf)
                                    (%bp-open-leaf-html-kind leaf)))
                          (close-leaf))
                        :consumed)))
                 (:indented
                  (cond
                    ((%bp-blank-line-p content)
                     (push (%bp-strip-columns content 4)
                           (%bp-open-leaf-pending-lines leaf))
                     :consumed)
                    ((>= (%bp-line-indent-columns content) 4)
                     (setf (%bp-open-leaf-lines leaf)
                           (append (list (%bp-strip-columns content 4))
                                   (%bp-open-leaf-pending-lines leaf)
                                   (%bp-open-leaf-lines leaf))
                           (%bp-open-leaf-pending-lines leaf) nil
                           (%bp-open-leaf-end-line leaf) physical-line)
                     :consumed)
                    (t
                     (close-leaf)
                     nil)))
                 (:table
                  (if (and (not (%bp-blank-line-p content))
                           (or (find #\| (%block-line-text content))
                               (= (length (%bp-open-leaf-table-alignments leaf)) 1)))
                      (progn
                        (push (%bp-table-row
                               source-buffer content
                               (%bp-open-leaf-table-alignments leaf) nil
                               (%bp-open-leaf-context leaf))
                              (%bp-open-leaf-table-rows leaf))
                        (setf (%bp-open-leaf-end-line leaf) physical-line)
                        :consumed)
                      (progn (close-leaf) nil))))))
             (lazy-paragraph-continuation-p (content mismatch)
               (let* ((parent (and (eq (%bp-open-frame-kind mismatch) :item)
                                   (frame-parent mismatch)))
                      (sibling (and parent
                                    (eq (%bp-open-frame-kind parent) :list)
                                    (sibling-record-p content parent)))
                      (delimiter-change (and parent
                                             (eq (%bp-open-frame-kind parent) :list)
                                             (list-delimiter-change-p
                                              content parent))))
                 (and mismatch leaf
                      (eq (%bp-open-leaf-kind leaf) :paragraph)
                      (not sibling)
                      (not delimiter-change)
                      (not (%bp-blank-line-p content))
                      (not (%bp-interrupts-paragraph-p content)))))
             (record-lazy-continuation (physical-line)
               (dolist (frame stack)
                 (setf (%bp-open-frame-end-line frame) physical-line)))
             (table-header-followed-by-separator-p (content)
               (let ((next-index (1+ cursor)))
                 (and (< next-index (length lines))
                      (multiple-value-bind (next-content mismatch)
                          (%bp-container-transform-line
                           (aref lines next-index) stack nil)
                        (let ((alignments (%bp-table-alignments next-content)))
                          (and (not mismatch)
                               alignments
                               (= (length alignments)
                                  (length (%bp-table-cells content)))
                               (find #\| (%block-line-text content))))))))
             (root-children ()
               (nreverse (%bp-open-frame-children root))))
      (loop while (< cursor (length lines))
            for physical-line = (aref lines cursor)
            do (multiple-value-bind (content mismatch)
                   (%bp-container-transform-line physical-line stack t)
                 (cond
                   (mismatch
                    (cond
                      ((lazy-paragraph-continuation-p content mismatch)
                       (record-lazy-continuation physical-line)
                       (mark-container-content)
                       (push content (%bp-open-leaf-lines leaf))
                       (setf (%bp-open-leaf-end-line leaf) physical-line)
                       (incf cursor))
                      (t
                       (close-leaf)
                       (let* ((mismatch-kind (%bp-open-frame-kind mismatch))
                              (parent (frame-parent mismatch))
                              (record (and (eq mismatch-kind :item)
                                           parent
                                           (eq (%bp-open-frame-kind parent) :list)
                                           (sibling-record-p content parent))))
                         (if record
                             (progn
                               (close-through mismatch)
                               (setf content
                                     (open-item physical-line content record))
                               (open-new-block physical-line content)
                               (incf cursor))
                             (progn
                               (close-through mismatch)
                               (when (eq mismatch-kind :item)
                                 (close-top-frame))))))))
                   (leaf
                    (let ((result (process-leaf-line physical-line content)))
                      (when result (incf cursor))))
                   (t
                    (open-new-block physical-line content)
                    (incf cursor)))))
      (close-leaf)
      (loop while (cdr stack) do (close-top-frame))
      (root-children))))

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

(defun %bp-default-nesting-depth ()
  256)

(defun %bp-default-front-matter-p ()
  t)

(defun parse-blocks
    (source-buffer &key (front-matter (%bp-default-front-matter-p))
                        (max-nesting-depth (%bp-default-nesting-depth)))
  "Parse block structure and defer paragraph, heading, and table-cell inlines."
  (check-type source-buffer source-buffer)
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
                 (%bp-parse-lines source-buffer lines "" 0 max-nesting-depth)))
    (let* ((reference-index *block-reference-index*)
           (document (make-document
                      :span (source-buffer-document-span source-buffer)
                      :continuation-prefix ""
                      :children children
                      :source (source-buffer-source source-buffer)
                      :source-format (source-buffer-format source-buffer)
                      :reference-index reference-index
                      :max-nesting-depth max-nesting-depth)))
      (funcall (symbol-function 'make-block-parse-result)
               :document document
               :inline-fragments (nreverse *block-inline-fragments*)
               :reference-index reference-index))))
