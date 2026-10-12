;;;; src/block-parser.lisp
(in-package #:cl-markdown-kit)

(defstruct (%block-line
             (:constructor %make-block-line
                 (text boundaries raw-start raw-end after)))
  (text "" :type string)
  (boundaries #() :type vector)
  (raw-start 0 :type (integer 0 *))
  (raw-end 0 :type (integer 0 *))
  (after 0 :type (integer 0 *)))

(defvar *block-inline-fragments* nil)
(defvar *block-reference-index* nil)

(declaim (ftype function %bp-parse-lines %bp-check-depth))
(declaim (ftype function parse-inline))

(defun %bp-space-p (character)
  (or (char= character #\Space) (char= character #\Tab)))

(defun %bp-line-boundary (line index)
  (aref (%block-line-boundaries line) index))

(defun %bp-line-slice (line start &optional (end (length (%block-line-text line))))
  (%make-block-line
   (subseq (%block-line-text line) start end)
   (subseq (%block-line-boundaries line) start (1+ end))
   (%block-line-raw-start line)
   (%block-line-raw-end line)
   (%block-line-after line)))

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
                                         (1+ index))
                       lines)
                 (setf start (1+ index))))
    (let* ((boundaries (make-array (1+ (- length start))))
           (line-text (subseq text start length)))
      (loop for offset from start to length
            for boundary-index from 0
            do (setf (aref boundaries boundary-index) offset))
      (push (%make-block-line line-text boundaries start length length) lines))
    (coerce (nreverse lines) 'vector)))

(defun %bp-leading-indent (text &optional (limit 3))
  (let ((index 0)
        (column 0))
    (loop while (< index (length text))
          for character = (char text index)
          while (%bp-space-p character)
          for next-column =
            (if (char= character #\Tab)
                (+ column (- 4 (mod column 4)))
                (1+ column))
          while (<= next-column limit)
          do (setf column next-column)
             (incf index))
    (values index column)))

(defun %bp-indent-columns (text &optional (end (length text)))
  (loop with column = 0
        for index from 0 below end
        for character = (char text index)
        while (%bp-space-p character)
        do (setf column
                 (if (char= character #\Tab)
                     (+ column (- 4 (mod column 4)))
                     (1+ column)))
        finally (return column)))

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
         (column 0)
         (partial-spaces 0))
    (loop while (and (< index (length text)) (< column columns)
                     (%bp-space-p (char text index)))
          for character = (char text index)
          for width = (if (char= character #\Tab)
                          (- 4 (mod column 4))
                          1)
          do (incf index)
             (if (> (+ column width) columns)
                 (setf partial-spaces (- (+ column width) columns)
                       column columns)
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
                            (%block-line-after line))))))

(defun %bp-strip-up-to-three (line)
  (multiple-value-bind (index columns)
      (%bp-leading-indent (%block-line-text line) 3)
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

(defun %bp-normalize-label-fallback (label)
  (let* ((collapsed
           (with-output-to-string (output)
             (let ((pending-space nil)
                   (started nil))
               (loop for character across label
                     do (if (find character '(#\Space #\Tab #\Newline #\Return))
                            (when started (setf pending-space t))
                            (progn
                              (when pending-space
                                (write-char #\Space output)
                                (setf pending-space nil))
                              (write-char character output)
                              (setf started t)))))))
         (unicode-package (find-package "SB-UNICODE"))
         (casefold (and unicode-package
                        (find-symbol "CASEFOLD" unicode-package))))
    (if (and casefold (fboundp casefold))
        (funcall (symbol-function casefold) collapsed)
        (string-downcase collapsed))))

(defun %bp-normalize-label (label)
  (let ((normalizer (find-symbol "NORMALIZE-LINK-LABEL"
                                 (find-package '#:cl-markdown-kit))))
    (if (and normalizer (fboundp normalizer))
        (funcall (symbol-function normalizer) label)
        (%bp-normalize-label-fallback label))))

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
  (or (%bp-space-p character)
      (find character '(#\Newline #\Return))))

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
                        (%block-line-after (aref lines (1- (length lines))))))))

(defun %bp-unescape-punctuation (text)
  (with-output-to-string (output)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (if (and (char= character #\\)
                      (< (1+ index) (length text))
                      (not (alphanumericp (char text (1+ index)))))
                 (progn (write-char (char text (1+ index)) output)
                        (incf index))
                 (write-char character output)))))

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
                  ((char= character #\\) (setf escaped t))
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
                  ((char= character #\\) (setf escaped t))
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
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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
          (let* ((raw-label (%bp-unescape-punctuation
                             (subseq text label-start index)))
                 (normalized (%bp-normalize-label raw-label))
                 (cursor (%bp-link-skip-whitespace text (+ index 2))))
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
                  (when (< after-space length)
                    (if (find (char text after-space) '(#\" #\' #\())
                        (progn
                          (unless (every #'%bp-link-whitespace-p
                                         (subseq text destination-end after-space))
                            (return-from %bp-link-definition nil))
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
                            (return-from %bp-link-definition nil))
                          (unless (position #\Newline text
                                            :start destination-end
                                            :end after-space)
                            (return-from %bp-link-definition nil)))))
                  (when (= after-space length)
                    (unless (every #'%bp-link-whitespace-p
                                   (subseq text destination-end length))
                      (return-from %bp-link-definition nil)))
                  (values raw-label normalized destination title end))))))))))

(defun %bp-link-label-final-line (lines index)
  (let* ((first-text (%block-line-text (aref lines index)))
         (start (1+ (nth-value 0 (%bp-leading-indent first-text 3))))
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
         (indent (nth-value 0 (%bp-leading-indent first-text 3))))
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
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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
            (let* ((raw-label (%bp-unescape-punctuation
                               (subseq text label-start close)))
                   (cursor (%bp-skip-whitespace text (+ close 2))))
              (values raw-label (%bp-normalize-label raw-label) cursor
                      indent))))))))

(defun %bp-atx-heading (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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
        (%bp-leading-indent text most-positive-fixnum)
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
        (%bp-leading-indent text most-positive-fixnum)
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
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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

(defparameter +bp-block-html-tags+
  '("address" "article" "aside" "base" "basefont" "blockquote" "body"
    "caption" "center" "col" "colgroup" "dd" "details" "dialog" "dir"
    "div" "dl" "dt" "fieldset" "figcaption" "figure" "footer" "form"
    "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header"
    "hr" "html" "iframe" "legend" "li" "link" "main" "menu" "menuitem"
    "nav" "noframes" "ol" "optgroup" "option" "p" "param" "search"
    "section" "summary" "table" "tbody" "td" "tfoot" "th" "thead"
    "title" "tr" "track" "ul"))

(defun %bp-html-block-type (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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
               ((and (not (and (< (1+ indent) length)
                               (char= (char text (1+ indent)) #\/)))
                     (member name '("script" "pre" "style" "textarea")
                             :test #'string=))
                (values 1 (concatenate 'string "</" name) name))
               ((member name +bp-block-html-tags+ :test #'string=)
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
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
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
            ((and (<= (char-code #\1) (char-code character)
                      (char-code #\9)))
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
              (let* ((marker-columns
                       (+ columns (- marker-end indent)))
                     (space-end (%bp-skip-whitespace text marker-end))
                     (space-columns
                       (if (= space-end marker-end)
                           1
                           (%bp-whitespace-width text marker-end space-end
                                                 marker-columns)))
                     (padding (if (<= space-columns 4)
                                 space-columns
                                 1))
                     (content-end
                       (if (= space-end marker-end)
                           marker-end
                           (if (> space-columns 4)
                               (1+ marker-end)
                               space-end)))
                     (content-indent (+ marker-columns padding)))
                (values kind start-number delimiter indent marker-end
                        content-end content-indent
                        (subseq text 0 content-end))))))))))

(defun %bp-list-interrupts-paragraph-p (line)
  (multiple-value-bind (kind start-number)
      (%bp-list-marker line)
    (and kind (or (eq kind :unordered) (= start-number 1)))))

(defun %bp-interrupts-paragraph-p (line)
  (or (nth-value 0 (%bp-atx-heading line))
      (nth-value 0 (%bp-thematic-break line))
      (nth-value 0 (%bp-fence-start line))
      (%bp-list-interrupts-paragraph-p line)
      (multiple-value-bind (indent columns) (%bp-leading-indent
                                              (%block-line-text line) 3)
        (declare (ignore columns))
        (%bp-prefix-p (%block-line-text line) ">" indent))
      (multiple-value-bind (type) (%bp-html-block-type line)
        (and type (<= type 6)))
      (nth-value 0 (%bp-footnote-start line))))

(defun %bp-code-indent-line (line columns)
  (%bp-strip-columns line columns))

(defun %bp-body-from-lines (lines &key (trailing-newline t))
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

(defun %bp-parse-fenced-code (source-buffer lines index context)
  (multiple-value-bind (character fence-length indentation fence-end
                                 info-string fence-start)
      (%bp-fence-start (aref lines index))
    (declare (ignore fence-end))
    (unless character (return-from %bp-parse-fenced-code nil))
    (let ((body-lines nil)
          (end-index index)
          (closed nil))
      (loop for cursor from (1+ index) below (length lines)
            for line = (aref lines cursor)
            do (if (%bp-fence-close-p line character fence-length)
                   (setf end-index cursor closed t)
                   (progn
                     (push (%bp-code-indent-line line indentation) body-lines)
                     (setf end-index cursor)))
            when closed do (return))
      (setf body-lines (nreverse body-lines))
      (let* ((block-lines (subseq lines index (1+ end-index)))
             (last-line (aref block-lines (1- (length block-lines))))
             (span (%bp-lines-span source-buffer block-lines fence-start
                                   (length (%block-line-text last-line))))
             (node (make-code-block
                    :span span
                    :continuation-prefix context
                    :style :fenced
                    :body (%bp-body-from-lines body-lines)
                    :fence-character character
                    :fence-length fence-length
                    :indentation indentation
                    :info-string (let ((trimmed (%bp-trimmed-string info-string)))
                                   (unless (zerop (length trimmed)) trimmed)))))
        (values node (1+ end-index))))))

(defun %bp-parse-indented-code (source-buffer lines index context)
  (let ((cursor index)
        (body-lines nil)
        (last-code-index nil))
    (unless (>= (%bp-indent-columns (%block-line-text (aref lines index))) 4)
      (return-from %bp-parse-indented-code nil))
    (loop while (< cursor (length lines))
          for line = (aref lines cursor)
          do (cond
               ((>= (%bp-indent-columns (%block-line-text line)) 4)
                (push (%bp-strip-columns line 4) body-lines)
                (unless (%bp-blank-line-p line)
                  (setf last-code-index cursor)))
               ((%bp-blank-line-p line)
                (push (%bp-line-slice line 0 0) body-lines))
               (t (return)))
             (incf cursor))
    (unless last-code-index (return-from %bp-parse-indented-code nil))
    (loop while (and body-lines (%bp-blank-line-p (car body-lines)))
          do (pop body-lines))
    (setf body-lines (nreverse body-lines))
    (let* ((block-lines (subseq lines index (1+ last-code-index)))
           (block-last (aref block-lines (1- (length block-lines))))
           (node (make-code-block
                  :span (%bp-lines-span source-buffer block-lines 0
                                        (length (%block-line-text block-last)))
                  :continuation-prefix context
                  :style :indented
                  :body (%bp-body-from-lines body-lines)
                  :fence-character nil
                  :fence-length 0
                  :indentation 4
                  :info-string nil)))
      (values node (1+ last-code-index)))))

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

(defun %bp-parse-html-block (source-buffer lines index context)
  (multiple-value-bind (kind terminator name)
      (%bp-html-block-type (aref lines index))
    (declare (ignore name))
    (unless kind (return-from %bp-parse-html-block nil))
    (let ((end-index index))
      (loop for cursor from index below (length lines)
            for line = (aref lines cursor)
            do (when (and (> cursor index) (>= kind 6)
                          (%bp-blank-line-p line))
                 (return))
               (setf end-index cursor)
               (when (and (<= kind 5)
                          (%bp-html-terminator-p line terminator kind))
                 (return)))
      (let* ((block-lines (subseq lines index (1+ end-index)))
             (start (nth-value 0 (%bp-leading-indent
                                  (%block-line-text (aref block-lines 0)) 3)))
             (node (make-html-block
                    :span (%bp-lines-span
                           source-buffer block-lines start
                           (length (%block-line-text
                                    (aref block-lines (1- (length block-lines))))))
                    :continuation-prefix context
                    :body (%bp-body-from-lines block-lines)
                    :kind kind)))
        (values node (1+ end-index))))))

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
                          (nth-value 0 (%bp-leading-indent
                                        (%block-line-text line) 3))
                          (length (%block-line-text line)))
     :continuation-prefix context
     :header-row-p header-p
     :cells (nreverse cells))))

(defun %bp-parse-table (source-buffer lines index context header-line)
  (let* ((delimiter-line (aref lines (1+ index)))
         (alignments (%bp-table-alignments delimiter-line)))
    (unless (and alignments
                 (= (length alignments)
                    (length (%bp-table-cells header-line))))
      (return-from %bp-parse-table nil))
    (let ((rows (list (%bp-table-row source-buffer header-line alignments t context)))
          (cursor (+ index 2)))
      (loop while (< cursor (length lines))
            for line = (aref lines cursor)
            while (and (not (%bp-blank-line-p line))
                       (or (find #\| (%block-line-text line))
                           (= (length alignments) 1)))
            do (push (%bp-table-row source-buffer line alignments nil context)
                     rows)
               (incf cursor))
      (setf rows (nreverse rows))
      (let* ((table-lines (subseq lines index cursor))
             (table (make-table
                     :span (%bp-lines-span
                            source-buffer table-lines
                            (nth-value 0 (%bp-leading-indent
                                          (%block-line-text header-line) 3))
                            (length (%block-line-text
                                     (aref table-lines
                                           (1- (length table-lines))))))
                     :continuation-prefix context
                     :alignments alignments
                     :rows rows)))
        (values table cursor)))))

(defun %bp-table-start-p (lines index)
  (and (< (1+ index) (length lines))
       (let* ((header-line (aref lines index))
              (alignments (%bp-table-alignments (aref lines (1+ index)))))
         (and alignments
              (= (length alignments)
                 (length (%bp-table-cells header-line)))))))

(defun %bp-prefix-text (line end)
  (subseq (%block-line-text line) 0 end))

(defun %bp-make-theta (source-buffer line context marker count start)
  (make-thematic-break
   :span (%bp-line-span source-buffer line start
                        (length (%block-line-text line)))
   :continuation-prefix context
   :marker-character marker
   :marker-count count))

(defun %bp-quote-marker (line)
  (let* ((text (%block-line-text line))
         (length (length text)))
    (multiple-value-bind (indent columns) (%bp-leading-indent text 3)
      (declare (ignore columns))
      (when (and (< indent length) (char= (char text indent) #\>))
        (let ((content-start (1+ indent)))
          (when (and (< content-start length)
                     (%bp-space-p (char text content-start)))
            (incf content-start))
          (values indent content-start))))))

(defun %bp-paragraphish-p (line)
  (and (not (%bp-blank-line-p line))
       (< (%bp-indent-columns (%block-line-text line)) 4)
       (not (%bp-interrupts-paragraph-p line))))

(defun %bp-lazy-paragraph-open-p (line)
  (multiple-value-bind (quote-indent content-start) (%bp-quote-marker line)
    (declare (ignore quote-indent))
    (if content-start
        (%bp-lazy-paragraph-open-p (%bp-line-slice line content-start))
        (multiple-value-bind (kind start-number delimiter indent marker-end
                              content-end content-indent prefix)
            (%bp-list-marker line)
          (declare (ignore start-number delimiter indent marker-end
                           content-indent prefix))
          (if kind
              (and (< content-end (length (%block-line-text line)))
                   (%bp-lazy-paragraph-open-p (%bp-line-slice line content-end)))
              (%bp-paragraphish-p line))))))

(defun %bp-collect-quote-lines (lines index)
  (let ((cursor index)
        (inner-lines nil)
        (first-prefix nil)
        (paragraph-open nil))
    (loop while (< cursor (length lines))
          for line = (aref lines cursor)
          do (multiple-value-bind (indent content-start) (%bp-quote-marker line)
               (cond
                 (indent
                  (unless first-prefix
                    (setf first-prefix (%bp-prefix-text line content-start)))
                  (let ((content (%bp-line-slice line content-start)))
                    (push content inner-lines)
                    (setf paragraph-open (%bp-lazy-paragraph-open-p content)))
                  (incf cursor))
                 ((and paragraph-open (not (%bp-blank-line-p line))
                       (not (%bp-interrupts-paragraph-p line)))
                  (push line inner-lines)
                  (setf paragraph-open (%bp-lazy-paragraph-open-p line))
                  (incf cursor))
                 (t (return)))))
    (values (nreverse inner-lines) cursor first-prefix)))

(defun %bp-list-marker-record (line)
  (multiple-value-bind (kind start-number delimiter indent marker-end content-end
                             content-indent prefix)
      (%bp-list-marker line)
    (when kind
      (list kind start-number delimiter indent marker-end content-end
            content-indent prefix))))

(defun %bp-list-marker-same-level-p (line list-kind delimiter base-indent)
  (let ((record (%bp-list-marker-record line)))
    (and record
         (eq (first record) list-kind)
         (char= (third record) delimiter)
         (= (fourth record) base-indent))))

(defun %bp-item-continuation-p (line item-indent paragraph-open)
  (let* ((text (%block-line-text line))
         (indent (%bp-indent-columns text)))
    (or (>= indent item-indent)
        (and paragraph-open
             (not (%bp-blank-line-p line))
             (not (%bp-interrupts-paragraph-p line))))))

(defun %bp-item-line-content (line indentation)
  (if (>= (%bp-indent-columns (%block-line-text line)) indentation)
      (%bp-strip-columns line indentation)
      line))

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

(defun %bp-parse-list (source-buffer lines index context depth limit)
  (let* ((first-record (%bp-list-marker-record (aref lines index)))
         (kind (first first-record))
         (start-number (second first-record))
         (delimiter (third first-record))
         (base-indent (fourth first-record))
         (cursor index)
         (items nil)
         (loose-p nil)
         (list-end-index nil))
    (unless first-record (return-from %bp-parse-list nil))
    (%bp-check-depth (1+ depth) limit)
    (loop while (< cursor (length lines))
          for marker-line = (aref lines cursor)
          for record = (%bp-list-marker-record marker-line)
          do (unless (and record
                          (eq (first record) kind)
                          (char= (third record) delimiter)
                          (= (fourth record) base-indent))
               (return))
             (%bp-check-depth (+ depth 2) limit)
             (let* ((content-end (sixth record))
                    (item-indent (seventh record))
                    (content-prefix
                      (make-string item-indent :initial-element #\Space))
                    (first-content (%bp-line-slice marker-line content-end))
                    (item-lines (list first-content))
                    (item-last-index cursor)
                    (paragraph-open (%bp-paragraphish-p first-content))
                    (item-next (1+ cursor)))
               (loop while (< item-next (length lines))
                     for candidate = (aref lines item-next)
                     do (cond
                          ((%bp-blank-line-p candidate)
                           (let ((next-index (1+ item-next)))
                             (cond
                               ((and (< next-index (length lines))
                                     (%bp-list-marker-same-level-p
                                      (aref lines next-index) kind delimiter
                                      base-indent))
                                (setf loose-p t item-next next-index)
                                (return))
                               ((and (< next-index (length lines))
                                     (>= (%bp-indent-columns
                                          (%block-line-text
                                           (aref lines next-index)))
                                         item-indent))
                                (push (%bp-line-slice candidate 0 0) item-lines)
                                (setf paragraph-open nil)
                                (incf item-next))
                               (t (return)))))
                          ((%bp-list-marker-same-level-p
                            candidate kind delimiter base-indent)
                           (return))
                          ((%bp-item-continuation-p
                            candidate item-indent paragraph-open)
                           (let ((content (%bp-item-line-content
                                           candidate item-indent)))
                             (push content item-lines)
                             (setf item-last-index item-next
                                   paragraph-open
                                   (and (not (%bp-blank-line-p content))
                                        (not (%bp-interrupts-paragraph-p content))))
                             (incf item-next)))
                          (t (return))))
               (setf item-lines (nreverse item-lines))
               (let* ((task-values
                        (multiple-value-list (%bp-task-marker (first item-lines))))
                      (task-p (first task-values))
                      (checked (second task-values))
                      (task-content (third task-values))
                      (child-lines (if task-p
                                       (cons task-content (rest item-lines))
                                       item-lines))
                      (child-context (concatenate 'string context content-prefix))
                      (children (%bp-parse-lines
                                 source-buffer child-lines child-context
                                 (+ depth 2) limit))
                      (item-span
                        (%bp-lines-span
                         source-buffer
                         (subseq lines cursor (1+ item-last-index))
                         (fourth record)
                         (length (%block-line-text
                                  (aref lines item-last-index)))))
                      (item
                        (if task-p
                            (make-task-list-item
                             :span item-span
                             :continuation-prefix context
                             :children children
                             :checked-p checked
                             :task-marker-span
                             (%bp-line-span source-buffer (first item-lines) 0 3)
                             :original-marker-character
                             (char (%block-line-text (first item-lines)) 1))
                            (make-list-item
                             :span item-span
                             :continuation-prefix context
                             :children children))))
                 (push item items))
               (setf list-end-index item-last-index
                     cursor item-next)
               (unless (and (< cursor (length lines))
                            (%bp-list-marker-same-level-p
                             (aref lines cursor) kind delimiter base-indent))
                 (return))))
    (when (null items) (return-from %bp-parse-list nil))
    (setf items (nreverse items))
    (setf loose-p (or loose-p (%bp-list-items-loose-p items)))
    (let ((node (make-list-node
                 :span (%bp-lines-span
                        source-buffer (subseq lines index (1+ list-end-index))
                        (fourth first-record)
                        (length (%block-line-text (aref lines list-end-index))))
                 :continuation-prefix context
                 :kind kind
                 :tight-p (not loose-p)
                 :start-number start-number
                 :marker-character delimiter
                 :delimiter-character (if (eq kind :ordered) delimiter nil)
                 :items items)))
      (values node cursor))))

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
                        (nth-value 0 (%bp-leading-indent
                                      (%block-line-text line) 3))
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

(defun %bp-parse-footnote-definition (source-buffer lines index context depth limit)
  (multiple-value-bind (raw-label normalized content-start indent)
      (%bp-footnote-start (aref lines index))
    (unless raw-label (return-from %bp-parse-footnote-definition nil))
    (%bp-check-depth (1+ depth) limit)
    (let* ((first-line (%bp-line-slice (aref lines index) content-start))
           (inner-lines (list first-line))
           (cursor (1+ index))
           (last-line-index index))
      (loop while (< cursor (length lines))
            for line = (aref lines cursor)
            do (cond
                 ((%bp-blank-line-p line)
                  (let ((next (and (< (1+ cursor) (length lines))
                                   (aref lines (1+ cursor)))))
                    (if (and next
                             (>= (%bp-indent-columns
                                  (%block-line-text next)) 4))
                        (progn (push line inner-lines)
                               (push (%bp-strip-columns next 4) inner-lines)
                               (setf last-line-index (1+ cursor)
                                     cursor (+ cursor 2)))
                        (return))))
                 ((>= (%bp-indent-columns (%block-line-text line)) 4)
                  (push (%bp-strip-columns line 4) inner-lines)
                  (setf last-line-index cursor)
                  (incf cursor))
                 ((and (not (%bp-blank-line-p line))
                       (%bp-paragraphish-p line))
                  (push line inner-lines)
                  (setf last-line-index cursor)
                  (incf cursor))
                 (t (return))))
      (setf inner-lines (nreverse inner-lines))
      (let* ((children (%bp-parse-lines
                        source-buffer inner-lines
                        (concatenate 'string context (make-string 4
                                                                  :initial-element #\Space))
                        (1+ depth) limit))
             (node (make-footnote-definition
                    :span (%bp-lines-span
                           source-buffer
                           (subseq lines index (1+ last-line-index))
                           indent
                           (length (%block-line-text
                                    (aref lines last-line-index))))
                    :continuation-prefix context
                    :raw-label raw-label
                    :normalized-label normalized
                    :children children)))
        (unless (nth-value 1 (gethash (%bp-footnote-index-key normalized)
                                      *block-reference-index*))
          (setf (gethash (%bp-footnote-index-key normalized)
                         *block-reference-index*) node))
        (values node cursor)))))

(defun %bp-parse-lines (source-buffer lines context depth limit)
  (let ((lines (if (vectorp lines) lines (coerce lines 'vector)))
        (cursor 0)
        (nodes nil))
    (loop while (< cursor (length lines))
          for line = (aref lines cursor)
          for link-definition =
            (multiple-value-list (%bp-link-definition-at lines cursor))
          do (cond
               ((%bp-blank-line-p line)
                (incf cursor))
               ((nth-value 0 (%bp-footnote-start line))
                (multiple-value-bind (node next)
                    (%bp-parse-footnote-definition
                     source-buffer lines cursor context depth limit)
                  (push node nodes)
                  (setf cursor next)))
               ((first link-definition)
                (push (%bp-parse-reference-definition
                       source-buffer (first link-definition) context
                       (second link-definition))
                      nodes)
                (setf cursor (third link-definition)))
               ((nth-value 0 (%bp-quote-marker line))
                (%bp-check-depth (1+ depth) limit)
                (multiple-value-bind (inner-lines next prefix)
                    (%bp-collect-quote-lines lines cursor)
                  (let* ((children (%bp-parse-lines
                                    source-buffer inner-lines
                                    (concatenate 'string context prefix)
                                    (1+ depth) limit))
                         (last-line (aref lines (1- next)))
                         (marker-start
                           (nth-value 0 (%bp-quote-marker line)))
                         (node (make-block-quote
                                :span (source-buffer-span
                                       source-buffer
                                       (%bp-line-boundary line marker-start)
                                       (%bp-line-boundary
                                        last-line
                                        (length (%block-line-text last-line))))
                                :continuation-prefix context
                                :children children)))
                    (push node nodes)
                    (setf cursor next))))
               ((nth-value 0 (%bp-thematic-break line))
                (multiple-value-bind (marker count indent)
                    (%bp-thematic-break line)
                  (push (%bp-make-theta source-buffer line context marker count indent)
                        nodes))
                (incf cursor))
               ((nth-value 0 (%bp-atx-heading line))
                (multiple-value-bind (level indent content-start content-end
                                            opening closing)
                    (%bp-atx-heading line)
                  (let* ((content-line (%bp-line-slice line content-start content-end))
                         (node (make-heading
                                :span (%bp-line-span
                                       source-buffer line indent
                                       (length (%block-line-text line)))
                                :continuation-prefix context
                                :level level
                                :style :atx
                                :marker-character #\#
                                :opening-marker-length opening
                                :closing-marker-length closing
                                :children nil)))
                    (%bp-add-fragment source-buffer node (list content-line))
                    (push node nodes)))
                (incf cursor))
               ((nth-value 0 (%bp-fence-start line))
                (multiple-value-bind (node next)
                    (%bp-parse-fenced-code source-buffer lines cursor context)
                  (push node nodes)
                  (setf cursor next)))
               ((nth-value 0 (%bp-html-block-type line))
                (multiple-value-bind (node next)
                    (%bp-parse-html-block source-buffer lines cursor context)
                  (push node nodes)
                  (setf cursor next)))
               ((nth-value 0 (%bp-list-marker line))
                (multiple-value-bind (node next)
                    (%bp-parse-list source-buffer lines cursor context depth limit)
                  (push node nodes)
                  (setf cursor next)))
               ((>= (%bp-indent-columns (%block-line-text line)) 4)
                (multiple-value-bind (node next)
                    (%bp-parse-indented-code source-buffer lines cursor context)
                  (push node nodes)
                  (setf cursor next)))
               (t
                (let ((paragraph-lines (list line))
                      (next-index (1+ cursor))
                      (handled nil))
                  (loop while (< next-index (length lines))
                        for next-line = (aref lines next-index)
                        do (cond
                             ((%bp-blank-line-p next-line) (return))
                             ((and (null (cdr paragraph-lines))
                                   (%bp-table-start-p lines cursor))
                              (multiple-value-bind (table end)
                                  (%bp-parse-table source-buffer lines cursor
                                                   context line)
                                (when table
                                  (push table nodes)
                                  (setf cursor end handled t)
                                  (return))))
                             ((%bp-table-start-p lines next-index)
                              (return))
                             ((nth-value 0 (%bp-setext-level next-line))
                              (multiple-value-bind (level marker count indent)
                                  (%bp-setext-level next-line)
                                (declare (ignore indent))
                                (let* ((content-lines
                                         (%bp-prepared-paragraph-lines
                                          (nreverse paragraph-lines)))
                                       (span-lines
                                         (concatenate 'vector
                                                      (coerce content-lines 'vector)
                                                      (vector next-line)))
                                       (node (make-heading
                                              :span (%bp-lines-span
                                                     source-buffer span-lines 0
                                                     (length
                                                      (%block-line-text next-line)))
                                              :continuation-prefix context
                                              :level level
                                              :style :setext
                                              :marker-character marker
                                              :opening-marker-length 0
                                              :closing-marker-length count
                                              :children nil)))
                                  (%bp-add-fragment source-buffer node content-lines)
                                  (push node nodes)
                                  (setf cursor (+ next-index 1)
                                        handled t)
                                  (return))))
                             ((%bp-interrupts-paragraph-p next-line)
                              (return))
                             (t
                              (push next-line paragraph-lines)
                              (incf next-index))))
                  (unless handled
                    (let ((content-lines (nreverse paragraph-lines)))
                      (push (%bp-make-paragraph
                             source-buffer content-lines context)
                            nodes)
                      (setf cursor next-index)))))))
    (nreverse nodes)))

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

(defun parse-blocks (source-buffer &key (max-nesting-depth 256))
  "Parse block structure and defer paragraph, heading, and table-cell inlines."
  (check-type source-buffer source-buffer)
  (unless (and (integerp max-nesting-depth) (plusp max-nesting-depth))
    (error 'type-error :datum max-nesting-depth
           :expected-type '(integer 1 *)))
  (let* ((*block-inline-fragments* nil)
         (*block-reference-index* (make-hash-table :test 'equal))
         (lines (%bp-split-lines (source-buffer-normalized-text source-buffer)))
         (children nil))
    (multiple-value-bind (front-matter next)
        (%bp-front-matter source-buffer lines)
      (when front-matter
        (push front-matter children)
        (setf lines (subseq lines next))))
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
