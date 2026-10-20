(in-package #:cl-markdown-kit)

(defstruct (%block-line
             (:constructor %make-block-line
                 (text boundaries raw-start raw-end after column-start)))
  (text "" :type string)
  (boundaries 0 :type (or (integer 0 *) vector))
  (raw-start 0 :type (integer 0 *))
  (raw-end 0 :type (integer 0 *))
  (after 0 :type (integer 0 *))
  (column-start 0 :type (integer 0 *)))

(defun %bp-line-boundary (line index)
  (let ((boundaries (%block-line-boundaries line)))
    (if (vectorp boundaries)
        (aref boundaries index)
        (+ boundaries index))))

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
  (let ((boundaries (%block-line-boundaries line)))
    (%make-block-line
     (%bp-array-slice-view (%block-line-text line) start end)
     (if (vectorp boundaries)
         (%bp-array-slice-view boundaries start (1+ end))
         (+ boundaries start))
     (%block-line-raw-start line)
     (%block-line-raw-end line)
     (%block-line-after line)
     (%bp-line-column-at line start))))

(defun %bp-line-prefix-spaces (line count)
  (let* ((text (%block-line-text line))
         (prefix-boundary (%bp-line-boundary line 0))
         (boundaries (make-array (+ count (length text) 1)))
         (prefixed-text
           (concatenate 'string (make-string count :initial-element #\Space)
                        text)))
    (loop for index from 0 to count
          do (setf (aref boundaries index) prefix-boundary))
    (loop for index from 1 to (length text)
          do (setf (aref boundaries (+ count index))
                   (%bp-line-boundary line index)))
    (%make-block-line prefixed-text boundaries
                      (%block-line-raw-start line)
                      (%block-line-raw-end line)
                      (%block-line-after line)
                      (- (%block-line-column-start line) count))))

(defun %bp-split-lines (text)
  (let ((lines nil)
        (start 0)
        (length (length text)))
    (loop for index from 0 below length
          when (char= (char text index) #\Newline)
            do (let ((line-text (%bp-array-slice-view text start index)))
                 (push (%make-block-line line-text start start index
                                         (1+ index) 0)
                       lines))
               (setf start (1+ index)))
    (let ((line-text (%bp-array-slice-view text start length)))
      (push (%make-block-line line-text start start length length 0) lines))
    (coerce (nreverse lines) 'vector)))

(defun %bp-leading-indent (text &optional (limit 3) (initial-column 0))
  (let ((index 0)
        (column initial-column))
    (loop while (< index (length text))
          for character = (char text index)
          while (%pc-space-p character)
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
        while (%pc-space-p character)
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
                     (%pc-space-p (char text index)))
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
  (every #'%pc-space-p (%block-line-text line)))

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
  (let* ((line-vector (coerce lines 'vector))
         (line-count (length line-vector)))
    (when lines
      (let* ((text-length
               (+ (1- line-count)
                  (loop for line across line-vector
                        sum (length (%block-line-text line)))))
             (text (make-string text-length))
             (segment-starts (make-array line-count))
             (segment-maps (make-array line-count))
             (text-index 0))
        (loop for line across line-vector
              for line-index from 0
              for line-text = (%block-line-text line)
              do (setf (aref segment-starts line-index) text-index
                       (aref segment-maps line-index)
                       (%block-line-boundaries line))
                 (replace text line-text :start1 text-index)
                 (incf text-index (length line-text))
                 (when (< line-index (1- line-count))
                   (setf (char text text-index) #\Newline)
                   (incf text-index))
              finally (assert (= text-index text-length)))
        (let* ((parent-map (source-buffer-boundary-map source-buffer))
             (boundary-map
               (%make-boundary-map
                (source-buffer-source source-buffer) nil text-length #() #()
                parent-map segment-starts segment-maps))
             (fragment
               (make-inline-fragment
                :node owner :text text :boundary-map boundary-map)))
          (push fragment *block-inline-fragments*))))
    owner))

(defun %bp-prefix-p (string prefix &optional (start 0))
 (and (<= (+ start (length prefix)) (length string))
      (loop for offset below (length prefix)
            always (char= (char string (+ start offset))
                          (char prefix offset)))))

(defun %bp-skip-whitespace (text index)
  (loop while (and (< index (length text))
                   (%pc-space-p (char text index)))
        do (incf index)
        finally (return index)))

(defun %bp-link-skip-whitespace (text index)
  (loop while (and (< index (length text))
                   (unicode-whitespace-p (char text index)))
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
