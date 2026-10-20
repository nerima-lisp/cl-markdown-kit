(in-package #:cl-markdown-kit)

(defun %bp-footnote-index-key (normalized-label)
  (cons :footnote normalized-label))

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
                     (values (%pc-unescape (subseq text start index))
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
                        (or (unicode-whitespace-p character)
                            (find character '(#\< #\>))))
                   (return)))
                (incf index))
       (when (or escaped (plusp depth) (= start index))
         (return-from %bp-parse-destination nil))
       (values (%pc-unescape (subseq text start index)) index)))))

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
                    (%pc-unescape raw-label)))
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
                    (if (find (char text after-space) +link-title-delimiters+)
                        (progn
                          (multiple-value-bind (parsed-title title-end)
                              (%pc-parse-title text after-space (length text) :backslash-mode :any-character)
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
                              (unless (every #'unicode-whitespace-p
                                             (subseq text title-end title-line-end))
                                (return-from %bp-link-definition nil)))
                            (setf title parsed-title end title-end)))
                        (progn
                          (unless (every #'unicode-whitespace-p
                                         (subseq text destination-end
                                                 destination-line-end))
                            (return-from %bp-link-definition nil)))))
                  (values raw-label normalized
                          destination
                          title
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
               (find (char text index) +link-title-delimiters+))
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
                  (let* ((extended-end (1+ close-line))
                         (extended-line
                           (%bp-join-lines (subseq lines index extended-end)))
                         (extended-values
                           (multiple-value-list
                            (%bp-link-definition extended-line))))
                    (when (first extended-values)
                      (setf end extended-end
                            candidate-line extended-line
                            parsed-values extended-values))))))))
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

(defun %bp-link-definition-start-p (line)
  (let* ((text (%block-line-text line))
         (indent (nth-value 0 (%bp-line-leading-indent line 3))))
    (and (< indent (length text))
         (char= (char text indent) #\[)
         (not (%bp-prefix-p text "[^" indent)))))

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
