(in-package #:cl-markdown-kit)
(declaim (ftype function %ip-angle-email-p %ip-reference-label-valid-p %ip-parse-range))

;; The shared scalar decoder does not apply HTML's C1 replacement table.
(defun %ip-reference-label-content (text start end)
  (with-output-to-string (output)
    (loop with index = start
          while (< index end)
          do (if (and (char= (char text index) #\\)
                      (< (1+ index) end)
                      (find (char text (1+ index)) +link-label-unescape-characters+))
                 (progn
                   (write-char (char text (1+ index)) output)
                   (incf index 2))
                 (progn
                   (write-char (char text index) output)
                   (incf index))))))

(defun %ip-angle-autolink (text start end html-closes)
  (when (and (< (1+ start) end) (char= (char text start) #\<))
    (let ((close (%ip-html-close-at html-closes 0 (1+ start) end)))
      (when close
        (let ((inner-start (1+ start))
              (inner-end close))
          (when (and (< inner-start inner-end)
                     (loop for index from inner-start below inner-end
                           for code = (char-code (char text index))
                           always (and (>= code #x21)
                                       (/= code #x7f)
                                       (not (find (char text index) +autolink-angle-excluded-characters+)))))
            (let* ((colon (position #\: text :start inner-start :end inner-end))
                   (email-at (position #\@ text :start inner-start :end inner-end)))
              (cond
                ((and colon
                      (<= 2 (- colon inner-start) 32)
                      (%pc-ascii-alpha-p (char text inner-start))
                      (loop for index from (1+ inner-start) below colon
                            always (let ((character (char text index)))
                                     (or (%pc-ascii-alphanumeric-p character)
                                         (find character +uri-scheme-extra-characters+)))))
                 (let* ((display (subseq text inner-start inner-end))
                        (destination (normalize-uri-percent-encoding display)))
                   (values destination display :angle-uri (1+ close))))
                ((and email-at
                      (> email-at inner-start)
                      (< email-at (1- inner-end))
                      (%ip-angle-email-p text inner-start inner-end email-at))
                 (let* ((display (subseq text inner-start inner-end))
                        (destination
                          (normalize-uri-percent-encoding
                           (concatenate 'string "mailto:" display))))
                   (values destination display :angle-email (1+ close))))))))))))

(defun %ip-angle-email-p (text start end at)
  (and (> at start)
       (< at (1- end))
       (loop for index from start below at
             always (find (char text index) +angle-email-local-part-characters+))
       (let ((cursor (1+ at)))
         (loop
           (let ((label-start cursor))
             (unless (%pc-ascii-alphanumeric-p (char text cursor))
               (return-from %ip-angle-email-p nil))
             (loop while (and (< cursor end)
                              (let ((character (char text cursor)))
                                (or (%pc-ascii-alphanumeric-p character)
                                    (char= character #\-))))
                   do (incf cursor))
             (when (or (> (- cursor label-start) 63)
                       (char= (char text (1- cursor)) #\-))
               (return-from %ip-angle-email-p nil))
             (cond
               ((= cursor end) (return t))
               ((char= (char text cursor) #\.)
                (incf cursor)
                (when (>= cursor end)
                  (return-from %ip-angle-email-p nil)))
               (t (return-from %ip-angle-email-p nil))))))))

(defun %ip-make-autolink (context destination display-text kind start end)
  (make-autolink :span (%ip-span context start end)
                 :destination destination
                 :display-text display-text
                 :kind kind))

(defun %ip-link-reference (context label)
  (let* ((normalized (normalize-link-label label))
         (definition (and (plusp (length normalized))
                          (gethash normalized
                                   (%ip-context-reference-index context)))))
    (when (link-reference-definition-p definition)
      definition)))

(defun %ip-footnote-reference (context label)
  (let* ((normalized (normalize-link-label label))
         (definition (and (plusp (length normalized))
                          (gethash (cons :footnote normalized)
                                   (%ip-context-reference-index context)))))
    (when (footnote-definition-p definition)
      normalized)))

(defun %ip-parse-link-destination (text index end)
  (let ((cursor index)
        (destination nil)
        (title nil))
    (setf cursor (%ip-skip-spnl text cursor end))
    (cond
      ((and (< cursor end) (char= (char text cursor) #\<))
       (let ((start (1+ cursor))
             (scan (1+ cursor)))
         (loop while (< scan end)
               for character = (char text scan)
               do (cond
                    ((char= character #\\)
                     (if (and (< (1+ scan) end)
                              (ascii-punctuation-p (char text (1+ scan))))
                         (incf scan 2)
                         (incf scan)))
                    ((char= character #\>)
                     (setf destination (%pc-unescape text start scan)
                           cursor (1+ scan))
                     (return))
                    ((or (char= character #\<)
                         (char= character #\Newline)
                         (zerop (char-code character)))
                     (return))
                    (t (incf scan)))
               finally (return))
         (unless destination (return-from %ip-parse-link-destination nil))))
      ((and (< cursor end)
            (not (or (char= (char text cursor) #\))
                     (ascii-control-p (char text cursor)))))
       (let ((start cursor)
             (depth 0))
         (loop while (< cursor end)
               for character = (char text cursor)
               do (cond
                    ((char= character #\\)
                     (if (and (< (1+ cursor) end)
                              (ascii-punctuation-p (char text (1+ cursor))))
                         (incf cursor 2)
                         (incf cursor)))
                    ((char= character #\()
                     (incf depth)
                     (incf cursor))
                    ((char= character #\))
                     (if (zerop depth)
                         (return)
                         (progn (decf depth) (incf cursor))))
                    ((or (ascii-control-p character)
                         (char= character #\Space))
                     (return))
                    (t (incf cursor)))
               finally (return))
         (unless (zerop depth)
           (return-from %ip-parse-link-destination nil))
         (setf destination (%pc-unescape text start cursor))))
      (t (setf destination "")))
    (let* ((after-destination cursor)
           (after-space (%ip-skip-spnl text cursor end))
           (had-space (> after-space after-destination)))
      (when (and had-space (< after-space end)
                 (find (char text after-space) +link-title-delimiters+))
        (multiple-value-bind (parsed-title title-end)
            (%pc-parse-title text after-space end :reject-nested-parentheses-p t)
          (when parsed-title
            (setf title parsed-title
                  cursor title-end))))
      (setf cursor (%ip-skip-spnl text cursor end))
      (when (and (< cursor end) (char= (char text cursor) #\)))
        (values (normalize-uri-percent-encoding destination)
                title (1+ cursor))))))

(defun %ip-rightmost-unescaped-close-paren (text)
  (loop with rightmost = -1
        with backslash-count = 0
        for index from 0 below (length text)
        for character = (char text index)
        do (if (char= character #\\)
               (incf backslash-count)
               (progn
                 (when (and (char= character #\))
                            (evenp backslash-count))
                   (setf rightmost index))
                 (setf backslash-count 0)))
        finally (return rightmost)))

(defun %ip-reference-candidate (context open close suffix-end)
  (let ((text (%ip-context-text context)))
    (when (and (< (1+ close) suffix-end)
               (char= (char text (1+ close)) #\[))
      (let* ((label-open (1+ close))
             (label-close (aref (%ip-context-bracket-opens context)
                                label-open)))
        (when (and (>= label-close 0)
                   (< label-close suffix-end)
                   (%ip-reference-label-valid-p context label-open label-close))
          (let* ((explicit-label (subseq text (1+ label-open) label-close))
                 (collapsed-p (zerop (length explicit-label)))
                 (raw-label (if collapsed-p
                                (subseq text (1+ open) close)
                                explicit-label))
                 (definition (%ip-link-reference
                              context
                              (%ip-reference-label-content
                               text
                               (if collapsed-p (1+ open) (1+ label-open))
                               (if collapsed-p close label-close)))))
            (when definition
              (values definition
                      (if collapsed-p :collapsed :full)
                      raw-label
                      (1+ label-close)))))))))

(defun %ip-reference-label-valid-p (context label-open label-close)
  (let* ((text (%ip-context-text context))
         (start (1+ label-open))
         (prefix (%ip-context-bracket-prefix context)))
    (and (zerop (- (aref prefix label-close)
                   (aref prefix start)))
         (or (= start label-close)
             (loop for index from start below label-close
                   for character = (char text index)
                   thereis (not (find character +reference-label-space-characters+)))))))

(defun %ip-link-candidate (context open close image-p range-end link-allowed on-match on-fail)
  (let* ((text (%ip-context-text context))
         (suffix-start (1+ close)))
    (let ((reference-suffix-p
            (and (< (1+ close) range-end)
                 (char= (char text (1+ close)) #\[)
                 (let ((suffix-close
                         (aref (%ip-context-bracket-opens context)
                               (1+ close))))
                   (and (>= suffix-close 0) (< suffix-close range-end)
                        (%ip-reference-label-valid-p
                         context (1+ close) suffix-close)))))
          (reference nil)
          (kind nil)
          (label nil)
          (destination nil)
          (title nil)
          (link-end nil))
      (when (and (< suffix-start range-end)
                 (char= (char text suffix-start) #\())
        (let ((rightmost-close
                (%ip-context-rightmost-unescaped-close-paren context)))
          (when (eq rightmost-close :unspecified)
            (setf rightmost-close (%ip-rightmost-unescaped-close-paren text)
                  (%ip-context-rightmost-unescaped-close-paren context)
                  rightmost-close))
          (when (> rightmost-close suffix-start)
            (multiple-value-bind (parsed-destination parsed-title parsed-end)
                (%ip-parse-link-destination text (1+ suffix-start) range-end)
              (when parsed-end
                (setf kind :inline
                      destination parsed-destination
                      title parsed-title
                      label nil
                      link-end parsed-end))))))
      (unless link-end
        (multiple-value-bind (definition reference-kind raw-label ref-end)
            (%ip-reference-candidate context open close range-end)
          (when definition
            (setf reference definition
                  kind reference-kind
                  label raw-label
                  destination (link-reference-definition-destination definition)
                  title (link-reference-definition-title definition)
                  link-end ref-end)))
        (unless (or link-end reference-suffix-p)
          (let* ((prefix (%ip-context-bracket-prefix context))
                 (has-inner-bracket
                   (> (aref prefix close) (aref prefix (1+ open)))))
            (when (and (not has-inner-bracket)
                       (or link-allowed image-p))
              (setf reference
                    (%ip-link-reference
                     context (%ip-reference-label-content
                              text (1+ open) close)))
              (when reference
                (setf kind :shortcut
                      label (subseq text (1+ open) close)
                      destination
                      (link-reference-definition-destination reference)
                      title (link-reference-definition-title reference)
                      link-end suffix-start))))))
      (if link-end
          (funcall on-match destination title kind label link-end)
          (funcall on-fail)))))
