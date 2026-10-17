;;;; src/inline-parser.lisp
(in-package #:cl-markdown-kit)

(declaim (ftype function %ip-angle-email-p %ip-reference-label-valid-p %ip-parse-range))

(defstruct (%ip-context
             (:constructor %ip-make-context
                 (text boundary-map reference-index limit backtick-next
                  bracket-opens bracket-closes bracket-prefix bracket-depths
                  bare-links html-closes)))
  (text "" :type string)
  boundary-map
  reference-index
  (limit +default-max-inline-delimiter-depth+ :type (integer 1 *))
  backtick-next
  bracket-opens
  bracket-closes
  bracket-prefix
  bracket-depths
  bare-links
  html-closes
  (rightmost-unescaped-close-paren :unspecified
                                   :type (or (integer -1 *)
                                             (member :unspecified)))
  (gfm-p t :type boolean))

(defstruct (%ip-delimiter
             (:constructor %ip-make-delimiter
                 (character start length can-open can-close)))
  character
  (start 0 :type (integer 0 *))
  (length 0 :type (integer 1 *))
  (remaining 0 :type (integer 0 *))
  (open-offset 0 :type (integer 0 *))
  (can-open nil :type boolean)
  (can-close nil :type boolean)
  (open-bucket nil))

(defstruct (%ip-pair
             (:constructor %ip-make-pair
                 (kind delimiter-character start inner-start inner-end end
                  &optional destination title reference-kind reference-label)))
  kind
  delimiter-character
  (start 0 :type (integer 0 *))
  (inner-start 0 :type (integer 0 *))
  (inner-end 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  destination
  title
  reference-kind
  reference-label)

(defstruct (%ip-atom (:constructor %ip-make-atom (start end node)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  node)

(defstruct (%ip-bracket
             (:constructor %ip-make-bracket (open close image-p)))
  (open 0 :type (integer 0 *))
  close
  (image-p nil :type boolean))

(defun %ip-span (context start end)
  (make-source-span (%ip-context-boundary-map context) start end))

(defun %ip-text-node (context value start end)
  (make-text :span (%ip-span context start end) :value value))

(defun %ip-skip-space (text index end)
  (loop while (and (< index end)
                   (%pc-space-p (char text index)))
        do (incf index)
        finally (return index)))

(defun %ip-skip-spnl (text index end)
  (let ((next (%ip-skip-space text index end)))
    (if (and (< next end) (char= (char text next) #\Newline))
        (%ip-skip-space text (1+ next) end)
        next)))

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

(defun %ip-backslash-escape-end (text index end)
  (when (and (< (1+ index) end)
             (char= (char text index) #\\)
             (ascii-punctuation-p (char text (1+ index))))
    (+ index 2)))

(defun %ip-build-backtick-index (text)
  (let ((positions (make-hash-table :test #'eql))
        (next-runs (make-hash-table :test #'eql))
        (index 0)
        (end (length text)))
    (loop while (< index end)
          do (cond
               ((char= (char text index) #\`)
                (let* ((run-end (%pc-backtick-run-end text index end))
                       (run-length (- run-end index))
                       (record (cons run-end -1)))
                  (setf (gethash index next-runs) record)
                  (push index (gethash run-length positions))
                  (setf index run-end)))
               (t (incf index))))
    (maphash (lambda (run-length run-positions)
               (declare (ignore run-length))
               (setf run-positions (nreverse run-positions))
               (loop for (left right) on run-positions
                     while right
                     do (setf (cdr (gethash left next-runs)) right)))
             positions)
    next-runs))

(defun %ip-backtick-close (index position)
  (let ((record (gethash position index)))
    (if record (cdr record) -1)))

(defun %ip-tag-name-end (text index end)
  (when (and (< index end) (%pc-ascii-alpha-p (char text index)))
    (loop with cursor = (1+ index)
          while (and (< cursor end)
                     (or (%pc-ascii-alphanumeric-p (char text cursor))
                         (char= (char text cursor) #\-)))
          do (incf cursor)
          finally (return cursor))))

(defun %ip-attribute-name-end (text index end)
  (when (and (< index end)
             (let ((character (char text index)))
               (or (%pc-ascii-alpha-p character)
                   (find character +html-attribute-start-characters+))))
    (loop with cursor = (1+ index)
          while (and (< cursor end)
                     (let ((character (char text cursor)))
                       (or (%pc-ascii-alphanumeric-p character)
                           (find character +html-attribute-name-extra-characters+))))
          do (incf cursor)
          finally (return cursor))))

(defun %ip-skip-html-space (text index end)
  (loop while (and (< index end) (%pc-html-space-p (char text index)))
        do (incf index)
        finally (return index)))

(defun %ip-build-pattern-next-indexes (text pattern)
  (let* ((length (length text))
         (first (search pattern text)))
    (when first
      (let ((matches (make-array length :initial-element nil))
            (next-indexes (make-array length :initial-element -1))
            (next -1)
            (search-from (1+ first)))
        (setf (aref matches first) t)
        (loop for match = (search pattern text :start2 search-from)
              while match
              do (setf (aref matches match) t
                       search-from (1+ match)))
        (loop for index downfrom (1- length) to 0
              do (when (aref matches index) (setf next index))
                 (setf (aref next-indexes index) next))
        next-indexes))))

(defun %ip-build-html-closes (text)
  (vector (%ip-build-pattern-next-indexes text ">")
          (%ip-build-pattern-next-indexes text "-->" )
          (%ip-build-pattern-next-indexes text "?>")
          (%ip-build-pattern-next-indexes text "]]>")))

(defun %ip-html-close-at (html-closes pattern-index start end)
  (let* ((indexes (aref html-closes pattern-index))
         (close (and indexes
                     (< start (length indexes))
                     (aref indexes start))))
    (and (>= (or close -1) 0) (< close end) close)))

(defun %ip-raw-html-open-tag-end (text name-end end)
  (let ((cursor name-end))
    (loop
      (let ((after-space (%ip-skip-html-space text cursor end)))
        (when (>= after-space end)
          (return-from %ip-raw-html-open-tag-end nil))
        (when (char= (char text after-space) #\>)
          (return-from %ip-raw-html-open-tag-end (1+ after-space)))
        (when (and (char= (char text after-space) #\/)
                   (< (1+ after-space) end)
                   (char= (char text (1+ after-space)) #\>))
          (return-from %ip-raw-html-open-tag-end (+ after-space 2)))
        (when (= after-space cursor)
          (return-from %ip-raw-html-open-tag-end nil))
        (setf cursor after-space))
      (let ((attribute-end (%ip-attribute-name-end text cursor end)))
        (unless attribute-end
          (return-from %ip-raw-html-open-tag-end nil))
        (let ((equals-position (%ip-skip-html-space text attribute-end end)))
          (setf cursor attribute-end)
          (when (and (< equals-position end)
                     (char= (char text equals-position) #\=))
            (setf cursor (%ip-skip-html-space text (1+ equals-position) end))
            (when (>= cursor end)
              (return-from %ip-raw-html-open-tag-end nil))
            (let ((quote (char text cursor)))
              (if (find quote +common-quote-characters+)
                  (let ((quote-end (position quote text :start (1+ cursor) :end end)))
                    (unless quote-end
                      (return-from %ip-raw-html-open-tag-end nil))
                    (setf cursor (1+ quote-end)))
                  (let ((value-start cursor))
                    (loop while (and (< cursor end)
                                     (not (%pc-html-space-p (char text cursor)))
                                     (not (find (char text cursor) +html-unquoted-attribute-excluded-characters+)))
                          do (incf cursor))
                    (when (= value-start cursor)
                      (return-from %ip-raw-html-open-tag-end nil)))))))))))

(defun %ip-raw-html-end (text start end html-closes)
  (cond
    ((and (<= (+ start 4) end)
          (string= text "<!--" :start1 start :end1 (+ start 4)))
     (cond
       ((and (< (+ start 4) end)
             (char= (char text (+ start 4)) #\>))
        (+ start 5))
       ((and (<= (+ start 6) end)
             (string= text "<!--->" :start1 start :end1 (+ start 6)))
        (+ start 6))
       (t
        (let ((close (%ip-html-close-at html-closes 1 (+ start 4) end)))
          (when close (+ close 3))))))
    ((and (< (1+ start) end)
          (char= (char text start) #\<)
          (char= (char text (1+ start)) #\?))
     (let ((close (%ip-html-close-at html-closes 2 (+ start 2) end)))
       (when close (+ close 2))))
    ((and (<= (+ start 9) end)
          (string= text "<![CDATA[" :start1 start :end1 (+ start 9)))
     (let ((close (%ip-html-close-at html-closes 3 (+ start 9) end)))
       (when close (+ close 3))))
    ((and (< (+ start 2) end)
          (char= (char text start) #\<)
          (char= (char text (1+ start)) #\!)
          (%pc-ascii-alpha-p (char text (+ start 2))))
     (let ((close (%ip-html-close-at html-closes 0 (+ start 2) end)))
       (when close (1+ close))))
    ((and (< (1+ start) end)
          (char= (char text start) #\<)
          (char= (char text (1+ start)) #\/))
     (let ((name-end (%ip-tag-name-end text (+ start 2) end)))
       (when name-end
         (let ((cursor (%ip-skip-html-space text name-end end)))
           (when (and (< cursor end) (char= (char text cursor) #\>))
             (1+ cursor))))))
    ((and (< (1+ start) end)
          (char= (char text start) #\<)
          (%pc-ascii-alpha-p (char text (1+ start))))
     (let ((name-end (%ip-tag-name-end text (1+ start) end)))
       (when name-end
         (%ip-raw-html-open-tag-end text name-end end))))
    (t nil)))

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

(defun %ip-build-bracket-index (text backtick-next html-closes depth-limit)
  (unless (find #\[ text)
    (return-from %ip-build-bracket-index (values nil nil nil nil)))
  (let* ((length (length text))
         (opens (make-array length :initial-element -1))
         (closes (make-array length :initial-element -1))
         (open-p (make-array length :initial-element nil))
         (bracket-deltas (make-array length :initial-element 0))
         (stack nil)
         (stack-depth 0)
         (index 0))
    (loop while (< index length)
          do (cond
               ((char= (char text index) #\\)
                (setf index (or (%ip-backslash-escape-end text index length)
                                (1+ index))))
               ((char= (char text index) #\`)
                (let* ((run-end (%pc-backtick-run-end text index length backtick-next))
                       (close (%ip-backtick-close backtick-next index)))
                  (setf index (if (and (>= close 0) (< close length))
                                  (+ close (- run-end index))
                                  run-end))))
               ((char= (char text index) #\<)
                (multiple-value-bind (destination display kind close)
                    (%ip-angle-autolink text index length html-closes)
                  (declare (ignore destination display kind))
                  (setf index (or close
                                  (%ip-raw-html-end text index length html-closes)
                                  (1+ index)))))
               ((char= (char text index) #\[)
                (%pc-check-depth (1+ stack-depth) depth-limit :inline)
                (incf stack-depth)
                (incf (aref bracket-deltas index))
                (setf (aref open-p index) t)
                (push index stack)
                (incf index))
               ((and (char= (char text index) #\]) stack)
                (let ((open (pop stack)))
                  (decf stack-depth)
                  (decf (aref bracket-deltas index))
                  (setf (aref opens open) index
                        (aref closes index) open)
                  (incf index)))
               (t (incf index))))
    (let ((prefix (make-array (1+ length) :initial-element 0))
          (depths (make-array (1+ length) :initial-element 0)))
      (loop for index below length
            do (setf (aref prefix (1+ index))
                     (+ (aref prefix index) (if (aref open-p index) 1 0))
                     (aref depths (1+ index))
                     (+ (aref depths index) (aref bracket-deltas index))))
      (values opens closes prefix depths))))

(defun %ip-build-bare-links (text)
  (let ((links (make-hash-table :test #'eql))
        (index 0)
        (end (length text)))
    (loop while (< index end)
          do (let ((candidate (%pc-bare-candidate-at text index end)))
               (if candidate
                   (progn
                     (setf (gethash index links) candidate
                           index (%pc-candidate-end candidate)))
                   (if (%pc-email-local-character-p (char text index))
                       (setf index
                             (loop with cursor = index
                                   while (and (< cursor end)
                                              (%pc-email-local-character-p (char text cursor)))
                                   do (incf cursor)
                                   finally (return cursor)))
                       (incf index)))))
    links))

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

(defun %ip-link-candidate (context open close image-p range-end link-allowed)
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
          (values destination title kind label link-end)
          (values nil nil nil nil nil)))))

(defun %ip-flanking (text start end run-end)
  (let* ((before (if (> start 0) (char text (1- start)) nil))
         (after (if (< run-end end) (char text run-end) nil))
         (before-space (or (null before) (unicode-whitespace-p before)))
         (after-space (or (null after) (unicode-whitespace-p after)))
         (before-punctuation (and before (%pc-commonmark-punctuation-p before)))
         (after-punctuation (and after (%pc-commonmark-punctuation-p after)))
         (left (and (not after-space)
                    (or (not after-punctuation)
                        before-space before-punctuation)))
         (right (and (not before-space)
                     (or (not before-punctuation)
                         after-space after-punctuation))))
    (values left right before-punctuation after-punctuation)))

(defun %ip-delimiter-bucket-index (delimiter)
  (let ((character (%ip-delimiter-character delimiter))
        (length (%ip-delimiter-length delimiter)))
    (cond
      ((char= character #\~) 6)
      (t (+ (if (%ip-delimiter-can-close delimiter) 3 0)
            (mod length 3))))))

(defun %ip-rule-of-three-blocked-p (opener closer)
  (and (or (%ip-delimiter-can-close opener)
           (%ip-delimiter-can-open closer))
       (not (zerop (mod (%ip-delimiter-length closer) 3)))
       (zerop (mod (+ (%ip-delimiter-length opener)
                      (%ip-delimiter-length closer))
                   3))))

(defun %ip-delimiter-buckets (openers character)
  (or (gethash character openers)
      (setf (gethash character openers) (make-array 7 :initial-element nil))))

(defun %ip-best-opener (openers closer)
  (let* ((character (%ip-delimiter-character closer))
         (buckets (gethash character openers))
         (best nil))
    (when buckets
      (loop for bucket-index below (if (char= character #\~) 7 6)
            for stack = (aref buckets bucket-index)
            for candidate = (first stack)
            when (and candidate
                      (%ip-delimiter-can-open candidate)
                      (or (char= character #\~)
                          (not (%ip-rule-of-three-blocked-p candidate closer)))
                      (or (not (char= character #\~))
                          (and (member (%ip-delimiter-remaining candidate) '(1 2))
                               (= (%ip-delimiter-remaining candidate)
                                  (%ip-delimiter-remaining closer))))
                      (or (null best)
                          (> (%ip-delimiter-start candidate)
                             (%ip-delimiter-start best))))
              do (setf best candidate)))
    best))

(defun %ip-pop-opener-if-empty
    (openers delimiter active-count active-stack total-active-count)
  (when (zerop (%ip-delimiter-remaining delimiter))
    (let* ((buckets (gethash (%ip-delimiter-character delimiter) openers))
           (bucket-index (%ip-delimiter-open-bucket delimiter)))
      ;; The selected opener remains at the head of both stacks until removed.
      (pop (aref buckets bucket-index))
      (pop (car active-stack))
      (setf (%ip-delimiter-open-bucket delimiter) nil)
      (decf (car active-count))
      (decf (car total-active-count)))))

(defun %ip-add-delimiter (start end character can-open can-close
                          openers delimiters pairs active-count active-stack
                          total-active-count bracket-depth depth-limit)
  (let* ((length (- end start))
         (run (%ip-make-delimiter character start length can-open can-close)))
    (setf (%ip-delimiter-remaining run) length)
    (push run delimiters)
    (when can-close
      (loop while (plusp (%ip-delimiter-remaining run))
            for opener = (%ip-best-opener openers run)
            while opener
            for opener-remaining = (%ip-delimiter-remaining opener)
            for closer-remaining = (%ip-delimiter-remaining run)
            for use-count =
              (cond
                ((char= character #\~) closer-remaining)
                ((and (>= opener-remaining 2) (>= closer-remaining 2)) 2)
                (t 1))
            for open-start = (+ (%ip-delimiter-start opener)
                                (%ip-delimiter-open-offset opener)
                                (- opener-remaining use-count))
            for close-start = (+ start (- length closer-remaining))
            do (loop until (eq (first (car active-stack)) opener)
                     for skipped = (pop (car active-stack))
                     do (let* ((bucket-index
                                 (%ip-delimiter-open-bucket skipped))
                               (buckets
                                 (gethash (%ip-delimiter-character skipped)
                                          openers)))
                          (pop (aref buckets bucket-index))
                          (setf (%ip-delimiter-open-bucket skipped) nil)
                          (decf (car active-count))
                          (decf (car total-active-count))))
               (push (%ip-make-pair
                      (cond ((char= character #\~) :strikethrough)
                            ((= use-count 2) :strong)
                            (t :emphasis))
                      character open-start (+ open-start use-count)
                      close-start (+ close-start use-count))
                     pairs)
               (decf (%ip-delimiter-remaining opener) use-count)
               (decf (%ip-delimiter-remaining run) use-count)
               (incf (%ip-delimiter-open-offset run) use-count)
               (%ip-pop-opener-if-empty
                openers opener active-count active-stack total-active-count)))
    (when (and can-open (plusp (%ip-delimiter-remaining run))
               (or (not (char= character #\~))
                   (member (%ip-delimiter-remaining run) '(1 2))))
      (let* ((bucket-index (%ip-delimiter-bucket-index run))
             (buckets (%ip-delimiter-buckets openers character)))
        (%pc-check-depth (+ bracket-depth (car total-active-count) 1) depth-limit :inline)
        (setf (%ip-delimiter-open-bucket run) bucket-index)
        (push run (aref buckets bucket-index))
        (push run (car active-stack))
        (incf (car active-count))
        (incf (car total-active-count))))
    (values delimiters pairs)))

(defun %ip-resolve-delimiter-runs
    (runs pairs bracket-depths bracket-closes range-start range-end depth-limit)
  (let* ((containers
           (coerce (sort (copy-list pairs)
                         #'< :key #'%ip-pair-inner-start)
                   'vector))
         (container-index 0)
         (container-stack nil)
         (groups (make-hash-table :test #'eq)))
    (dolist (run (sort (copy-list runs) #'< :key #'%ip-delimiter-start))
      (let ((position (%ip-delimiter-start run)))
        (loop while (and container-stack
                         (>= position
                             (%ip-pair-inner-end (first container-stack))))
              do (pop container-stack))
        (loop while (and (< container-index (length containers))
                         (<= (%ip-pair-inner-start
                              (aref containers container-index))
                             position))
              for container = (aref containers container-index)
              do (when (< position (%ip-pair-inner-end container))
                   (push container container-stack))
              (incf container-index))
        (push run (gethash (first container-stack) groups))))
    (let* ((run-events nil)
           (states (make-hash-table :test #'eq))
           ;; Unmatched records remain counted after their scope ends.
           (total-active-count (list 0))
           (resolved-delimiters nil)
           (resolved-pairs pairs)
           (bracket-count 0)
           (position range-start))
      (maphash (lambda (scope scoped-runs)
                 (dolist (run scoped-runs)
                   (push (cons scope run) run-events)))
               groups)
      (setf run-events
            (sort run-events #'<
                  :key (lambda (event)
                         (%ip-delimiter-start (cdr event)))))
      (labels ((advance-bracket-position ()
                 (when bracket-depths
                   (let ((delta
                           (- (aref bracket-depths (1+ position))
                              (aref bracket-depths position))))
                     (cond
                       ((plusp delta)
                        (%pc-check-depth (+ bracket-count (car total-active-count) 1) depth-limit :inline)
                        (incf bracket-count))
                       ((and (minusp delta)
                             (>= (aref bracket-closes position) range-start))
                        (decf bracket-count))))))
               (process-run (scope run)
                 (let* ((state
                          (or (gethash scope states)
                              (setf (gethash scope states)
                                    (list (make-hash-table :test #'eql)
                                          nil nil (list 0) (list nil)))))
                        (openers (first state))
                        (delimiters (second state))
                        (matched-pairs (third state))
                        (scope-count (fourth state))
                        (scope-stack (fifth state)))
                   (multiple-value-setq (delimiters matched-pairs)
                     (%ip-add-delimiter
                      (%ip-delimiter-start run)
                      (+ (%ip-delimiter-start run)
                         (%ip-delimiter-length run))
                      (%ip-delimiter-character run)
                      (%ip-delimiter-can-open run)
                      (%ip-delimiter-can-close run)
                      openers delimiters matched-pairs scope-count scope-stack
                      total-active-count bracket-count depth-limit))
                   (setf (second state) delimiters
                         (third state) matched-pairs))))
        (dolist (event run-events)
          (let ((run-position (%ip-delimiter-start (cdr event))))
            (loop while (< position run-position)
                  do (advance-bracket-position)
                     (incf position))
            (process-run (car event) (cdr event))
            (advance-bracket-position)
            (incf position)))
        (loop while (< position range-end)
              do (advance-bracket-position)
                 (incf position))
        (maphash (lambda (scope state)
                   (declare (ignore scope))
                   (setf resolved-delimiters
                         (nconc (second state) resolved-delimiters)
                         resolved-pairs
                         (nconc (third state) resolved-pairs)))
                 states))
      (values resolved-delimiters resolved-pairs))))

(defun %ip-delimiter-text-atoms (context delimiters pairs)
  (let* ((text (%ip-context-text context))
         (used (make-array (length text) :element-type 'bit :initial-element 0))
         (atoms nil))
    (dolist (pair pairs)
      (loop for position from (%ip-pair-start pair)
              below (%ip-pair-inner-start pair)
            do (setf (sbit used position) 1))
      (loop for position from (%ip-pair-inner-end pair)
              below (%ip-pair-end pair)
            do (setf (sbit used position) 1)))
    (dolist (delimiter delimiters)
      (let ((cursor (%ip-delimiter-start delimiter))
            (end (+ (%ip-delimiter-start delimiter)
                     (%ip-delimiter-length delimiter))))
        (loop while (< cursor end)
              do (if (= 1 (sbit used cursor))
                     (incf cursor)
                     (let ((literal-start cursor))
                       (loop while (and (< cursor end)
                                        (zerop (sbit used cursor)))
                             do (incf cursor))
                       (push (%ip-make-atom
                              literal-start cursor
                              (%ip-text-node context
                                             (subseq text literal-start cursor)
                                             literal-start cursor))
                             atoms)))))
    atoms)))

(defun %ip-pair-node (context pair children)
  (let ((span (%ip-span context (%ip-pair-start pair) (%ip-pair-end pair))))
    (case (%ip-pair-kind pair)
      (:link
       (make-link :span span
                  :destination (%ip-pair-destination pair)
                  :title (%ip-pair-title pair)
                  :reference-kind (%ip-pair-reference-kind pair)
                  :reference-label (%ip-pair-reference-label pair)
                  :children children))
      (:image
       (make-image :span span
                   :destination (%ip-pair-destination pair)
                   :title (%ip-pair-title pair)
                   :reference-kind (%ip-pair-reference-kind pair)
                   :reference-label (%ip-pair-reference-label pair)
                   :children children))
      (:emphasis
       (make-emphasis :span span
                      :delimiter-character (%ip-pair-delimiter-character pair)
                      :children children))
      (:strong
       (make-strong :span span
                    :delimiter-character (%ip-pair-delimiter-character pair)
                    :children children))
      (:strikethrough
       (make-strikethrough :span span :delimiter-character #\~
                           :children children)))))

(defun %ip-build-range (context start end atoms pairs base-depth)
  (let* ((events (make-array (- end start) :initial-element nil))
         (children (make-hash-table :test #'eq))
         (child-tails (make-hash-table :test #'eq))
         (root nil)
         (root-tail nil)
         (pair-stack nil))
    (labels ((queue-event (position event)
               (push event (aref events (- position start))))
             (append-entry (owner entry)
               (let ((cell (list entry)))
                 (if owner
                     (let ((tail (gethash owner child-tails)))
                       (if tail
                           (setf (cdr tail) cell)
                           (setf (gethash owner children) cell))
                       (setf (gethash owner child-tails) cell))
                     (progn
                       (if root-tail
                           (setf (cdr root-tail) cell)
                           (setf root cell))
                       (setf root-tail cell)))))
             (attach-event (event position)
               (loop while (and pair-stack
                                (>= position
                                    (%ip-pair-inner-end (first pair-stack))))
                     do (pop pair-stack))
               (let ((owner (first pair-stack)))
                 (append-entry owner event))
               (when (eq (car event) :pair)
                 (%pc-check-depth (+ base-depth (length pair-stack) 1) (%ip-context-limit context) :inline)
                 (push (cdr event) pair-stack)))
             (render (entries range-start range-end)
               (let ((output nil)
                     (cursor range-start)
                     (pending-text nil)
                     (pending-text-start nil)
                     (pending-text-end nil))
                 (labels ((flush-text ()
                            (when pending-text
                              (push (%ip-text-node
                                     context
                                     (get-output-stream-string pending-text)
                                     pending-text-start pending-text-end)
                                    output)
                              (setf pending-text nil
                                    pending-text-start nil
                                    pending-text-end nil)))
                          (add-text (value text-start text-end)
                            (unless pending-text
                              (setf pending-text (make-string-output-stream)
                                    pending-text-start text-start))
                            (write-string value pending-text)
                            (setf pending-text-end text-end))
                          (emit-atom (atom)
                            (let ((node (%ip-atom-node atom)))
                              (if (text-p node)
                                  (add-text (text-value node)
                                            (%ip-atom-start atom)
                                            (%ip-atom-end atom))
                                  (progn (flush-text) (push node output)))))
                          (emit-gap (gap-end)
                            (when (< cursor gap-end)
                              (add-text (subseq (%ip-context-text context)
                                                cursor gap-end)
                                        cursor gap-end))))
                   (dolist (entry entries)
                     (if (eq (car entry) :pair)
                         (let ((pair (cdr entry)))
                           (emit-gap (%ip-pair-start pair))
                           (flush-text)
                           (push (%ip-pair-node
                                  context pair
                                  (render (gethash pair children)
                                          (%ip-pair-inner-start pair)
                                          (%ip-pair-inner-end pair)))
                                 output)
                           (setf cursor (%ip-pair-end pair)))
                         (let ((atom (cdr entry)))
                           (emit-gap (%ip-atom-start atom))
                           (emit-atom atom)
                           (setf cursor (%ip-atom-end atom)))))
                   (emit-gap range-end)
                   (flush-text)
                   (nreverse output)))))
      (dolist (atom atoms)
        (when (and (<= start (%ip-atom-start atom))
                   (< (%ip-atom-start atom) end)
                   (<= (%ip-atom-end atom) end))
          (queue-event (%ip-atom-start atom) (cons :atom atom))))
      (dolist (pair pairs)
        (when (and (<= start (%ip-pair-start pair))
                   (< (%ip-pair-start pair) end)
                   (<= (%ip-pair-end pair) end))
          (queue-event (%ip-pair-start pair) (cons :pair pair))))
      (loop for position from start below end
            for queued = (nreverse (aref events (- position start)))
            do (dolist (event queued) (attach-event event position)))
      (render root start end))))

(defun %ip-register-atom (atoms node start end)
  (push (%ip-make-atom start end node) atoms)
  atoms)

(defun %ip-parse-range (context start end link-allowed base-depth)
  (let* ((text (%ip-context-text context))
         (atoms nil)
         (delimiter-runs nil)
         (pairs nil)
         (bracket-stack nil)
         (bracket-count 0)
         (link-opener-barrier -1)
         (index start))
    (labels ((add-node (node node-start node-end)
               (setf atoms (%ip-register-atom atoms node node-start node-end)))
             (add-text (value node-start node-end)
               (add-node (%ip-text-node context value node-start node-end)
                         node-start node-end))
             (current-range-end ()
               (let ((bracket (first bracket-stack)))
                 (if (and bracket (< index (%ip-bracket-close bracket)))
                     (min end (%ip-bracket-close bracket))
                     end)))
             (parent-range-end ()
               (let ((parent (second bracket-stack)))
                 (if parent
                     (min end (%ip-bracket-close parent))
                     end)))
             (push-bracket (open close image-p)
               (%pc-check-depth (+ base-depth bracket-count 1) (%ip-context-limit context) :inline)
               (push (%ip-make-bracket open close image-p) bracket-stack)
               (incf bracket-count))
             (pop-bracket ()
               (pop bracket-stack)
               (decf bracket-count))
             (add-bracket-opener-text (bracket)
               (let* ((open (%ip-bracket-open bracket))
                      (image-p (%ip-bracket-image-p bracket))
                      (text-start (if image-p (1- open) open))
                      (text-value (if image-p "![" "[")))
                 (add-text text-value text-start (1+ open))))
             (close-bracket ()
               (let* ((bracket (first bracket-stack))
                      (open (and bracket
                                 (aref (%ip-context-bracket-closes context)
                                       index))))
                 (when (and bracket open (>= open 0)
                            (= open (%ip-bracket-open bracket)))
                   (let ((image-p (%ip-bracket-image-p bracket)))
                     (multiple-value-bind (destination title kind label link-end)
                         (if (or image-p link-allowed)
                             (%ip-link-candidate
                              context open index image-p (parent-range-end)
                              link-allowed)
                             (values nil nil nil nil nil))
                       (if (and link-end
                                (or image-p (> open link-opener-barrier)))
                           (progn
                             (push (%ip-make-pair
                                    (if image-p :image :link)
                                    nil
                                    (if image-p (1- open) open)
                                    (1+ open)
                                    index
                                    link-end
                                    destination
                                    title
                                    (or kind :inline)
                                    label)
                                   pairs)
                             (unless image-p
                               (setf link-opener-barrier open))
                             (pop-bracket)
                             (setf index link-end))
                           (progn
                             (add-bracket-opener-text bracket)
                             (pop-bracket)
                             (add-text "]" index (1+ index))
                             (incf index)))))
                   t)))
             (emit-delimiter (run-end character)
               (multiple-value-bind (left right before-punctuation after-punctuation)
                   (%ip-flanking text index (length text) run-end)
                 (let* ((underscore-p (char= character #\_))
                        (can-open (if underscore-p
                                      (and left (or (not right) before-punctuation))
                                      left))
                        (can-close (if underscore-p
                                       (and right (or (not left) after-punctuation))
                                       right))
                        (run-length (- run-end index)))
                   (if (or (and (char= character #\~)
                                (not (%ip-context-gfm-p context)))
                           (and (char= character #\~) (> run-length 2)))
                       (add-text (subseq text index run-end) index run-end)
                       (if (or can-open can-close)
                           (push (%ip-make-delimiter
                                  character index run-length can-open can-close)
                                 delimiter-runs)
                           (add-text (subseq text index run-end) index run-end))))
                 (setf index run-end))))
      (loop while (< index end)
            for character = (char text index)
            for scan-end = (current-range-end)
            for candidate = (gethash index (%ip-context-bare-links context))
            do (cond
                 ((and candidate (<= (%pc-candidate-end candidate) scan-end))
                  (if link-allowed
                      (progn
                        (add-node (%ip-make-autolink
                                   context (%pc-candidate-destination candidate)
                                   (%pc-candidate-display-text candidate)
                                   (%pc-candidate-kind candidate)
                                   index (%pc-candidate-end candidate))
                                  index (%pc-candidate-end candidate))
                        (setf link-opener-barrier index))
                      (add-text (subseq text index (%pc-candidate-end candidate))
                                index (%pc-candidate-end candidate)))
                  (setf index (%pc-candidate-end candidate)))
                 ((char= character #\\)
                  (cond
                    ((and (< (1+ index) scan-end)
                          (char= (char text (1+ index)) #\Newline))
                     (let ((break-end (%ip-skip-space text (+ index 2) scan-end)))
                       (add-node (make-hard-break
                                  :span (%ip-span context index break-end))
                                 index break-end)
                       (setf index break-end)))
                    ((%ip-backslash-escape-end text index scan-end)
                     (add-text (string (char text (1+ index))) index (+ index 2))
                     (incf index 2))
                    (t (add-text "\\" index (1+ index))
                       (incf index))))
                 ((char= character #\`)
                  (let* ((run-end (%pc-backtick-run-end text index scan-end (%ip-context-backtick-next context)))
                         (close (%ip-backtick-close
                                 (%ip-context-backtick-next context) index)))
                    (if (and (>= close 0) (< close scan-end)
                             (<= (+ close (- run-end index)) scan-end))
                        (let* ((close-end (+ close (- run-end index)))
                               (literal (subseq text run-end close)))
                          (setf literal (substitute #\Space #\Newline literal))
                          (when (and (> (length literal) 0)
                                     (char= (char literal 0) #\Space)
                                     (char= (char literal (1- (length literal)))
                                            #\Space)
                                     (find-if-not (lambda (char)
                                                    (char= char #\Space))
                                                  literal))
                            (setf literal (subseq literal 1 (1- (length literal)))))
                          (add-node (make-code-span
                                     :span (%ip-span context index close-end)
                                     :literal-text literal
                                     :backtick-run-length (- run-end index))
                                    index close-end)
                          (setf index close-end))
                        (progn
                          (add-text (subseq text index run-end) index run-end)
                          (setf index run-end)))))
                 ((char= character #\&)
                  (multiple-value-bind (value next) (%pc-entity-at text index scan-end)
                    (if value
                        (progn (add-text value index next) (setf index next))
                        (progn (add-text "&" index (1+ index)) (incf index)))))
                 ((char= character #\<)
                  (multiple-value-bind (destination display kind autolink-end)
                      (%ip-angle-autolink text index scan-end
                                          (%ip-context-html-closes context))
                    (let ((html-end (and (not autolink-end)
                                         (%ip-raw-html-end
                                          text index scan-end
                                          (%ip-context-html-closes context)))))
                      (cond
                        ((and autolink-end link-allowed)
                         (add-node (%ip-make-autolink context destination display kind
                                                      index autolink-end)
                                   index autolink-end)
                         (setf link-opener-barrier index
                               index autolink-end))
                        ((and autolink-end (not link-allowed))
                         (add-text (subseq text index autolink-end)
                                   index autolink-end)
                         (setf index autolink-end))
                        (html-end
                         (add-node (make-html-inline
                                    :span (%ip-span context index html-end)
                                    :body (subseq text index html-end))
                                   index html-end)
                         (setf index html-end))
                        (t (add-text "<" index (1+ index)) (incf index))))))
                 ((char= character #\Newline)
                  (let ((break-end (%ip-skip-space text (1+ index) scan-end)))
                    (add-node (make-soft-break
                               :span (%ip-span context index break-end))
                              index break-end)
                    (setf index break-end)))
                 ((%pc-space-p character)
                  (let ((space-end (%ip-skip-space text index scan-end)))
                    (if (and (< space-end scan-end)
                             (char= (char text space-end) #\Newline))
                        (let* ((line-end (1+ space-end))
                               (break-end (%ip-skip-space text line-end scan-end))
                               (spaces (loop for position from index below space-end
                                             count (char= (char text position)
                                                          #\Space))))
                          (if (>= spaces 2)
                              (add-node (make-hard-break
                                         :span (%ip-span context index
                                                         break-end))
                                        index break-end)
                              (add-node
                               (make-soft-break
                                :span (%ip-span context index break-end))
                               index break-end))
                          (setf index break-end))
                        (progn
                          (add-text (subseq text index space-end) index space-end)
                          (setf index space-end)))))
                 ((and (char= character #\[)
                       (<= (+ index 2) scan-end)
                       (char= (char text (1+ index)) #\^)
                       (%ip-context-gfm-p context)
                       (let ((close (aref (%ip-context-bracket-opens context) index)))
                         (and (>= close 0) (< close scan-end)
                              (let ((normalized
                                      (%ip-footnote-reference
                                       context
                                       (%pc-unescape text (+ index 2) close))))
                                (when normalized
                                  (add-node
                                   (make-footnote-reference
                                    :span (%ip-span context index (1+ close))
                                    :raw-label (subseq text (+ index 2) close)
                                    :normalized-label normalized)
                                   index (1+ close))
                                  (setf index (1+ close))
                                  t)))))
                  nil)
                 ((char= character #\!)
                  (if (and (< (1+ index) scan-end)
                           (char= (char text (1+ index)) #\[))
                      (let* ((open (1+ index))
                             (close (aref (%ip-context-bracket-opens context) open))
                             (footnote-p
                               (and (%ip-context-gfm-p context)
                                    (>= close 0)
                                    (< close scan-end)
                                    (< (+ open 2) close)
                                    (char= (char text (+ open 1)) #\^)
                                    (%ip-footnote-reference
                                     context (%pc-unescape text (+ open 2) close)))))
                        (cond
                          (footnote-p
                           (add-text "!" index (1+ index))
                           (incf index))
                          ((and (>= close 0) (< close scan-end))
                           (push-bracket open close t)
                           (incf index 2))
                          (t
                           (add-text "![" index (+ index 2))
                           (incf index 2))))
                      (progn (add-text "!" index (1+ index)) (incf index))))
                 ((char= character #\[)
                  (let ((close (aref (%ip-context-bracket-opens context) index)))
                    (if (and (>= close 0) (< close scan-end))
                        (push-bracket index close nil)
                        (add-text "[" index (1+ index)))
                    (incf index)))
                 ((char= character #\])
                  (unless (close-bracket)
                    (add-text "]" index (1+ index))
                    (incf index)))
                 ((find character +inline-delimiter-characters+)
                  (let ((run-end index))
                    (loop while (and (< run-end scan-end)
                                     (char= (char text run-end) character))
                          do (incf run-end))
                    (emit-delimiter run-end character)))
                 (t
                  (let ((run-end index))
                    (loop while (and (< run-end scan-end)
                                     (not (find (char text run-end) +inline-text-special-characters+))
                                     (not (gethash run-end
                                                   (%ip-context-bare-links context))))
                          do (incf run-end))
                    (when (= run-end index) (incf run-end))
                    (add-text (subseq text index run-end) index run-end)
                    (setf index run-end)))))
      (if (and (null delimiter-runs) (null pairs))
          (%ip-build-range context start end (nreverse atoms) nil base-depth)
          (multiple-value-bind (delimiters resolved-pairs)
              (%ip-resolve-delimiter-runs
               delimiter-runs pairs (%ip-context-bracket-depths context)
               (%ip-context-bracket-closes context) start end
               (%ip-context-limit context))
            (let* ((literal-atoms (%ip-delimiter-text-atoms
                                   context delimiters resolved-pairs))
                   (all-atoms (nconc (nreverse atoms) literal-atoms)))
              (%ip-build-range context start end all-atoms resolved-pairs
                               base-depth)))))))

(defun %ip-inline-end (text)
  (loop with end = (length text)
        while (and (plusp end)
                   (%pc-space-p (char text (1- end))))
          do (decf end)
        finally (return end)))

(defun parse-inline (text boundary-map reference-index
                     &key (max-inline-delimiter-depth +default-max-inline-delimiter-depth+) (gfm t))
  "Parse inline Markdown TEXT using its source map and reference index."
  (check-type text string)
  (check-type boundary-map boundary-map)
  (check-type reference-index hash-table)
  (check-type gfm boolean)
  (unless (and (integerp max-inline-delimiter-depth)
               (plusp max-inline-delimiter-depth))
    (error 'type-error :datum max-inline-delimiter-depth
           :expected-type '(integer 1 *)))
  (unless (= (length text) (boundary-map-length boundary-map))
    (error "Inline text length ~D does not match boundary map length ~D."
           (length text) (boundary-map-length boundary-map)))
  (let* ((backtick-next (%ip-build-backtick-index text))
         (html-closes (%ip-build-html-closes text))
         (bracket-indexes (multiple-value-list
                           (%ip-build-bracket-index
                            text backtick-next html-closes
                            max-inline-delimiter-depth)))
         (bare-links (if gfm
                         (%ip-build-bare-links text)
                         (make-hash-table :test #'eql)))
         (context (%ip-make-context text boundary-map reference-index
                                    max-inline-delimiter-depth backtick-next
                                    (first bracket-indexes)
                                    (second bracket-indexes)
                                    (third bracket-indexes)
                                    (fourth bracket-indexes)
                                    bare-links html-closes)))
    (setf (%ip-context-gfm-p context) gfm)
    (%ip-parse-range context 0 (%ip-inline-end text) t 0)))
