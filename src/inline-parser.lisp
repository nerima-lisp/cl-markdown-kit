;;;; src/inline-parser.lisp
(in-package #:cl-markdown-kit)

(declaim (ftype function %ip-angle-email-p %ip-decode-numeric-reference
                %ip-reference-label-valid-p
                %ip-linkish-in-tree-p
                %ip-parse-range %ip-parse-title))

(defstruct (%ip-context
             (:constructor %ip-make-context
                 (text boundary-map reference-index limit backtick-next
                  bracket-opens bracket-closes bracket-prefix bare-links
                  html-closes)))
  (text "" :type string)
  boundary-map
  reference-index
  (limit 4096 :type (integer 1 *))
  backtick-next
  bracket-opens
  bracket-closes
  bracket-prefix
  bare-links
  html-closes)

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
                 (kind delimiter-character open-run-start close-run-start
                  start inner-start inner-end end)))
  kind
  delimiter-character
  (open-run-start 0 :type (integer 0 *))
  (close-run-start 0 :type (integer 0 *))
  (start 0 :type (integer 0 *))
  (inner-start 0 :type (integer 0 *))
  (inner-end 0 :type (integer 0 *))
  (end 0 :type (integer 0 *)))

(defstruct (%ip-atom (:constructor %ip-make-atom (start end node)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  node)

(defstruct (%ip-bracket
             (:constructor %ip-make-bracket (open close image-p active-p)))
  (open 0 :type (integer 0 *))
  close
  (image-p nil :type boolean)
  (active-p t :type boolean))

(defstruct (%ip-candidate
             (:constructor %ip-make-candidate
                 (start end destination display-text kind)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  (destination "" :type string)
  (display-text "" :type string)
  kind)

(defun %ip-check-depth (depth limit)
  (when (> depth limit)
    (error 'markdown-inline-delimiter-depth-exceeded
           :limit-name :inline-delimiter-depth
           :limit limit
           :actual depth
           :context "inline parsing"
           :message "maximum inline delimiter depth exceeded")))

(defun %ip-span (context start end)
  (make-source-span (%ip-context-boundary-map context) start end))

(defun %ip-text-node (context value start end)
  (make-text :span (%ip-span context start end) :value value))

(defun %ip-ascii-alpha-p (character)
  (let ((code (char-code character)))
    (or (<= #x41 code #x5A) (<= #x61 code #x7A))))

(defun %ip-ascii-digit-p (character)
  (let ((code (char-code character)))
    (<= #x30 code #x39)))

(defun %ip-ascii-alphanumeric-p (character)
  (or (%ip-ascii-alpha-p character) (%ip-ascii-digit-p character)))

(defun %ip-ascii-prefix-p (text start prefix)
  (and (<= (+ start (length prefix)) (length text))
       (loop for offset below (length prefix)
             for actual = (char text (+ start offset))
             for expected = (char prefix offset)
             always (char-equal actual expected))))

(defun %ip-skip-space (text index end)
  (loop while (and (< index end)
                   (find (char text index) '(#\Space #\Tab)))
        do (incf index)
        finally (return index)))

(defun %ip-skip-spnl (text index end)
  (let ((next (%ip-skip-space text index end)))
    (if (and (< next end) (char= (char text next) #\Newline))
        (%ip-skip-space text (1+ next) end)
        next)))

(defun %ip-entity-at (text start end)
  (when (and (< (1+ start) end) (char= (char text start) #\&))
    (let ((cursor (1+ start)))
      (cond
        ((and (< cursor end) (char= (char text cursor) #\#))
         (incf cursor)
         (let ((radix 10))
           (when (and (< cursor end) (find (char text cursor) "xX"))
             (setf radix 16)
             (incf cursor))
           (let ((digits-start cursor))
             (loop while (and (< cursor end)
                              (if (= radix 16)
                                  (or (%ip-ascii-digit-p (char text cursor))
                                      (find (char text cursor) "abcdefABCDEF"))
                                  (%ip-ascii-digit-p (char text cursor))))
                   do (incf cursor))
             (when (and (> cursor digits-start)
                        (< cursor end)
                        (char= (char text cursor) #\;))
               (let ((value (%ip-decode-numeric-reference
                             (subseq text digits-start cursor) radix)))
                 (when value (values value (1+ cursor))))))))
        ((%ip-ascii-alpha-p (char text cursor))
         (let ((name-start cursor))
           (loop while (and (< cursor end)
                            (< (- cursor name-start) 32)
                            (or (%ip-ascii-alphanumeric-p (char text cursor))))
                 do (incf cursor))
           (when (and (< cursor end) (char= (char text cursor) #\;))
             (let ((value (lookup-html5-entity
                           (subseq text name-start (1+ cursor)))))
               (when value (values value (1+ cursor)))))))))))

;; The shared scalar decoder does not apply HTML's C1 replacement table.
(defun %ip-decode-numeric-reference (digits radix)
  (let ((decoded (decode-numeric-character-reference digits radix)))
    (when decoded
      (let ((replacement
              (case (parse-integer digits :radix radix)
                (#x80 #x20AC) (#x82 #x201A) (#x83 #x0192) (#x84 #x201E)
                (#x85 #x2026) (#x86 #x2020) (#x87 #x2021) (#x88 #x02C6)
                (#x89 #x2030) (#x8A #x0160) (#x8B #x2039) (#x8C #x0152)
                (#x8E #x017D) (#x91 #x2018) (#x92 #x2019) (#x93 #x201C)
                (#x94 #x201D) (#x95 #x2022) (#x96 #x2013) (#x97 #x2014)
                (#x98 #x02DC) (#x99 #x2122) (#x9A #x0161) (#x9B #x203A)
                (#x9C #x0153) (#x9E #x017E) (#x9F #x0178))))
        (if replacement
            (string (code-char replacement))
            decoded)))))

(defun %ip-unescape (text start end)
  (with-output-to-string (output)
    (loop with index = start
          while (< index end)
          do (cond
               ((and (char= (char text index) #\\)
                     (< (1+ index) end)
                     (ascii-punctuation-p (char text (1+ index))))
                (write-char (char text (1+ index)) output)
                (incf index 2))
               (t
                (multiple-value-bind (value next) (%ip-entity-at text index end)
                  (if value
                      (progn (write-string value output) (setf index next))
                      (progn (write-char (char text index) output)
                             (incf index)))))))))

(defun %ip-reference-label-content (text start end)
  (with-output-to-string (output)
    (loop with index = start
          while (< index end)
          do (if (and (char= (char text index) #\\)
                      (< (1+ index) end)
                      (find (char text (1+ index)) "[]"))
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

(defun %ip-backtick-run-end (text index end)
  (loop with cursor = index
        while (and (< cursor end) (char= (char text cursor) #\`))
        do (incf cursor)
        finally (return cursor)))

(defun %ip-build-backtick-index (text)
  (let ((positions (make-hash-table :test #'eql))
        (next-runs (make-array (length text) :initial-element -1))
        (index 0)
        (end (length text)))
    (loop while (< index end)
          do (cond
               ((char= (char text index) #\\)
                (setf index (or (%ip-backslash-escape-end text index end)
                                (1+ index))))
               ((char= (char text index) #\`)
                (let* ((run-end (%ip-backtick-run-end text index end))
                       (run-length (- run-end index)))
                  (push index (gethash run-length positions))
                  (setf index run-end)))
               (t (incf index))))
    (maphash (lambda (run-length run-positions)
               (declare (ignore run-length))
               (setf run-positions (nreverse run-positions))
               (loop for (left right) on run-positions
                     while right
                     do (setf (aref next-runs left) right)))
             positions)
    next-runs))

(defun %ip-tag-name-end (text index end)
  (when (and (< index end) (%ip-ascii-alpha-p (char text index)))
    (loop with cursor = (1+ index)
          while (and (< cursor end)
                     (or (%ip-ascii-alphanumeric-p (char text cursor))
                         (char= (char text cursor) #\-)))
          do (incf cursor)
          finally (return cursor))))

(defun %ip-attribute-name-end (text index end)
  (when (and (< index end)
             (let ((character (char text index)))
               (or (%ip-ascii-alpha-p character)
                   (find character "_:"))))
    (loop with cursor = (1+ index)
          while (and (< cursor end)
                     (let ((character (char text cursor)))
                       (or (%ip-ascii-alphanumeric-p character)
                           (find character ":._-"))))
          do (incf cursor)
          finally (return cursor))))

(defun %ip-html-space-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun %ip-skip-html-space (text index end)
  (loop while (and (< index end) (%ip-html-space-p (char text index)))
        do (incf index)
        finally (return index)))

(defun %ip-build-pattern-next-indexes (text pattern)
  (let* ((length (length text))
         (matches (make-array length :initial-element nil))
         (next-indexes (make-array length :initial-element -1))
         (next -1)
         (search-from 0))
    (loop for match = (search pattern text :start2 search-from)
          while match
          do (setf (aref matches match) t
                   search-from (1+ match)))
    (loop for index downfrom (1- length) to 0
          do (when (aref matches index) (setf next index))
             (setf (aref next-indexes index) next))
    next-indexes))

(defun %ip-build-html-closes (text)
  (vector (%ip-build-pattern-next-indexes text ">")
          (%ip-build-pattern-next-indexes text "-->" )
          (%ip-build-pattern-next-indexes text "?>")
          (%ip-build-pattern-next-indexes text "]]>")))

(defun %ip-html-close-at (html-closes pattern-index start end)
  (let* ((indexes (and html-closes (aref html-closes pattern-index)))
         (close (and indexes (< start (length indexes))
                     (aref indexes start))))
    (and (>= (or close -1) 0) (< close end) close)))

(defun %ip-raw-html-open-tag-end (text name-end end)
  (let ((cursor name-end))
    (loop
      (setf cursor (%ip-skip-html-space text cursor end))
      (when (>= cursor end)
        (return-from %ip-raw-html-open-tag-end nil))
      (when (char= (char text cursor) #\>)
        (return-from %ip-raw-html-open-tag-end (1+ cursor)))
      (when (and (char= (char text cursor) #\/)
                 (< (1+ cursor) end)
                 (char= (char text (1+ cursor)) #\>))
        (return-from %ip-raw-html-open-tag-end (+ cursor 2)))
      (let ((attribute-end (%ip-attribute-name-end text cursor end)))
        (unless attribute-end
          (return-from %ip-raw-html-open-tag-end nil))
        (setf cursor (%ip-skip-html-space text attribute-end end))
        (when (and (< cursor end) (char= (char text cursor) #\=))
          (setf cursor (%ip-skip-html-space text (1+ cursor) end))
          (when (>= cursor end)
            (return-from %ip-raw-html-open-tag-end nil))
          (let ((quote (char text cursor)))
            (if (find quote "\"'")
                (let ((quote-end (position quote text :start (1+ cursor) :end end)))
                  (unless quote-end
                    (return-from %ip-raw-html-open-tag-end nil))
                  (setf cursor (1+ quote-end)))
                (let ((value-start cursor))
                  (loop while (and (< cursor end)
                                   (not (%ip-html-space-p (char text cursor)))
                                   (not (find (char text cursor) "\"'=<>`")))
                        do (incf cursor))
                  (when (= value-start cursor)
                    (return-from %ip-raw-html-open-tag-end nil))))))))))

(defun %ip-raw-html-end (text start end &optional html-closes)
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
        (let ((close (or (%ip-html-close-at html-closes 1 (+ start 4) end)
                         (and (null html-closes)
                              (search "-->" text :start2 (+ start 4)
                                      :end2 end)))))
          (when close (+ close 3))))))
    ((and (< (1+ start) end)
          (char= (char text start) #\<)
          (char= (char text (1+ start)) #\?))
     (let ((close (or (%ip-html-close-at html-closes 2 (+ start 2) end)
                      (and (null html-closes)
                           (search "?>" text :start2 (+ start 2) :end2 end)))))
       (when close (+ close 2))))
    ((and (<= (+ start 9) end)
          (string= text "<![CDATA[" :start1 start :end1 (+ start 9)))
     (let ((close (or (%ip-html-close-at html-closes 3 (+ start 9) end)
                      (and (null html-closes)
                           (search "]]>" text :start2 (+ start 9) :end2 end)))))
       (when close (+ close 3))))
    ((and (< (+ start 2) end)
          (char= (char text start) #\<)
          (char= (char text (1+ start)) #\!)
          (let ((code (char-code (char text (+ start 2)))))
            (<= (char-code #\A) code (char-code #\Z))))
     (let ((close (or (%ip-html-close-at html-closes 0 (+ start 2) end)
                      (and (null html-closes)
                           (position #\> text :start (+ start 2) :end end)))))
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
          (%ip-ascii-alpha-p (char text (1+ start))))
     (let ((name-end (%ip-tag-name-end text (1+ start) end)))
       (when name-end
         (%ip-raw-html-open-tag-end text name-end end))))
    (t nil)))

(defun %ip-angle-autolink (text start end &optional html-closes)
  (when (and (< (1+ start) end) (char= (char text start) #\<))
    (let ((close (if html-closes
                     (%ip-html-close-at html-closes 0 (1+ start) end)
                     (position #\> text :start (1+ start) :end end))))
      (when close
        (let ((inner-start (1+ start))
              (inner-end close))
          (when (and (< inner-start inner-end)
                     (loop for index from inner-start below inner-end
                           for code = (char-code (char text index))
                           always (and (>= code #x21)
                                       (/= code #x7f)
                                       (not (find (char text index) "<>")))))
            (let* ((colon (position #\: text :start inner-start :end inner-end))
                   (email-at (position #\@ text :start inner-start :end inner-end)))
              (cond
                ((and colon
                      (<= 2 (- colon inner-start) 32)
                      (%ip-ascii-alpha-p (char text inner-start))
                      (loop for index from (1+ inner-start) below colon
                            always (let ((character (char text index)))
                                     (or (%ip-ascii-alphanumeric-p character)
                                         (find character "+.-")))))
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
             always (find (char text index)
                          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.!#$%&'*+/=?^_`{|}~-"))
       (let ((cursor (1+ at)))
         (loop
           (let ((label-start cursor))
             (unless (and (< cursor end)
                          (%ip-ascii-alphanumeric-p (char text cursor)))
               (return-from %ip-angle-email-p nil))
             (loop while (and (< cursor end)
                              (let ((character (char text cursor)))
                                (or (%ip-ascii-alphanumeric-p character)
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
  (let* ((length (length text))
         (opens (make-array length :initial-element -1))
         (closes (make-array length :initial-element -1))
         (open-p (make-array length :initial-element nil))
         (stack nil)
         (stack-depth 0)
         (index 0))
    (loop while (< index length)
          do (cond
               ((char= (char text index) #\\)
                (setf index (or (%ip-backslash-escape-end text index length)
                                (1+ index))))
               ((char= (char text index) #\`)
                (let* ((run-end (%ip-backtick-run-end text index length))
                       (close (aref backtick-next index)))
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
                (%ip-check-depth (1+ stack-depth) depth-limit)
                (incf stack-depth)
                (setf (aref open-p index) t)
                (push index stack)
                (incf index))
               ((and (char= (char text index) #\]) stack)
                (let ((open (pop stack)))
                  (decf stack-depth)
                  (setf (aref opens open) index
                        (aref closes index) open)
                  (incf index)))
               (t (incf index))))
    (let ((prefix (make-array (1+ length) :initial-element 0)))
      (loop for index below length
            do (setf (aref prefix (1+ index))
                     (+ (aref prefix index) (if (aref open-p index) 1 0))))
      (values opens closes prefix))))

(defun %ip-email-local-character-p (character)
  (or (%ip-ascii-alphanumeric-p character)
      (find character ".-_+")))

(defun %ip-gfm-domain-character-p (character)
  (or (%ip-ascii-alphanumeric-p character)
      (find character "_-")))

(defun %ip-gfm-domain-end (text start end &optional email-p)
  (let ((cursor start)
        (segments nil))
    (loop
      (let ((segment-start cursor))
        (loop while (and (< cursor end)
                         (%ip-gfm-domain-character-p (char text cursor)))
              do (incf cursor))
        (when (= cursor segment-start)
          (return-from %ip-gfm-domain-end nil))
        (push (cons segment-start cursor) segments))
      (cond
        ((= cursor end) (return))
        ((char= (char text cursor) #\.)
         (if (and (< (1+ cursor) end)
                  (%ip-gfm-domain-character-p (char text (1+ cursor))))
             (incf cursor)
             (return)))
        (t (return))))
    (let* ((ordered-segments (nreverse segments))
           (last-two (last ordered-segments 2))
           (domain-end (cdr (car (last ordered-segments)))))
      (when (and (> (length ordered-segments) 1)
                 (or email-p
                     (every (lambda (segment)
                              (not (position #\_ text
                                             :start (car segment)
                                             :end (cdr segment))))
                            last-two))
                 (or (not email-p)
                     (not (find (char text (1- domain-end)) "-_"))))
        domain-end))))

(defun %ip-email-domain-end (text start end)
  (%ip-gfm-domain-end text start end t))

(defun %ip-url-end (text start end)
  (loop with cursor = start
        while (and (< cursor end)
                   (let ((character (char text cursor)))
                     (and (not (unicode-whitespace-p character))
                          (not (char= character #\<)))))
        do (incf cursor)
        finally (return cursor)))

(defun %ip-trim-url-end (text start end)
  (let ((open-parentheses 0)
        (close-parentheses 0)
        (cursor end))
    (loop for index from start below end
          do (case (char text index)
               (#\( (incf open-parentheses))
               (#\) (incf close-parentheses))))
    (loop while (> cursor start)
          for character = (char text (1- cursor))
          while (or (find character "?!.,:*_~")
                    (and (char= character #\))
                         (> close-parentheses open-parentheses)))
          do (when (char= character #\))
               (decf close-parentheses))
             (decf cursor))
    (when (and (> cursor start)
               (char= (char text (1- cursor)) #\;))
      (let ((ampersand (position #\& text :start start :end (1- cursor)
                                 :from-end t)))
        (when (and ampersand
                   (< ampersand (1- cursor))
                   (< (1+ ampersand) (1- cursor))
                   (loop for index from (1+ ampersand) below (1- cursor)
                         always (%ip-ascii-alphanumeric-p (char text index))))
          (setf cursor ampersand))))
    cursor))

(defun %ip-bare-candidate-at (text index end)
  (when (or (= index 0)
            (let ((previous (char text (1- index))))
              (or (unicode-whitespace-p previous)
                  (find previous "*_~("))))
    (cond
      ((or (%ip-ascii-prefix-p text index "https://")
           (%ip-ascii-prefix-p text index "http://")
           (%ip-ascii-prefix-p text index "www."))
       (let* ((www-p (%ip-ascii-prefix-p text index "www."))
              (prefix-length (cond (www-p 4)
                                   ((%ip-ascii-prefix-p text index "https://") 8)
                                   (t 7)))
              (domain-end (%ip-gfm-domain-end
                           text (+ index prefix-length) end))
              (raw-end (and domain-end (%ip-url-end text index end)))
              (link-end (and raw-end (%ip-trim-url-end text index raw-end))))
         (when (and link-end (>= link-end domain-end))
           (let* ((display (subseq text index link-end))
                  (destination
                    (normalize-uri-percent-encoding
                     (if www-p
                         (concatenate 'string "http://" display)
                         display))))
             (%ip-make-candidate index link-end destination display :bare-url)))))
      ((%ip-email-local-character-p (char text index))
       (let ((local-end index))
         (loop while (and (< local-end end)
                          (%ip-email-local-character-p (char text local-end)))
               do (incf local-end))
         (when (and (< local-end end) (char= (char text local-end) #\@))
           (let ((domain-end (%ip-email-domain-end text (1+ local-end) end)))
             (when domain-end
               (let* ((display (subseq text index domain-end))
                      (destination
                        (normalize-uri-percent-encoding
                         (concatenate 'string "mailto:" display))))
                 (%ip-make-candidate index domain-end destination display
                                     :bare-email))))))))))

(defun %ip-build-bare-links (text)
  (let ((links (make-hash-table :test #'eql))
        (index 0)
        (end (length text)))
    (loop while (< index end)
          do (let ((candidate (%ip-bare-candidate-at text index end)))
               (if candidate
                   (progn
                     (setf (gethash index links) candidate
                           index (%ip-candidate-end candidate)))
                   (if (%ip-email-local-character-p (char text index))
                       (setf index
                             (loop with cursor = index
                                   while (and (< cursor end)
                                              (%ip-email-local-character-p
                                               (char text cursor)))
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
                     (setf destination (%ip-unescape text start scan)
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
                     (ascii-control-p (char text cursor))
                     (char= (char text cursor) #\Space))))
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
         (setf destination (%ip-unescape text start cursor))))
      (t (setf destination "")))
    (let* ((after-destination cursor)
           (after-space (%ip-skip-spnl text cursor end))
           (had-space (> after-space after-destination)))
      (when (and had-space (< after-space end)
                 (find (char text after-space) "\"'("))
        (multiple-value-bind (parsed-title title-end)
            (%ip-parse-title text after-space end)
          (when parsed-title
            (setf title parsed-title
                  cursor title-end))))
      (setf cursor (%ip-skip-spnl text cursor end))
      (when (and (< cursor end) (char= (char text cursor) #\)))
        (values (normalize-uri-percent-encoding destination)
                title (1+ cursor))))))

(defun %ip-parse-title (text index end)
  (let* ((opening (char text index))
         (closing (if (char= opening #\() #\) opening))
         (cursor (1+ index)))
    (loop while (< cursor end)
          for character = (char text cursor)
          do (cond
               ((char= character #\\)
                (if (and (< (1+ cursor) end)
                         (ascii-punctuation-p (char text (1+ cursor))))
                    (incf cursor 2)
                    (incf cursor)))
               ((char= character closing)
                (return (values (%ip-unescape text (1+ index) cursor)
                                (1+ cursor))))
               ((and (char= opening #\() (char= character #\())
                (return-from %ip-parse-title (values nil nil)))
               (t (incf cursor)))
          finally (return (values nil nil)))))

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
                   thereis (not (find character '(#\Space #\Tab #\Newline))))))))

(defun %ip-link-candidate (context open image-p range-end link-allowed
                           base-depth)
  (let* ((text (%ip-context-text context))
         (close (and (< open range-end)
                     (aref (%ip-context-bracket-opens context) open))))
    (unless (and (>= (or close -1) 0) (< close range-end))
      (return-from %ip-link-candidate nil))
    (let ((suffix-start (1+ close))
          (reference-suffix-p
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
        (multiple-value-bind (parsed-destination parsed-title parsed-end)
            (%ip-parse-link-destination text (1+ suffix-start) range-end)
          (when parsed-end
            (setf kind :inline
                  destination parsed-destination
                  title parsed-title
                  label nil
                  link-end parsed-end))))
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
      (unless link-end (return-from %ip-link-candidate nil))
      (%ip-check-depth (1+ base-depth) (%ip-context-limit context))
      (let* ((children (%ip-parse-range context (1+ open) close
                                        link-allowed
                                        (1+ base-depth)))
             (nested-link-p
               (and (not image-p)
                    (%ip-linkish-in-tree-p children))))
        (when nested-link-p
          (return-from %ip-link-candidate nil))
        (let ((span-start (if image-p (1- open) open))
              (span-end link-end))
          (values (if image-p
                      (make-image :span (%ip-span context span-start span-end)
                                  :destination destination :title title
                                  :reference-kind (or kind :inline)
                                  :reference-label label :children children)
                      (make-link :span (%ip-span context span-start span-end)
                                 :destination destination :title title
                                 :reference-kind (or kind :inline)
                                 :reference-label label :children children))
                  link-end))))))

(defun %ip-node-link-like-p (node)
  (or (link-p node) (autolink-p node)))

(defun %ip-linkish-in-tree-p (nodes)
  (some (lambda (node)
          (or (%ip-node-link-like-p node)
              (%ip-linkish-in-tree-p (node-children node))))
        nodes))

(defun %ip-flanking (text start end run-end)
  (let* ((before (if (> start 0) (char text (1- start)) nil))
         (after (if (< run-end end) (char text run-end) nil))
         (before-space (or (null before) (unicode-whitespace-p before)))
         (after-space (or (null after) (unicode-whitespace-p after)))
         (before-punctuation (and before (unicode-punctuation-p before)))
         (after-punctuation (and after (unicode-punctuation-p after)))
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
                          (and (>= (%ip-delimiter-remaining candidate) 2)
                               (>= (%ip-delimiter-remaining closer) 2)))
                      (or (null best)
                          (> (%ip-delimiter-start candidate)
                             (%ip-delimiter-start best))))
              do (setf best candidate)))
    best))

(defun %ip-pop-opener-if-empty (openers delimiter active-count active-stack)
  (when (zerop (%ip-delimiter-remaining delimiter))
    (let* ((buckets (gethash (%ip-delimiter-character delimiter) openers))
           (bucket-index (%ip-delimiter-open-bucket delimiter))
           (stack (aref buckets bucket-index)))
      (when (eq (first stack) delimiter)
        (pop (aref buckets bucket-index)))
      (when (eq (first (car active-stack)) delimiter)
        (pop (car active-stack)))
      (setf (%ip-delimiter-open-bucket delimiter) nil)
      (decf (car active-count)))))

(defun %ip-add-delimiter (context text start end character can-open can-close
                          openers delimiters pairs active-count active-stack
                          bracket-count
                          base-depth)
  (declare (ignore text))
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
                ((char= character #\~)
                 (if (and (>= opener-remaining 2) (>= closer-remaining 2)) 2 0))
                ((and (>= opener-remaining 2) (>= closer-remaining 2)) 2)
                (t 1))
            while (plusp use-count)
            for open-start = (+ (%ip-delimiter-start opener)
                                (%ip-delimiter-open-offset opener)
                                (- opener-remaining use-count))
            for close-start = (+ start (- length closer-remaining))
            do (loop while (and (first (car active-stack))
                                (not (eq (first (car active-stack)) opener)))
                     for skipped = (pop (car active-stack))
                     do (let* ((bucket-index
                                 (%ip-delimiter-open-bucket skipped))
                               (buckets
                                 (gethash (%ip-delimiter-character skipped)
                                          openers))
                               (stack (aref buckets bucket-index)))
                          (when (eq (first stack) skipped)
                            (pop (aref buckets bucket-index)))
                          (setf (%ip-delimiter-open-bucket skipped) nil)
                          (decf (car active-count))))
               (push (%ip-make-pair
                      (cond ((char= character #\~) :strikethrough)
                            ((= use-count 2) :strong)
                            (t :emphasis))
                      character (%ip-delimiter-start opener) start
                      open-start (+ open-start use-count)
                      close-start (+ close-start use-count))
                     pairs)
               (decf (%ip-delimiter-remaining opener) use-count)
               (decf (%ip-delimiter-remaining run) use-count)
               (incf (%ip-delimiter-open-offset run) use-count)
               (%ip-pop-opener-if-empty openers opener active-count active-stack)
               (when (and (char= character #\~)
                          (= (%ip-delimiter-remaining opener) 1))
                 (setf (%ip-delimiter-remaining opener) 0)
                 (%ip-pop-opener-if-empty openers opener active-count
                                          active-stack))))
    (when (and can-open (plusp (%ip-delimiter-remaining run))
               (or (not (char= character #\~))
                   (>= (%ip-delimiter-remaining run) 2)))
      (%ip-check-depth (+ base-depth (car active-count) bracket-count 1)
                       (%ip-context-limit context))
      (let* ((bucket-index (%ip-delimiter-bucket-index run))
             (buckets (%ip-delimiter-buckets openers character)))
        (setf (%ip-delimiter-open-bucket run) bucket-index)
        (push run (aref buckets bucket-index))
        (push run (car active-stack))
        (incf (car active-count))))
    (values delimiters pairs)))

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

(defun %ip-build-range (context start end atoms pairs)
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

(defun %ip-register-atom (context atoms node start end)
  (declare (ignore context))
  (push (%ip-make-atom start end node) atoms)
  atoms)

(defun %ip-bracket-stack-pop (stack open)
  (let ((entry (find open stack :key #'%ip-bracket-open :test #'eql)))
    (when entry
      (setf stack (remove entry stack :test #'eq :count 1))
      (values stack entry))))

(defun %ip-parse-range (context start end link-allowed base-depth)
  (let* ((text (%ip-context-text context))
         (atoms nil)
         (delimiters nil)
         (pairs nil)
         (openers (make-hash-table :test #'eql))
         (active-count (list 0))
         (active-stack (list nil))
         (bracket-stack nil)
         (bracket-count 0)
         (index start))
    (labels ((add-node (node node-start node-end)
               (setf atoms (%ip-register-atom context atoms node node-start node-end)))
             (add-text (value node-start node-end)
               (add-node (%ip-text-node context value node-start node-end)
                         node-start node-end))
             (push-bracket (open close image-p active-p)
               (%ip-check-depth (+ base-depth (car active-count)
                                   bracket-count 1)
                                (%ip-context-limit context))
               (push (%ip-make-bracket open close image-p active-p)
                     bracket-stack)
               (incf bracket-count))
             (pop-bracket-at (close)
               (let ((open (aref (%ip-context-bracket-closes context) close)))
                 (when (>= open 0)
                   (multiple-value-bind (new-stack entry)
                       (%ip-bracket-stack-pop bracket-stack open)
                     (when entry
                       (setf bracket-stack new-stack)
                       (decf bracket-count))))))
             (deactivate-links ()
               (dolist (bracket bracket-stack)
                 (unless (%ip-bracket-image-p bracket)
                   (setf (%ip-bracket-active-p bracket) nil))))
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
                   (if (and (char= character #\~) (< run-length 2))
                       (add-text (subseq text index run-end) index run-end)
                       (if (or can-open can-close)
                           (multiple-value-setq (delimiters pairs)
                             (%ip-add-delimiter
                              context text index run-end character can-open can-close
                              openers delimiters pairs active-count active-stack
                              bracket-count
                              base-depth))
                           (add-text (subseq text index run-end) index run-end))))
                 (setf index run-end))))
      (loop while (< index end)
            for character = (char text index)
            do (let ((candidate (gethash index (%ip-context-bare-links context))))
                 (cond
                   ((and candidate (<= (%ip-candidate-end candidate) end))
                    (if link-allowed
                        (add-node (%ip-make-autolink
                                   context (%ip-candidate-destination candidate)
                                   (%ip-candidate-display-text candidate)
                                   (%ip-candidate-kind candidate)
                                   index (%ip-candidate-end candidate))
                                  index (%ip-candidate-end candidate))
                        (add-text (subseq text index (%ip-candidate-end candidate))
                                  index (%ip-candidate-end candidate)))
                    (setf index (%ip-candidate-end candidate)))
                   ((char= character #\\)
                    (cond
                      ((and (< (1+ index) end)
                            (char= (char text (1+ index)) #\Newline))
                       (add-node (make-hard-break
                                  :span (%ip-span context index (+ index 2)))
                                 index (+ index 2))
                       (incf index 2))
                      ((%ip-backslash-escape-end text index end)
                       (add-text (string (char text (1+ index))) index (+ index 2))
                       (incf index 2))
                      (t (add-text "\\" index (1+ index))
                         (incf index))))
                   ((char= character #\`)
                    (let* ((run-end (%ip-backtick-run-end text index end))
                           (close (aref (%ip-context-backtick-next context) index)))
                      (if (and (>= close 0) (< close end)
                               (<= (+ close (- run-end index)) end))
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
                    (multiple-value-bind (value next) (%ip-entity-at text index end)
                      (if value
                          (progn (add-text value index next) (setf index next))
                          (progn (add-text "&" index (1+ index)) (incf index)))))
                   ((char= character #\<)
                    (multiple-value-bind (destination display kind autolink-end)
                        (%ip-angle-autolink text index end
                                            (%ip-context-html-closes context))
                      (let ((html-end (and (not autolink-end)
                                           (%ip-raw-html-end
                                            text index end
                                            (%ip-context-html-closes context)))))
                        (cond
                          ((and autolink-end link-allowed)
                           (add-node (%ip-make-autolink context destination display kind
                                                        index autolink-end)
                                     index autolink-end)
                           (setf index autolink-end))
                          ((and autolink-end (not link-allowed))
                           (add-text (subseq text index autolink-end)
                                     index autolink-end)
                           (setf index autolink-end))
                          (html-end
                           (let ((html-end html-end))
                           (add-node (make-html-inline
                                      :span (%ip-span context index html-end)
                                      :body (subseq text index html-end))
                                     index html-end)
                           (setf index html-end)))
                          (t (add-text "<" index (1+ index)) (incf index))))))
                   ((char= character #\Newline)
                    (add-node (make-soft-break :span (%ip-span context index (1+ index)))
                              index (1+ index))
                    (incf index))
                   ((find character '(#\Space #\Tab))
                    (let ((space-end (%ip-skip-space text index end)))
                      (if (and (< space-end end)
                               (char= (char text space-end) #\Newline))
                          (let ((spaces (loop for position from index below space-end
                                              count (char= (char text position)
                                                           #\Space))))
                            (if (>= spaces 2)
                                (add-node (make-hard-break
                                           :span (%ip-span context index
                                                           (1+ space-end)))
                                          index (1+ space-end))
                                (add-node
                                 (make-soft-break
                                  :span (%ip-span context space-end (1+ space-end)))
                                 space-end (1+ space-end)))
                            (setf index (1+ space-end)))
                          (let ((space-end (loop with cursor = index
                                                 while (and (< cursor end)
                                                            (find (char text cursor)
                                                                  '(#\Space #\Tab)))
                                                 do (incf cursor)
                                                 finally (return cursor))))
                            (add-text (subseq text index space-end)
                                      index space-end)
                            (setf index space-end)))))
                   ((and (char= character #\[)
                         (<= (+ index 2) end)
                         (char= (char text (1+ index)) #\^)
                         (let ((close (aref (%ip-context-bracket-opens context) index)))
                           (and (>= close 0) (< close end)
                                (let ((normalized
                                        (%ip-footnote-reference
                                         context
                                         (%ip-unescape text (+ index 2) close))))
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
                    (if (and (< (1+ index) end)
                             (char= (char text (1+ index)) #\[))
                        (let ((open (1+ index)))
                          (multiple-value-bind (node link-end)
                              (%ip-link-candidate context open t end link-allowed
                                                  (+ base-depth (car active-count)
                                                     bracket-count))
                            (if node
                                (progn (add-node node index link-end)
                                       (setf index link-end))
                                (let ((close
                                        (aref (%ip-context-bracket-opens context)
                                              open)))
                                  (add-text "![" index (+ index 2))
                                  (push-bracket open (and (>= close 0) close) t nil)
                                  (incf index 2)))))
                        (progn (add-text "!" index (1+ index)) (incf index))))
                   ((char= character #\[)
                    (multiple-value-bind (node link-end)
                        (if link-allowed
                            (%ip-link-candidate context index nil end t
                                                (+ base-depth (car active-count)
                                                   bracket-count))
                            (values nil nil))
                      (if node
                          (progn (add-node node index link-end)
                                 (deactivate-links)
                                 (setf index link-end))
                          (let ((close
                                  (aref (%ip-context-bracket-opens context) index)))
                            (add-text "[" index (1+ index))
                            (push-bracket index (and (>= close 0) close) nil t)
                            (incf index)))))
                   ((char= character #\])
                    (pop-bracket-at index)
                    (add-text "]" index (1+ index))
                    (incf index))
                   ((find character '(#\* #\_ #\~))
                    (let ((run-end index))
                      (loop while (and (< run-end end)
                                       (char= (char text run-end) character))
                            do (incf run-end))
                      (emit-delimiter run-end character)))
                   (t
                    (let ((run-end index))
                      (loop while (and (< run-end end)
                       (not (find (char text run-end)
                                  '(#\\ #\` #\& #\< #\Newline #\Space #\Tab
                                    #\! #\[ #\] #\* #\_ #\~)))
                                       (not (gethash run-end
                                                     (%ip-context-bare-links context))))
                            do (incf run-end))
                      (when (= run-end index) (incf run-end))
                      (add-text (subseq text index run-end) index run-end)
                      (setf index run-end)))))))
      (let* ((literal-atoms (%ip-delimiter-text-atoms
                             context delimiters pairs))
             (all-atoms (nconc (nreverse atoms) literal-atoms)))
        (%ip-build-range context start end all-atoms pairs))))

(defun parse-inline (text boundary-map reference-index
                     &key (max-inline-delimiter-depth 4096))
  "Parse inline Markdown TEXT using its source map and reference index."
  (check-type text string)
  (check-type boundary-map boundary-map)
  (check-type reference-index hash-table)
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
         (bare-links (%ip-build-bare-links text))
         (context (%ip-make-context text boundary-map reference-index
                                    max-inline-delimiter-depth backtick-next
                                    (first bracket-indexes)
                                    (second bracket-indexes)
                                    (third bracket-indexes)
                                    bare-links html-closes)))
    (%ip-parse-range context 0 (length text) t 0)))
