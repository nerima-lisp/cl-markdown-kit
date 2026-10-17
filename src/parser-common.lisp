(in-package #:cl-markdown-kit)

(defparameter +default-max-nesting-depth+
  256)

(defparameter +default-max-inline-delimiter-depth+
  4096)

(defparameter +max-ordered-list-marker-digits+
  9)

(defparameter +html-block-type-1-tags+
  '("script" "pre" "style" "textarea"))

(defparameter +html-block-type-6-tags+
  '("address" "article"
              "aside"
              "base"
              "basefont"
              "blockquote"
              "body"
              "caption"
              "center"
              "col"
              "colgroup"
              "dd"
              "details"
              "dialog"
              "dir"
              "div"
              "dl"
              "dt"
              "fieldset"
              "figcaption"
              "figure"
              "footer"
              "form"
              "frame"
              "frameset"
              "h1"
              "h2"
              "h3"
              "h4"
              "h5"
              "h6"
              "head"
              "header"
              "hr"
              "html"
              "iframe"
              "legend"
              "li"
              "link"
              "main"
              "menu"
              "menuitem"
              "nav"
              "noframes"
              "ol"
              "optgroup"
              "option"
              "p"
              "param"
              "search"
              "section"
              "summary"
              "table"
              "tbody"
              "td"
              "tfoot"
              "th"
              "thead"
              "title"
              "tr"
              "track"
              "ul"))

(defparameter +gfm-tagfilter-tags+
  '("title" "textarea"
            "style"
            "xmp"
            "iframe"
            "noembed"
            "noframes"
            "script"
            "plaintext"))

(defparameter +html-c1-replacements+
  '((#x80 . #x20AC) (#x82 . #x201A)
    (#x83 . #x0192)
    (#x84 . #x201E)
    (#x85 . #x2026)
    (#x86 . #x2020)
    (#x87 . #x2021)
    (#x88 . #x02C6)
    (#x89 . #x2030)
    (#x8A . #x0160)
    (#x8B . #x2039)
    (#x8C . #x0152)
    (#x8E . #x017D)
    (#x91 . #x2018)
    (#x92 . #x2019)
    (#x93 . #x201C)
    (#x94 . #x201D)
    (#x95 . #x2022)
    (#x96 . #x2013)
    (#x97 . #x2014)
    (#x98 . #x02DC)
    (#x99 . #x2122)
    (#x9A . #x0161)
    (#x9B . #x203A)
    (#x9C . #x0153)
    (#x9E . #x017E)
    (#x9F . #x0178)))

(defparameter +unicode-punctuation-categories+
  '(:pc :pd :ps :pe :pi :pf :po))

(defparameter +unicode-symbol-categories+
  '(:sm :sc :sk :so))

(defparameter +unicode-non-domain-categories+
  '(:cc :cf :cs :co :cn))

(defparameter +unicode-whitespace-control-codepoints+
  '(#x09 #x0A #x0C #x0D))

(defun %pc-unicode-punctuation-p (character)
  (not
   (null
    (member (sb-unicode:general-category character)
            +unicode-punctuation-categories+
            :test
            #'eq))))

(defun %pc-unicode-symbol-p (character)
  (member (sb-unicode:general-category character)
          +unicode-symbol-categories+
          :test
          #'eq))

(defparameter +html-attribute-start-characters+
  "_:")

(defparameter +html-attribute-name-extra-characters+
  ":._-")

(defparameter +html-unquoted-attribute-excluded-characters+
  (concatenate 'string (string #\") "'=<>" (string (code-char #x60))))

(defparameter +autolink-angle-excluded-characters+
  "<>")

(defparameter +uri-scheme-extra-characters+
  "+.-")

(defparameter +angle-email-local-part-characters+
  (concatenate 'string
               "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.!#$%&'*+/=?^_"
               (string (code-char #x60))
               "{|}~-"))

(defparameter +email-local-part-extra-characters+
  ".-_+")

(defparameter +gfm-domain-extra-characters+
  "_-")

(defparameter +bare-url-trailing-punctuation-characters+
  "?!.,:*_~")

(defparameter +common-quote-characters+
  (list #\" #\'))

(defparameter +link-label-unescape-characters+
  (list #\\ #\[ #\]))

(defparameter +reference-label-space-characters+
  (list #\Space #\Tab #\Newline))

(defparameter +inline-delimiter-characters+
  (list #\* #\_ #\~))

(defparameter +inline-text-special-characters+
  (list #\\ #\` #\& #\< #\Newline #\Space #\Tab #\! #\[ #\] #\* #\_ #\~))

(defparameter +commonmark-space-characters+
  (list #\Space #\Tab))

(defparameter +html-space-characters+
  (list #\Space #\Tab #\Newline #\Return #\Page))

(defparameter +link-label-space-characters+
  (list #\Space #\Tab #\Newline #\Return))

(defparameter +numeric-reference-hex-prefix-characters+
  "xX")

(defparameter +ascii-hexadecimal-letter-characters+
  "abcdefABCDEF")

(defparameter +bare-url-preceding-excluded-characters+
  ":/@<")

(declaim (ftype function
                ascii-punctuation-p
                decode-numeric-character-reference
                lookup-html5-entity
                unicode-whitespace-p
                normalize-uri-percent-encoding
                %pc-entity-at
                %pc-unescape
                %pc-email-local-character-p
                %pc-gfm-domain-end
                %pc-email-domain-end
                %pc-url-end
                %pc-trim-url-end))

(defun %pc-space-p (character)
  (find character +commonmark-space-characters+))

(defun %pc-html-space-p (character)
  (find character +html-space-characters+))

(defun %pc-link-label-space-p (character)
  (find character +link-label-space-characters+))

(defun %pc-trim-indices (text start end)
  (loop while (and (< start end) (%pc-space-p (char text start)))
        do (incf start))
  (loop while (and (< start end) (%pc-space-p (char text (1- end))))
        do (decf end))
  (values start end))

(defun %pc-trimmed-string (text)
  (multiple-value-bind (start end) (%pc-trim-indices text 0 (length text))
    (subseq text start end)))

(defun %pc-ascii-alpha-p (character)
  (let ((code (char-code character)))
    (or (<= #x41 code #x5A) (<= #x61 code #x7A))))

(defun %pc-ascii-digit-p (character)
  (let ((code (char-code character)))
    (<= #x30 code #x39)))

(defun %pc-ascii-alphanumeric-p (character)
  (or (%pc-ascii-alpha-p character) (%pc-ascii-digit-p character)))

(defun %pc-ascii-prefix-p (text start prefix)
  (and (<= (+ start (length prefix)) (length text))
       (loop for offset below (length prefix)
             for actual = (char text (+ start offset))
             for expected = (char prefix offset)
             always (char-equal actual expected))))

(defun %pc-backtick-run-end (text index end &optional backtick-index)
  (let ((record (and backtick-index (gethash index backtick-index))))
    (if record
        (min (car record) end)
        (loop with cursor = index
              while (and (< cursor end) (= (char-code (char text cursor)) #x60))
              do (incf cursor)
              finally (return cursor)))))

(defun %pc-backtick-matching-run-ends (text start end)
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
               ((= (char-code character) #x60)
                (let* ((run-end (%pc-backtick-run-end text index end))
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

(defparameter +link-title-delimiters+
  (list #\" #\' #\())

(defun %pc-check-depth (depth limit kind)
  (when (> depth limit)
    (ecase kind
      (:block
       (error 'markdown-nesting-depth-exceeded
              :limit-name
              :nesting-depth
              :limit
              limit
              :actual
              depth
              :context
              "block parsing"
              :message
              "maximum block nesting depth exceeded"))
      (:inline
       (error 'markdown-inline-delimiter-depth-exceeded
              :limit-name
              :inline-delimiter-depth
              :limit
              limit
              :actual
              depth
              :context
              "inline parsing"
              :message
              "maximum inline delimiter depth exceeded")))))

(defun %pc-parse-title (text index
                             end
                             &key
                             (backslash-mode :punctuation)
                             reject-nested-parentheses-p)
  (when
      (or (>= index end)
          (not (member (char text index) +link-title-delimiters+ :test #'char=)))
    (return-from %pc-parse-title
      (values nil nil)))
  (let* ((opening (char text index))
         (closing
          (if (char= opening #\()
              #\)
              opening))
         (cursor (1+ index)))
    (loop while (< cursor end)
          for character = (char text cursor)
          do (cond
               ((char= character #\\)
                (if (or (eq backslash-mode :any-character)
                        (and (< (1+ cursor) end)
                             (ascii-punctuation-p (char text (1+ cursor)))))
                    (incf cursor 2)
                    (incf cursor)))
               ((char= character closing)
                (return
                 (values (%pc-unescape text (1+ index) cursor) (1+ cursor))))
               ((and reject-nested-parentheses-p
                     (char= opening #\()
                     (char= character #\())
                (return-from %pc-parse-title
                  (values nil nil)))
               (t (incf cursor)))
          finally (return (values nil nil)))))

(defun %pc-c1-replacement (codepoint)
  (cdr (assoc codepoint +html-c1-replacements+ :test #'=)))

(defun %pc-decode-numeric-reference (digits radix)
  (let* ((decoded (decode-numeric-character-reference digits radix))
         (replacement
          (and decoded (%pc-c1-replacement (parse-integer digits :radix radix)))))
    (if replacement
        (string (code-char replacement))
        decoded)))

(defstruct
    (%pc-candidate
     (:constructor %pc-make-candidate (start end destination display-text kind)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  (destination "" :type string)
  (display-text "" :type string)
  kind)

(defun %pc-entity-at (text start end)
  (when (and (< (1+ start) end) (char= (char text start) #\&))
    (let ((cursor (1+ start)))
      (cond
        ((char= (char text cursor) #\#)
          (incf cursor)
          (let ((radix 10))
            (when
                (and (< cursor end)
                     (find (char text cursor)
                           +numeric-reference-hex-prefix-characters+))
              (setf radix 16)
              (incf cursor))
            (let ((digits-start cursor))
              (loop while (and (< cursor end)
                               (if (= radix 16)
                                   (or (%pc-ascii-digit-p (char text cursor))
                                       (find (char text cursor)
                                             +ascii-hexadecimal-letter-characters+))
                                   (%pc-ascii-digit-p (char text cursor))))
                    do (incf cursor))
              (when
                  (and (> cursor digits-start)
                       (< cursor end)
                       (char= (char text cursor) #\;))
                (let ((value
                       (%pc-decode-numeric-reference
                        (subseq text digits-start cursor)
                        radix)))
                  (when value
                    (values value (1+ cursor))))))))
        ((%pc-ascii-alpha-p (char text cursor))
         (let ((name-start cursor))
           (loop while (and (< cursor end)
                            (< (- cursor name-start) 32)
                            (or (%pc-ascii-alphanumeric-p (char text cursor))))
                 do (incf cursor))
           (when (and (< cursor end) (char= (char text cursor) #\;))
             (let ((value
                    (lookup-html5-entity (subseq text name-start (1+ cursor)))))
               (when value
                 (values value (1+ cursor)))))))))))

(defun %pc-bare-url-start-p (text index)
  (or (zerop index)
      (let ((previous (char text (1- index))))
        (and (not (%pc-ascii-alphanumeric-p previous))
             (not (find previous +bare-url-preceding-excluded-characters+))))))

(defun %pc-bare-email-start-p (text index)
  (or (zerop index)
      (let ((previous (char text (1- index))))
        (and (not (%pc-ascii-alphanumeric-p previous))
             (not (char= previous #\@))))))

(defun %pc-bare-candidate-at (text index end)
  (cond
    ((and (%pc-bare-url-start-p text index)
          (or (%pc-ascii-prefix-p text index "https://")
              (%pc-ascii-prefix-p text index "http://")
              (%pc-ascii-prefix-p text index "www.")))
     (let* ((www-p (%pc-ascii-prefix-p text index "www."))
            (prefix-length
             (cond
               (www-p 4)
               ((%pc-ascii-prefix-p text index "https://") 8)
               (t 7)))
            (domain-end
             (%pc-gfm-domain-end text
                                 (+ index prefix-length)
                                 end
                                 nil
                                 (not www-p)))
            (raw-end (and domain-end (%pc-url-end text index end)))
            (link-end (and raw-end (%pc-trim-url-end text index raw-end))))
       (when (and link-end (>= link-end domain-end))
         (let* ((display (subseq text index link-end))
                (destination
                 (normalize-uri-percent-encoding
                  (if www-p
                      (concatenate 'string "http://" display)
                      display))))
           (%pc-make-candidate index link-end destination display :bare-url)))))
    ((and (%pc-bare-email-start-p text index)
          (%pc-ascii-prefix-p text index "mailto:"))
     (let* ((address-start (+ index (length "mailto:")))
            (local-end address-start))
       (loop while (and (< local-end end)
                        (%pc-email-local-character-p (char text local-end)))
             do (incf local-end))
       (when (and (< local-end end) (char= (char text local-end) #\@))
         (let ((domain-end (%pc-email-domain-end text (1+ local-end) end)))
           (when domain-end
             (let ((display (subseq text index domain-end)))
               (%pc-make-candidate index
                                   domain-end
                                   (normalize-uri-percent-encoding display)
                                   display
                                   :bare-email)))))))
    ((and (%pc-bare-url-start-p text index)
          (%pc-ascii-prefix-p text index "xmpp:"))
     (let* ((address-start (+ index (length "xmpp:")))
            (local-end address-start))
       (loop while (and (< local-end end)
                        (%pc-email-local-character-p (char text local-end)))
             do (incf local-end))
       (when (and (< local-end end) (char= (char text local-end) #\@))
         (let ((domain-end (%pc-email-domain-end text (1+ local-end) end)))
           (when domain-end
             (let* ((raw-end (%pc-url-end text index end))
                    (link-end (%pc-trim-url-end text index raw-end))
                    (display (subseq text index link-end))
                    (destination (normalize-uri-percent-encoding display)))
               (%pc-make-candidate index link-end destination display :bare-url)))))))
    ((and (%pc-bare-email-start-p text index)
          (%pc-email-local-character-p (char text index)))
     (let ((local-end index))
       (loop while (and (< local-end end)
                        (%pc-email-local-character-p (char text local-end)))
             do (incf local-end))
       (when (and (< local-end end) (char= (char text local-end) #\@))
         (let ((domain-end (%pc-email-domain-end text (1+ local-end) end)))
           (when domain-end
             (let* ((display (subseq text index domain-end))
                    (destination
                     (normalize-uri-percent-encoding
                      (concatenate 'string "mailto:" display))))
               (%pc-make-candidate index
                                   domain-end
                                   destination
                                   display
                                   :bare-email)))))))))

(defun %pc-table-cells (text)
  (let* ((length (length text))
         (start 0)
         (end length)
         (cells nil)
         (index 0)
         (backtick-matches (%pc-backtick-matching-run-ends text start end))
         (escaped nil))
    (multiple-value-setq (start end) (%pc-trim-indices text start end))
    (when (and (< start end) (char= (char text start) #\|))
      (incf start))
    (setf index start)
    (let ((cell-start start))
      (loop while (< index end)
            for character = (char text index)
            do (cond
                 (escaped
                  (setf escaped nil))
                 ((char= character #\\)
                  (setf escaped t))
                 ((char= character #\`)
                  (let ((run-end index))
                    (loop while (and (< run-end end)
                                     (char= (char text run-end) #\`))
                          do (incf run-end))
                    (setf index (1-
                                 (or (gethash index backtick-matches) run-end)))))
                 ((char= character #\|)
                   (push (cons cell-start index) cells)
                   (setf cell-start (1+ index)))) (incf index))
      (push (cons cell-start end) cells)
      (when
          (and (> (length cells) 1)
               (= cell-start end)
               (char= (char text (1- end)) #\|))
        (pop cells)))
    (nreverse cells)))

(defun %pc-email-local-character-p (character)
  (or (%pc-ascii-alphanumeric-p character)
      (find character +email-local-part-extra-characters+)))

(defun %pc-commonmark-punctuation-p (character)
  (or (ascii-punctuation-p character)
      (%pc-unicode-punctuation-p character)
      (not (null (%pc-unicode-symbol-p character)))))

(defun %pc-gfm-domain-character-p (character)
  (or (%pc-ascii-alphanumeric-p character)
      (find character +gfm-domain-extra-characters+)
      (let ((category (sb-unicode:general-category character)))
        (and (> (char-code character) #x7F)
             (not (unicode-whitespace-p character))
             (not (%pc-unicode-punctuation-p character))
             (not (member category +unicode-non-domain-categories+ :test #'eq))))))

(defun %pc-gfm-domain-end (text start end &optional email-p allow-single-label)
  (let ((cursor start)
        (segments nil))
    (loop (let ((segment-start cursor))
            (loop while (and (< cursor end)
                             (%pc-gfm-domain-character-p (char text cursor)))
                  do (incf cursor))
            (when (= cursor segment-start)
              (return-from %pc-gfm-domain-end
                nil))
            (push (cons segment-start cursor) segments)) (cond
                                                           ((= cursor end)
                                                            (return))
                                                           ((char=
                                                             (char text cursor)
                                                             #\.)
                                                            (if (and
                                                                 (< (1+ cursor)
                                                                    end)
                                                                 (%pc-gfm-domain-character-p
                                                                  (char text
                                                                        (1+
                                                                         cursor))))
                                                                (incf cursor)
                                                                (return)))
                                                           (t (return))))
    (let* ((ordered-segments (nreverse segments))
           (last-two (last ordered-segments 2))
           (domain-end (cdr (car (last ordered-segments)))))
      (when
          (and (or allow-single-label (> (length ordered-segments) 1))
               (or email-p
                   (every
                    (lambda (segment)
                      (not
                       (position #\_
                                 text
                                 :start
                                 (car segment)
                                 :end
                                 (cdr segment))))
                    last-two))
               (or (not email-p)
                   (every
                    (lambda (segment)
                      (let ((segment-start (car segment))
                            (segment-end (cdr segment)))
                        (and
                         (not
                          (position #\_
                                    text
                                    :start
                                    segment-start
                                    :end
                                    segment-end))
                         (not (char= (char text segment-start) #\-))
                         (not (char= (char text (1- segment-end)) #\-)))))
                    ordered-segments)))
        domain-end))))

(defun %pc-email-domain-end (text start end)
  (%pc-gfm-domain-end text start end t))

(defun %pc-url-end (text start end)
  (loop with cursor = start
        while (and (< cursor end)
                   (let ((character (char text cursor)))
                     (and (not (unicode-whitespace-p character))
                          (not (char= character #\<)))))
        do (incf cursor)
        finally (return cursor)))

(defun %pc-trim-url-end (text start end)
  (let ((open-parentheses 0)
        (close-parentheses 0)
        (cursor end))
    (loop for index from start below
          end
          do (case (char text index)
               (#\( (incf open-parentheses))
               (#\) (incf close-parentheses))))
    (loop while (> cursor start)
          for character = (char text (1- cursor))
          while (or (find character +bare-url-trailing-punctuation-characters+)
                    (find character +common-quote-characters+)
                    (and (char= character #\))
                         (> close-parentheses open-parentheses)))
          do (when (char= character #\))
               (decf close-parentheses)) (decf cursor))
    (when (and (> cursor start) (char= (char text (1- cursor)) #\;))
      (let ((ampersand
             (position #\& text :start start :end (1- cursor) :from-end t)))
        (when
            (and ampersand
                 (< ampersand (1- cursor))
                 (< (1+ ampersand) (1- cursor))
                 (loop for index from (1+ ampersand) below (1- cursor)
                       always (%pc-ascii-alphanumeric-p (char text index))))
          (setf cursor ampersand))))
    cursor))

(defun %pc-unescape (text &optional (start 0) (end (length text)))
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
                (multiple-value-bind (value next) (%pc-entity-at text index end)
                  (if value
                      (progn
                        (write-string value output)
                        (setf index next))
                      (progn
                        (write-char (char text index) output)
                        (incf index)))))))))
