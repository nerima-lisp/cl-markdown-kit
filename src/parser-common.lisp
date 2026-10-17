(in-package #:cl-markdown-kit)

(defconstant +default-max-nesting-depth+ 256)

(defconstant +default-max-inline-delimiter-depth+ 4096)

(defconstant +max-ordered-list-marker-digits+ 9)

(sb-ext:defglobal +html-block-type-1-tags+ '("script" "pre" "style" "textarea"))

(sb-ext:defglobal +html-block-type-6-tags+ '("address" "article"
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

(sb-ext:defglobal +gfm-tagfilter-tags+ '("title" "textarea"
            "style"
            "xmp"
            "iframe"
            "noembed"
            "noframes"
            "script"
            "plaintext"))

(sb-ext:defglobal +html-attribute-start-characters+ "_:")

(sb-ext:defglobal +html-attribute-name-extra-characters+ ":._-")

(sb-ext:defglobal +html-unquoted-attribute-excluded-characters+ (concatenate 'string (string #\") "'=<>" (string (code-char #x60))))

(sb-ext:defglobal +autolink-angle-excluded-characters+ "<>")

(sb-ext:defglobal +uri-scheme-extra-characters+ "+.-")

(sb-ext:defglobal +angle-email-local-part-characters+ (concatenate 'string
               "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.!#$%&'*+/=?^_"
               (string (code-char #x60))
               "{|}~-"))

(sb-ext:defglobal +email-local-part-extra-characters+ ".-_+")

(sb-ext:defglobal +gfm-domain-extra-characters+ "_-")

(sb-ext:defglobal +bare-url-trailing-punctuation-characters+ "?!.,:*_~")

(sb-ext:defglobal +common-quote-characters+ (list #\" #\'))

(sb-ext:defglobal +link-label-unescape-characters+ (list #\\ #\[ #\]))

(sb-ext:defglobal +reference-label-space-characters+ (list #\Space #\Tab #\Newline))

(sb-ext:defglobal +inline-delimiter-characters+ (list #\* #\_ #\~))

(sb-ext:defglobal +inline-text-special-characters+ (list #\\ #\` #\& #\< #\Newline #\Space #\Tab #\! #\[ #\] #\* #\_ #\~))

(sb-ext:defglobal +numeric-reference-hex-prefix-characters+ "xX")

(sb-ext:defglobal +ascii-hexadecimal-letter-characters+ "abcdefABCDEF")

(sb-ext:defglobal +bare-url-preceding-excluded-characters+ ":/@<")

(defun %pc-trim-indices (text start end)
  (loop while (and (< start end) (%pc-space-p (char text start)))
        do (incf start))
  (loop while (and (< start end) (%pc-space-p (char text (1- end))))
        do (decf end))
  (values start end))

(defun %pc-trimmed-string (text)
  (multiple-value-bind (start end) (%pc-trim-indices text 0 (length text))
    (subseq text start end)))

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

(sb-ext:defglobal +link-title-delimiters+ (list #\" #\' #\())

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
                            (%pc-ascii-alphanumeric-p (char text cursor)))
                 do (incf cursor))
           (when (and (< cursor end) (char= (char text cursor) #\;))
             (let ((value
                    (lookup-html5-entity (subseq text name-start (1+ cursor)))))
               (when value
                 (values value (1+ cursor)))))))))))

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
