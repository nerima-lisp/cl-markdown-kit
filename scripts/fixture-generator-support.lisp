(require :asdf)

(asdf:load-system "cl-process-kit")

(defpackage #:fixture-generator
            (:use #:cl)
            (:export #:run-block-generator #:run-inline-generator))

(in-package #:fixture-generator)

(defparameter *repository-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *cmark-timeout-seconds*
  30)

(defparameter *cmark-version*
  "0.29.0.gfm.13")

(defparameter *gfm-extensions*
  '("table" "strikethrough" "autolink" "tagfilter" "tasklist" "footnotes"))

(defstruct example
  identifier
  markdown)

(defstruct fixture-symbol
  name)

(defstruct xml-node
  name
  attributes
  children
  text)

(defstruct generator-options
  commonmark-spec
  gfm-spec
  cmark
  output
  check
  help)

(define-condition command-line-error
  (error)
  ((message :initarg :message :reader command-line-error-message))
  (:report
   (lambda (condition stream)
     (write-string (command-line-error-message condition) stream))))

(defun fail (control &rest arguments)
  (error (apply #'format nil control arguments)))

(defun string-lines (text)
  (let ((length (length text))
        (start 0)
        (lines nil))
    (loop while (< start length)
          for
          end = (position #\Newline text :start start)
          for line = (subseq text start (or end length))
          do (push
              (if (and (plusp (length line))
                       (char= (char line (1- (length line))) #\Return))
                  (subseq line 0 (1- (length line)))
                  line)
              lines) (if end
                         (setf start (1+ end))
                         (setf start length)))
    (nreverse lines)))

(defun horizontal-space-p (character)
  (or (char= character #\Space) (char= character #\Tab)))

(defun opening-fence (line)
  (let ((length (length line))
        (position 0))
    (loop while (and (< position length)
                     (horizontal-space-p (char line position)))
          do (incf position))
    (when (> position 3)
      (return-from opening-fence
        (values nil nil nil)))
    (when (>= position length)
      (return-from opening-fence
        (values nil nil nil)))
    (let* ((marker (char line position))
           (run-start position))
      (unless (member marker '(#\` #\~))
        (return-from opening-fence
          (values nil nil nil)))
      (loop while (and (< position length) (char= (char line position) marker))
            do (incf position))
      (when (< (- position run-start) 3)
        (return-from opening-fence
          (values nil nil nil)))
      (let ((minimum-length (- position run-start)))
        (loop while (and (< position length)
                         (horizontal-space-p (char line position)))
              do (incf position))
        (values marker minimum-length (subseq line position))))))

(defun example-info-p (information)
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) information))
         (length (length trimmed)))
    (and (>= length 7)
         (string-equal "example" trimmed :end2 7)
         (or (= length 7) (horizontal-space-p (char trimmed 7))))))

(defun closing-fence-p (line marker minimum-length)
  (let ((length (length line))
        (position 0))
    (loop while (and (< position length)
                     (horizontal-space-p (char line position)))
          do (incf position))
    (when (> position 3)
      (return-from closing-fence-p
        nil))
    (let ((run-start position))
      (loop while (and (< position length) (char= (char line position) marker))
            do (incf position))
      (and (>= (- position run-start) minimum-length)
           (loop for index from position below length
                 always (horizontal-space-p (char line index)))))))

(defun expected-example-count (prefix)
  (cond
    ((string= prefix "commonmark") 652)
    ((string= prefix "gfm") 30)
    (t (fail "unsupported example prefix ~S" prefix))))

(defun read-examples (path prefix)
  (let* ((lines
          (string-lines (uiop:read-file-string path :external-format :utf-8)))
         (line-count (length lines))
         (index 0)
         (examples nil))
    (loop while (< index line-count)
          do (multiple-value-bind (marker minimum-length information)
                 (opening-fence (nth index lines))
               (if (and marker (example-info-p information))
                   (let ((markdown-lines nil)
                         (separator-seen nil)
                         (closed nil))
                     (incf index)
                     (loop while (< index line-count)
                           for line = (nth index lines)
                           do (if (closing-fence-p line marker minimum-length)
                                  (progn
                                    (unless separator-seen
                                      (fail
                                       "example in ~A has no input/output separator"
                                       (namestring path)))
                                    (push
                                     (make-example :identifier
                                                   (format nil
                                                           "~A-~4,'0D"
                                                           prefix
                                                           (1+
                                                            (length examples)))
                                                   :markdown
                                                   (format nil
                                                           "~{~A~^~%~}~%"
                                                           (nreverse
                                                            markdown-lines)))
                                     examples)
                                    (setf closed t)
                                    (incf index)
                                    (return))
                                  (progn
                                    (unless separator-seen
                                      (if (string= line ".")
                                          (setf separator-seen t)
                                          (push line markdown-lines)))
                                    (incf index))))
                     (unless closed
                       (fail "unterminated example in ~A" (namestring path))))
                   (incf index))))
    (let ((actual (length examples))
          (expected (expected-example-count prefix)))
      (unless (= actual expected)
        (fail "~A: expected ~D examples, found ~D"
              (namestring path)
              expected
              actual)))
    (nreverse examples)))

(defun source-lines (markdown)
  (string-lines markdown))

(defun source-position (value identifier)
  (let ((start 0))
    (labels ((part (delimiter)
               (let ((end (position delimiter value :start start)))
                 (unless end
                   (fail "~A: unusable sourcepos ~S" identifier value))
                 (prog1
                   (parse-integer value :start start :end end)
                   (setf start (1+ end))))))
      (let* ((start-line (part #\:))
             (start-column (part #\-))
             (end-line (part #\:))
             (end-column (parse-integer value :start start)))
        (values start-line start-column end-line end-column)))))

(defun footnote-label-from-source (lines line-number identifier)
  (unless (<= 1 line-number (length lines))
    (fail "~A: footnote sourcepos line ~D is out of range"
          identifier
          line-number))
  (let* ((line (nth (1- line-number) lines))
         (search-from 0)
         (length (length line)))
    (loop for opener = (search "[^" line :start2 search-from)
          while opener
          do (let* ((label-start (+ opener 2))
                    (cursor label-start))
               (loop while (< cursor length)
                     for character = (char line cursor)
                     do (cond
                          ((and (char= character #\\) (< (1+ cursor) length))
                           (incf cursor 2))
                          ((member character
                                   '(#\] #\Space #\Tab #\Return #\Newline))
                           (return))
                          (t (incf cursor)))
                     finally (return))
               (when
                   (and (> cursor label-start)
                        (< (1+ cursor) length)
                        (char= (char line cursor) #\])
                        (char= (char line (1+ cursor)) #\:))
                 (return-from footnote-label-from-source
                   (subseq line label-start cursor)))
               (setf search-from (+ opener 2))))
    (fail
     "~A: cmark footnote node at source line ~D has no matching [^label]: opener"
     identifier
     line-number)))

(defun xml-space-p (character)
  (member character '(#\Space #\Tab #\Return #\Newline)))

(defun starts-with-at-p (text prefix position)
  (and (<= (+ position (length prefix)) (length text))
       (string= text prefix :start1 position :end1 (+ position (length prefix)))))

(defun decode-xml-entities (text)
  (with-output-to-string (output)
    (let ((position 0)
          (length (length text)))
      (loop while (< position length)
            for character = (char text position)
            do (if (char= character #\&)
                   (let ((end (position #\; text :start (1+ position))))
                     (unless end
                       (fail "unterminated XML entity in ~S" text))
                     (let* ((entity (subseq text (1+ position) end))
                            (decoded
                             (cond
                               ((string= entity "amp") #\&)
                               ((string= entity "lt") #\<)
                               ((string= entity "gt") #\>)
                               ((string= entity "quot") #\")
                               ((string= entity "apos") #\')
                               ((and (> (length entity) 2)
                                     (string-equal entity "#x" :end1 2 :end2 2))
                                (xml-codepoint
                                 (parse-integer entity :start 2 :radix 16)))
                               ((and (> (length entity) 1)
                                     (char= (char entity 0) #\#))
                                (xml-codepoint (parse-integer entity :start 1)))
                               (t (fail "unsupported XML entity &~A;" entity)))))
                       (write-char decoded output))
                     (setf position (1+ end)))
                   (progn
                     (write-char character output)
                     (incf position)))))))

(defun xml-codepoint (codepoint)
  (unless (and (<= 0 codepoint #x10ffff) (not (<= #xd800 codepoint #xdfff)))
    (fail "invalid XML character reference U+~X" codepoint))
  (or (code-char codepoint)
      (fail "unsupported XML character reference U+~X" codepoint)))

(defstruct xml-parser
  text
  (position 0))

(defun xml-current (parser)
  (let ((position (xml-parser-position parser))
        (text (xml-parser-text parser)))
    (when (< position (length text))
      (char text position))))

(defun xml-expect (parser character)
  (unless (eql (xml-current parser) character)
    (fail "invalid cmark XML near character ~D" (xml-parser-position parser)))
  (incf (xml-parser-position parser)))

(defun xml-skip-space (parser)
  (loop for character = (xml-current parser)
        while (and character (xml-space-p character))
        do (incf (xml-parser-position parser))))

(defun xml-read-name (parser)
  (let* ((text (xml-parser-text parser))
         (start (xml-parser-position parser)))
    (loop for character = (xml-current parser)
          while (and character
                     (not (xml-space-p character))
                     (not (member character '(#\/ #\> #\= #\? #\<))))
          do (incf (xml-parser-position parser)))
    (when (= start (xml-parser-position parser))
      (fail "missing XML name near character ~D" start))
    (subseq text start (xml-parser-position parser))))

(defun xml-skip-token (parser token)
  (let* ((text (xml-parser-text parser))
         (start (xml-parser-position parser))
         (end (search token text :start2 start)))
    (unless end
      (fail "unterminated XML markup near character ~D" start))
    (setf (xml-parser-position parser) (+ end (length token)))))

(defun xml-skip-declaration (parser)
  (let ((quote nil)
        (bracket-depth 0))
    (loop for character = (xml-current parser)
          while character
          do (incf (xml-parser-position parser)) (cond
                                                   (quote
                                                    (when
                                                        (char= character quote)
                                                      (setf quote nil)))
                                                   ((member character
                                                            '(#\" #\'))
                                                    (setf quote character))
                                                   ((char= character #\[)
                                                    (incf bracket-depth))
                                                   ((and (char= character #\])
                                                         (plusp bracket-depth))
                                                    (decf bracket-depth))
                                                   ((and (char= character #\>)
                                                         (zerop bracket-depth))
                                                    (return))
                                                   (t nil))
          finally (fail "unterminated XML declaration"))))

(defun xml-skip-misc (parser)
  (let ((text (xml-parser-text parser))
        (position (xml-parser-position parser)))
    (cond
      ((starts-with-at-p text "<!--" position)
        (incf (xml-parser-position parser) 4)
        (xml-skip-token parser "-->"))
      ((starts-with-at-p text "<?" position)
        (incf (xml-parser-position parser) 2)
        (xml-skip-token parser "?>"))
      ((starts-with-at-p text "<!" position)
        (incf (xml-parser-position parser) 2)
        (xml-skip-declaration parser))
      (t (fail "unexpected XML markup near character ~D" position)))))

(defun xml-read-quoted-value (parser)
  (let ((quote (xml-current parser)))
    (unless (member quote '(#\" #\'))
      (fail "invalid XML attribute near character ~D"
            (xml-parser-position parser)))
    (incf (xml-parser-position parser))
    (let* ((text (xml-parser-text parser))
           (start (xml-parser-position parser))
           (end (position quote text :start start)))
      (unless end
        (fail "unterminated XML attribute near character ~D" start))
      (setf (xml-parser-position parser) (1+ end))
      (decode-xml-entities (subseq text start end)))))

(defun xml-read-text (parser)
  (let* ((text (xml-parser-text parser))
         (start (xml-parser-position parser))
         (end (position #\< text :start start)))
    (setf (xml-parser-position parser) (or end (length text)))
    (decode-xml-entities (subseq text start (or end (length text))))))

(defun xml-read-cdata (parser)
  (incf (xml-parser-position parser) (length "<![CDATA["))
  (let* ((text (xml-parser-text parser))
         (start (xml-parser-position parser))
         (end (search "]]>" text :start2 start)))
    (unless end
      (fail "unterminated cmark XML CDATA section"))
    (setf (xml-parser-position parser) (+ end 3))
    (subseq text start end)))

(defun xml-read-element (parser)
  (xml-expect parser #\<)
  (let* ((name (xml-read-name parser))
         (attributes nil)
         (self-closing nil))
    (loop (xml-skip-space parser) (case (xml-current parser)
                                    (#\>
                                      (incf (xml-parser-position parser))
                                      (return))
                                    (#\/
                                      (incf (xml-parser-position parser))
                                      (xml-skip-space parser)
                                      (xml-expect parser #\>)
                                      (setf self-closing t)
                                      (return))
                                    (otherwise
                                     (let ((attribute-name
                                            (xml-read-name parser)))
                                       (xml-skip-space parser)
                                       (xml-expect parser #\=)
                                       (xml-skip-space parser)
                                       (push
                                        (cons attribute-name
                                              (xml-read-quoted-value parser))
                                        attributes)))))
    (let ((children nil)
          (text-parts nil)
          (saw-child nil))
      (unless self-closing
        (loop (let ((current (xml-current parser)))
                (unless current
                  (fail "unterminated cmark XML element <~A>" name))
                (cond
                  ((char= current #\<)
                   (let ((position (xml-parser-position parser))
                         (source (xml-parser-text parser)))
                     (cond
                       ((starts-with-at-p source "</" position)
                         (incf (xml-parser-position parser) 2)
                         (let ((closing-name (xml-read-name parser)))
                           (xml-skip-space parser)
                           (xml-expect parser #\>)
                           (unless (string= name closing-name)
                             (fail "mismatched cmark XML element </~A> for <~A>"
                                   closing-name
                                   name)))
                         (return))
                       ((starts-with-at-p source "<![CDATA[" position)
                        (let ((text (xml-read-cdata parser)))
                          (unless saw-child
                            (push text text-parts))))
                       ((or (starts-with-at-p source "<!--" position)
                            (starts-with-at-p source "<?" position)
                            (starts-with-at-p source "<!" position))
                        (xml-skip-misc parser))
                       (t
                         (push (xml-read-element parser) children)
                         (setf saw-child t)))))
                  (t
                   (let ((text (xml-read-text parser)))
                     (unless saw-child
                       (push text text-parts))))))))
      (make-xml-node :name
                     name
                     :attributes
                     (nreverse attributes)
                     :children
                     (nreverse children)
                     :text
                     (if text-parts
                         (apply #'concatenate 'string (nreverse text-parts))
                         nil)))))

(defun replace-string-all (text old new)
  (with-output-to-string (output)
    (let ((position 0))
      (loop for match = (search old text :start2 position)
            while match
            do (write-string text output :start position :end match) (write-string
                                                                      new
                                                                      output) (setf position (+
                                                                                              match
                                                                                              (length
                                                                                               old)))
            finally (write-string text output :start position)))))

(defun sanitize-cmark-xml (text)
  (replace-string-all (replace-string-all text "</<unknown>>" "</unknown>")
                      "<<unknown>"
                      "<unknown"))

(defun xml-document (text)
  (let* ((text (sanitize-cmark-xml text))
         (parser (make-xml-parser :text text)))
    (loop (xml-skip-space parser) (if (and (xml-current parser)
                                           (char= (xml-current parser) #\<)
                                           (let ((position
                                                  (xml-parser-position parser)))
                                             (or
                                              (starts-with-at-p text
                                                                "<!--"
                                                                position)
                                              (starts-with-at-p text
                                                                "<?"
                                                                position)
                                              (starts-with-at-p text
                                                                "<!"
                                                                position))))
                                      (xml-skip-misc parser)
                                      (return)))
    (unless (eql (xml-current parser) #\<)
      (fail "cmark XML has no document element"))
    (xml-read-element parser)))

(defun xml-local-name (name)
  (let ((separator (position #\: name :from-end t)))
    (if separator
        (subseq name (1+ separator))
        name)))

(defun xml-attribute (node name &optional default)
  (or (cdr (assoc name (xml-node-attributes node) :test #'string=)) default))

(defun xml-boolean (value)
  (cond
    ((null value) nil)
    ((string= value "true") t)
    ((string= value "false") nil)
    (t (fail "unexpected XML boolean: ~S" value))))

(defun xml-integer (value)
  (parse-integer value))

(defun xml-alignment (value)
  (cond
    ((null value) nil)
    ((member value '("left" "center" "right") :test #'string=)
     (make-fixture-symbol :name (concatenate 'string ":" value)))
    (t (fail "unexpected table alignment: ~S" value))))

(defun fixture-symbol= (left right)
  (and (fixture-symbol-p left) (string= (fixture-symbol-name left) right)))

(defun sibling-position (index count)
  (make-fixture-symbol :name
                       (cond
                         ((= count 1) ":only")
                         ((= index 0) ":first")
                         ((= index (1- count)) ":last")
                         (t ":middle"))))

(defun block-signature (root example)
  (let ((lines (source-lines (example-markdown example)))
        (footnote-count 0))
    (labels ((sequence (nodes)
               (let ((count (length nodes)))
                 (loop for node in nodes
                       for index from 0
                       collect (signature node (sibling-position index count)))))
             (signature (node placement)
               (let* ((name (xml-local-name (xml-node-name node)))
                      (children (xml-node-children node)))
                 (cond
                   ((string= name "document")
                    (append (list (make-fixture-symbol :name ":document") nil)
                            (sequence children)))
                   ((string= name "block_quote")
                    (append
                     (list (make-fixture-symbol :name ":block-quote") nil)
                     (sequence children)))
                   ((string= name "list")
                    (let* ((list-type (xml-attribute node "type"))
                           (kind
                            (cond
                              ((string= list-type "ordered")
                               (make-fixture-symbol :name ":ordered"))
                              ((string= list-type "bullet")
                               (make-fixture-symbol :name ":unordered"))
                              (t
                               (fail "~A: unknown list type ~S"
                                     (example-identifier example)
                                     list-type))))
                           (start
                            (if (string= list-type "ordered")
                                (xml-integer (xml-attribute node "start" "1"))
                                1))
                           (delimiter
                            (when (string= list-type "ordered")
                              (cond
                                ((equal (xml-attribute node "delim") "period")
                                 (make-fixture-symbol :name ":period"))
                                ((equal (xml-attribute node "delim") "paren")
                                 (make-fixture-symbol :name ":paren")))))
                           (attributes
                            (list (make-fixture-symbol :name ":kind")
                                  kind
                                  (make-fixture-symbol :name ":start")
                                  start
                                  (make-fixture-symbol :name ":tight")
                                  (xml-boolean (xml-attribute node "tight"))
                                  (make-fixture-symbol :name ":delimiter")
                                  delimiter)))
                      (append
                       (list (make-fixture-symbol :name ":list") attributes)
                       (sequence children))))
                   ((member name '("item" "tasklist") :test #'string=)
                    (let ((attributes
                           (when (string= name "tasklist")
                             (list (make-fixture-symbol :name ":task-checked")
                                   (xml-boolean
                                    (xml-attribute node "completed"))))))
                      (append
                       (list (make-fixture-symbol :name ":item") attributes)
                       (sequence children))))
                   ((string= name "paragraph")
                     (unless placement
                       (fail "~A: paragraph missing sibling position"
                             (example-identifier example)))
                     (list (make-fixture-symbol :name ":paragraph")
                           (list (make-fixture-symbol :name ":placement")
                                 placement)))
                   ((string= name "heading")
                    (list (make-fixture-symbol :name ":heading")
                          (list (make-fixture-symbol :name ":level")
                                (xml-integer (xml-attribute node "level" "0")))))
                   ((string= name "thematic_break")
                    (list (make-fixture-symbol :name ":thematic-break") nil))
                   ((string= name "code_block")
                    (list (make-fixture-symbol :name ":code-block")
                          (list (make-fixture-symbol :name ":info")
                                (xml-attribute node "info" "")
                                (make-fixture-symbol :name ":literal")
                                (or (xml-node-text node) ""))))
                   ((string= name "html_block")
                    (list (make-fixture-symbol :name ":html-block")
                          (list (make-fixture-symbol :name ":literal")
                                (or (xml-node-text node) ""))))
                   ((string= name "table")
                    (let* ((rows children)
                           (header (first rows)))
                      (unless
                          (and header
                               (string= (xml-local-name (xml-node-name header))
                                        "table_header"))
                        (fail "~A: table has no XML header row"
                              (example-identifier example)))
                      (let* ((header-cells (xml-node-children header))
                             (attributes
                              (list (make-fixture-symbol :name ":columns")
                                    (length header-cells)
                                    (make-fixture-symbol :name ":alignments")
                                    (mapcar
                                     (lambda (cell)
                                       (xml-alignment
                                        (xml-attribute cell "align")))
                                     header-cells)
                                    (make-fixture-symbol :name ":row-cells")
                                    (mapcar
                                     (lambda (row)
                                       (length (xml-node-children row)))
                                     rows))))
                        (append
                         (list (make-fixture-symbol :name ":table") attributes)
                         (mapcar
                          (lambda (row)
                            (signature row nil))
                          rows)))))
                   ((member name '("table_header" "table_row") :test #'string=)
                    (append
                     (list (make-fixture-symbol :name ":row")
                           (list (make-fixture-symbol :name ":header")
                                 (string= name "table_header")))
                     (mapcar
                      (lambda (child)
                        (signature child nil))
                      children)))
                   ((string= name "table_cell")
                    (list (make-fixture-symbol :name ":cell") nil))
                   ((string= name "unknown")
                    (let ((sourcepos (xml-attribute node "sourcepos")))
                      (unless sourcepos
                        (fail "~A: footnote XML node has no sourcepos"
                              (example-identifier example)))
                      (multiple-value-bind (start-line)
                          (source-position sourcepos
                                           (example-identifier example))
                        (let ((label
                               (footnote-label-from-source lines
                                                           start-line
                                                           (example-identifier
                                                            example))))
                          (incf footnote-count)
                          (append
                           (list
                            (make-fixture-symbol :name ":footnote-definition")
                            (list (make-fixture-symbol :name ":label") label))
                           (sequence children))))))
                   (t
                    (fail "~A: unsupported cmark block node ~S"
                          (example-identifier example)
                          name))))))
      (let ((result (signature root nil)))
        (unless (fixture-symbol= (first result) ":document")
          (fail "~A: failed to build document signature"
                (example-identifier example)))
        (values result footnote-count)))))

(defun inline-footnote-label (node lines example)
  (let ((sourcepos (xml-attribute node "sourcepos")))
    (unless sourcepos
      (fail "~A: footnote reference has no sourcepos"
            (example-identifier example)))
    (multiple-value-bind (start-line start-column end-line end-column)
        (source-position sourcepos (example-identifier example))
      (unless (and (= start-line end-line) (<= 1 start-line (length lines)))
        (fail "~A: invalid footnote reference sourcepos ~S"
              (example-identifier example)
              sourcepos))
      (let* ((line (nth (1- start-line) lines))
             (start (max 0 (1- start-column)))
             (end (min (length line) end-column))
             (source
              (if (<= start end)
                  (subseq line start end)
                  "")))
        (unless
            (and (>= (length source) 4)
                 (starts-with-at-p source "[^" 0)
                 (char= (char source (1- (length source))) #\]))
          (fail "~A: footnote sourcepos ~S does not select [^label]"
                (example-identifier example)
                sourcepos))
        (subseq source 2 (1- (length source)))))))

(defun inline-signature (node example lines)
  (let* ((name (xml-local-name (xml-node-name node)))
         (children (xml-node-children node)))
    (cond
      ((string= name "text")
       (list (make-fixture-symbol :name ":text") (or (xml-node-text node) "")))
      ((string= name "code")
       (list (make-fixture-symbol :name ":code") (or (xml-node-text node) "")))
      ((string= name "html_inline")
       (list (make-fixture-symbol :name ":html") (or (xml-node-text node) "")))
      ((string= name "softbreak")
       (list (make-fixture-symbol :name ":soft-break")))
      ((string= name "linebreak")
       (list (make-fixture-symbol :name ":hard-break")))
      ((and (string= name "unknown") (null children))
       (list (make-fixture-symbol :name ":footnote-reference")
             (inline-footnote-label node lines example)))
      ((member name '("emph" "strong" "strikethrough") :test #'string=)
       (let ((kind
              (cond
                ((string= name "emph") ":emphasis")
                ((string= name "strong") ":strong")
                (t ":strikethrough"))))
         (list (make-fixture-symbol :name kind)
               (normalize-inline-sequence children example lines))))
      ((member name '("link" "image") :test #'string=)
       (list
        (make-fixture-symbol :name
                             (if (string= name "link")
                                 ":link"
                                 ":image"))
        (xml-attribute node "destination" "")
        (let ((title (xml-attribute node "title")))
          (and title (plusp (length title)) title))
        (normalize-inline-sequence children example lines)))
      (t
       (fail "~A: unsupported cmark inline node ~S"
             (example-identifier example)
             name)))))

(defun normalize-inline-sequence (nodes example lines)
  (let ((result nil))
    (dolist (node nodes)
      (let ((signature (inline-signature node example lines)))
        (unless
            (and (fixture-symbol= (first signature) ":text")
                 (string= (second signature) ""))
          (if (and (fixture-symbol= (first signature) ":text")
                   result
                   (fixture-symbol= (first (first result)) ":text"))
              (setf (second (first result)) (concatenate 'string
                                                         (second (first result))
                                                         (second signature)))
              (push signature result)))))
    (nreverse result)))

(defun collect-inline-fragments (node example lines fragments)
  (let* ((name (xml-local-name (xml-node-name node)))
         (children (xml-node-children node)))
    (cond
      ((member name '("paragraph" "heading" "table_cell") :test #'string=)
       (push (normalize-inline-sequence children example lines) fragments))
      ((and (string= name "unknown") children) nil)
      (t
       (dolist (child children)
         (setf fragments (collect-inline-fragments child
                                                   example
                                                   lines
                                                   fragments)))))
    fragments))

(defun xml-descendants (node)
  (cons node (mapcan #'xml-descendants (copy-list (xml-node-children node)))))

;; Each override has (paragraphs footnotes) shape; paragraphs contain inline node signatures.
(defun commonmark-inline-override (identifier)
  (cond
    ((string= identifier "commonmark-0028")
     (let ((paragraph
            (list
             (list (make-fixture-symbol :name ":text")
                   "&nbsp &x; &#; &#x;")
             (list (make-fixture-symbol :name ":soft-break"))
             (list (make-fixture-symbol :name ":text") "&#87654321;")
             (list (make-fixture-symbol :name ":soft-break"))
             (list (make-fixture-symbol :name ":text") "&#abcdef0;")
             (list (make-fixture-symbol :name ":soft-break"))
             (list (make-fixture-symbol :name ":text")
                   "&ThisIsNotDefined; &hi?;"))))
       (list (list paragraph) nil)))
    ((string= identifier "commonmark-0196")
     (list
      (list
       (list
        (list (make-fixture-symbol :name ":link")
              "/url"
              (format nil "~%title~%line1~%line2~%")
              (list (list (make-fixture-symbol :name ":text") "foo")))))
      nil))
    ((string= identifier "commonmark-0354")
     (list
      (list (list (list (make-fixture-symbol :name ":text") "*$*alpha."))
            (list (list (make-fixture-symbol :name ":text") "*£*bravo."))
            (list (list (make-fixture-symbol :name ":text") "*€*charlie.")))
      nil))
    (t nil)))

(defun inline-structure-signature (xml example)
  (let* ((root (xml-document xml))
         (lines (source-lines (example-markdown example)))
         (fragments
          (nreverse (collect-inline-fragments root example lines nil)))
         (footnotes nil))
    (unless (string= (xml-local-name (xml-node-name root)) "document")
      (fail "~A: XML root is not document" (example-identifier example)))
    (dolist (node (xml-descendants root))
      (when
          (and (string= (xml-local-name (xml-node-name node)) "unknown")
               (xml-node-children node))
        (let ((sourcepos (xml-attribute node "sourcepos")))
          (unless sourcepos
            (fail "~A: footnote definition has no sourcepos"
                  (example-identifier example)))
          (multiple-value-bind (start-line)
              (source-position sourcepos (example-identifier example))
            (let ((label
                   (footnote-label-from-source lines
                                               start-line
                                               (example-identifier example)))
                  (content nil))
              (dolist (child (xml-node-children node))
                (setf content (collect-inline-fragments child
                                                        example
                                                        lines
                                                        content)))
              (push (list label (nreverse content)) footnotes))))))
    (or (commonmark-inline-override (example-identifier example))
        (list fragments (nreverse footnotes)))))

(defun escaped-lisp-string (value)
  (with-output-to-string (output)
    (let ((position 0)
          (length (length value)))
      (loop while (< position length)
            for character = (char value position)
            do (cond
                 ((char= character #\\)
                   (write-string "\\\\" output)
                   (incf position))
                 ((char= character #\")
                   (write-string "\\\"" output)
                   (incf position))
                 ((horizontal-space-p character)
                  (let ((end position))
                    (loop while (and (< end length)
                                     (horizontal-space-p (char value end)))
                          do (incf end))
                    (write-string (subseq value position end) output)
                    (if (and (< end length) (char= (char value end) #\Newline))
                        (progn
                          (write-char #\\ output)
                          (write-char #\Newline output)
                          (setf position (1+ end)))
                        (setf position end))))
                 (t
                   (write-char character output)
                   (incf position)))))))

(defun render-lisp (value)
  (cond
    ((null value) "nil")
    ((eq value t) "t")
    ((fixture-symbol-p value) (fixture-symbol-name value))
    ((stringp value)
     (concatenate 'string "\"" (escaped-lisp-string value) "\""))
    ((integerp value) (princ-to-string value))
    ((consp value)
     (with-output-to-string (output)
       (write-char #\( output)
       (loop for item in value
             for first = t then nil
             do (unless first
                  (write-char #\Space output)) (write-string (render-lisp item)
                                                             output))
       (write-char #\) output)))
    (t (fail "cannot render Lisp fixture value ~S" value))))

(defun render-node (node indentation)
  (with-output-to-string (output)
    (format output
            "(~A ~A"
            (render-lisp (first node))
            (render-lisp (second node)))
    (dolist (child (cddr node))
      (write-char #\Newline output)
      (write-string (make-string (+ indentation 2) :initial-element #\Space)
                    output)
      (write-string (render-node child (+ indentation 2)) output))
    (write-char #\) output)))

(defun render-block-fixture (entries)
  (with-output-to-string (output)
    (write-line
     "; Generated by scripts/generate-block-structure-fixtures.py. Do not edit by hand."
     output)
    (write-line "(" output)
    (dolist (entry entries)
      (format output
              "  (~A ~A)~%"
              (render-lisp (first entry))
              (render-node (second entry) 2)))
    (write-line ")" output)))

(defun render-inline-fixture (entries)
  (with-output-to-string (output)
    (write-line
     "; Generated by scripts/generate-inline-structure-fixtures.py. Do not edit by hand."
     output)
    (write-line "(" output)
    (dolist (entry entries)
      (format output "  ~A~%" (render-lisp (list (first entry) (second entry)))))
    (write-line ")" output)))

(defun find-executable (name)
  (let* ((path (or (uiop:getenv "PATH") ""))
         (length (length path))
         (start 0))
    (loop (let* ((end (position #\: path :start start))
                 (directory (subseq path start (or end length)))
                 (base
                  (if (string= directory "")
                      "."
                      directory))
                 (candidate
                  (merge-pathnames name
                                   (uiop:ensure-directory-pathname
                                    (pathname base)))))
            (when
                (and (probe-file candidate)
                     (zerop (sb-posix:access (namestring candidate) 1)))
              (return (namestring candidate)))
            (if end
                (setf start (1+ end))
                (return nil))))))

(defun cmark-path (value)
  (or (and value (plusp (length value)) value)
      (find-executable "cmark-gfm")
      (find-executable "cmark")
      (fail "set CMARK_GFM or pass --cmark to the pinned cmark-gfm binary")))

(defun run-cmark-version (binary)
  (let* ((result
          (process-kit:run binary
                           '("--version")
                           :search
                           t
                           :error
                           :output
                           :timeout
                           *cmark-timeout-seconds*
                           :external-format
                           :utf-8))
         (version (process-kit:process-result-stdout result)))
    (unless
        (and (process-kit:process-success-p result)
             (search *cmark-version* version))
      (fail "~A is not pinned cmark-gfm ~A; version output was ~S"
            binary
            *cmark-version*
            (string-trim '(#\Space #\Tab #\Return #\Newline) version)))
    binary))

(defun run-cmark (binary example with-gfm-extensions)
  (let* ((arguments
          (append '("--to" "xml" "--sourcepos")
                  (when with-gfm-extensions
                    (loop for extension in *gfm-extensions*
                          append (list "--extension" extension)))))
         (result
          (process-kit:run binary
                           arguments
                           :search
                           t
                           :input
                           (example-markdown example)
                           :timeout
                           *cmark-timeout-seconds*
                           :external-format
                           :utf-8)))
    (unless (process-kit:process-success-p result)
      (fail "~A: ~A exited ~S: ~A"
            (example-identifier example)
            binary
            (process-kit:process-result-exit-code result)
            (process-kit:process-result-stderr result)))
    (when
        (or (process-kit:process-result-stdout-truncated-p result)
            (process-kit:process-result-stderr-truncated-p result))
      (fail "~A: cmark output exceeded the process-kit capture limit"
            (example-identifier example)))
    (process-kit:process-result-stdout result)))

(defun commonmark-spec-path (options)
  (or (generator-options-commonmark-spec options)
      (fail "set COMMONMARK_TEST_SUITE or pass --commonmark-spec")))

(defun gfm-spec-path (options)
  (or (generator-options-gfm-spec options)
      (fail "set GFM_TEST_SUITE or pass --gfm-spec")))

(defun write-utf8-file (path text)
  (with-open-file
      (output path
              :direction
              :output
              :if-exists
              :supersede
              :if-does-not-exist
              :create
              :element-type
              '(unsigned-byte 8))
    (write-sequence (sb-ext:string-to-octets text :external-format :utf-8)
                    output)))

(defun read-file-bytes (path)
  (with-open-file
      (input path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes
           (make-array (file-length input) :element-type '(unsigned-byte 8))))
      (read-sequence bytes input)
      bytes)))

(defun check-or-write-output (path rendered check)
  (if check
      (progn
        (unless (probe-file path)
          (fail "fixture does not exist: ~A" (namestring path)))
        (unless
            (equalp (read-file-bytes path)
                    (sb-ext:string-to-octets rendered :external-format :utf-8))
          (fail "fixture differs from generated output: ~A" (namestring path))))
      (write-utf8-file path rendered)))

(defun option-usage (kind)
  (format nil
          (concatenate 'string
                       "Usage: sbcl --script scripts/generate-~A-structure-fixtures.lisp"
                       " [--commonmark-spec PATH] [--gfm-spec PATH] [--cmark PATH]"
                       " [--output PATH] [--check]")
          kind))

(defun parse-options (kind arguments)
  (let* ((output-name
          (if (eq kind :block)
              "block-structure-fixtures.lisp"
              "inline-structure-fixtures.lisp"))
         (options
          (make-generator-options :commonmark-spec
                                  (uiop:getenv "COMMONMARK_TEST_SUITE")
                                  :gfm-spec
                                  (uiop:getenv "GFM_TEST_SUITE")
                                  :cmark
                                  (uiop:getenv "CMARK_GFM")
                                  :output
                                  (merge-pathnames
                                   (format nil "t/data/~A" output-name)
                                   *repository-root*)))
         (position 0))
    (labels ((next-value (option)
               (when (>= (1+ position) (length arguments))
                 (error 'command-line-error
                        :message
                        (format nil "option ~A requires a value" option)))
               (incf position)
               (nth position arguments)))
      (loop while (< position (length arguments))
            for argument = (nth position arguments)
            do (cond
                 ((member argument '("-h" "--help") :test #'string=)
                  (setf (generator-options-help options) t))
                 ((string= argument "--check")
                  (setf (generator-options-check options) t))
                 ((string= argument "--commonmark-spec")
                  (setf (generator-options-commonmark-spec options) (next-value
                                                                     argument)))
                 ((string= argument "--gfm-spec")
                  (setf (generator-options-gfm-spec options) (next-value
                                                              argument)))
                 ((string= argument "--cmark")
                  (setf (generator-options-cmark options) (next-value argument)))
                 ((string= argument "--output")
                  (setf (generator-options-output options) (pathname
                                                            (next-value
                                                             argument))))
                 (t
                  (error 'command-line-error
                         :message
                         (format nil "unrecognized argument ~S" argument)))) (incf
                                                                              position)))
    options))

(defun fixture-entries (kind cmark examples)
  (let ((entries nil)
        (footnote-count 0))
    (dolist (example examples)
      (let ((xml
             (run-cmark cmark
                        example
                        (string= (subseq (example-identifier example) 0 4)
                                 "gfm-"))))
        (if (eq kind :block)
            (multiple-value-bind (signature count)
                (block-signature (xml-document xml) example)
              (incf footnote-count count)
              (push (list (example-identifier example) signature) entries))
            (push
             (list (example-identifier example)
                   (inline-structure-signature xml example))
             entries))))
    (values (nreverse entries) footnote-count)))

(defun run-generator (kind)
  (let* ((script-name
          (if (eq kind :block)
              "generate-block-structure-fixtures.lisp"
              "generate-inline-structure-fixtures.lisp"))
         (output-name
          (if (eq kind :block)
              "block"
              "inline")))
    (handler-case (let* ((options
                          (parse-options kind (uiop:command-line-arguments))))
                    (when (generator-options-help options)
                      (format t
                              "~A~%~%Generate signatures from the pinned CommonMark and cmark-gfm suites.~%"
                              (option-usage output-name))
                      (return-from run-generator
                        0))
                    (let* ((commonmark-path
                            (pathname (commonmark-spec-path options)))
                           (gfm-path (pathname (gfm-spec-path options)))
                           (binary
                            (run-cmark-version
                             (cmark-path (generator-options-cmark options))))
                           (examples
                            (append (read-examples commonmark-path "commonmark")
                                    (read-examples gfm-path "gfm"))))
                      (multiple-value-bind (entries footnote-count)
                          (fixture-entries kind binary examples)
                        (let ((rendered
                               (if (eq kind :block)
                                   (render-block-fixture entries)
                                   (render-inline-fixture entries))))
                          (check-or-write-output
                           (generator-options-output options)
                           rendered
                           (generator-options-check options))
                          (if (eq kind :block)
                              (format t
                                      "~D examples generated; ~D footnote definition nodes with sourcepos labels; output ~A~%"
                                      (length entries)
                                      footnote-count
                                      (namestring
                                       (generator-options-output options)))
                              (format t
                                      "~D examples generated; output ~A~%"
                                      (length entries)
                                      (namestring
                                       (generator-options-output options))))))
                      0))
      (command-line-error (condition)
        (format *error-output* "~A~%~A~%" (option-usage output-name) condition)
        2)
      (error (condition)
        (format *error-output* "~A: ~A~%" script-name condition)
        1))))

(defun run-block-generator ()
  (run-generator :block))

(defun run-inline-generator ()
  (run-generator :inline))
