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
                                    ;; Keep the Markdown input; fixture values come from cmark's parsed structure.
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
                     ;; cmark emits inline text as child <text> elements; a mixed-content sample
                     ;; confirmed that direct text after a child is not part of that representation.
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

;; cmark-gfm 0.29 emits `<<unknown>` and `</<unknown>>` in raw XML.
;; Normalize these malformed tags before the XML reader processes them.
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

(progn
  (let ((directory (uiop:pathname-directory-pathname *load-truename*)))
    (load (merge-pathnames "fixture-generator-overrides.lisp" directory))
    (load (merge-pathnames "fixture-generator-signatures.lisp" directory))
    (load (merge-pathnames "fixture-generator-runtime.lisp" directory))))
