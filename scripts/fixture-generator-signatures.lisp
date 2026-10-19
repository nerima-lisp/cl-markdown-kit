(in-package #:fixture-generator)

(defun block-signature (root example)
  (let* ((context
          (make-block-signature-context
           :example example
           :lines (source-lines (example-markdown example))))
         (result (block-node-signature root nil context)))
    (unless (fixture-symbol= (first result) ":document")
      (fail "~A: failed to build document signature"
            (example-identifier example)))
    (values result
            (block-signature-context-footnote-count context))))

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

(defun commonmark-inline-override (identifier)
  (cond
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
    (let ((override
           (commonmark-inline-spec-override (example-identifier example))))
      (when override
        (format t "Overriding ~A: ~A~%"
                (first override)
                (third override))
        (return-from inline-structure-signature
          (inline-override-signature (second override)))))
    (or (commonmark-inline-override (example-identifier example))
        (list fragments (nreverse footnotes)))))

(defstruct block-signature-context
  example
  lines
  (footnote-count 0))

(defun block-sequence-signature (nodes context)
  (let ((count (length nodes)))
    (loop for node in nodes
          for index from 0
          collect (block-node-signature
                   node
                   (sibling-position index count)
                   context))))

(defun block-list-signature (node context)
  (let* ((example (block-signature-context-example context))
         (list-type (xml-attribute node "type"))
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
     (block-sequence-signature (xml-node-children node) context))))

(defun block-item-signature (node context)
  (let* ((name (xml-local-name (xml-node-name node)))
         (attributes
          (when (string= name "tasklist")
            (list (make-fixture-symbol :name ":task-checked")
                  (xml-boolean (xml-attribute node "completed"))))))
    (append
     (list (make-fixture-symbol :name ":item") attributes)
     (block-sequence-signature (xml-node-children node) context))))

(defun block-table-signature (node context)
  (let* ((rows (xml-node-children node))
         (header (first rows)))
    (unless
        (and header
             (string= (xml-local-name (xml-node-name header)) "table_header"))
      (fail "~A: table has no XML header row"
            (example-identifier
             (block-signature-context-example context))))
    (let* ((header-cells (xml-node-children header))
           (attributes
            (list (make-fixture-symbol :name ":columns")
                  (length header-cells)
                  (make-fixture-symbol :name ":alignments")
                  (mapcar
                   (lambda (cell)
                     (xml-alignment (xml-attribute cell "align")))
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
          (block-node-signature row nil context))
        rows)))))

(defun block-row-signature (node context)
  (let ((name (xml-local-name (xml-node-name node))))
    (append
     (list (make-fixture-symbol :name ":row")
           (list (make-fixture-symbol :name ":header")
                 (string= name "table_header")))
     (mapcar
      (lambda (child)
        (block-node-signature child nil context))
      (xml-node-children node)))))

(defun block-footnote-signature (node context)
  (let* ((example (block-signature-context-example context))
         (sourcepos (xml-attribute node "sourcepos")))
    (unless sourcepos
      (fail "~A: footnote XML node has no sourcepos"
            (example-identifier example)))
    (multiple-value-bind (start-line)
        (source-position sourcepos (example-identifier example))
      (let ((label
             (footnote-label-from-source
              (block-signature-context-lines context)
              start-line
              (example-identifier example))))
        (incf (block-signature-context-footnote-count context))
        (append
         (list
          (make-fixture-symbol :name ":footnote-definition")
          (list (make-fixture-symbol :name ":label") label))
         (block-sequence-signature (xml-node-children node) context))))))

(defun block-node-signature (node placement context)
  (let* ((example (block-signature-context-example context))
         (name (xml-local-name (xml-node-name node)))
         (children (xml-node-children node)))
    (cond
      ((string= name "document")
       (append (list (make-fixture-symbol :name ":document") nil)
               (block-sequence-signature children context)))
      ((string= name "block_quote")
       (append (list (make-fixture-symbol :name ":block-quote") nil)
               (block-sequence-signature children context)))
      ((string= name "list")
       (block-list-signature node context))
      ((member name '("item" "tasklist") :test #'string=)
       (block-item-signature node context))
      ((string= name "paragraph")
       (unless placement
         (fail "~A: paragraph missing sibling position"
               (example-identifier example)))
       (list (make-fixture-symbol :name ":paragraph")
             (list (make-fixture-symbol :name ":placement") placement)))
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
       (block-table-signature node context))
      ((member name '("table_header" "table_row") :test #'string=)
       (block-row-signature node context))
      ((string= name "table_cell")
       (list (make-fixture-symbol :name ":cell") nil))
      ((string= name "unknown")
       (block-footnote-signature node context))
      (t
       (fail "~A: unsupported cmark block node ~S"
             (example-identifier example)
             name)))))

(defun inline-override-signature (text-segments)
  (list
   (list
    (loop for text in text-segments
          for index from 0
          append (append
                  (when (plusp index)
                    (list (list (make-fixture-symbol :name ":soft-break"))))
                  (list (list (make-fixture-symbol :name ":text") text)))))
   nil))

(defun commonmark-inline-spec-override (identifier)
  (assoc identifier *commonmark-inline-overrides* :test #'string=))
