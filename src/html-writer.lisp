(in-package #:cl-markdown-kit)

(defstruct (%html-writer-context
             (:constructor %make-html-writer-context
                 (&key stream raw-html unsafe-urls gfm footnote-definitions
                       footnote-numbers footnote-reference-counts
                       footnote-reference-indices footnote-order
                       footnote-ids footnote-reference-ids)))
  stream
  last-character
  raw-html
  unsafe-urls
  gfm
  footnote-definitions
  footnote-numbers
  footnote-reference-counts
  footnote-reference-indices
  footnote-order
  footnote-ids
  footnote-reference-ids
  tree-parent
  tree-grandparent
  tree-child-index
  tree-last-child-p
  table-body-open-p
  plain-alt-root)

(defun %hw-write (context string)
  (unless (zerop (length string))
    (write-string string (%html-writer-context-stream context))
    (setf (%html-writer-context-last-character context)
          (char string (1- (length string)))))
  string)

(defun %hw-write-char (context character)
  (write-char character (%html-writer-context-stream context))
  (setf (%html-writer-context-last-character context) character)
  character)

(defun %hw-newline (context)
  (when (and (%html-writer-context-last-character context)
             (not (char= (%html-writer-context-last-character context)
                         #\Newline)))
    (%hw-write-char context #\Newline)))

(defun %hw-escaped-text (context string)
  (%hw-write context (escape-html-text string)))

(defun %hw-escaped-attribute (context string)
  (%hw-write context (escape-html-attribute string)))

(defun %hw-href (context uri)
  (unless (and (not (%html-writer-context-unsafe-urls context))
               (dangerous-url-p uri))
    (%hw-escaped-attribute context (normalize-uri-percent-encoding uri))))

(defun %hw-tagfilter-match-p (string start)
  (let* ((length (length string))
         (index (1+ start)))
    (when (and (< index length) (char= (char string index) #\/))
      (incf index))
    (some (lambda (tag)
            (let ((end (+ index (length tag))))
              (and (<= end length)
                   (string-equal tag string :start2 index :end2 end)
                   (< end length)
                   (let ((next (char string end)))
                     (or (%pc-html-space-p next)
                         (char= next #\>)
                         (and (char= next #\/)
                              (< (1+ end) length)
                              (char= (char string (1+ end)) #\>)))))))
          +gfm-tagfilter-tags+)))

(defun %hw-filter-html (context string)
  (loop with start = 0
        for index = (position #\< string :start start)
        while index
        do (%hw-write context (subseq string start index))
           (if (%hw-tagfilter-match-p string index)
               (%hw-write context "&lt;")
               (%hw-write-char context #\<))
           (setf start (1+ index))
        finally (%hw-write context (subseq string start))))

(defun %hw-write-raw-html (context string)
  (if (%html-writer-context-gfm context)
      (%hw-filter-html context string)
      (%hw-write context string)))

(defun %hw-footnote-id-candidate (prefix label)
  (format nil "~A~A" prefix (normalize-uri-percent-encoding label)))

(defun %hw-footnote-reference-id-candidate (label reference-index)
  (let ((base (%hw-footnote-id-candidate "fnref-" label)))
    (if (= reference-index 1)
        base
        (format nil "~A-~D" base reference-index))))

(defun %hw-allocate-footnote-ids (candidates)
  (let ((reserved (make-hash-table :test #'equal))
        (allocated (make-hash-table :test #'equal)))
    (dolist (candidate candidates)
      (setf (gethash candidate reserved) t))
    (mapcar (lambda (candidate)
              (if (gethash candidate allocated)
                  (loop for suffix from 2
                        for identifier = (format nil "~A-~D" candidate suffix)
                        unless (or (gethash identifier reserved)
                                   (gethash identifier allocated))
                          do (setf (gethash identifier allocated) t)
                             (return identifier))
                  (progn
                    (setf (gethash candidate allocated) t)
                    candidate)))
            candidates)))

(defun %hw-build-footnote-context (document)
  (let ((definitions (make-hash-table :test #'equal))
        (numbers (make-hash-table :test #'equal))
        (reference-counts (make-hash-table :test #'equal))
        (reference-identifiers nil)
        (order nil)
        (next-number 0))
    (walk-node-tree-cps
     document
     (lambda (node phase parent grandparent previous-sibling next-sibling
                   child-index last-child-p)
       (declare (ignore parent grandparent previous-sibling next-sibling
                        child-index last-child-p))
       (when (eq phase :enter)
         (when (footnote-definition-p node)
           (setf (gethash (footnote-definition-normalized-label node)
                          definitions)
                 node)))))
    (walk-node-tree-cps
     document
     (lambda (node phase parent grandparent previous-sibling next-sibling
                   child-index last-child-p)
       (declare (ignore parent grandparent previous-sibling next-sibling
                        child-index last-child-p))
       (when (eq phase :enter)
         (when (footnote-reference-p node)
           (let ((label (footnote-reference-normalized-label node)))
             (when (gethash label definitions)
               (unless (gethash label numbers)
                 (incf next-number)
                 (setf (gethash label numbers) next-number)
                 (push label order))
               (let ((reference-index
                       (incf (gethash label reference-counts 0))))
                 (push (list label reference-index
                             (%hw-footnote-reference-id-candidate
                              label reference-index))
                       reference-identifiers))))))))
    (let* ((order (nreverse order))
           (reference-identifiers (nreverse reference-identifiers))
           (footnote-ids (make-hash-table :test #'equal))
           (footnote-reference-ids (make-hash-table :test #'equal))
           (allocated-footnote-ids
             (%hw-allocate-footnote-ids
              (mapcar (lambda (label)
                        (%hw-footnote-id-candidate "fn-" label))
                      order)))
           (allocated-reference-ids
             (%hw-allocate-footnote-ids
              (mapcar #'third reference-identifiers))))
      (loop for label in order
            for identifier in allocated-footnote-ids
            do (setf (gethash label footnote-ids) identifier))
      (loop for entry in reference-identifiers
            for identifier in allocated-reference-ids
            do (setf (gethash (list (first entry) (second entry))
                              footnote-reference-ids)
                     identifier))
      (values definitions numbers reference-counts order
              footnote-ids footnote-reference-ids))))

(defun %hw-footnote-reference-index (context label)
  (incf (gethash label
                 (%html-writer-context-footnote-reference-indices context)
                 0)))

(defun %hw-write-footnote-reference (context node)
  (let* ((label (footnote-reference-normalized-label node))
         (number (gethash label (%html-writer-context-footnote-numbers context)))
         (reference-index (%hw-footnote-reference-index context label))
         (footnote-id (gethash label
                               (%html-writer-context-footnote-ids context)))
         (reference-id
           (gethash (list label reference-index)
                    (%html-writer-context-footnote-reference-ids context))))
    (when number
      (%hw-write context "<sup class=\"footnote-ref\"><a href=\"#")
      (%hw-escaped-attribute context footnote-id)
      (%hw-write context "\" id=\"")
      (%hw-escaped-attribute context reference-id)
      (%hw-write context "\" data-footnote-ref>")
      (%hw-write context (format nil "~D" number))
      (%hw-write context "</a></sup>"))))

(defun %hw-write-footnote-backrefs (context label)
  (let ((number (gethash label (%html-writer-context-footnote-numbers context)))
        (count (gethash label (%html-writer-context-footnote-reference-counts
                               context))))
    (when number
      (loop for reference-index from 1 to count
            do (when (> reference-index 1)
                 (%hw-write context " "))
               (%hw-write context "<a href=\"#")
               (%hw-escaped-attribute
                context
                (gethash (list label reference-index)
                         (%html-writer-context-footnote-reference-ids
                          context)))
               (%hw-write context
                          (format nil
                                  "\" class=\"footnote-backref\" data-footnote-backref data-footnote-backref-idx=\"~D~A\" aria-label=\"Back to reference ~D~A\">↩"
                                  number
                                  (if (> reference-index 1)
                                      (format nil "-~D" reference-index)
                                      "")
                                  number
                                  (if (> reference-index 1)
                                      (format nil "-~D" reference-index)
                                      "")))
               (when (> reference-index 1)
                 (%hw-write context (format nil "<sup class=\"footnote-ref\">~D</sup>"
                                            reference-index)))
               (%hw-write context "</a>")))))

(defun %hw-inline-plain-alt-p (context node)
  (let ((root (%html-writer-context-plain-alt-root context)))
    (and root (not (eq root node)))))

(define-node-dispatch (%hw-render-inline-event (context node phase))
    :operation :html-renderer
    :family :inline
  (text
   (when (eq phase :enter)
     (if (%hw-inline-plain-alt-p context node)
         (%hw-escaped-attribute context (text-value node))
         (%hw-escaped-text context (text-value node))))
   (when (eq phase :enter) :descend))
  (code-span
   (when (eq phase :enter)
     (if (%hw-inline-plain-alt-p context node)
         (%hw-escaped-attribute context (code-span-literal-text node))
         (progn
           (%hw-write context "<code>")
           (%hw-escaped-text context (code-span-literal-text node))
           (%hw-write context "</code>"))))
   (when (eq phase :enter) :descend))
  (html-inline
   (when (eq phase :enter)
     (if (%hw-inline-plain-alt-p context node)
         (%hw-escaped-attribute context (html-inline-body node))
         (case (%html-writer-context-raw-html context)
           (:omit (%hw-write context "<!-- raw HTML omitted -->"))
           (:escape (%hw-escaped-text context (html-inline-body node)))
           (:pass-through (%hw-write-raw-html context (html-inline-body node))))))
   (when (eq phase :enter) :descend))
  (soft-break
   (when (eq phase :enter)
     (%hw-write-char context
                     (if (%hw-inline-plain-alt-p context node)
                         #\Space
                         #\Newline)))
   (when (eq phase :enter) :descend))
  (hard-break
   (when (eq phase :enter)
     (if (%hw-inline-plain-alt-p context node)
         (%hw-write-char context #\Space)
         (progn
           (%hw-write context "<br />")
           (%hw-write-char context #\Newline))))
   (when (eq phase :enter) :descend))
  (emphasis
   (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "<em>"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "</em>")))))
  (strong
   (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "<strong>"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "</strong>")))))
  (strikethrough
   (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "<del>"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "</del>")))))
  (link
   (case phase
     (:enter
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "<a href=\"")
        (%hw-href context (link-destination node))
        (when (link-title node)
          (%hw-write context "\" title=\"")
          (%hw-escaped-attribute context (link-title node)))
        (%hw-write context "\">"))
      :descend)
     (:leave
      (unless (%hw-inline-plain-alt-p context node)
        (%hw-write context "</a>")))))
  (image
   (cond
     ((and (eq phase :leave)
           (eq node (%html-writer-context-plain-alt-root context)))
      (setf (%html-writer-context-plain-alt-root context) nil)
      (when (image-title node)
        (%hw-write context "\" title=\"")
        (%hw-escaped-attribute context (image-title node)))
      (%hw-write context "\" />"))
     ((%hw-inline-plain-alt-p context node)
      (when (eq phase :enter) :descend))
     ((eq phase :enter)
      (%hw-write context "<img src=\"")
      (%hw-href context (image-destination node))
      (%hw-write context "\" alt=\"")
      (setf (%html-writer-context-plain-alt-root context) node)
      :descend)))
  (autolink
   (when (eq phase :enter)
     (if (%hw-inline-plain-alt-p context node)
         (%hw-escaped-attribute context (autolink-display-text node))
         (progn
           (%hw-write context "<a href=\"")
           (%hw-href context (autolink-destination node))
           (%hw-write context "\">")
           (%hw-escaped-text context (autolink-display-text node))
           (%hw-write context "</a>"))))
   (when (eq phase :enter) :descend))
  (footnote-reference
   (when (and (eq phase :enter)
              (not (%hw-inline-plain-alt-p context node)))
     (%hw-write-footnote-reference context node))
   (when (eq phase :enter) :descend)))

(defun %hw-code-info-token (info)
  (when (and info (plusp (length info)))
    (let* ((start (position-if-not (lambda (char)
                                     (find char '(#\Space #\Tab #\Newline
                                                  #\Return #\Page)))
                                   info))
           (end (and start
                     (position-if (lambda (char)
                                    (find char '(#\Space #\Tab #\Newline
                                                 #\Return #\Page)))
                                  info :start start))))
      (when start
        (subseq info start end)))))

(defun %hw-write-code-block (context node)
  (%hw-newline context)
  (let ((info (%hw-code-info-token (code-block-info-string node))))
    (if info
        (progn
          (%hw-write context "<pre><code class=\"language-")
          (%hw-escaped-attribute context info)
          (%hw-write context "\">")
          (%hw-escaped-text context (code-block-body node))
          (%hw-write context "</code></pre>"))
        (progn
          (%hw-write context "<pre><code>")
          (%hw-escaped-text context (code-block-body node))
          (%hw-write context "</code></pre>")))
    (%hw-write-char context #\Newline)))

(defun %hw-tight-list-paragraph-p (context)
  (let ((parent (%html-writer-context-tree-parent context))
        (grandparent (%html-writer-context-tree-grandparent context)))
    (and (list-item-p parent)
         (list-node-p grandparent)
         (list-node-tight-p grandparent))))

(defun %hw-write-list-item-open (context node)
  (%hw-newline context)
  (%hw-write context "<li>")
  (when (task-list-item-p node)
    (%hw-write context
               (if (task-list-item-checked-p node)
                   "<input type=\"checkbox\" checked=\"\" disabled=\"\" /> "
                   "<input type=\"checkbox\" disabled=\"\" /> "))))

(defun %hw-write-list-item-close (context)
  (%hw-write context "</li>")
  (%hw-write-char context #\Newline))

(defun %hw-write-list-open (context node)
  (%hw-newline context)
  (if (eq (list-node-kind node) :ordered)
      (if (= (list-node-start-number node) 1)
          (progn
            (%hw-write context "<ol>")
            (%hw-write-char context #\Newline))
          (%hw-write context
                     (format nil "<ol start=\"~D\">~%"
                             (list-node-start-number node))))
      (progn
        (%hw-write context "<ul>")
        (%hw-write-char context #\Newline))))

(defun %hw-write-list-close (context node)
  (%hw-write context (if (eq (list-node-kind node) :ordered) "</ol>" "</ul>"))
  (%hw-write-char context #\Newline))

(defun %hw-write-table-open (context)
  (%hw-newline context)
  (%hw-write context "<table>")
  (setf (%html-writer-context-table-body-open-p context) nil))

(defun %hw-write-table-close (context)
  (when (%html-writer-context-table-body-open-p context)
    (%hw-newline context)
    (%hw-write context "</tbody>")
    (setf (%html-writer-context-table-body-open-p context) nil))
  (%hw-newline context)
  (%hw-write context "</table>")
  (%hw-write-char context #\Newline))

(defun %hw-write-table-row-open (context node)
  (if (table-row-header-row-p node)
      (progn
        (%hw-newline context)
        (%hw-write context "<thead>")
        (%hw-newline context))
      (unless (%html-writer-context-table-body-open-p context)
        (%hw-newline context)
        (%hw-write context "<tbody>")
        (%hw-write-char context #\Newline)
        (setf (%html-writer-context-table-body-open-p context) t)))
  (%hw-newline context)
  (%hw-write context "<tr>"))

(defun %hw-write-table-row-close (context node)
  (%hw-newline context)
  (%hw-write context "</tr>")
  (when (table-row-header-row-p node)
    (%hw-newline context)
    (%hw-write context "</thead>")))

(defun %hw-write-table-cell-open (context node)
  (let* ((row (%html-writer-context-tree-parent context))
         (table (%html-writer-context-tree-grandparent context))
         (alignment (or (table-cell-alignment node)
                        (nth (%html-writer-context-tree-child-index context)
                             (table-alignments table)))))
    (%hw-newline context)
    (%hw-write context (if (table-row-header-row-p row) "<th" "<td"))
    (when alignment
      (%hw-write context " align=\"")
      (%hw-escaped-attribute context (string-downcase (string alignment)))
      (%hw-write-char context #\"))
    (%hw-write-char context #\>)))

(defun %hw-write-table-cell-close (context)
  (%hw-write context
             (if (table-row-header-row-p
                  (%html-writer-context-tree-parent context))
                 "</th>"
                 "</td>")))

(defun %hw-write-footnote-definition (context node)
  (let ((label (footnote-definition-normalized-label node))
        (children (footnote-definition-children node)))
    (%hw-newline context)
    (%hw-write context "<li id=\"")
    (%hw-escaped-attribute
     context
     (gethash label (%html-writer-context-footnote-ids context)))
    (%hw-write context "\">")
    (%hw-write-char context #\Newline)
    (%hw-render-node-tree context node)
    (unless (and children (paragraph-p (car (last children))))
      (%hw-newline context)
      (%hw-write-footnote-backrefs context label)
      (%hw-write-char context #\Newline))
    (%hw-write context "</li>")
    (%hw-write-char context #\Newline)))

(defun %hw-write-footnotes (context)
  (let ((order (%html-writer-context-footnote-order context))
        (definitions (%html-writer-context-footnote-definitions context)))
    (when order
      (%hw-newline context)
      (%hw-write context "<section class=\"footnotes\" data-footnotes>")
      (%hw-write-char context #\Newline)
      (%hw-write context "<ol>")
      (%hw-write-char context #\Newline)
      (dolist (label order)
        (%hw-write-footnote-definition
         context (gethash label definitions)))
      (%hw-write context "</ol>")
      (%hw-write-char context #\Newline)
      (%hw-write context "</section>")
      (%hw-write-char context #\Newline))))

(progn
  (define-node-dispatch (%hw-render-block-event (context node phase))
      :operation :html-renderer
      :family :block
    (document
     (when (eq phase :enter) :descend))
    (front-matter :skip)
    (link-reference-definition :skip)
    (footnote-definition
     (if (null (%html-writer-context-tree-parent context))
         :descend
         :skip))
    (block-quote
     (case phase
       (:enter
        (%hw-newline context)
        (%hw-write context "<blockquote>")
        (%hw-write-char context #\Newline)
        :descend)
       (:leave
        (%hw-newline context)
        (%hw-write context "</blockquote>")
        (%hw-write-char context #\Newline))))
    (list-node
     (case phase
       (:enter
        (%hw-write-list-open context node)
        :descend)
       (:leave
        (%hw-write-list-close context node))))
    (task-list-item
     (case phase
       (:enter
        (%hw-write-list-item-open context node)
        :descend)
       (:leave
        (%hw-write-list-item-close context))))
    (list-item
     (case phase
       (:enter
        (%hw-write-list-item-open context node)
        :descend)
       (:leave
        (%hw-write-list-item-close context))))
    (paragraph
     (let ((parent (%html-writer-context-tree-parent context)))
       (case phase
         (:enter
          (unless (%hw-tight-list-paragraph-p context)
            (%hw-newline context)
            (%hw-write context "<p>"))
          :descend)
         (:leave
          (when (and (footnote-definition-p parent)
                     (%html-writer-context-tree-last-child-p context))
            (%hw-write-char context #\Space)
            (%hw-write-footnote-backrefs
             context (footnote-definition-normalized-label parent)))
          (unless (%hw-tight-list-paragraph-p context)
            (%hw-write context "</p>")
            (%hw-write-char context #\Newline))))))
    (heading
     (case phase
       (:enter
        (%hw-newline context)
        (%hw-write context (format nil "<h~D>" (heading-level node)))
        :descend)
       (:leave
        (%hw-write context (format nil "</h~D>" (heading-level node)))
        (%hw-write-char context #\Newline))))
    (thematic-break
     (when (eq phase :enter)
       (%hw-newline context)
       (%hw-write context "<hr />")
       (%hw-write-char context #\Newline))
     (when (eq phase :enter) :descend))
    (code-block
     (when (eq phase :enter)
       (%hw-write-code-block context node)
       :descend))
    (html-block
     (when (eq phase :enter)
       (%hw-newline context)
       (case (%html-writer-context-raw-html context)
         (:omit (%hw-write context "<!-- raw HTML omitted -->"))
         (:escape (%hw-escaped-text context (html-block-body node)))
         (:pass-through (%hw-write-raw-html context (html-block-body node))))
       (%hw-newline context)
       :descend))
    (table
     (case phase
       (:enter
        (%hw-write-table-open context)
        :descend)
       (:leave
        (%hw-write-table-close context))))
    (table-row
     (case phase
       (:enter
        (%hw-write-table-row-open context node)
        :descend)
       (:leave
        (%hw-write-table-row-close context node))))
    (table-cell
     (case phase
       (:enter
        (%hw-write-table-cell-open context node)
        :descend)
       (:leave
        (%hw-write-table-cell-close context)))))
  (verify-node-dispatches :operation :html-renderer :families '(:block :inline))
  (defun %hw-render-node-tree (context root)
    (walk-node-tree-cps
     root
     (lambda (node phase parent grandparent previous-sibling next-sibling
                   child-index last-child-p)
       (declare (ignore previous-sibling next-sibling))
       (setf (%html-writer-context-tree-parent context) parent
             (%html-writer-context-tree-grandparent context) grandparent
             (%html-writer-context-tree-child-index context) child-index
             (%html-writer-context-tree-last-child-p context) last-child-p)
       (cond
         ((inline-node-p node)
          (%hw-render-inline-event context node phase))
         ((block-node-p node)
          (%hw-render-block-event context node phase))
         (t
          (error "Unsupported HTML node: ~S" node)))))))

(defun write-html-to-stream (document stream &key (raw-html :omit) unsafe-urls
                                                  (gfm t))
  "Write DOCUMENT as HTML to STREAM and return STREAM."
  (check-type document document)
  (check-type stream stream)
  (unless (output-stream-p stream)
    (error "STREAM is not open for output"))
  (unless (member raw-html '(:omit :escape :pass-through))
    (error "RAW-HTML must be :OMIT, :ESCAPE, or :PASS-THROUGH"))
  (unless (typep unsafe-urls 'boolean)
    (error "UNSAFE-URLS must be a boolean"))
  (unless (typep gfm 'boolean)
    (error "GFM must be a boolean"))
  (multiple-value-bind (definitions numbers reference-counts order
                        footnote-ids footnote-reference-ids)
      (%hw-build-footnote-context document)
    (let ((context (%make-html-writer-context
                    :stream stream
                    :raw-html raw-html
                    :unsafe-urls unsafe-urls
                    :gfm gfm
                    :footnote-definitions definitions
                    :footnote-numbers numbers
                    :footnote-reference-counts reference-counts
                    :footnote-reference-indices (make-hash-table :test #'equal)
                    :footnote-order order
                    :footnote-ids footnote-ids
                    :footnote-reference-ids footnote-reference-ids)))
      (%hw-render-node-tree context document)
      (%hw-write-footnotes context)))
  stream)

(defun write-html (document &key (raw-html :omit) unsafe-urls (gfm t))
  "Return DOCUMENT rendered as HTML."
  (with-output-to-string (stream)
    (write-html-to-stream document stream
                          :raw-html raw-html
                          :unsafe-urls unsafe-urls
                          :gfm gfm)))
