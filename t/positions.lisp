(in-package #:cl-markdown-kit/test)

(defun positions-test-kind (node)
  (typecase node
    (cl-markdown-kit:document :document)
    (cl-markdown-kit:heading :heading)
    (cl-markdown-kit:code-block :code-block)
    (cl-markdown-kit:block-quote :block-quote)
    (cl-markdown-kit:list-node :list)
    (cl-markdown-kit:task-list-item :task-list-item)
    (cl-markdown-kit:list-item :list-item)
    (cl-markdown-kit:paragraph :paragraph)
    (cl-markdown-kit:thematic-break :thematic-break)
    (cl-markdown-kit:front-matter :front-matter)
    (cl-markdown-kit:html-block :html-block)
    (cl-markdown-kit:link-reference-definition :link-reference-definition)
    (cl-markdown-kit:table :table)
    (cl-markdown-kit:table-row :table-row)
    (cl-markdown-kit:table-cell :table-cell)
    (cl-markdown-kit:footnote-definition :footnote-definition)
    (cl-markdown-kit:text :text)
    (cl-markdown-kit:code-span :code-span)
    (cl-markdown-kit:html-inline :html-inline)
    (cl-markdown-kit:soft-break :soft-break)
    (cl-markdown-kit:hard-break :hard-break)
    (cl-markdown-kit:emphasis :emphasis)
    (cl-markdown-kit:strong :strong)
    (cl-markdown-kit:link :link)
    (cl-markdown-kit:image :image)
    (cl-markdown-kit:strikethrough :strikethrough)
    (cl-markdown-kit:autolink :autolink)
    (cl-markdown-kit:footnote-reference :footnote-reference)
    (t :unknown-node)))

(defun positions-test-lines (string)
  (let ((lines nil) (start 0) (index 0) (length (length string)))
    (loop while (< index length)
          for character = (char string index)
          do (if (find character '(#\Return #\Newline))
                 (progn
                   (push (subseq string start index) lines)
                   (when (and (char= character #\Return)
                              (< (1+ index) length)
                              (char= (char string (1+ index)) #\Newline))
                     (incf index))
                   (incf index)
                   (setf start index))
                 (incf index)))
    (push (subseq string start length) lines)
    (nreverse lines)))

(defun positions-test-hspace-p (character)
  (find character '(#\Space #\Tab)))

(defun positions-test-leading-run (string character)
  (loop for index from 0 below (length string)
        while (char= (char string index) character)
        count index))

(defun positions-test-trailing-run (string character)
  (loop for index downfrom (1- (length string)) to 0
        while (char= (char string index) character)
        count index))

(defun positions-test-atx-syntax-p (node slice)
  (let* ((line (first (positions-test-lines slice)))
         (count (and line (positions-test-leading-run line #\#))))
    (and line (= count (cl-markdown-kit:heading-opening-marker-length node))
         (<= 1 count 6)
         (or (= count (length line))
             (positions-test-hspace-p (char line count))))))

(defun positions-test-fence-syntax-p (node slice)
  (let* ((line (first (positions-test-lines slice)))
         (marker (cl-markdown-kit:code-block-fence-character node)))
    (and line marker (plusp (length line))
         (char= (char line 0) marker)
         (>= (positions-test-leading-run line marker)
             (cl-markdown-kit:code-block-fence-length node)))))

(defun positions-test-node-syntax-error (node slice)
  (typecase node
    (cl-markdown-kit:document
     (unless (string= slice (cl-markdown-kit:document-source node))
       :document-source))
    (cl-markdown-kit:heading
     (when (and (eq (cl-markdown-kit:heading-style node) :atx)
                (not (positions-test-atx-syntax-p node slice)))
       :atx-heading-opener))
    (cl-markdown-kit:code-block
     (when (and (eq (cl-markdown-kit:code-block-style node) :fenced)
                (not (positions-test-fence-syntax-p node slice)))
       :fenced-code-opener))
    (cl-markdown-kit:block-quote
     (unless (and (plusp (length slice)) (char= (char slice 0) #\>))
       :block-quote-opener))
    (cl-markdown-kit:front-matter
     (let ((lines (positions-test-lines slice)))
       (unless (and (string= (first lines) "---")
                    (string= (car (last lines)) "---"))
         :front-matter-delimiters)))
    (cl-markdown-kit:thematic-break
     (let* ((line (string-trim '(#\Space #\Tab)
                               (first (positions-test-lines slice))))
            (marker (and (plusp (length line)) (char line 0))))
       (unless (and marker (find marker "*_- ") (>= (count marker line) 3)
                    (every (lambda (character)
                             (or (char= character marker)
                                 (positions-test-hspace-p character)))
                           line))
         :thematic-break-markers)))
    (cl-markdown-kit:html-block
     (unless (and (plusp (length slice)) (char= (char slice 0) #\<))
       :html-block-opener))
    (cl-markdown-kit:link-reference-definition
     (unless (and (plusp (length slice)) (char= (char slice 0) #\[)
                  (search "]:" slice :start2 1))
       :reference-definition-markers))
    (cl-markdown-kit:text
     (unless (string= (cl-markdown-kit:text-value node)
                      (cl-markdown-kit::%ip-unescape slice 0 (length slice)))
       :text-source-spelling))
    (cl-markdown-kit:code-span
     (let ((run (cl-markdown-kit:code-span-backtick-run-length node)))
       (unless (and (>= (length slice) (* 2 run))
                    (= (positions-test-leading-run slice #\`) run)
                    (= (positions-test-trailing-run slice #\`) run))
         :code-span-backticks)))
    (cl-markdown-kit:html-inline
     (unless (and (>= (length slice) 2) (char= (char slice 0) #\<)
                  (char= (char slice (1- (length slice))) #\>))
       :html-inline-delimiters))
    ((or cl-markdown-kit:emphasis cl-markdown-kit:strong)
     (let* ((marker (typecase node
                      (cl-markdown-kit:emphasis
                       (cl-markdown-kit:emphasis-delimiter-character node))
                      (cl-markdown-kit:strong
                       (cl-markdown-kit:strong-delimiter-character node))))
            (run (if (typep node 'cl-markdown-kit:strong) 2 1)))
       (unless (and (>= (length slice) (* 2 run))
                    (>= (positions-test-leading-run slice marker) run)
                    (>= (positions-test-trailing-run slice marker) run))
         :inline-delimiters)))
    (cl-markdown-kit:strikethrough
     (let ((opening (positions-test-leading-run slice #\~))
           (closing (positions-test-trailing-run slice #\~)))
       (unless (and (member opening '(1 2))
                    (= opening closing)
                    (> (length slice) (* 2 opening)))
         :strikethrough-delimiters)))
    ((or cl-markdown-kit:link cl-markdown-kit:image)
     (let* ((image-p (typep node 'cl-markdown-kit:image))
            (prefix (if image-p "![" "["))
            (kind (if image-p
                      (cl-markdown-kit:image-reference-kind node)
                      (cl-markdown-kit:link-reference-kind node)))
            (last (and (plusp (length slice)) (char slice (1- (length slice))))))
       (unless (and (>= (length slice) (length prefix))
                    (string= prefix slice :end2 (length prefix))
                    (case kind
                      (:inline (and (char= last #\)) (position #\] slice)))
                      ((:full :collapsed :shortcut) (char= last #\]))
                      (otherwise nil)))
         :link-image-delimiters)))
    (cl-markdown-kit:footnote-reference
     (unless (and (>= (length slice) 4) (string= "[^" slice :end2 2)
                  (char= (char slice (1- (length slice))) #\]))
       :footnote-reference-delimiters))
    ((or cl-markdown-kit:soft-break cl-markdown-kit:hard-break)
     (unless (or (position #\Return slice) (position #\Newline slice))
       :line-break-source))
    (cl-markdown-kit:autolink
     (let ((kind (cl-markdown-kit:autolink-kind node)))
       (when (and (member kind '(:angle-uri :angle-email))
                  (not (and (>= (length slice) 3)
                            (char= (char slice 0) #\<)
                            (char= (char slice (1- (length slice))) #\>))))
         :autolink-delimiters)))
    (t nil)))

(defun positions-test-range (span source)
  (when span
    (let ((start (cl-parser-kit:span-start span))
          (end (cl-parser-kit:span-end span)))
      (when (and (integerp start) (integerp end)
                 (<= 0 start end (length source)))
        (list start end)))))

(defun positions-test-fixture-text (text)
  (substitute #\Tab #\→ text))

(defun positions-test-generated-inputs ()
  (let ((bom (or (code-char #xfeff) (error "U+FEFF is unavailable")))
        (crlf (concatenate 'string (string #\Return) (string #\Newline))))
    (list
     (list "generated-bom-crlf-tab"
           (concatenate 'string (string bom)
                        "# Header\twith tab" crlf crlf
                        "Text\twith [link](destination)."
                        crlf "~~~lisp" crlf
                        "(+ 1 2)" crlf "~~~")))))

(defun positions-test-example (id source gfm)
  (let ((document
          (handler-case
              (cl-markdown-kit:parse-string source :front-matter nil :gfm gfm)
            (error (condition)
              (return-from positions-test-example
                (values (list (list id :parser nil :parse nil
                                    (princ-to-string condition)))
                        0)))))
        (violations nil)
        (node-count 0))
    (labels ((record (kind path property offset detail)
               (push (list id kind path property offset detail) violations))
             (visit (node path parent-range)
               (incf node-count)
               (let* ((kind (positions-test-kind node))
                      (span (cl-markdown-kit:node-span node))
                      (range (positions-test-range span source))
                      (children (cl-markdown-kit:node-children node)))
                 ;; architecture.md: parsed nodes require spans; public constructors may omit them.
                 (unless span (record kind path :missing-span nil nil))
                 (when span
                   (unless (string= (cl-parser-kit:span-source span) source)
                     (record kind path :span-source
                             (cl-parser-kit:span-start span) :not-input-source))
                   (unless range
                     (record kind path :source-range
                             (cl-parser-kit:span-start span)
                             (list :start (cl-parser-kit:span-start span)
                                   :end (cl-parser-kit:span-end span)
                                   :source-length (length source))))
                   (when range
                     (let ((syntax-error
                             (positions-test-node-syntax-error
                              node (subseq source (first range) (second range)))))
                       (when (and parent-range
                                  (not (<= (first parent-range) (first range)
                                           (second range) (second parent-range))))
                         (record kind path :parent-contains-child (first range)
                                 (list :parent parent-range :child range)))
                       (when syntax-error
                         (record kind path :node-syntax (first range) syntax-error)))))
                 (loop for left on children
                       for left-index from 0
                       for left-range = (positions-test-range
                                         (cl-markdown-kit:node-span (first left))
                                         source)
                       do (loop for right in (rest left)
                                for right-index from (1+ left-index)
                                for right-range = (positions-test-range
                                                   (cl-markdown-kit:node-span right)
                                                   source)
                                when (and left-range right-range
                                          (< (max (first left-range)
                                                  (first right-range))
                                             (min (second left-range)
                                                  (second right-range))))
                                  do (record
                                      (list (positions-test-kind (first left))
                                            (positions-test-kind right))
                                      (append path
                                              (list :siblings left-index right-index))
                                      :siblings-do-not-overlap
                                      (max (first left-range) (first right-range))
                                      (list :left left-range :right right-range))))
                 (loop for child in children
                       for index from 0
                       do (visit child (append path (list index)) range)))))
      (visit document nil nil))
    (values (nreverse violations) node-count)))

(describe "T-004 source positions"
  (it "checks spans for all CommonMark and GFM examples"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (generated (positions-test-generated-inputs))
           (violations nil)
           (node-count 0))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (labels ((check-one (id source gfm-p)
                 (multiple-value-bind (found count)
                     (positions-test-example id source gfm-p)
                   (setf violations (nconc violations found))
                   (incf node-count count))))
        (dolist (example commonmark)
          (check-one (spec-example-id example)
                     (positions-test-fixture-text
                      (spec-example-markdown example)) nil))
        (dolist (example gfm)
          (check-one (spec-example-id example)
                     (positions-test-fixture-text
                      (spec-example-markdown example)) t))
        (dolist (input generated)
          (check-one (first input) (second input) t)))
      (format t "T-004 examples: CommonMark ~D, GFM ~D, generated ~D; nodes ~D; violations ~D~%"
              (length commonmark) (length gfm) (length generated)
              node-count (length violations))
      (dolist (violation violations)
        (destructuring-bind (id kind path property offset detail) violation
          (format t "T-004 violation: example=~A node=~S path=~S property=~S offset=~S detail=~S~%"
                  id kind path property offset detail)))
      (expect (length violations) :to-equal 0))))
