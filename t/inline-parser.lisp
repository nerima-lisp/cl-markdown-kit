(in-package #:cl-markdown-kit/test)
(defun inline-parser-test-parse (text &optional reference-index &rest options)
  (destructuring-bind (&key (max-inline-delimiter-depth 4096) (gfm t)) options
    (let* ((buffer (cl-markdown-kit:make-source-buffer text))
           (index (or reference-index (make-hash-table :test #'equal))))
      (cl-markdown-kit::parse-inline
       (cl-markdown-kit:source-buffer-normalized-text buffer)
       (cl-markdown-kit::source-buffer-boundary-map buffer)
       index
       :max-inline-delimiter-depth max-inline-delimiter-depth
       :gfm gfm))))

(defun inline-parser-test-parse-range (text link-allowed &optional start end)
  (let* ((buffer (cl-markdown-kit:make-source-buffer text))
         (normalized (cl-markdown-kit:source-buffer-normalized-text buffer))
         (boundary-map (cl-markdown-kit::source-buffer-boundary-map buffer))
         (index (make-hash-table :test #'equal))
         (limit 4096)
         (backtick-next (cl-markdown-kit::%ip-build-backtick-index normalized))
         (html-closes (cl-markdown-kit::%ip-build-html-closes normalized)))
    (multiple-value-bind (opens closes prefix bracket-depths)
        (cl-markdown-kit::%ip-build-bracket-index
         normalized backtick-next html-closes limit)
      (cl-markdown-kit::%ip-parse-range
       (cl-markdown-kit::%ip-make-context
        normalized boundary-map index limit backtick-next opens closes prefix
        bracket-depths (cl-markdown-kit::%ip-build-bare-links normalized)
        html-closes)
       (or start 0) (or end (length normalized)) link-allowed 0))))

(defun inline-parser-test-span (node)
  (let ((span (cl-markdown-kit:node-span node)))
    (list (cl-parser-kit:span-start span)
          (cl-parser-kit:span-end span)
          (cl-parser-kit:span-start-line span)
          (cl-parser-kit:span-start-column span)
          (cl-parser-kit:span-end-line span)
          (cl-parser-kit:span-end-column span))))

(defun inline-parser-test-ranges-valid-p (node source)
  (let* ((span (cl-markdown-kit:node-span node))
         (start (and span (cl-parser-kit:span-start span)))
         (end (and span (cl-parser-kit:span-end span)))
         (children (cl-markdown-kit:node-children node)))
    (and span
         (string= (cl-parser-kit:span-source span) source)
         (<= 0 start end (length source))
         (loop with previous-end = start
               for child in children
               for child-span = (cl-markdown-kit:node-span child)
               for child-start = (and child-span
                                      (cl-parser-kit:span-start child-span))
               for child-end = (and child-span
                                    (cl-parser-kit:span-end child-span))
               do (unless (and child-span
                               (<= start child-start child-end end)
                               (<= previous-end child-start)
                               (inline-parser-test-ranges-valid-p child source))
                    (return-from inline-parser-test-ranges-valid-p nil))
                  (setf previous-end child-end)
               finally (return t)))))

(defun inline-parser-test-reference-index ()
  (let ((index (make-hash-table :test #'equal)))
    (setf (gethash "r" index)
          (cl-markdown-kit:make-link-reference-definition
           :raw-label "r" :normalized-label "r"
           :destination "/target" :title "Title" :eligible-p t)
          (gethash (cons :footnote "note") index)
          (cl-markdown-kit:make-footnote-definition
           :raw-label "note" :normalized-label "note"))
    index))

(defun inline-parser-test-signals-depth-p (text limit)
  (handler-case
      (progn
        (inline-parser-test-parse text nil :max-inline-delimiter-depth limit)
        nil)
    (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded () t)))

(defun inline-parser-test-read-data (name)
  (let ((path (merge-pathnames
               (format nil "t/data/~A" name)
               (asdf:system-source-directory :cl-markdown-kit))))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (let* ((*read-eval* nil)
             (eof (gensym "EOF"))
             (data (read stream nil eof)))
        (when (eq data eof)
          (error "The inline structure data file is empty: ~A" name))
        (unless (eq (read stream nil eof) eof)
          (error "The inline structure data file has multiple forms: ~A" name))
        data))))

(declaim (ftype function inline-parser-test-node-sequence-signature
                  inline-parser-test-node-signature))

(defun inline-parser-test-node-sequence-signature (nodes)
  (let ((result nil))
    (dolist (node nodes (nreverse result))
      (let ((signature (inline-parser-test-node-signature node)))
        (cond
          ((and (eq (first signature) :text)
                (string= (second signature) "")))
          ((and (eq (first signature) :text)
                result
                (eq (first (first result)) :text))
           (setf (second (first result))
                 (concatenate 'string (second (first result))
                              (second signature))))
          (t (push signature result)))))))

(defun inline-parser-test-node-signature (node)
  (typecase node
    (cl-markdown-kit:text
     (list :text (cl-markdown-kit:text-value node)))
    (cl-markdown-kit:code-span
     (list :code (cl-markdown-kit:code-span-literal-text node)))
    (cl-markdown-kit:html-inline
     (list :html (cl-markdown-kit:html-inline-body node)))
    (cl-markdown-kit:soft-break '(:soft-break))
    (cl-markdown-kit:hard-break '(:hard-break))
    (cl-markdown-kit:emphasis
     (list :emphasis
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:emphasis-children node))))
    (cl-markdown-kit:strong
     (list :strong
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:strong-children node))))
    (cl-markdown-kit:strikethrough
     (list :strikethrough
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:strikethrough-children node))))
    (cl-markdown-kit:link
     (list :link
           (cl-markdown-kit:link-destination node)
           (cl-markdown-kit:link-title node)
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:link-children node))))
    (cl-markdown-kit:image
     (list :image
           (cl-markdown-kit:image-destination node)
           (cl-markdown-kit:image-title node)
           (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:image-children node))))
    (cl-markdown-kit:autolink
     (list :link
           (cl-markdown-kit:autolink-destination node)
           nil
           (list (list :text (cl-markdown-kit:autolink-display-text node)))))
    (cl-markdown-kit:footnote-reference
     (list :footnote-reference
           (cl-markdown-kit:footnote-reference-raw-label node)))
    (otherwise
     (error "Unsupported inline node in structural signature: ~S" node))))

(defun inline-parser-test-block-fragments (node)
  (typecase node
    (cl-markdown-kit:paragraph
     (list (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:paragraph-children node))))
    (cl-markdown-kit:heading
     (list (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:heading-children node))))
    (cl-markdown-kit:table-cell
     (list (inline-parser-test-node-sequence-signature
            (cl-markdown-kit:table-cell-children node))))
    (cl-markdown-kit:footnote-definition nil)
    (otherwise
     (mapcan #'inline-parser-test-block-fragments
             (cl-markdown-kit:node-children node)))))

(defun inline-parser-test-footnote-definitions (root)
  (let ((definitions nil))
    (labels ((visit (node)
               (when (typep node 'cl-markdown-kit:footnote-definition)
                 (push node definitions))
               (dolist (child (cl-markdown-kit:node-children node))
                 (visit child))))
      (visit root))
    (nreverse definitions)))

(defun inline-parser-test-structure-signature (example)
  (handler-case
      (let* ((document
               (cl-markdown-kit:parse-string
                (spec-example-markdown example)
                :front-matter nil
                :gfm (string= "gfm-"
                              (spec-example-id example)
                              :end2 4)))
             (fragments (inline-parser-test-block-fragments document))
             (footnotes
               (loop for definition in
                       (inline-parser-test-footnote-definitions document)
                     collect
                     (list
                      (cl-markdown-kit:footnote-definition-raw-label definition)
                      (mapcan #'inline-parser-test-block-fragments
                              (cl-markdown-kit:footnote-definition-children
                               definition))))))
        (values (list fragments footnotes) nil))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun inline-parser-test-normalize-node-signature (signature)
  (case (first signature)
    ((:link :image)
     (list (first signature)
           (cl-markdown-kit::normalize-uri-percent-encoding
            (second signature))
           (third signature)
           (mapcar #'inline-parser-test-normalize-node-signature
                   (fourth signature))))
    ((:emphasis :strong :strikethrough)
     (list (first signature)
           (mapcar #'inline-parser-test-normalize-node-signature
                   (second signature))))
    (otherwise signature)))

(defun inline-parser-test-trim-text-signature (signature side)
  (when (eq (first signature) :text)
    (setf (second signature)
          (ecase side
            (:left (string-left-trim '(#\Space #\Tab) (second signature)))
            (:right (string-right-trim '(#\Space #\Tab) (second signature)))))))

(defun inline-parser-test-canonicalize-fragment (signatures)
  (let ((signatures
          (mapcar #'inline-parser-test-normalize-node-signature
                  (copy-tree signatures))))
    (when signatures
      (inline-parser-test-trim-text-signature (first signatures) :left)
      (inline-parser-test-trim-text-signature (car (last signatures)) :right)
      (loop for index below (length signatures)
            for signature = (nth index signatures)
            when (member (first signature) '(:soft-break :hard-break))
              do (when (plusp index)
                   (inline-parser-test-trim-text-signature
                    (nth (1- index) signatures) :right))
                 (when (< index (1- (length signatures)))
                   (inline-parser-test-trim-text-signature
                    (nth (1+ index) signatures) :left))))
    (let ((result nil))
      (dolist (signature signatures (nreverse result))
        (unless (and (eq (first signature) :text)
                     (zerop (length (second signature))))
          (if (and (eq (first signature) :text)
                   result
                   (eq (first (first result)) :text))
              (setf (second (first result))
                    (concatenate 'string (second (first result))
                                 (second signature)))
              (push signature result)))))))

(defun inline-parser-test-canonicalize-fragments (fragments)
  (mapcar #'inline-parser-test-canonicalize-fragment fragments))

(defun inline-parser-test-structure-difference (expected actual error-message)
  (if error-message
      (list :parse-error error-message)
      (destructuring-bind (expected-fragments expected-footnotes) expected
        (destructuring-bind (actual-fragments actual-footnotes) actual
          (let ((difference
                  (block-parser-test-first-difference
                   (inline-parser-test-canonicalize-fragments expected-fragments)
                   (inline-parser-test-canonicalize-fragments actual-fragments))))
            (or difference
                (loop for (label expected-content) in expected-footnotes
                      for actual-footnote =
                        (find label actual-footnotes :key #'first :test #'string=)
                      unless actual-footnote
                        do (return (list :missing-footnote label))
                      do (let ((difference
                                 (block-parser-test-first-difference
                                  (inline-parser-test-canonicalize-fragments
                                   expected-content)
                                  (inline-parser-test-canonicalize-fragments
                                   (second actual-footnote)))))
                           (when difference
                             (return (list :footnote label difference)))))))))))
(defun inline-parser-test-context (text)
  (let* ((buffer (cl-markdown-kit:make-source-buffer text))
         (normalized (cl-markdown-kit:source-buffer-normalized-text buffer))
         (boundary-map (cl-markdown-kit::source-buffer-boundary-map buffer))
         (reference-index (make-hash-table :test #'equal))
         (limit 4096)
         (backtick-next (cl-markdown-kit::%ip-build-backtick-index normalized))
         (html-closes (cl-markdown-kit::%ip-build-html-closes normalized)))
    (multiple-value-bind (bracket-opens bracket-closes bracket-prefix bracket-depths)
        (cl-markdown-kit::%ip-build-bracket-index
         normalized backtick-next html-closes limit)
      (cl-markdown-kit::%ip-make-context
       normalized boundary-map reference-index limit backtick-next
       bracket-opens bracket-closes bracket-prefix bracket-depths
       (cl-markdown-kit::%ip-build-bare-links normalized) html-closes))))
