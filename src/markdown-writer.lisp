;;;; src/markdown-writer.lisp
(in-package #:cl-markdown-kit)

(declaim (special *mw-line-start-p*
                  *mw-line-indent*
                  *mw-table-cell-p*
                  *mw-escape-table-pipes-p*
                  *mw-text-autolink-breakpoints*
                  *mw-heading-p*
                  *mw-autolink-label-p*
                  *mw-escape-trailing-bang-p*
                  *mw-list-boundary-token*
                  *mw-list-boundary-count*
                  *mw-following-block-p*
                  *mw-minimize-inline-escapes-p*
                  *mw-heading-closing-hash-allowed-p*))

(defun %mw-write-string (string stream)
  (write-string string stream)
  (loop for character across string
        do (cond
             ((char= character #\Newline)
              (setf *mw-line-start-p* t
                    *mw-line-indent* 0))
             ((and *mw-line-start-p*
                   (char= character #\Space))
              (incf *mw-line-indent*))
             ((and *mw-line-start-p*
                   (char= character #\Tab))
              (incf *mw-line-indent*
                    (- 4 (mod *mw-line-indent* 4))))
             (t
              (setf *mw-line-start-p* nil
                    *mw-line-indent* 0))))
  string)

(defun %mw-write-character (character stream)
  (%mw-write-string (string character) stream))

(defun %mw-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun %mw-line-end (text start)
  (or (position #\Newline text :start start)
      (length text)))

(defun %mw-longest-run (text character)
  (loop with longest = 0
        with index = 0
        while (< index (length text))
        do (if (char= (char text index) character)
               (let ((run-end
                       (loop for cursor from index below (length text)
                             while (char= (char text cursor) character)
                             finally (return cursor))))
                 (setf longest (max longest (- run-end index))
                       index run-end))
               (incf index))
        finally (return longest)))

(defun %mw-heading-marker-p (text start end)
  (let ((cursor start))
    (loop while (and (< cursor end) (char= (char text cursor) #\#))
          do (incf cursor))
    (and (> cursor start)
         (<= (- cursor start) 6)
         (or (= cursor end)
             (%mw-horizontal-space-p (char text cursor))))))

(defun %mw-list-marker-p (text start end)
  (when (< start end)
    (let ((character (char text start)))
      (or (and (find character '(#\- #\+ #\*))
               (or (= (1+ start) end)
                   (%mw-horizontal-space-p (char text (1+ start)))))
          (and (<= (char-code #\0) (char-code character)
                   (char-code #\9))
               (let ((cursor start))
                 (loop while (and (< cursor end)
                                  (<= (char-code #\0)
                                      (char-code (char text cursor))
                                      (char-code #\9)))
                       do (incf cursor))
                 (and (<= (- cursor start) 9)
                      (< cursor end)
                      (find (char text cursor) '(#\. #\)))
                      (or (= (1+ cursor) end)
                          (%mw-horizontal-space-p (char text (1+ cursor)))))))))))

(defun %mw-marker-line-p (text start end markers minimum)
  (let ((marker (and (< start end) (char text start)))
        (cursor start)
        (count 0))
    (when (and marker (find marker markers))
      (loop while (< cursor end)
            do (cond
                 ((char= (char text cursor) marker)
                  (incf count)
                  (incf cursor))
                 ((%mw-horizontal-space-p (char text cursor))
                  (incf cursor))
                 (t (return))))
      (and (>= count minimum) (= cursor end)))))

(defun %mw-unparsed-autolink-prefix-length (text index)
  (cond
    ((and (%ip-bare-url-start-p text index)
          (%ip-ascii-prefix-p text index "https://"))
     (length "https://"))
    ((and (%ip-bare-url-start-p text index)
          (%ip-ascii-prefix-p text index "http://"))
     (length "http://"))
    ((and (%ip-bare-url-start-p text index)
          (%ip-ascii-prefix-p text index "www."))
     (length "www."))
    ((and (%ip-bare-email-start-p text index)
          (%ip-ascii-prefix-p text index "mailto:"))
     (length "mailto:"))
    ((and (%ip-bare-url-start-p text index)
          (%ip-ascii-prefix-p text index "xmpp:"))
     (length "xmpp:"))))

(defun %mw-setext-marker-line-p (text start end)
  (let ((cursor start)
        (marker (and (< start end) (char text start))))
    (when (find marker '(#\= #\-))
      (loop while (< cursor end)
            do (cond
                 ((char= (char text cursor) marker)
                  (incf cursor))
                 ((%mw-horizontal-space-p (char text cursor))
                  (incf cursor))
                 (t (return))))
      (= cursor end))))

(defun %mw-line-prefix-hazard-p (text start end)
  (or (%mw-heading-marker-p text start end)
      (%mw-list-marker-p text start end)
      (and (< start end)
           (char= (char text start) #\>))
      (%mw-marker-line-p text start end '(#\* #\_ #\-) 3)
      (%mw-setext-marker-line-p text start end)))

(defun %mw-entity-at-p (text index)
  (nth-value 0 (%ip-entity-at text index (length text))))

(defun %mw-fence-marker-line-p (text)
  (let ((marker (and (plusp (length text)) (char text 0))))
    (when (find marker '(#\` #\~))
      (let ((end (or (position-if-not (lambda (character)
                                        (char= character marker))
                                      text)
                     (length text))))
        (>= end 3)))))

(defun %mw-inline-markup-free-p (text)
  (let* ((content-end (position-if-not #'%mw-horizontal-space-p
                                       text :from-end t))
         ;; Trailing spaces are emitted as character references below, so they
         ;; should not disable minimal escaping for the preceding text.
         (content (subseq text 0 (if content-end (1+ content-end) 0)))
         (source-buffer (make-source-buffer content))
         (normalized-text (source-buffer-normalized-text source-buffer))
         (children
           (parse-inline normalized-text
                         (source-buffer-boundary-map source-buffer)
                         (make-hash-table :test #'equal))))
    (and (string= content normalized-text)
         (every #'text-p children)
         (string= normalized-text
                  (with-output-to-string (stream)
                    (dolist (child children)
                      (write-string (text-value child) stream)))))))

(defun %mw-unmatched-bracket-p (text index)
  (case (char text index)
    (#\[ (null (position #\] text :start (1+ index))))
    (#\] (null (position #\[ text :end index)))))

(defun %mw-heading-closing-marker-p (text index)
  (and (or (zerop index)
           (%mw-horizontal-space-p (char text (1- index))))
       (loop for cursor from index below (length text)
             always (or (char= (char text cursor) #\#)
                        (%mw-horizontal-space-p (char text cursor))))))

(defun %mw-inline-tail-has-nonspace-p (nodes)
  (some (lambda (node)
          (typecase node
            (text
             (position-if-not #'%mw-horizontal-space-p (text-value node)))
            (html-inline
             (position-if-not #'%mw-horizontal-space-p
                              (html-inline-body node)))
            (otherwise t)))
        nodes))

(defun %mw-inline-text-run-node-p (node)
  (or (text-p node)
      (and (html-inline-p node)
           (zerop (length (html-inline-body node))))))

(defun %mw-inline-delimiter-character (node)
  (typecase node
    (emphasis (or (emphasis-delimiter-character node) #\*))
    (strong (or (strong-delimiter-character node) #\*))
    (strikethrough (or (strikethrough-delimiter-character node) #\~))
    (code-span #\`)))

(defun %mw-unmatched-open-bracket-indices (text)
  (let ((open-indices nil))
    (loop for character across text
          for index from 0
          do (cond
               ((char= character #\[)
                (push index open-indices))
               ((and (char= character #\]) open-indices)
                (pop open-indices))))
    open-indices))

(defun %mw-inline-link-like-node-p (node)
  (or (link-p node)
      (image-p node)
      (autolink-p node)
      (footnote-reference-p node)))

(defun %mw-inline-node-starts-link-label-p (node)
  (or (link-p node)
      (footnote-reference-p node)
      (and (autolink-p node)
           (not (string= (autolink-display-text node)
                         (autolink-destination node))))))

(defun %mw-inline-boundary-escape-indices (text previous next)
  (let ((escape-indices (make-hash-table :test #'eql))
        (end (length text)))
    (labels ((force-escape (index)
               (setf (gethash index escape-indices) t)))
      (when (plusp end)
        (let ((delimiter (%mw-inline-delimiter-character previous)))
          (when (and delimiter (char= (char text 0) delimiter))
            (force-escape 0)))
        (let ((delimiter (%mw-inline-delimiter-character next)))
          (when (and delimiter (char= (char text (1- end)) delimiter))
            (force-escape (1- end))))
        (when (or (code-span-p previous) (code-span-p next))
          (loop for character across text
                for index from 0
                when (char= character #\`)
                  do (force-escape index)))
        (when (and (%mw-inline-link-like-node-p previous)
                   (char= (char text 0) #\[))
          (force-escape 0))
        (when (%mw-inline-link-like-node-p next)
          (dolist (index (%mw-unmatched-open-bracket-indices text))
            (force-escape index))))
      escape-indices)))

(defun %mw-heading-closing-hash-allowed-after-p (remaining)
  (and *mw-heading-p*
       *mw-heading-closing-hash-allowed-p*
       (not (%mw-inline-tail-has-nonspace-p remaining))))

(defun %mw-autolink-breakpoints (text)
  (let ((breakpoints (make-hash-table :test #'eql))
        (index 0)
        (end (length text)))
    (loop while (< index end)
          do (let ((candidate (%ip-bare-candidate-at text index end)))
               (if candidate
                   (let* ((candidate-end (%ip-candidate-end candidate))
                          (email-scheme-p
                            (and (or (%ip-ascii-prefix-p text index "mailto:")
                                     (%ip-ascii-prefix-p text index "xmpp:"))
                                 (position #\@ text :start index :end candidate-end)))
                          (breakpoint
                            (cond
                              (email-scheme-p email-scheme-p)
                              ((%ip-ascii-prefix-p text index "www.")
                               (+ index 3))
                              ((or (%ip-ascii-prefix-p text index "http://")
                                   (%ip-ascii-prefix-p text index "https://")
                                   (%ip-ascii-prefix-p text index "mailto:")
                                   (%ip-ascii-prefix-p text index "xmpp:"))
                               (position #\: text :start index :end candidate-end))
                              (t
                               (position #\@ text :start index
                                         :end candidate-end)))))
                     (when breakpoint
                       (setf (gethash breakpoint breakpoints) t))
                     (unless email-scheme-p
                       (let ((at (position #\@ text :start index
                                           :end candidate-end)))
                         (when at
                           (setf (gethash at breakpoints) t))))
                     (setf index candidate-end))
                   (let* ((prefix-length
                            (%mw-unparsed-autolink-prefix-length text index))
                          (scheme-colon-index
                            (cond
                              ((and prefix-length
                                    (%ip-ascii-prefix-p text index "mailto:"))
                               (+ index (1- (length "mailto:"))))
                              ((and prefix-length
                                    (%ip-ascii-prefix-p text index "xmpp:"))
                               (+ index (1- (length "xmpp:")))))))
                     (cond
                       (scheme-colon-index
                        (setf (gethash scheme-colon-index breakpoints) t
                              index (+ index prefix-length)))
                       (prefix-length
                        (setf (gethash index breakpoints) :mask
                              index (+ index prefix-length)))
                       (t
                        (incf index)))))))
    breakpoints))

(defun %mw-write-character-reference (character stream)
  (%mw-write-string (format nil "&#~D;" (char-code character)) stream))

(defun %mw-escape-text-character-p
    (text index boundary-escapes escape-terminal-backslash-p)
  (let ((character (char text index)))
    (or (and boundary-escapes (gethash index boundary-escapes))
        (and (char= character #\\)
             (or (and escape-terminal-backslash-p
                      (= index (1- (length text))))
                 (and (< (1+ index) (length text))
                      (let ((next (char text (1+ index))))
                        (or (char= next #\Newline)
                            (ascii-punctuation-p next))))))
        (and (find character '(#\` #\* #\_ #\< #\~))
             (not *mw-minimize-inline-escapes-p*))
        (and (find character '(#\[ #\]))
             (not (and *mw-minimize-inline-escapes-p*
                       (%mw-unmatched-bracket-p text index))))
        (and *mw-heading-p*
             *mw-heading-closing-hash-allowed-p*
             (char= character #\#)
             (%mw-heading-closing-marker-p text index))
        (and *mw-escape-trailing-bang-p*
             (char= character #\!)
             (= index (1- (length text)))
             (not *mw-heading-p*))
        (and (or *mw-table-cell-p* *mw-escape-table-pipes-p*)
             (char= character #\|))
        (and (char= character #\&)
             (%mw-entity-at-p text index))
        (and *mw-text-autolink-breakpoints*
             (gethash index *mw-text-autolink-breakpoints*))
        (and *mw-line-start-p*
             (not *mw-heading-p*)
             (<= *mw-line-indent* 3)
             (%mw-line-prefix-hazard-p
              text index (%mw-line-end text index))))))

(defun %mw-write-text (text stream &optional boundary-escapes
                                           escape-terminal-backslash-p
                                           (encode-terminal-whitespace-p t))
  (let* ((end (length text))
         (index 0)
         (*mw-text-autolink-breakpoints* (%mw-autolink-breakpoints text)))
    (loop while (< index end)
          for character = (char text index)
          do (cond
               ((char= character #\Newline)
                (if (or *mw-heading-p* *mw-table-cell-p*
                        (and (> index 0)
                             (char= (char text (1- index)) #\Newline))
                        (and (< (1+ index) end)
                             (char= (char text (1+ index)) #\Newline)))
                    (%mw-write-string "&#10;" stream)
                    (%mw-write-character character stream))
                (incf index))
               ((and encode-terminal-whitespace-p
                     (%mw-horizontal-space-p character)
                     (let ((line-end (%mw-line-end text index)))
                       (null (position-if-not #'%mw-horizontal-space-p text
                                              :start index :end line-end))))
                (let ((line-end (%mw-line-end text index)))
                  (loop while (< index line-end)
                        for current = (char text index)
                        do (%mw-write-string
                            (if (char= current #\Tab) "&#9;" "&#32;")
                            stream)
                           (incf index))))
               ((and *mw-line-start-p*
                     (%mw-horizontal-space-p character))
                (%mw-write-character-reference character stream)
                (incf index)
                (loop while (and (< index end)
                                 (%mw-horizontal-space-p (char text index)))
                      for current = (char text index)
                      do (if (char= current #\Tab)
                             (%mw-write-character-reference current stream)
                             (%mw-write-character current stream))
                         (incf index)))
               ((%mw-escape-text-character-p text index boundary-escapes
                                             escape-terminal-backslash-p)
                (cond
                  ((and *mw-line-start-p*
                        (digit-char-p character 10)
                        (<= *mw-line-indent* 3)
                        (%mw-line-prefix-hazard-p
                         text index (%mw-line-end text index)))
                   (%mw-write-character-reference character stream))
                  ((and (char= character #\\)
                         (< (1+ index) end)
                         (ascii-punctuation-p (char text (1+ index))))
                   (%mw-write-string "\\\\" stream))
                  ((and *mw-text-autolink-breakpoints*
                        (eq (gethash index *mw-text-autolink-breakpoints*) :mask))
                   (%mw-write-character-reference character stream))
                  ((and *mw-autolink-label-p*
                        *mw-text-autolink-breakpoints*
                        (gethash index *mw-text-autolink-breakpoints*))
                   (%mw-write-character-reference character stream))
                  (t
                   (%mw-write-character #\\ stream)
                   (%mw-write-character character stream)))
                (incf index))
               (t
                (%mw-write-character character stream)
                (incf index)))))
  text)

(progn
  (defstruct (%mw-render-frame
              (:constructor %mw-make-render-frame
                  (&key (line-start-p t) (line-indent 0)
                        table-cell-p escape-table-p heading-p autolink-label-p
                        escape-trailing-bang-p following-block-p
                        minimize-inline-escapes-p
                        heading-closing-hash-allowed-p
                        escape-terminal-backslash-p prefix)))
    (line-start-p t)
    (line-indent 0)
    table-cell-p
    escape-table-p
    heading-p
    autolink-label-p
    escape-trailing-bang-p
    following-block-p
    minimize-inline-escapes-p
    heading-closing-hash-allowed-p
    escape-terminal-backslash-p
    (prefix "" :type string))

  (defstruct (%mw-render-session
              (:constructor %mw-make-render-session
                  (&key (frames (make-hash-table :test #'eq))
                        (results (make-hash-table :test #'eq)))))
    frames
    results
    parent
    grandparent
    previous-sibling
    next-sibling
    child-index
    last-child-p)

  (defun %mw-inline-children-parent-p (node)
    (or (paragraph-p node)
        (heading-p node)
        (table-cell-p node)
        (emphasis-p node)
        (strong-p node)
        (link-p node)
        (image-p node)
        (strikethrough-p node)))

  (defun %mw-inline-wrapper-p (node)
    (or (emphasis-p node)
        (strong-p node)
        (link-p node)
        (image-p node)
        (strikethrough-p node)))

  (defun %mw-copy-render-frame (frame)
    (%mw-make-render-frame
     :line-start-p (%mw-render-frame-line-start-p frame)
     :line-indent (%mw-render-frame-line-indent frame)
     :table-cell-p (%mw-render-frame-table-cell-p frame)
     :escape-table-p (%mw-render-frame-escape-table-p frame)
     :heading-p (%mw-render-frame-heading-p frame)
     :autolink-label-p (%mw-render-frame-autolink-label-p frame)
     :escape-trailing-bang-p
     (%mw-render-frame-escape-trailing-bang-p frame)
     :following-block-p (%mw-render-frame-following-block-p frame)
     :minimize-inline-escapes-p
     (%mw-render-frame-minimize-inline-escapes-p frame)
     :heading-closing-hash-allowed-p
     (%mw-render-frame-heading-closing-hash-allowed-p frame)
     :escape-terminal-backslash-p
     (%mw-render-frame-escape-terminal-backslash-p frame)))

  (defun %mw-make-node-render-frame (session node)
  (let* ((parent (%mw-render-session-parent session))
         (parent-frame
           (and parent
                (gethash parent (%mw-render-session-frames session))))
         (next (%mw-render-session-next-sibling session))
         (following
           (or (and parent-frame
                    (%mw-render-frame-following-block-p parent-frame))
               next)))
    (cond
      ((and parent-frame (%mw-inline-children-parent-p parent))
       (let ((frame (%mw-copy-render-frame parent-frame)))
         (when (or (%mw-inline-wrapper-p parent)
                   (not (%mw-inline-text-run-node-p node)))
           (setf (%mw-render-frame-heading-closing-hash-allowed-p frame)
                 nil))
         (when (%mw-inline-wrapper-p parent)
           (setf (%mw-render-frame-minimize-inline-escapes-p frame) nil
                 (%mw-render-frame-escape-terminal-backslash-p frame) t))
         frame))
      ((paragraph-p node)
       (%mw-make-render-frame
        :following-block-p following
        :minimize-inline-escapes-p t))
      ((heading-p node)
       (%mw-make-render-frame
        :following-block-p following
        :minimize-inline-escapes-p t
        :heading-p t
        :heading-closing-hash-allowed-p t))
      ((table-cell-p node)
       (%mw-make-render-frame
        :following-block-p following
        :table-cell-p t
        :minimize-inline-escapes-p t))
      (t
       (%mw-make-render-frame :following-block-p following)))))

  (defun %mw-capture-render-frame (frame function)
    (let ((*mw-line-start-p* (%mw-render-frame-line-start-p frame))
          (*mw-line-indent* (%mw-render-frame-line-indent frame))
          (*mw-table-cell-p* (%mw-render-frame-table-cell-p frame))
          (*mw-escape-table-pipes-p*
            (%mw-render-frame-escape-table-p frame))
          (*mw-heading-p* (%mw-render-frame-heading-p frame))
          (*mw-autolink-label-p*
            (%mw-render-frame-autolink-label-p frame))
          (*mw-escape-trailing-bang-p*
            (%mw-render-frame-escape-trailing-bang-p frame))
          (*mw-following-block-p*
            (%mw-render-frame-following-block-p frame))
          (*mw-minimize-inline-escapes-p*
            (%mw-render-frame-minimize-inline-escapes-p frame))
          (*mw-heading-closing-hash-allowed-p*
            (%mw-render-frame-heading-closing-hash-allowed-p frame)))
      (let ((result (with-output-to-string (stream)
                      (funcall function stream))))
        (setf (%mw-render-frame-line-start-p frame) *mw-line-start-p*
              (%mw-render-frame-line-indent frame) *mw-line-indent*)
        result)))

  (defun %mw-advance-render-frame (frame text)
    (%mw-capture-render-frame
     frame
     (lambda (stream)
       (%mw-write-string text stream))))

  (defun %mw-render-child-results (children results)
    (with-output-to-string (stream)
      (dolist (child children)
        (write-string (or (gethash child results) "") stream))))

  (defun %mw-render-inline-children
      (children stream &optional minimize-escapes-p
                           escape-terminal-backslash-p results)
    (declare (ignore minimize-escapes-p escape-terminal-backslash-p))
    (when results
      (write-string (%mw-render-child-results children results) stream))))


(defun %mw-escape-label (label)
  (with-output-to-string (stream)
    (loop for character across label
          do (cond
               ((find character '(#\\ #\[ #\]))
                (write-char #\\ stream)
                (write-char character stream))
               ((char= character #\&)
                (write-string "&amp;" stream))
               ((char= character #\Newline)
                (write-char #\Space stream))
               (t
                (write-char character stream))))))

(defun %mw-write-link-string (text stream &key title-p)
  (loop with end = (length text)
        for index from 0 below end
        for character = (char text index)
        do (cond
             ((char= character #\\)
              (%mw-write-string "\\\\" stream))
             ((and title-p (char= character #\"))
              (%mw-write-string "\\\"" stream))
             ((and (not title-p) (find character '(#\< #\>)))
              (%mw-write-character #\\ stream)
              (%mw-write-character character stream))
             ((and (or *mw-table-cell-p* *mw-escape-table-pipes-p*)
                   (char= character #\|))
              (%mw-write-string "\\|" stream))
             ((and (char= character #\&)
                   (%mw-entity-at-p text index))
              (%mw-write-string "\\&" stream))
             ((and (not title-p) (char= character #\Newline))
              (%mw-write-string "&#10;" stream))
             (t
              (%mw-write-character character stream)))))

(defun %mw-write-link-destination (destination stream)
  (%mw-write-character #\< stream)
  (%mw-write-link-string destination stream)
  (%mw-write-character #\> stream))

(defun %mw-write-link-title (title stream)
  (when title
    (%mw-write-string " \"" stream)
    (%mw-write-link-string title stream :title-p t)
    (%mw-write-character #\" stream)))

(progn
  (defun %mw-render-inline-text-run (context node frame)
  (if (%mw-inline-text-run-node-p
       (%mw-render-session-previous-sibling context))
      ""
      (let* ((parent (%mw-render-session-parent context))
             (children (and parent (node-children parent)))
             (tail (and children (member node children :test #'eq)))
             (remaining tail)
             (text-nodes nil))
        (loop while (and remaining
                         (%mw-inline-text-run-node-p (first remaining)))
              do (push (pop remaining) text-nodes))
        (setf text-nodes (nreverse text-nodes))
        (let* ((text
                 (with-output-to-string (output)
                   (dolist (text-node text-nodes)
                     (when (text-p text-node)
                       (write-string (text-value text-node) output)))))
               (base-minimize-p
                 (%mw-render-frame-minimize-inline-escapes-p frame))
               (effective-minimize-p
                 (and base-minimize-p
                      (not (%mw-fence-marker-line-p text))
                      (%mw-inline-markup-free-p text)))
               (boundary-escapes
                 (and base-minimize-p
                      (%mw-inline-boundary-escape-indices
                       text
                       (%mw-render-session-previous-sibling context)
                       (first remaining))))
               (escape-trailing-bang-p
                 (and (%mw-inline-node-starts-link-label-p
                       (first remaining))
                      (plusp (length text))
                      (char= (char text (1- (length text))) #\!)))
               (heading-hash-allowed-p
                 (let ((*mw-heading-p*
                         (%mw-render-frame-heading-p frame))
                       (*mw-heading-closing-hash-allowed-p*
                         (%mw-render-frame-heading-closing-hash-allowed-p
                          frame)))
                   (%mw-heading-closing-hash-allowed-after-p remaining)))
               (encode-terminal-whitespace-p
                 (or (null remaining)
                     (soft-break-p (first remaining))
                     (hard-break-p (first remaining)))))
          (%mw-capture-render-frame
           frame
           (lambda (stream)
             (let ((*mw-minimize-inline-escapes-p*
                     effective-minimize-p)
                   (*mw-escape-trailing-bang-p*
                     escape-trailing-bang-p)
                   (*mw-heading-closing-hash-allowed-p*
                     heading-hash-allowed-p))
               (%mw-write-text
                text stream boundary-escapes
                (or remaining
                    (%mw-render-frame-following-block-p frame)
                    (%mw-render-frame-escape-terminal-backslash-p frame))
                encode-terminal-whitespace-p))))))))

  (defun %mw-inline-wrapper-prefix (node kind)
    (ecase kind
      (:emphasis
       (string (or (emphasis-delimiter-character node) #\*)))
      (:strong
       (make-string 2
                   :initial-element
                   (or (strong-delimiter-character node) #\*)))
      (:strikethrough "~~")
      (:link "[")
      (:image "![")))

  (defun %mw-inline-wrapper-suffix (node frame kind)
    (ecase kind
      (:emphasis
       (%mw-inline-wrapper-prefix node kind))
      (:strong
       (%mw-inline-wrapper-prefix node kind))
      (:strikethrough "~~")
      ((:link :image)
       (%mw-capture-render-frame
        frame
        (lambda (stream)
          (%mw-write-string "](" stream)
          (%mw-write-link-destination
           (if (link-p node)
               (link-destination node)
               (image-destination node))
           stream)
          (%mw-write-link-title
           (if (link-p node)
               (link-title node)
               (image-title node))
           stream)
          (%mw-write-character #\) stream))))))

  (defun %mw-render-inline-result (context node frame kind)
    (let ((children (node-children node))
          (results (%mw-render-session-results context)))
      (ecase kind
        (:text
         (%mw-render-inline-text-run context node frame))
        (:code-span
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (let* ((literal
                     (substitute #\Space #\Newline
                                 (code-span-literal-text node)))
                   (backtick (code-char 96))
                   (fence-length (1+ (%mw-longest-run literal backtick)))
                   (fence (make-string (max 1 fence-length)
                                       :initial-element backtick))
                   (non-space-p
                     (find-if-not
                      (lambda (character) (char= character #\Space))
                      literal))
                   (pad-p
                     (and (plusp (length literal))
                          non-space-p
                          (or (and (char= (char literal 0) #\Space)
                                   (char= (char literal (1- (length literal)))
                                          #\Space))
                              (char= (char literal 0) backtick)
                              (char= (char literal (1- (length literal)))
                                     backtick)))))
              (%mw-write-string fence stream)
              (when pad-p (%mw-write-character #\Space stream))
              (%mw-write-string literal stream)
              (when pad-p (%mw-write-character #\Space stream))
              (%mw-write-string fence stream))))) (:html-inline
         (if (zerop (length (html-inline-body node)))
             (%mw-render-inline-text-run context node frame)
             (%mw-capture-render-frame
              frame
              (lambda (stream)
                (%mw-write-string (html-inline-body node) stream))))) (:soft-break
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (if (%mw-render-frame-heading-p frame)
                (%mw-write-string "&#10;" stream)
                (%mw-write-character #\Newline stream))))) (:hard-break
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (%mw-write-string (format nil "\\~%") stream)))) ((:emphasis :strong :strikethrough :link :image)
         (concatenate
          'string
          (%mw-render-frame-prefix frame)
          (%mw-render-child-results children results)
          (%mw-inline-wrapper-suffix node frame kind))) (:autolink
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (let ((display-text (autolink-display-text node))
                  (destination (autolink-destination node)))
              (if (string= display-text destination)
                  (progn
                    (%mw-write-character #\< stream)
                    (%mw-write-string display-text stream)
                    (%mw-write-character #\> stream))
                  (progn
                    (%mw-write-character #\[ stream)
                    (let ((*mw-autolink-label-p* t)
                          (*mw-minimize-inline-escapes-p* nil))
                      (%mw-write-text display-text stream nil t))
                    (%mw-write-string "](" stream)
                    (%mw-write-link-destination destination stream)
                    (%mw-write-character #\) stream))))))) (:footnote-reference
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (%mw-write-string "[^" stream)
            (%mw-write-string
             (%mw-escape-label
              (footnote-reference-normalized-label node))
             stream)
            (%mw-write-character #\] stream))))))) (defun %mw-render-inline-dispatch-event (context node phase kind)
    (let ((frame
            (gethash node (%mw-render-session-frames context))))
      (case phase
        (:enter
         (when (%mw-inline-wrapper-p node)
           (let ((prefix (%mw-inline-wrapper-prefix node kind)))
             (setf (%mw-render-frame-prefix frame) prefix)
             (%mw-advance-render-frame frame prefix)))
         :descend)
        (:leave
         (setf (gethash node (%mw-render-session-results context))
               (%mw-render-inline-result context node frame kind))
         nil)
        (otherwise
         (error "Unsupported Markdown renderer phase ~S." phase))))) (define-node-dispatch (%mw-render-inline-event (context node phase))
      :operation :markdown-renderer
      :family :inline
    (text
     (%mw-render-inline-dispatch-event context node phase :text))
    (code-span
     (%mw-render-inline-dispatch-event context node phase :code-span))
    (html-inline
     (%mw-render-inline-dispatch-event context node phase :html-inline))
    (soft-break
     (%mw-render-inline-dispatch-event context node phase :soft-break))
    (hard-break
     (%mw-render-inline-dispatch-event context node phase :hard-break))
    (emphasis
     (%mw-render-inline-dispatch-event context node phase :emphasis))
    (strong
     (%mw-render-inline-dispatch-event context node phase :strong))
    (link
     (%mw-render-inline-dispatch-event context node phase :link))
    (image
     (%mw-render-inline-dispatch-event context node phase :image))
    (strikethrough
     (%mw-render-inline-dispatch-event context node phase :strikethrough))
    (autolink
     (%mw-render-inline-dispatch-event context node phase :autolink))
    (footnote-reference
     (%mw-render-inline-dispatch-event context node phase :footnote-reference))))


(defun %mw-render-inline-string (children results
                                               &key table-cell-p
                                                 escape-table-pipes-p
                                                 minimize-escapes-p)
  (declare (ignore table-cell-p escape-table-pipes-p minimize-escapes-p))
  (%mw-render-child-results children results))
(defun %mw-table-cell-ranges (line)
  (%bp-table-cells (%make-block-line line #() 0 0 0 0)))

(defun %mw-table-alignment-line-p (line)
  (let ((cells (%mw-table-cell-ranges line)))
    (when (and cells
               (every (lambda (range)
                        (let* ((cell (%bp-trimmed-string
                                      (subseq line (car range) (cdr range))))
                               (length (length cell))
                               (start (if (and (plusp length)
                                               (char= (char cell 0) #\:))
                                          1 0))
                               (end (if (and (> length start)
                                             (char= (char cell (1- length)) #\:))
                                        (1- length)
                                        length)))
                          (and (< start end)
                               (loop for index from start below end
                                     always (char= (char cell index) #\-)))))
                      cells))
      (length cells))))

(defun %mw-table-alignment-breaker-position (text)
  (let ((lines nil)
        (start 0)
        (alignment-breaker-position nil))
    (loop for end = (position #\Newline text :start start)
          do (push (cons start (or end (length text))) lines)
          while end
          do (setf start (1+ end)))
    (setf lines (nreverse lines))
    (loop for tail on lines
          for line = (car tail)
          for next = (cadr tail)
          while next
          do (let* ((next-start (car next))
                    (next-end (cdr next))
                    (first-non-space
                      (or (position-if-not #'%mw-horizontal-space-p
                                           text :start next-start
                                           :end next-end)
                          next-end))
                    (header-has-pipe
                      (position #\| text :start (car line) :end (cdr line))))
               (when (and header-has-pipe
                          (= (length (%mw-table-cell-ranges
                                      (subseq text (car line) (cdr line))))
                             (or (%mw-table-alignment-line-p
                                  (subseq text next-start next-end))
                                 0))
                          (< first-non-space next-end))
                 (setf alignment-breaker-position
                       (or (position #\| text :start next-start :end next-end)
                           (position #\: text :start next-start :end next-end)
                           (position #\- text :start next-start :end next-end)))
                 (return))))
    alignment-breaker-position))

(defun %mw-table-start-follows-p (text)
  (not (null (%mw-table-alignment-breaker-position text))))

(defun %mw-render-paragraph (node results)
  (let* ((children (paragraph-children node))
         (text (%mw-render-inline-string children results))
         (alignment-breaker-position
           (%mw-table-alignment-breaker-position text)))
    (if alignment-breaker-position
        (concatenate 'string
                     (subseq text 0 alignment-breaker-position)
                     "\\"
                     (subseq text alignment-breaker-position))
        text)))

(defun %mw-render-blocks
    (nodes results &optional (separator (format nil "~%~%")))
  (with-output-to-string (stream)
    (loop with previous = nil
          for remaining-nodes on nodes
          for node = (car remaining-nodes)
          do (when previous
               (if (and *mw-list-boundary-token*
                        (list-node-p previous)
                        (list-node-p node)
                        (eq (list-node-kind previous)
                            (list-node-kind node)))
                   (progn
                     (write-string separator stream)
                     (write-string *mw-list-boundary-token* stream)
                     (write-string separator stream)
                     (incf *mw-list-boundary-count*))
                   (write-string separator stream)))
             (write-string (or (gethash node results) "") stream)
             (setf previous node))))

(defun %mw-prefix-lines (text prefix)
  (if (zerop (length text))
      text
      (with-output-to-string (stream)
        (write-string prefix stream)
        (loop for index from 0 below (length text)
              for character = (char text index)
              do (write-char character stream)
                 (when (and (char= character #\Newline)
                            (< (1+ index) (length text)))
                   (write-string prefix stream))))))

(defun %mw-prefix-continuation-lines (text prefix)
  (with-output-to-string (stream)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (write-char character stream)
             (when (and (char= character #\Newline)
                        (< (1+ index) (length text))
                        (not (char= (char text (1+ index)) #\Newline)))
               (write-string prefix stream)))))

(defun %mw-render-code-block (node)
  (let* ((body (code-block-body node))
         (info (or (code-block-info-string node) ""))
         (fence-character (if (find #\` info) #\~ #\`))
         (fence-length
           (max 3
                (1+ (%mw-longest-run body fence-character))))
         (fence (make-string fence-length :initial-element fence-character)))
    (with-output-to-string (stream)
      (write-string fence stream)
      (unless (zerop (length info))
        (write-char #\Space stream)
        (write-string info stream))
      (write-char #\Newline stream)
      (write-string body stream)
      (when (or (zerop (length body))
                (char= (char body (1- (length body))) #\Newline)
                *mw-following-block-p*)
        (unless (or (zerop (length body))
                    (char= (char body (1- (length body))) #\Newline))
          (write-char #\Newline stream))
        (write-string fence stream)))))

(defun %mw-list-marker (list-node index)
  (if (eq (list-node-kind list-node) :unordered)
      "- "
      (format nil "~D. "
              (min 999999999
                   (+ (list-node-start-number list-node) index)))))

(defun %mw-trailing-newline-count (text)
  (loop for index downfrom (1- (length text)) to 0
        while (char= (char text index) #\Newline)
        count t))

(defun %mw-render-list (node results)
  (let* ((items (list-node-items node))
         (tight-p (list-node-tight-p node))
         (separator-count (if tight-p 1 2))
         (child-separator (if tight-p (format nil "~%")
                              (format nil "~%~%"))))
    (with-output-to-string (stream)
      (let ((previous-content ""))
        (loop for remaining-items on items
              for item = (car remaining-items)
              for index from 0
              for marker = (%mw-list-marker node index)
              for continuation-prefix =
                (make-string (length marker) :initial-element #\Space)
              for children = (node-children item)
              for task-p = (task-list-item-p item)
              for task-marker = (and task-p
                                     (if (task-list-item-checked-p item)
                                         "[x] "
                                         "[ ] "))
              for child-markdown = (when children
                                     (%mw-render-blocks
                                      children results child-separator))
              for content =
                (cond
                  ((and task-p children
                        (paragraph-p (first children)))
                   (%mw-prefix-continuation-lines child-markdown
                                                  continuation-prefix))
                  (task-p
                   (if children
                       (concatenate
                        'string (format nil "~%")
                        (%mw-prefix-lines child-markdown continuation-prefix))
                       ""))
                  (children
                   (%mw-prefix-continuation-lines child-markdown
                                                  continuation-prefix))
                  (t ""))
              do (when (plusp index)
                   (loop repeat
                         (max 0 (- separator-count
                                   (%mw-trailing-newline-count
                                    previous-content)))
                         do (write-char #\Newline stream)))
                 (write-string marker stream)
                 (when task-marker
                   (if (and children (paragraph-p (first children)))
                       (write-string task-marker stream)
                       (write-string
                        (string-right-trim '(#\Space) task-marker) stream)))
                 (write-string content stream)
                 (setf previous-content content))))))

(defun %mw-render-table-row (row results)
  (with-output-to-string (stream)
    (write-char #\| stream)
    (dolist (cell (table-row-cells row))
      (write-char #\Space stream)
      (write-string
       (%mw-render-inline-string (table-cell-children cell) results
                                 :table-cell-p t
                                 :minimize-escapes-p t)
       stream)
      (write-string " |" stream))))

(defun %mw-alignment-marker (alignment)
  (case alignment
    (:left ":---")
    (:right "---:")
    (:center ":---:")
    (otherwise "---")))

(defun %mw-render-table (node results)
  (let ((rows (table-rows node))
        (alignments (table-alignments node)))
    (if (null rows)
        ""
        (with-output-to-string (stream)
          (write-string (%mw-render-table-row (first rows) results) stream)
          (write-char #\Newline stream)
          (write-char #\| stream)
          (loop for cell in (table-row-cells (first rows))
                for alignment in alignments
                do (write-char #\Space stream)
                   (write-string (%mw-alignment-marker alignment) stream)
                   (write-string " |" stream))
          (dolist (row (rest rows))
            (write-char #\Newline stream)
            (write-string (%mw-render-table-row row results) stream))))))

(defun %mw-normalize-newlines (text)
  (with-output-to-string (stream)
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (unless (and (< (1+ index) (length text))
                             (char= (char text (1+ index)) #\Newline))
                  (write-char #\Newline stream)))
               (t
                (write-char character stream))))))

(progn
  (defun %mw-render-block-result (context node frame kind)
    (let ((results (%mw-render-session-results context)))
      (ecase kind
        (:document
         (%mw-render-blocks (document-children node) results))
        (:front-matter
         (let ((raw-text (%mw-normalize-newlines
                          (front-matter-raw-text node))))
           (with-output-to-string (stream)
             (write-string "---" stream)
             (write-char #\Newline stream)
             (write-string raw-text stream)
             (unless (or (zerop (length raw-text))
                         (char= (char raw-text (1- (length raw-text)))
                                #\Newline))
               (write-char #\Newline stream))
             (write-string "---" stream))))
        (:block-quote
         (let ((children (block-quote-children node)))
           (if (null children)
               ">"
               (%mw-prefix-lines
                (%mw-render-blocks children results)
                "> "))))
        (:list-node
         (%mw-render-list node results))
        ((:list-item :task-list-item)
         "")
        (:paragraph
         (%mw-render-paragraph node results))
        (:heading
         (concatenate
          'string
          (%mw-render-frame-prefix frame)
          (%mw-render-child-results (heading-children node) results)))
        (:thematic-break
         "* * *")
        (:code-block
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (%mw-write-string (%mw-render-code-block node) stream))))
        (:html-block
         (html-block-body node))
        (:link-reference-definition
         (%mw-capture-render-frame
          frame
          (lambda (stream)
            (%mw-write-character #\[ stream)
            (%mw-write-string
             (%mw-escape-label
              (let ((normalized
                      (link-reference-definition-normalized-label node)))
                (if (plusp (length normalized))
                    normalized
                    (link-reference-definition-raw-label node))))
             stream)
            (%mw-write-string "]: " stream)
            (%mw-write-link-destination
             (link-reference-definition-destination node) stream)
            (%mw-write-link-title
             (link-reference-definition-title node) stream)))) (:table
         (%mw-render-table node results)) (:table-row
         (%mw-render-table-row node results)) (:table-cell
         (%mw-render-child-results (table-cell-children node) results)) (:footnote-definition
         (with-output-to-string (stream)
           (write-string "[^" stream)
           (write-string
            (%mw-escape-label
             (footnote-definition-normalized-label node))
            stream)
           (write-string "]:" stream)
           (unless (null (footnote-definition-children node))
             (write-char #\Newline stream)
             (write-string
              (%mw-prefix-lines
               (%mw-render-blocks
                (footnote-definition-children node) results)
               "    ")
              stream)))))))
  (defun %mw-render-block-dispatch-event (context node phase kind)
    (let ((frame (gethash node (%mw-render-session-frames context))))
      (case phase
        (:enter
         (when (heading-p node)
           (let ((prefix
                   (concatenate
                    'string
                    (make-string (heading-level node) :initial-element #\#)
                    (if (null (heading-children node)) "" " "))))
             (setf (%mw-render-frame-prefix frame) prefix)
             (%mw-advance-render-frame frame prefix)))
         :descend)
        (:leave
         (setf (gethash node (%mw-render-session-results context))
               (%mw-render-block-result context node frame kind))
         nil)
        (otherwise
         (error "Unsupported Markdown renderer phase ~S." phase))))) (define-node-dispatch (%mw-render-block-event (context node phase))
      :operation :markdown-renderer
      :family :block
    (document
     (%mw-render-block-dispatch-event context node phase :document))
    (front-matter
     (%mw-render-block-dispatch-event context node phase :front-matter))
    (block-quote
     (%mw-render-block-dispatch-event context node phase :block-quote))
    (list-node
     (%mw-render-block-dispatch-event context node phase :list-node))
    (task-list-item
     (%mw-render-block-dispatch-event context node phase :task-list-item))
    (list-item
     (%mw-render-block-dispatch-event context node phase :list-item))
    (paragraph
     (%mw-render-block-dispatch-event context node phase :paragraph))
    (heading
     (%mw-render-block-dispatch-event context node phase :heading))
    (thematic-break
     (%mw-render-block-dispatch-event context node phase :thematic-break))
    (code-block
     (%mw-render-block-dispatch-event context node phase :code-block))
    (html-block
     (%mw-render-block-dispatch-event context node phase :html-block))
    (link-reference-definition
     (%mw-render-block-dispatch-event
      context node phase :link-reference-definition))
    (table
     (%mw-render-block-dispatch-event context node phase :table))
    (table-row
     (%mw-render-block-dispatch-event context node phase :table-row))
    (table-cell
     (%mw-render-block-dispatch-event context node phase :table-cell))
    (footnote-definition
     (%mw-render-block-dispatch-event
      context node phase :footnote-definition))) (verify-node-dispatches
   :operation :markdown-renderer
   :families '(:block :inline)))

(defun %mw-substring-count (string substring)
  (loop with start = 0
        for position = (search substring string :start2 start)
        while position
        sum 1
        do (setf start (+ position (length substring)))))

(defun %mw-replace-substring (string substring replacement)
  (with-output-to-string (stream)
    (loop with start = 0
          for position = (search substring string :start2 start)
          while position
          do (write-string string stream :start start :end position)
             (write-string replacement stream)
             (setf start (+ position (length substring)))
          finally (write-string string stream :start start))))

(defun %mw-list-boundary-definition (markdown)
  (loop for index from 0
        for label = (format nil "cl-markdown-kit-writer-list-separator-~D"
                            index)
        unless (search label markdown :test #'char-equal)
          return (format nil "[~A]: <>" label)))

(progn
  (defun %mw-render-document-tree (document)
    (let ((context (%mw-make-render-session)))
      (walk-node-tree-cps
       document
       (lambda (node phase parent grandparent previous-sibling next-sibling
                     child-index last-child-p)
         (setf (%mw-render-session-parent context) parent
               (%mw-render-session-grandparent context) grandparent
               (%mw-render-session-previous-sibling context) previous-sibling
               (%mw-render-session-next-sibling context) next-sibling
               (%mw-render-session-child-index context) child-index
               (%mw-render-session-last-child-p context) last-child-p)
         (when (eq phase :enter)
           (setf (gethash node (%mw-render-session-frames context))
                 (%mw-make-node-render-frame context node)))
         (let ((event-result
                 (cond
                   ((inline-node-p node)
                    (%mw-render-inline-event context node phase))
                   ((block-node-p node)
                    (%mw-render-block-event context node phase))
                   (t
                    (error "Unsupported Markdown node: ~S" node)))))
           (when (and (eq phase :leave)
                      parent
                      (%mw-inline-children-parent-p parent))
             (let ((parent-frame
                     (gethash parent
                              (%mw-render-session-frames context))))
               (when parent-frame
                 (%mw-advance-render-frame
                  parent-frame
                  (or (gethash node
                               (%mw-render-session-results context))
                      "")))))
           event-result)))
      (gethash document (%mw-render-session-results context))))

  (defun %mw-render-document-markdown (document)
    (let ((*mw-line-start-p* t)
          (*mw-line-indent* 0)
          (*mw-table-cell-p* nil)
          (*mw-escape-table-pipes-p* nil)
          (*mw-text-autolink-breakpoints* nil)
          (*mw-heading-p* nil)
          (*mw-autolink-label-p* nil)
          (*mw-escape-trailing-bang-p* nil)
          (*mw-list-boundary-token* nil)
          (*mw-list-boundary-count* 0)
          (*mw-following-block-p* nil)
          (*mw-minimize-inline-escapes-p* nil)
          (*mw-heading-closing-hash-allowed-p* nil))
      (loop with token-length = 1
            for token = (concatenate
                         'string
                         (make-string token-length :initial-element #\Null)
                         "markdown-writer-list-boundary"
                         (make-string token-length :initial-element #\Null))
            do (let* ((boundary-count 0)
                      (markdown
                        (let ((*mw-list-boundary-token* token)
                              (*mw-list-boundary-count* 0))
                          (prog1 (%mw-render-document-tree document)
                            (setf boundary-count *mw-list-boundary-count*))))
                      (token-count (%mw-substring-count markdown token)))
                 (when (= boundary-count token-count)
                   (return
                     (%mw-replace-substring
                      markdown token
                      (%mw-list-boundary-definition markdown))))
                 (incf token-length))))))

(defun write-markdown-to-stream (document stream)
  (check-type document document)
  (unless (output-stream-p stream)
    (error 'type-error :datum stream
           :expected-type '(satisfies output-stream-p)))
  (write-string (%mw-render-document-markdown document) stream))

(defun write-markdown (document)
  (with-output-to-string (stream)
    (write-markdown-to-stream document stream)))
