(in-package #:cl-markdown-kit)

(defun %mw-write-string (string stream)
  (write-string string stream)
  (loop for character across string
        do (cond
             ((char= character #\Newline)
              (setf *mw-line-start-p* t
                    *mw-line-indent* 0))
             ((and *mw-line-start-p* (char= character #\Space))
              (incf *mw-line-indent*))
             ((and *mw-line-start-p* (char= character #\Tab))
              (incf *mw-line-indent* (- 4 (mod *mw-line-indent* 4))))
             (t
              (setf *mw-line-start-p* nil
                    *mw-line-indent* 0))))
  string)

(defun %mw-write-character (character stream)
  (%mw-write-string (string character) stream))

(defun %mw-line-end (text start)
  (or (position #\Newline text :start start) (length text)))

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
         (or (= cursor end) (%pc-space-p (char text cursor))))))

(defun %mw-list-marker-p (text start end)
  (when (< start end)
    (let ((character (char text start)))
      (or
       (and (find character '(#\- #\+ #\*))
            (or (= (1+ start) end) (%pc-space-p (char text (1+ start)))))
       (and (<= (char-code #\0) (char-code character) (char-code #\9))
            (let ((cursor start))
              (loop while (and (< cursor end)
                               (<= (char-code #\0)
                                   (char-code (char text cursor))
                                   (char-code #\9)))
                    do (incf cursor))
              (and (<= (- cursor start) +max-ordered-list-marker-digits+)
                   (< cursor end)
                   (find (char text cursor) '(#\. #\)))
                   (or (= (1+ cursor) end)
                       (%pc-space-p (char text (1+ cursor)))))))))))

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
                 ((%pc-space-p (char text cursor)) (incf cursor))
                 (t (return))))
      (and (>= count minimum) (= cursor end)))))

(defun %mw-unparsed-autolink-prefix-length (text index)
  (cond
    ((and (%pc-bare-url-start-p text index)
          (%pc-ascii-prefix-p text index "https://")) (length "https://"))
    ((and (%pc-bare-url-start-p text index)
          (%pc-ascii-prefix-p text index "http://")) (length "http://"))
    ((and (%pc-bare-url-start-p text index)
          (%pc-ascii-prefix-p text index "www.")) (length "www."))
    ((and (%pc-bare-email-start-p text index)
          (%pc-ascii-prefix-p text index "mailto:")) (length "mailto:"))
    ((and (%pc-bare-url-start-p text index)
          (%pc-ascii-prefix-p text index "xmpp:")) (length "xmpp:"))))

(defun %mw-setext-marker-line-p (text start end)
  (let ((cursor start)
        (marker (and (< start end) (char text start))))
    (when (find marker '(#\= #\-))
      (loop while (< cursor end)
            do (cond
                 ((char= (char text cursor) marker) (incf cursor))
                 ((%pc-space-p (char text cursor)) (incf cursor))
                 (t (return))))
      (= cursor end))))

(defun %mw-line-prefix-hazard-p (text start end)
  (or (%mw-heading-marker-p text start end)
      (%mw-list-marker-p text start end)
      (and (< start end) (char= (char text start) #\>))
      (%mw-marker-line-p text start end '(#\* #\_ #\-) 3)
      (%mw-setext-marker-line-p text start end)))

(defun %mw-entity-at-p (text index)
  (nth-value 0 (%pc-entity-at text index (length text))))

(defun %mw-fence-marker-line-p (text)
  (let ((marker (and (plusp (length text)) (char text 0))))
    (when (find marker '(#\` #\~))
      (let ((end
             (or
              (position-if-not
               (lambda (character)
                 (char= character marker))
               text)
              (length text))))
        (>= end 3)))))

(defun %mw-inline-markup-free-p (text)
  (let* ((content-end (position-if-not #'%pc-space-p text :from-end t))
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
  (and (or (zerop index) (%pc-space-p (char text (1- index))))
       (loop for cursor from index below (length text)
             always (or (char= (char text cursor) #\#)
                        (%pc-space-p (char text cursor))))))

(defun %mw-inline-tail-has-nonspace-p (nodes)
  (some
   (lambda (node)
     (typecase node
       (text (position-if-not #'%pc-space-p (text-value node)))
       (html-inline (position-if-not #'%pc-space-p (html-inline-body node)))
       (otherwise t)))
   nodes))

(defun %mw-inline-text-run-node-p (node)
  (or (text-p node)
      (and (html-inline-p node) (zerop (length (html-inline-body node))))))

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
               ((char= character #\[) (push index open-indices))
               ((and (char= character #\]) open-indices) (pop open-indices))))
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
           (not
            (string= (autolink-display-text node) (autolink-destination node))))))

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
        (when
            (and (%mw-inline-link-like-node-p previous)
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
          do (let ((candidate (%pc-bare-candidate-at text index end)))
               (if candidate
                   (let* ((candidate-end (%pc-candidate-end candidate))
                          (email-scheme-p
                           (and
                            (or (%pc-ascii-prefix-p text index "mailto:")
                                (%pc-ascii-prefix-p text index "xmpp:"))
                            (position #\@ text :start index :end candidate-end)))
                          (breakpoint
                           (cond
                             (email-scheme-p email-scheme-p)
                             ((%pc-ascii-prefix-p text index "www.")
                              (+ index 3))
                             ((or (%pc-ascii-prefix-p text index "http://")
                                  (%pc-ascii-prefix-p text index "https://")
                                  (%pc-ascii-prefix-p text index "mailto:")
                                  (%pc-ascii-prefix-p text index "xmpp:"))
                              (position #\:
                                        text
                                        :start
                                        index
                                        :end
                                        candidate-end))
                             (t
                              (position #\@
                                        text
                                        :start
                                        index
                                        :end
                                        candidate-end)))))
                     (when breakpoint
                       (setf (gethash breakpoint breakpoints) t))
                     (unless email-scheme-p
                       (let ((at
                              (position #\@
                                        text
                                        :start
                                        index
                                        :end
                                        candidate-end)))
                         (when at
                           (setf (gethash at breakpoints) t))))
                     (setf index candidate-end))
                   (let* ((prefix-length
                           (%mw-unparsed-autolink-prefix-length text index))
                          (scheme-colon-index
                           (cond
                             ((and prefix-length
                                   (%pc-ascii-prefix-p text index "mailto:"))
                              (+ index (1- (length "mailto:"))))
                             ((and prefix-length
                                   (%pc-ascii-prefix-p text index "xmpp:"))
                              (+ index (1- (length "xmpp:")))))))
                     (cond
                       (scheme-colon-index
                        (setf (gethash scheme-colon-index breakpoints) t
                              index (+ index prefix-length)))
                       (prefix-length
                        (setf (gethash index breakpoints) :mask
                              index (+ index prefix-length)))
                       (t (incf index)))))))
    breakpoints))

(defun %mw-write-character-reference (character stream)
  (%mw-write-string (format nil "&#~D;" (char-code character)) stream))

(defun %mw-escape-text-character-p (text index
                                         boundary-escapes
                                         escape-terminal-backslash-p)
  (let ((character (char text index)))
    (or (and boundary-escapes (gethash index boundary-escapes))
        (and (char= character #\\)
             (or (and escape-terminal-backslash-p (= index (1- (length text))))
                 (and (< (1+ index) (length text))
                      (let ((next (char text (1+ index))))
                        (or (char= next #\Newline) (ascii-punctuation-p next))))))
        (and (find character +mw-escape-punctuation-characters+)
             (not *mw-minimize-inline-escapes-p*))
        (and (find character +mw-escape-bracket-characters+)
             (not
              (and *mw-minimize-inline-escapes-p*
                   (%mw-unmatched-bracket-p text index))))
        (and *mw-heading-p*
             *mw-heading-closing-hash-allowed-p*
             (char= character #\#)
             (%mw-heading-closing-marker-p text index))
        (and *mw-escape-trailing-bang-p*
             (char= character #\!)
             (= index (1- (length text)))
             (not *mw-heading-p*))
        (and *mw-table-cell-p* (char= character #\|))
        (and (char= character #\&) (%mw-entity-at-p text index))
        (and *mw-text-autolink-breakpoints*
             (gethash index *mw-text-autolink-breakpoints*))
        (and *mw-line-start-p*
             (not *mw-heading-p*)
             (<= *mw-line-indent* 3)
             (%mw-line-prefix-hazard-p text index (%mw-line-end text index))))))

(defun %mw-write-text (text stream
                            &optional
                            boundary-escapes
                            escape-terminal-backslash-p
                            (encode-terminal-whitespace-p t))
  (let* ((end (length text))
         (index 0)
         (*mw-text-autolink-breakpoints* (%mw-autolink-breakpoints text)))
    (loop while (< index end)
          for character = (char text index)
          do (cond
               ((char= character #\Newline)
                 (if (or *mw-heading-p*
                         *mw-table-cell-p*
                         (and (> index 0)
                              (char= (char text (1- index)) #\Newline))
                         (and (< (1+ index) end)
                              (char= (char text (1+ index)) #\Newline)))
                     (%mw-write-string "&#10;" stream)
                     (%mw-write-character character stream))
                 (incf index))
               ((and encode-terminal-whitespace-p
                     (%pc-space-p character)
                     (let ((line-end (%mw-line-end text index)))
                       (null
                        (position-if-not #'%pc-space-p
                                         text
                                         :start
                                         index
                                         :end
                                         line-end))))
                (let ((line-end (%mw-line-end text index)))
                  (loop while (< index line-end)
                        for current = (char text index)
                        do (%mw-write-string
                            (if (char= current #\Tab)
                                "&#9;"
                                "&#32;")
                            stream) (incf index))))
               ((and *mw-line-start-p* (%pc-space-p character))
                 (%mw-write-character-reference character stream)
                 (incf index)
                 (loop while (and (< index end) (%pc-space-p (char text index)))
                       for current = (char text index)
                       do (if (char= current #\Tab)
                              (%mw-write-character-reference current stream)
                              (%mw-write-character current stream)) (incf index)))
               ((%mw-escape-text-character-p text
                                             index
                                             boundary-escapes
                                             escape-terminal-backslash-p)
                 (cond
                   ((and *mw-line-start-p*
                         (digit-char-p character 10)
                         (<= *mw-line-indent* 3)
                         (%mw-line-prefix-hazard-p text
                                                   index
                                                   (%mw-line-end text index)))
                    (%mw-write-character-reference character stream))
                   ((and (char= character #\\)
                         (< (1+ index) end)
                         (ascii-punctuation-p (char text (1+ index))))
                    (%mw-write-string "\\\\" stream))
                   ((and *mw-text-autolink-breakpoints*
                         (eq (gethash index *mw-text-autolink-breakpoints*)
                             :mask))
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
