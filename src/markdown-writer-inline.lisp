(in-package #:cl-markdown-kit)

(defun %mw-escape-label (label)
  (with-output-to-string (stream)
    (loop for character across label
          do (cond
               ((find character '(#\\ #\[ #\]))
                 (write-char #\\ stream)
                 (write-char character stream))
               ((char= character #\&) (write-string "&amp;" stream))
               ((char= character #\Newline) (write-char #\Space stream))
               (t (write-char character stream))))))

(defun %mw-write-link-string (text stream &key title-p)
  (loop with
        end = (length text)
        for index from 0 below
        end
        for character = (char text index)
        do (cond
             ((char= character #\\) (%mw-write-string "\\\\" stream))
             ((and title-p (char= character #\"))
              (%mw-write-string "\\\"" stream))
             ((and (not title-p) (find character '(#\< #\>)))
               (%mw-write-character #\\ stream)
               (%mw-write-character character stream))
             ((and *mw-table-cell-p* (char= character #\|))
              (%mw-write-string "\\|" stream))
             ((and (char= character #\&) (%mw-entity-at-p text index))
              (%mw-write-string "\\&" stream))
             ((and (not title-p) (char= character #\Newline))
              (%mw-write-string "&#10;" stream))
             (t (%mw-write-character character stream)))))

(defun %mw-write-link-destination (destination stream)
  (%mw-write-character #\< stream)
  (%mw-write-link-string destination stream)
  (%mw-write-character #\> stream))

(defun %mw-write-link-title (title stream)
  (when title
    (%mw-write-string " \"" stream)
    (%mw-write-link-string title stream :title-p t)
    (%mw-write-character #\" stream)))

(defun %mw-render-inline-text-run (context node frame)
  (if (%mw-inline-text-run-node-p (%mw-render-session-previous-sibling context))
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
                     (%mw-inline-boundary-escape-indices text
                                                         (%mw-render-session-previous-sibling
                                                          context)
                                                         (first remaining))))
               (escape-trailing-bang-p
                (and (%mw-inline-node-starts-link-label-p (first remaining))
                     (plusp (length text))
                     (char= (char text (1- (length text))) #\!)))
               (heading-hash-allowed-p
                (let ((*mw-heading-p* (%mw-render-frame-heading-p frame))
                      (*mw-heading-closing-hash-allowed-p*
                       (%mw-render-frame-heading-closing-hash-allowed-p frame)))
                  (%mw-heading-closing-hash-allowed-after-p remaining)))
               (encode-terminal-whitespace-p
                (or (null remaining)
                    (soft-break-p (first remaining))
                    (hard-break-p (first remaining)))))
          (%mw-capture-render-frame frame
                                    (lambda (stream)
                                      (let ((*mw-minimize-inline-escapes-p*
                                             effective-minimize-p)
                                            (*mw-escape-trailing-bang-p*
                                             escape-trailing-bang-p)
                                            (*mw-heading-closing-hash-allowed-p*
                                             heading-hash-allowed-p))
                                        (%mw-write-text text
                                                        stream
                                                        boundary-escapes
                                                        (or remaining
                                                            (%mw-render-frame-following-block-p
                                                             frame)
                                                            (%mw-render-frame-escape-terminal-backslash-p
                                                             frame))
                                                        encode-terminal-whitespace-p))))))))

(defun %mw-inline-wrapper-prefix (node kind)
  (ecase kind
    (:emphasis (string (or (emphasis-delimiter-character node) #\*)))
    (:strong
     (make-string 2 :initial-element (or (strong-delimiter-character node) #\*)))
    (:strikethrough "~~")
    (:link "[")
    (:image "![")))

(defun %mw-inline-wrapper-suffix (node frame kind)
  (ecase kind
    (:emphasis (%mw-inline-wrapper-prefix node kind))
    (:strong (%mw-inline-wrapper-prefix node kind))
    (:strikethrough "~~")
    ((:link :image)
     (%mw-capture-render-frame frame
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
      (:text (%mw-render-inline-text-run context node frame))
      (:code-span (%mw-render-inline-code-span-result frame node))
      (:html-inline
       (if (zerop (length (html-inline-body node)))
           (%mw-render-inline-text-run context node frame)
           (%mw-capture-render-frame frame
                                     (lambda (stream)
                                       (%mw-write-string (html-inline-body node)
                                                         stream)))))
      (:soft-break
       (%mw-capture-render-frame frame
                                 (lambda (stream)
                                   (if (%mw-render-frame-heading-p frame)
                                       (%mw-write-string "&#10;" stream)
                                       (%mw-write-character #\Newline stream)))))
      (:hard-break
       (%mw-capture-render-frame frame
                                 (lambda (stream)
                                   (%mw-write-string (format nil "\\~%") stream))))
      ((:emphasis :strong :strikethrough :link :image)
       (concatenate 'string
                    (%mw-render-frame-prefix frame)
                    (%mw-render-child-results children results)
                    (%mw-inline-wrapper-suffix node frame kind)))
      (:autolink (%mw-render-inline-autolink-result frame node))
      (:footnote-reference
       (%mw-capture-render-frame frame
                                 (lambda (stream)
                                   (%mw-write-string "[^" stream)
                                   (%mw-write-string
                                    (%mw-escape-label
                                     (footnote-reference-normalized-label node))
                                    stream)
                                   (%mw-write-character #\] stream)))))))

(defun %mw-render-inline-dispatch-event (context node phase kind)
  (let ((frame (gethash node (%mw-render-session-frames context))))
    (case phase
      (:enter
       (when (%mw-inline-wrapper-p node)
         (let ((prefix (%mw-inline-wrapper-prefix node kind)))
           (setf (%mw-render-frame-prefix frame) prefix)
           (%mw-advance-render-frame frame prefix)))
       :descend)
      (:leave
       (%mw-store-render-result
        context
        node
        (node-children node)
        (%mw-render-inline-result context node frame kind))
       nil))))

(define-node-dispatch (%mw-render-inline-event (context node phase))
                      :operation
                      :markdown-renderer
                      :family
                      :inline
                      (text
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :text))
                      (code-span
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :code-span))
                      (html-inline
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :html-inline))
                      (soft-break
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :soft-break))
                      (hard-break
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :hard-break))
                      (emphasis
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :emphasis))
                      (strong
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :strong))
                      (link
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :link))
                      (image
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :image))
                      (strikethrough
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :strikethrough))
                      (autolink
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :autolink))
                      (footnote-reference
                       (%mw-render-inline-dispatch-event context
                                                         node
                                                         phase
                                                         :footnote-reference)))

(defun %mw-render-inline-code-span-result (frame node)
  (%mw-capture-render-frame frame
                            (lambda (stream)
                              (let* ((literal
                                      (substitute #\Space
                                                  #\Newline
                                                  (code-span-literal-text node)))
                                     (backtick (code-char 96))
                                     (fence-length
                                      (1+ (%mw-longest-run literal backtick)))
                                     (fence
                                      (make-string (max 1 fence-length)
                                                   :initial-element
                                                   backtick))
                                     (non-space-p
                                      (find-if-not
                                       (lambda (character)
                                         (char= character #\Space))
                                       literal))
                                     (pad-p
                                      (and (plusp (length literal))
                                           non-space-p
                                           (or
                                            (and
                                             (char= (char literal 0) #\Space)
                                             (char=
                                              (char literal
                                                    (1- (length literal)))
                                              #\Space))
                                            (char= (char literal 0) backtick)
                                            (char=
                                             (char literal
                                                   (1- (length literal)))
                                             backtick)))))
                                (%mw-write-string fence stream)
                                (when pad-p
                                  (%mw-write-character #\Space stream))
                                (%mw-write-string literal stream)
                                (when pad-p
                                  (%mw-write-character #\Space stream))
                                (%mw-write-string fence stream)))))

(defun %mw-render-inline-autolink-result (frame node)
  (%mw-capture-render-frame frame
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
                                        (%mw-write-text display-text
                                                        stream
                                                        nil
                                                        t))
                                      (%mw-write-string "](" stream)
                                      (%mw-write-link-destination destination
                                                                  stream)
                                      (%mw-write-character #\) stream)))))))
