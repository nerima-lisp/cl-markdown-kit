(in-package #:cl-markdown-kit)

(defstruct (%ip-range-state
               (:constructor %ip-make-range-state
                   (context start end link-allowed base-depth)))
    context
    (start 0 :type (integer 0 *))
    (end 0 :type (integer 0 *))
    (link-allowed nil :type boolean)
    (base-depth 0 :type (integer 0 *))
    (index 0 :type (integer 0 *))
    (atoms nil)
    (delimiter-runs nil)
    (pairs nil)
    (bracket-stack nil)
    (bracket-count 0 :type (integer 0 *))
    (link-opener-barrier -1 :type integer)
    pending-text
    pending-text-start
    pending-text-end)

(defun %ip-range-flush-text (state)
  (when (%ip-range-state-pending-text state)
    (let* ((context (%ip-range-state-context state))
           (text-start (%ip-range-state-pending-text-start state))
           (text-end (%ip-range-state-pending-text-end state))
           (value (get-output-stream-string
                   (%ip-range-state-pending-text state))))
      (setf (%ip-range-state-atoms state)
            (%ip-register-atom
             (%ip-range-state-atoms state)
             (%ip-text-node context value text-start text-end)
             text-start text-end)
            (%ip-range-state-pending-text state) nil
            (%ip-range-state-pending-text-start state) nil
            (%ip-range-state-pending-text-end state) nil))))

(defun %ip-range-add-node (state node start end)
  (%ip-range-flush-text state)
  (setf (%ip-range-state-atoms state)
        (%ip-register-atom (%ip-range-state-atoms state) node start end)))

(defun %ip-range-add-text (state value start end)
  (when (and (%ip-range-state-pending-text state)
             (/= (%ip-range-state-pending-text-end state) start))
    (%ip-range-flush-text state))
  (unless (%ip-range-state-pending-text state)
    (setf (%ip-range-state-pending-text state) (make-string-output-stream)
          (%ip-range-state-pending-text-start state) start))
  (write-string value (%ip-range-state-pending-text state))
  (setf (%ip-range-state-pending-text-end state) end))

(defun %ip-range-current-end (state)
  (let* ((index (%ip-range-state-index state))
         (end (%ip-range-state-end state))
         (bracket (first (%ip-range-state-bracket-stack state))))
    (if (and bracket (< index (%ip-bracket-close bracket)))
        (min end (%ip-bracket-close bracket))
      end)))

(defun %ip-range-parent-end (state)
  (let ((parent (second (%ip-range-state-bracket-stack state)))
        (end (%ip-range-state-end state)))
    (if parent (min end (%ip-bracket-close parent)) end)))

(defun %ip-range-push-bracket (state open close image-p)
  (%pc-check-depth
   (+ (%ip-range-state-base-depth state)
      (%ip-range-state-bracket-count state) 1)
   (%ip-context-limit (%ip-range-state-context state))
   :inline)
  (push (%ip-make-bracket open close image-p)
        (%ip-range-state-bracket-stack state))
  (incf (%ip-range-state-bracket-count state)))

(defun %ip-range-pop-bracket (state)
  (pop (%ip-range-state-bracket-stack state))
  (decf (%ip-range-state-bracket-count state)))

(defun %ip-range-add-bracket-opener-text (state bracket)
  (let* ((open (%ip-bracket-open bracket))
         (image-p (%ip-bracket-image-p bracket))
         (text-start (if image-p (1- open) open))
         (text-value (if image-p "![" "[")))
    (%ip-range-add-text state text-value text-start (1+ open))))

(defun %ip-range-reject-bracket (state bracket)
  (let ((index (%ip-range-state-index state)))
    (%ip-range-add-bracket-opener-text state bracket)
    (%ip-range-pop-bracket state)
    (%ip-range-add-text state "]" index (1+ index))
    (incf (%ip-range-state-index state))))

(defun %ip-range-accept-link (state bracket destination title kind label link-end)
  (let* ((open (%ip-bracket-open bracket))
         (image-p (%ip-bracket-image-p bracket)))
    (if (and link-end
             (or image-p
                 (> open (%ip-range-state-link-opener-barrier state))))
        (progn
          (push (%ip-make-pair
                 (if image-p :image :link)
                 nil
                 (if image-p (1- open) open)
                 (1+ open)
                 (%ip-range-state-index state)
                 link-end
                 destination title (or kind :inline) label)
                (%ip-range-state-pairs state))
          (unless image-p
            (setf (%ip-range-state-link-opener-barrier state) open))
          (%ip-range-pop-bracket state)
          (setf (%ip-range-state-index state) link-end))
      (%ip-range-reject-bracket state bracket))))

(defun %ip-range-close-bracket (state)
  (let* ((context (%ip-range-state-context state))
         (index (%ip-range-state-index state))
         (bracket (first (%ip-range-state-bracket-stack state)))
         (open (and bracket
                    (aref (%ip-context-bracket-closes context) index))))
    (when (and bracket open (>= open 0)
               (= open (%ip-bracket-open bracket)))
      (if (or (%ip-bracket-image-p bracket)
              (%ip-range-state-link-allowed state))
          (%ip-link-candidate
           context open index (%ip-bracket-image-p bracket)
           (%ip-range-parent-end state)
           (%ip-range-state-link-allowed state)
           (lambda (destination title kind label link-end)
             (%ip-range-accept-link
              state bracket destination title kind label link-end))
           (lambda ()
             (%ip-range-reject-bracket state bracket)))
        (%ip-range-reject-bracket state bracket))
      t)))

(defun %ip-range-emit-delimiter (state run-end character)
  (let* ((text (%ip-context-text (%ip-range-state-context state)))
         (index (%ip-range-state-index state)))
    (multiple-value-bind (left right before-punctuation after-punctuation)
        (%ip-flanking text index (length text) run-end)
      (let* ((underscore-p (char= character #\_))
             (can-open (if underscore-p
                           (and left (or (not right) before-punctuation))
                         left))
             (can-close (if underscore-p
                            (and right (or (not left) after-punctuation))
                          right))
             (run-length (- run-end index)))
        (if (or (and (char= character +ip-tilde-delimiter-character+)
                     (not (%ip-context-gfm-p
                           (%ip-range-state-context state))))
                (and (char= character +ip-tilde-delimiter-character+)
                     (> run-length 2)))
            (%ip-range-add-text state (subseq text index run-end) index run-end)
          (if (or can-open can-close)
              (progn
                (%ip-range-flush-text state)
                (push (%ip-make-delimiter
                       character index run-length can-open can-close)
                      (%ip-range-state-delimiter-runs state)))
            (%ip-range-add-text state (subseq text index run-end)
                                index run-end)))))
    (setf (%ip-range-state-index state) run-end)))

(defun %ip-range-scan-bare-link (state candidate)
  (let* ((index (%ip-range-state-index state))
         (end (%pc-candidate-end candidate))
         (context (%ip-range-state-context state)))
    (if (%ip-range-state-link-allowed state)
        (progn
          (%ip-range-add-node
           state
           (%ip-make-autolink
            context (%pc-candidate-destination candidate)
            (%pc-candidate-display-text candidate)
            (%pc-candidate-kind candidate) index end)
           index end)
          (setf (%ip-range-state-link-opener-barrier state) index))
      (%ip-range-add-text
       state (subseq (%ip-context-text context) index end) index end))
    (setf (%ip-range-state-index state) end)))

(defun %ip-range-scan-backslash (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state)))
    (cond
     ((and (< (1+ index) scan-end)
           (char= (char text (1+ index)) #\Newline))
      (let ((break-end (%ip-skip-space text (+ index 2) scan-end)))
        (%ip-range-add-node
         state
         (make-hard-break :span (%ip-span context index break-end))
         index break-end)
        (setf (%ip-range-state-index state) break-end)))
     ((%ip-backslash-escape-end text index scan-end)
      (%ip-range-add-text state (string (char text (1+ index)))
                          index (+ index 2))
      (incf (%ip-range-state-index state) 2))
     (t
      (%ip-range-add-text state "\\" index (1+ index))
      (incf (%ip-range-state-index state)))))
  )
(defun %ip-range-scan-backtick (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state))
         (backtick-next (%ip-context-backtick-next context))
         (run-end (%pc-backtick-run-end text index scan-end backtick-next))
         (close (%ip-backtick-close backtick-next index)))
    (if (and (>= close 0) (< close scan-end)
             (<= (+ close (- run-end index)) scan-end))
        (let* ((close-end (+ close (- run-end index)))
               (literal (subseq text run-end close)))
          (setf literal (substitute #\Space #\Newline literal))
          (when (and (> (length literal) 0)
                     (char= (char literal 0) #\Space)
                     (char= (char literal (1- (length literal))) #\Space)
                     (find-if-not (lambda (char) (char= char #\Space))
                                  literal))
            (setf literal (subseq literal 1 (1- (length literal)))))
          (%ip-range-add-node
           state
           (make-code-span
            :span (%ip-span context index close-end)
            :literal-text literal
            :backtick-run-length (- run-end index))
           index close-end)
          (setf (%ip-range-state-index state) close-end))
      (progn
        (%ip-range-add-text state (subseq text index run-end)
                            index run-end)
        (setf (%ip-range-state-index state) run-end))))
  )
(defun %ip-range-scan-entity (state scan-end)
  (let* ((text (%ip-context-text (%ip-range-state-context state)))
         (index (%ip-range-state-index state)))
    (multiple-value-bind (value next) (%pc-entity-at text index scan-end)
      (if value
          (progn
            (%ip-range-add-text state value index next)
            (setf (%ip-range-state-index state) next))
        (progn
          (%ip-range-add-text state "&" index (1+ index))
          (incf (%ip-range-state-index state))))))
  )
(defun %ip-range-scan-angle (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state)))
    (multiple-value-bind (destination display kind autolink-end)
        (%ip-angle-autolink
         text index scan-end (%ip-context-html-closes context))
      (let ((html-end (and (not autolink-end)
                           (%ip-raw-html-end
                            text index scan-end
                            (%ip-context-html-closes context)))))
        (cond
         ((and autolink-end (%ip-range-state-link-allowed state))
          (%ip-range-add-node
           state (%ip-make-autolink context destination display kind
                                    index autolink-end)
           index autolink-end)
          (setf (%ip-range-state-link-opener-barrier state) index
                (%ip-range-state-index state) autolink-end))
         ((and autolink-end
               (not (%ip-range-state-link-allowed state)))
          (%ip-range-add-text state (subseq text index autolink-end)
                              index autolink-end)
          (setf (%ip-range-state-index state) autolink-end))
         (html-end
          (%ip-range-add-node
           state
           (make-html-inline
            :span (%ip-span context index html-end)
            :body (subseq text index html-end))
           index html-end)
          (setf (%ip-range-state-index state) html-end))
         (t
          (%ip-range-add-text state "<" index (1+ index))
          (incf (%ip-range-state-index state)))))))
  )
(defun %ip-range-scan-newline (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state))
         (break-end (%ip-skip-space text (1+ index) scan-end)))
    (%ip-range-add-node
     state (make-soft-break :span (%ip-span context index break-end))
     index break-end)
    (setf (%ip-range-state-index state) break-end))
  )
(defun %ip-range-scan-space (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state))
         (space-end (%ip-skip-space text index scan-end)))
    (if (and (< space-end scan-end)
             (char= (char text space-end) #\Newline))
        (let* ((line-end (1+ space-end))
               (break-end (%ip-skip-space text line-end scan-end))
               (spaces (loop for position from index below space-end
                             count (char= (char text position) #\Space))))
          (%ip-range-add-node
           state
           (if (>= spaces 2)
               (make-hard-break :span (%ip-span context index break-end))
             (make-soft-break :span (%ip-span context index break-end)))
           index break-end)
          (setf (%ip-range-state-index state) break-end))
      (progn
        (%ip-range-add-text state (subseq text index space-end)
                            index space-end)
        (setf (%ip-range-state-index state) space-end))))
  )
(defun %ip-range-scan-footnote-reference (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state)))
    (when (and (<= (+ index 2) scan-end)
               (char= (char text (1+ index)) #\^)
               (%ip-context-gfm-p context))
      (let ((close (aref (%ip-context-bracket-opens context) index)))
        (when (and (>= close 0) (< close scan-end))
          (let ((normalized
                 (%ip-footnote-reference
                  context (%pc-unescape text (+ index 2) close))))
            (when normalized
              (%ip-range-add-node
               state
               (make-footnote-reference
                :span (%ip-span context index (1+ close))
                :raw-label (subseq text (+ index 2) close)
                :normalized-label normalized)
               index (1+ close))
              (setf (%ip-range-state-index state) (1+ close))
              t))))))
  )
(defun %ip-range-scan-image-open (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state)))
    (if (and (< (1+ index) scan-end)
             (char= (char text (1+ index)) #\[))
        (let* ((open (1+ index))
               (close (aref (%ip-context-bracket-opens context) open))
               (footnote-p
                (and (%ip-context-gfm-p context)
                     (>= close 0) (< close scan-end)
                     (< (+ open 2) close)
                     (char= (char text (+ open 1)) #\^)
                     (%ip-footnote-reference
                      context (%pc-unescape text (+ open 2) close)))))
          (cond
           (footnote-p
            (%ip-range-add-text state "!" index (1+ index))
            (incf (%ip-range-state-index state)))
           ((and (>= close 0) (< close scan-end))
            (%ip-range-push-bracket state open close t)
            (incf (%ip-range-state-index state) 2))
           (t
            (%ip-range-add-text state "![" index (+ index 2))
            (incf (%ip-range-state-index state) 2))))
      (progn
        (%ip-range-add-text state "!" index (1+ index))
        (incf (%ip-range-state-index state)))))
  )
(defun %ip-range-scan-bracket-open (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (index (%ip-range-state-index state))
         (close (aref (%ip-context-bracket-opens context) index)))
    (if (and (>= close 0) (< close scan-end))
        (%ip-range-push-bracket state index close nil)
      (%ip-range-add-text state "[" index (1+ index)))
    (incf (%ip-range-state-index state))))
(defun %ip-range-scan-bracket-close (state)
  (unless (%ip-range-close-bracket state)
    (let* ((index (%ip-range-state-index state)))
      (%ip-range-add-text state "]" index (1+ index))
      (incf (%ip-range-state-index state))))
  )
(defun %ip-range-scan-delimiter (state scan-end character)
  (let* ((text (%ip-context-text (%ip-range-state-context state)))
         (index (%ip-range-state-index state))
         (run-end index))
    (loop while (and (< run-end scan-end)
                     (char= (char text run-end) character))
          do (incf run-end))
    (%ip-range-emit-delimiter state run-end character))
  )
(defun %ip-range-scan-text (state scan-end)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state))
         (run-end index))
    (loop while (and (< run-end scan-end)
                     (not (find (char text run-end)
                                +inline-text-special-characters+))
                     (not (gethash run-end (%ip-context-bare-links context))))
          do (incf run-end))
    (when (= run-end index) (incf run-end))
    (%ip-range-add-text state (subseq text index run-end) index run-end)
    (setf (%ip-range-state-index state) run-end))
  )
(defun %ip-range-scan-token (state)
  (let* ((context (%ip-range-state-context state))
         (text (%ip-context-text context))
         (index (%ip-range-state-index state))
         (character (char text index))
         (scan-end (%ip-range-current-end state))
         (candidate (gethash index (%ip-context-bare-links context))))
    (cond
     ((and candidate (<= (%pc-candidate-end candidate) scan-end))
      (%ip-range-scan-bare-link state candidate))
     ((char= character #\\)
      (%ip-range-scan-backslash state scan-end))
     ((char= character #\`)
      (%ip-range-scan-backtick state scan-end))
     ((char= character #\&)
      (%ip-range-scan-entity state scan-end))
     ((char= character #\<)
      (%ip-range-scan-angle state scan-end))
     ((char= character #\Newline)
      (%ip-range-scan-newline state scan-end))
     ((%pc-space-p character)
      (%ip-range-scan-space state scan-end))
     ((and (char= character #\[)
           (%ip-range-scan-footnote-reference state scan-end))
      nil)
     ((char= character #\!)
      (%ip-range-scan-image-open state scan-end))
     ((char= character #\[)
      (%ip-range-scan-bracket-open state scan-end))
     ((char= character #\])
      (%ip-range-scan-bracket-close state))
     ((find character +inline-delimiter-characters+)
      (%ip-range-scan-delimiter state scan-end character))
     (t
      (%ip-range-scan-text state scan-end))))
  )
(defun %ip-range-scan (state)
  (loop while (< (%ip-range-state-index state) (%ip-range-state-end state))
        do (%ip-range-scan-token state)))

(defun %ip-range-finalize (state)
  (let* ((context (%ip-range-state-context state))
         (start (%ip-range-state-start state))
         (end (%ip-range-state-end state))
         (base-depth (%ip-range-state-base-depth state))
         (atoms (%ip-range-state-atoms state))
         (pairs (%ip-range-state-pairs state)))
    (%ip-range-flush-text state)
    (setf atoms (%ip-range-state-atoms state))
    (if (and (null (%ip-range-state-delimiter-runs state))
             (null pairs))
        (%ip-build-range context start end (nreverse atoms) nil base-depth)
      (multiple-value-bind (delimiters resolved-pairs)
          (%ip-resolve-delimiter-runs
           (%ip-range-state-delimiter-runs state)
           pairs (%ip-context-bracket-depths context)
           (%ip-context-bracket-closes context) start end
           (%ip-context-limit context))
        (let* ((literal-atoms
                (%ip-delimiter-text-atoms context delimiters resolved-pairs))
               (all-atoms (nconc (nreverse atoms) literal-atoms)))
          (%ip-build-range context start end all-atoms resolved-pairs
                           base-depth)))))
  )
(defun %ip-parse-range (context start end link-allowed base-depth)
  (let ((state (%ip-make-range-state
                context start end link-allowed base-depth)))
    (setf (%ip-range-state-index state) start)
    (%ip-range-scan state)
    (%ip-range-finalize state)))
