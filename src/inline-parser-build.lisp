(in-package #:cl-markdown-kit)

(defun %ip-span (context start end)
  (make-source-span (%ip-context-boundary-map context) start end))

(defun %ip-text-node (context value start end)
  (make-text :span (%ip-span context start end) :value value))

(defun %ip-delimiter-text-atoms (context delimiters pairs)
  (let* ((text (%ip-context-text context))
         (used (make-array (length text) :element-type 'bit :initial-element 0))
         (atoms nil))
    (dolist (pair pairs)
      (loop for position from (%ip-pair-start pair)
              below (%ip-pair-inner-start pair)
            do (setf (sbit used position) 1))
      (loop for position from (%ip-pair-inner-end pair)
              below (%ip-pair-end pair)
            do (setf (sbit used position) 1)))
    (dolist (delimiter delimiters)
      (let ((cursor (%ip-delimiter-start delimiter))
            (end (+ (%ip-delimiter-start delimiter)
                     (%ip-delimiter-length delimiter))))
        (loop while (< cursor end)
              do (if (= 1 (sbit used cursor))
                     (incf cursor)
                     (let ((literal-start cursor))
                       (loop while (and (< cursor end)
                                        (zerop (sbit used cursor)))
                             do (incf cursor))
                       (push (%ip-make-atom
                              literal-start cursor
                              (%ip-text-node context
                                             (subseq text literal-start cursor)
                                             literal-start cursor))
                             atoms)))))
    atoms)))

(defun %ip-pair-node (context pair children)
  (let ((span (%ip-span context (%ip-pair-start pair) (%ip-pair-end pair))))
    (case (%ip-pair-kind pair)
      (:link
       (make-link :span span
                  :destination (%ip-pair-destination pair)
                  :title (%ip-pair-title pair)
                  :reference-kind (%ip-pair-reference-kind pair)
                  :reference-label (%ip-pair-reference-label pair)
                  :children children))
      (:image
       (make-image :span span
                   :destination (%ip-pair-destination pair)
                   :title (%ip-pair-title pair)
                   :reference-kind (%ip-pair-reference-kind pair)
                   :reference-label (%ip-pair-reference-label pair)
                   :children children))
      (:emphasis
       (make-emphasis :span span
                      :delimiter-character (%ip-pair-delimiter-character pair)
                      :children children))
      (:strong
       (make-strong :span span
                    :delimiter-character (%ip-pair-delimiter-character pair)
                    :children children))
      (:strikethrough
       (make-strikethrough :span span :delimiter-character #\~
                           :children children)))))

(defun %ip-build-append-entry (owner entry children child-tails root root-tail)
    (let ((cell (list entry)))
      (if owner
          (let ((tail (gethash owner child-tails)))
            (if tail
                (setf (cdr tail) cell)
                (setf (gethash owner children) cell))
            (setf (gethash owner child-tails) cell))
          (progn
            (if root-tail
                (setf (cdr root-tail) cell)
                (setf root cell))
            (setf root-tail cell)))
      (values root root-tail)))

(defun %ip-build-attach-event
    (event position pair-stack children child-tails root root-tail context base-depth)
  (loop while (and pair-stack
                   (>= position (%ip-pair-inner-end (first pair-stack))))
        do (pop pair-stack))
  (multiple-value-setq (root root-tail)
                       (%ip-build-append-entry
                        (first pair-stack) event children child-tails root root-tail))
  (when (eq (car event) :pair)
    (%pc-check-depth
     (+ base-depth (length pair-stack) 1)
     (%ip-context-limit context)
     :inline)
    (push (cdr event) pair-stack))
  (values pair-stack root root-tail))

(defun %ip-build-queue-events (events start end atoms pairs)
  (dolist (atom atoms)
    (when (and (<= start (%ip-atom-start atom))
               (< (%ip-atom-start atom) end)
               (<= (%ip-atom-end atom) end))
      (push (cons :atom atom)
            (aref events (- (%ip-atom-start atom) start)))))
  (dolist (pair pairs)
    (when (and (<= start (%ip-pair-start pair))
               (< (%ip-pair-start pair) end)
               (<= (%ip-pair-end pair) end))
      (push (cons :pair pair)
            (aref events (- (%ip-pair-start pair) start))))))

(defun %ip-build-render (context children entries start end)
  (labels ((render (entries range-start range-end)
                   (let ((output nil)
                         (cursor range-start)
                         (pending-text nil)
                         (pending-text-start nil)
                         (pending-text-end nil))
                     (labels ((flush-text ()
                                          (when pending-text
                                            (push (%ip-text-node
                                                   context
                                                   (get-output-stream-string pending-text)
                                                   pending-text-start pending-text-end)
                                                  output)
                                            (setf pending-text nil
                                                  pending-text-start nil
                                                  pending-text-end nil)))
                              (add-text (value text-start text-end)
                                        (unless pending-text
                                          (setf pending-text (make-string-output-stream)
                                                pending-text-start text-start))
                                        (write-string value pending-text)
                                        (setf pending-text-end text-end))
                              (emit-atom (atom)
                                         (let ((node (%ip-atom-node atom)))
                                           (if (text-p node)
                                               (add-text (text-value node)
                                                         (%ip-atom-start atom)
                                                         (%ip-atom-end atom))
                                             (progn (flush-text) (push node output)))))
                              (emit-gap (gap-end)
                                        (when (< cursor gap-end)
                                          (add-text (subseq (%ip-context-text context)
                                                            cursor gap-end)
                                                    cursor gap-end))))
                       (dolist (entry entries)
                         (if (eq (car entry) :pair)
                             (let ((pair (cdr entry)))
                               (emit-gap (%ip-pair-start pair))
                               (flush-text)
                               (push (%ip-pair-node
                                      context pair
                                      (render (gethash pair children)
                                              (%ip-pair-inner-start pair)
                                              (%ip-pair-inner-end pair)))
                                     output)
                               (setf cursor (%ip-pair-end pair)))
                           (let ((atom (cdr entry)))
                             (emit-gap (%ip-atom-start atom))
                             (emit-atom atom)
                             (setf cursor (%ip-atom-end atom)))))
                       (emit-gap range-end)
                       (flush-text)
                       (nreverse output)))))
    (render entries start end)))

(defun %ip-build-range (context start end atoms pairs base-depth)
  (let* ((events (make-array (- end start) :initial-element nil))
         (children (make-hash-table :test #'eq))
         (child-tails (make-hash-table :test #'eq))
         (root nil)
         (root-tail nil)
         (pair-stack nil))
    (%ip-build-queue-events events start end atoms pairs)
    (loop for position from start below end
          for queued = (nreverse (aref events (- position start)))
          do (dolist (event queued)
               (multiple-value-setq (pair-stack root root-tail)
                                    (%ip-build-attach-event
                                     event position pair-stack children child-tails
                                     root root-tail context base-depth))))
    (%ip-build-render context children root start end)))

(defun %ip-register-atom (atoms node start end)
  (push (%ip-make-atom start end node) atoms)
  atoms)
