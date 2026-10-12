;;;; src/edit.lisp
(in-package #:cl-markdown-kit)

(defun %edit-document-source (document)
  (or (document-source document)
      (let ((span (node-span document)))
        (and span (cl-parser-kit:span-source span)))
      (error "Cannot edit a document without a source snapshot.")))

(defun %line-ending-at (source index &optional (limit (length source)))
  (when (< index limit)
    (case (char source index)
      (#\Return
       (if (and (< (1+ index) limit)
                (char= (char source (1+ index)) #\Newline))
           (values :crlf 2)
           (values :cr 1)))
      (#\Newline (values :lf 1))
      (otherwise (values nil 0)))))

(defun %line-ending-string (style)
  (case style
    (:cr (string #\Return))
    (:crlf (coerce (list #\Return #\Newline) 'string))
    (otherwise (string #\Newline))))

(defun %find-line-ending (source start end)
  (loop with index = start
        while (< index end)
        do (multiple-value-bind (style width)
               (%line-ending-at source index end)
             (if style
                 (return-from %find-line-ending style)
                 (incf index (max width 1)))))
  nil)

(defun %nearest-line-ending-before (source offset)
  (loop with index = 0
        with nearest = nil
        while (< index offset)
        do (multiple-value-bind (style width)
               (%line-ending-at source index offset)
             (if style
                 (setf nearest style
                       index (+ index width))
                 (incf index (max width 1))))
        finally (return nearest)))

(defun %nearest-line-ending-after (source offset)
  (%find-line-ending source offset (length source)))

(defun %edit-line-ending (source start end)
  (%line-ending-string
   (or (%find-line-ending source start end)
       (%nearest-line-ending-before source start)
       (%nearest-line-ending-after source end)
       :lf)))

(defun %normalize-newlines (text line-ending)
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          do (cond
               ((char= character #\Return)
                (write-string line-ending output)
                (incf index (if (and (< (1+ index) (length text))
                                    (char= (char text (1+ index)) #\Newline))
                               2 1)))
               ((char= character #\Newline)
                (write-string line-ending output)
                (incf index))
               (t
                (write-char character output)
                (incf index))))))

(defun %prefix-continuation-lines (text prefix line-ending)
  (with-output-to-string (output)
    (loop for character across text
          do (if (char= character #\Newline)
                 (progn
                   (write-string line-ending output)
                   (write-string prefix output))
                 (write-char character output)))))

(defun %trailing-line-ending-count (text)
  (loop with index = (length text)
        with count = 0
        while (plusp index)
        for character = (char text (1- index))
        do (cond
             ((char= character #\Newline)
              (decf index)
              (when (and (plusp index)
                         (char= (char text (1- index)) #\Return))
                (decf index))
              (incf count))
             ((char= character #\Return)
              (decf index)
              (incf count))
             (t (return count)))
        finally (return count)))

(defun %leading-line-ending-count (text)
  (loop with index = 0
        with count = 0
        while (< index (length text))
        do (multiple-value-bind (style width)
               (%line-ending-at text index)
             (if style
                 (progn (incf count) (incf index width))
                 (return count)))
        finally (return count)))

(defun %has-content-p (text)
  (loop for character across text
        thereis (and (not (member character '(#\Space #\Tab #\Return #\Newline)))
                     (/= (char-code character) #xfeff))))

(defun %repeat-line-ending (line-ending count)
  (with-output-to-string (output)
    (loop repeat count
          do
      (write-string line-ending output))))

(defun %ensure-boundary-separation (prefix replacement suffix line-ending)
  (let ((result replacement))
    (cond
      ((and (%has-content-p prefix)
            (%has-content-p suffix)
            (not (%has-content-p result)))
       (let* ((existing (+ (%trailing-line-ending-count prefix)
                           (%leading-line-ending-count result)
                           (%leading-line-ending-count suffix)))
              (missing (max 0 (- 2 existing))))
         (setf result
               (concatenate 'string result
                            (%repeat-line-ending line-ending missing)))))
      (t
       (when (and (%has-content-p prefix) (%has-content-p result))
         (let ((missing
                 (max 0 (- 2 (+ (%trailing-line-ending-count prefix)
                                (%leading-line-ending-count result))))))
           (when (plusp missing)
             (setf result
                   (concatenate 'string
                                (%repeat-line-ending line-ending missing)
                                result)))))
       (when (and (%has-content-p result) (%has-content-p suffix))
         (let ((missing
                 (max 0 (- 2 (+ (%trailing-line-ending-count result)
                                (%leading-line-ending-count suffix))))))
           (when (plusp missing)
             (setf result
                   (concatenate 'string result
                                (%repeat-line-ending line-ending missing))))))))
    result))

(defun %trailing-newline-p (source)
  (and (plusp (length source))
       (find (char source (1- (length source))) '(#\Return #\Newline))))

(defun %set-trailing-newline-state (candidate original line-ending)
  (let ((end (length candidate)))
    (loop while (and (plusp end)
                     (find (char candidate (1- end)) '(#\Return #\Newline)))
          do (decf end))
    (concatenate 'string
                 (subseq candidate 0 end)
                 (if (%trailing-newline-p original) line-ending ""))))

(defun %span-range (span source)
  (unless (and (typep span 'cl-parser-kit:span)
               (string= (cl-parser-kit:span-source span) source))
    (error "The edit target must have a source span from this document."))
  (let ((start (cl-parser-kit:span-start span))
        (end (cl-parser-kit:span-end span)))
    (unless (<= 0 start end (length source))
      (error "The edit target has an invalid source span [~D,~D)." start end))
    (cons start end)))

(defun %node-in-document-p (document target)
  (let ((found nil))
    (walk-nodes (lambda (node)
                  (when (eq node target)
                    (setf found t)))
                document)
    found))

(defun %range-contains-node-p (range node)
  (let ((span (node-span node)))
    (and range span
         (<= (car range)
             (cl-parser-kit:span-start span)
             (cl-parser-kit:span-end span)
             (cdr range)))))

(defun %semantic-text (node excluded-p)
  (with-output-to-string (output)
    (labels ((write-node (current)
               (unless (funcall excluded-p current)
                 (cond
                   ((or (thematic-break-p current)
                        (link-reference-definition-p current)))
                   ((text-p current)
                    (write-string (text-value current) output))
                   ((code-span-p current)
                    (write-string (code-span-literal-text current) output))
                   ((code-block-p current)
                    (write-string (code-block-body current) output))
                   ((html-inline-p current)
                    (write-string (html-inline-body current) output))
                   ((html-block-p current)
                    (write-string (html-block-body current) output))
                   ((front-matter-p current)
                    (write-string (front-matter-raw-text current) output))
                   ((autolink-p current)
                    (write-string (autolink-display-text current) output))
                   (t
                    (dolist (child (node-children current))
                      (write-node child)))))))
      (write-node node))))

(defun %node-semantic-attributes (node)
  (cond
    ((heading-p node)
     (list :level (heading-level node)))
    ((code-block-p node)
     (list :info-string (code-block-info-string node)))
    ((link-p node)
     (list :destination (link-destination node)
           :title (link-title node)
           :reference-kind (link-reference-kind node)
           :reference-label (link-reference-label node)))
    ((image-p node)
     (list :destination (image-destination node)
           :title (image-title node)
           :reference-kind (image-reference-kind node)
           :reference-label (image-reference-label node)))
    ((autolink-p node)
     (list :destination (autolink-destination node)))
    ((list-node-p node)
     (list :kind (list-node-kind node)
           :start-number (list-node-start-number node)
           :delimiter (list-node-delimiter-character node)
           :tight-p (list-node-tight-p node)))
    ((task-list-item-p node)
     (list :checked-p (task-list-item-checked-p node)))
    ((table-p node)
     (list :alignments (table-alignments node)))
    ((table-row-p node)
     (list :header-row-p (table-row-header-row-p node)))
    ((table-cell-p node)
     (list :alignment (table-cell-alignment node)))
    ((html-block-p node)
     (list :kind (html-block-kind node)))
    ((front-matter-p node)
     (list :opening (front-matter-opening-delimiter node)
           :closing (front-matter-closing-delimiter node)))
    ((footnote-definition-p node)
     (list :label (footnote-definition-raw-label node)
           :normalized-label (footnote-definition-normalized-label node)))
    ((footnote-reference-p node)
     (list :label (footnote-reference-raw-label node)
           :normalized-label (footnote-reference-normalized-label node)))
    ((link-reference-definition-p node)
     (list :label (link-reference-definition-raw-label node)
           :normalized-label (link-reference-definition-normalized-label node)
           :destination (link-reference-definition-destination node)
           :title (link-reference-definition-title node)
           :eligible-p (link-reference-definition-eligible-p node)))
    (t nil)))

(defun %semantic-attributes (node excluded-p)
  (labels ((collect (current)
             (unless (funcall excluded-p current)
               (cons (list (type-of current)
                           (%node-semantic-attributes current))
                     (mapcan #'collect (node-children current))))))
    (collect node)))

(defun %block-projections (document excluded-p)
  (let ((entries nil))
    (walk-nodes
     (lambda (node)
       (when (and (block-node-p node)
                  (not (document-p node))
                  (node-span node)
                  (not (funcall excluded-p node)))
         (let ((span (node-span node)))
           (push (list (cl-parser-kit:span-start span)
                       (cl-parser-kit:span-end span)
                       (type-of node)
                       (%semantic-text node excluded-p)
                       (%semantic-attributes node excluded-p))
                 entries))))
     document)
    (nreverse entries)))

(defun %map-edit-range
    (range start end replacement-length candidate-length source-length)
  (let* ((range-start (car range))
         (range-end (cdr range))
         (delta (- replacement-length (- end start)))
         (mapped-start
           (cond
             ((<= range-end start) range-start)
             ((>= range-start end) (+ range-start delta))
             (t (if (< range-start start) range-start start))))
         (mapped-end
           (cond
             ((<= range-end start) range-end)
             ((>= range-start end) (+ range-end delta))
             ((> range-end end) (+ range-end delta))
             (t (+ start replacement-length)))))
    (when (= end source-length)
      (setf mapped-end candidate-length))
    (cons (min candidate-length (max 0 mapped-start))
          (min candidate-length (max 0 mapped-end)))))

(defun %map-projection-span (entry start end replacement-length)
  (let* ((span-start (first entry))
         (span-end (second entry))
         (delta (- replacement-length (- end start))))
    (cond
      ((<= span-end start)
       (values span-start span-end))
      ((>= span-start end)
       (values (+ span-start delta) (+ span-end delta)))
      (t
       (values (if (< span-start start) span-start start)
               (if (> span-end end)
                   (+ span-end delta)
                   (+ start replacement-length)))))))

(defun %projection-difference (before after start end replacement-length)
  (loop for old-entry in before
        for new-entry in after
        for index from 1
        do (multiple-value-bind (mapped-start mapped-end)
               (%map-projection-span old-entry start end replacement-length)
             (unless (and (= mapped-start (first new-entry))
                          (= mapped-end (second new-entry))
                          (equal (nth 2 old-entry) (nth 2 new-entry))
                          (equal (nth 3 old-entry) (nth 3 new-entry))
                          (equal (nth 4 old-entry) (nth 4 new-entry)))
               (return-from %projection-difference
                 (format nil
                         "Non-target block ~D changed at [~D,~D)."
                         index mapped-start mapped-end)))))
  (cond
    ((> (length before) (length after))
     (format nil "Non-target block ~D was removed." (1+ (length after))))
    ((< (length before) (length after))
     (format nil "Non-target block ~D was added." (1+ (length before))))))

(defun %candidate-structure-difference
    (document candidate source start end replacement-length target-range)
  (let* ((candidate-document
           (parse-string candidate
                         :front-matter (document-front-matter-p document)
                         :gfm (document-gfm-p document)
                         :max-nesting-depth (document-max-nesting-depth document)
                         :max-inline-delimiter-depth
                         (document-max-inline-delimiter-depth document)))
         (candidate-target-range
           (if target-range
               (%map-edit-range target-range start end replacement-length
                                (length candidate) (length source))
               (let ((candidate-end (min (length candidate)
                                         (+ start replacement-length))))
                 (when (= end (length source))
                   (setf candidate-end (length candidate)))
                 (cons (min start candidate-end) candidate-end))))
         (before
           (%block-projections
            document
            (lambda (node) (%range-contains-node-p target-range node))))
         (after
           (%block-projections
            candidate-document
            (lambda (node)
              (%range-contains-node-p candidate-target-range node)))))
    (%projection-difference before after start end replacement-length)))

(defun %edit-candidate (source start end replacement original line-ending)
  (let ((candidate
          (concatenate 'string
                       (subseq source 0 start)
                       replacement
                       (subseq source end))))
    (if (= end (length source))
        (%set-trailing-newline-state candidate original line-ending)
        candidate)))

(defun %perform-edit (document start end markdown target target-range)
  (let ((source (%edit-document-source document)))
    (unless (and (integerp start) (integerp end)
                 (<= 0 start end (length source)))
      (error "Invalid edit interval [~S,~S) for source length ~D."
             start end (length source)))
    (let* ((line-ending (%edit-line-ending source start end))
           (replacement (%normalize-newlines markdown line-ending)))
      (labels ((candidate-for (replacement-text)
                 (%edit-candidate source start end replacement-text source line-ending))
               (difference-for (candidate replacement-text)
                 (%candidate-structure-difference
                  document candidate source start end (length replacement-text)
                  target-range)))
        (let* ((candidate (candidate-for replacement))
               (difference (difference-for candidate replacement)))
          (unless difference
            (return-from %perform-edit candidate))
          (let* ((prefix (subseq source 0 start))
                 (suffix (subseq source end))
                 (separated
                   (%ensure-boundary-separation prefix replacement suffix line-ending)))
            (unless (string= replacement separated)
              (let* ((retry (candidate-for separated))
                     (retry-difference (difference-for retry separated)))
                (unless retry-difference
                  (return-from %perform-edit retry))
                (setf difference retry-difference)))
            (error 'markdown-structure-changed-error
                   :target target
                   :difference difference
                   :message "The edit would change non-target Markdown structure.")))))))

(defun %section-edit-range (document address source)
  (let* ((section (find-section document address))
         (heading (section-record-heading section))
         (span (section-record-span section)))
    (if (null heading)
        (values section
                (if (and (plusp (length source))
                         (= (char-code (char source 0)) #xfeff))
                    1 0)
                (length source))
        (let ((range (%span-range span source)))
          (values section (car range) (cdr range))))))

(defun %after-line-ending (source offset)
  (multiple-value-bind (style width)
      (%line-ending-at source offset)
    (declare (ignore style))
    (+ offset (or width 0))))

(defun replace-section-body (document address markdown)
  (check-type document document)
  (check-type markdown string)
  (let ((source (%edit-document-source document)))
    (multiple-value-bind (section section-start section-end)
        (%section-edit-range document address source)
      (let* ((heading (section-record-heading section))
             (target-range (cons section-start section-end))
             (heading-end
               (when heading
                 (cl-parser-kit:span-end
                  (node-span (heading-record-node heading)))))
             (body-start
               (if heading
                   (%after-line-ending source heading-end)
                   section-start)))
        (let ((replacement
                (if (and heading
                         (= heading-end (length source))
                         (plusp (length markdown)))
                    (concatenate 'string
                                 (%edit-line-ending source body-start section-end)
                                 markdown)
                    markdown)))
          (%perform-edit document body-start section-end replacement
                         section target-range))))))

(defun replace-section (document address markdown)
  (check-type document document)
  (check-type markdown string)
  (let ((source (%edit-document-source document)))
    (multiple-value-bind (section start end)
        (%section-edit-range document address source)
      (%perform-edit document start end markdown section (cons start end)))))

(defun delete-section (document address)
  (check-type document document)
  (let ((source (%edit-document-source document)))
    (multiple-value-bind (section start end)
        (%section-edit-range document address source)
      (%perform-edit document start end "" section (cons start end)))))

(defun %section-record-for-heading (records heading)
  (find (heading-record-node heading) records
        :key #'heading-record-node :test #'eq))

(defun %direct-child-section-records (records selected selected-end)
  (let ((stack nil)
        (children nil))
    (dolist (record records)
      (let* ((span (heading-record-section-span record))
             (start (and span (cl-parser-kit:span-start span)))
             (level (heading-record-level record)))
        (when (and start (>= start (cl-parser-kit:span-start
                                    (node-span (heading-record-node selected))))
                   (< start selected-end))
          (loop while (and stack
                           (>= (heading-record-level (first stack)) level))
                do (pop stack))
          (when (eq (first stack) selected)
            (push record children))
          (push record stack))))
    (nreverse children)))

(defun %insert-section-offset (document address position source)
  (let* ((section (find-section document address))
         (heading (section-record-heading section))
         (all-records (document-headings document))
         (direct-records
           (remove-if-not #'heading-record-section-span all-records)))
    (cond
      ((null heading)
       (case position
         (:before (if (and (plusp (length source))
                           (= (char-code (char source 0)) #xfeff))
                      1 0))
         ((:after :last-child) (length source))))
      (t
       (let* ((selected (%section-record-for-heading direct-records heading))
              (selected-span (heading-record-section-span selected))
              (selected-end (cl-parser-kit:span-end selected-span)))
         (case position
           (:before (cl-parser-kit:span-start selected-span))
           (:after selected-end)
           (:last-child
            (let ((children
                    (%direct-child-section-records direct-records selected selected-end)))
              (if children
                  (cl-parser-kit:span-end
                   (heading-record-section-span (car (last children))))
                  selected-end)))))))))

(defun %default-insert-position ()
  :after)

(defun insert-section (document address markdown
                       &key (position (%default-insert-position)))
  (check-type document document)
  (check-type markdown string)
  (unless (member position '(:before :after :last-child))
    (error 'type-error :datum position
           :expected-type '(member :before :after :last-child)))
  (let* ((source (%edit-document-source document))
         (offset (%insert-section-offset document address position source)))
    (%perform-edit document offset offset markdown address nil)))

(defun replace-node (document node markdown)
  (check-type document document)
  (check-type markdown string)
  (unless (and (block-node-p node)
               (not (document-p node))
               (%node-in-document-p document node))
    (error "REPLACE-NODE requires a source-backed block node in DOCUMENT."))
  (let* ((source (%edit-document-source document))
         (range (%span-range (node-span node) source))
         (line-ending (%edit-line-ending source (car range) (cdr range)))
         (logical (%normalize-newlines markdown (string #\Newline)))
         (replacement
           (%prefix-continuation-lines
            logical (block-node-continuation-prefix node) line-ending)))
    (%perform-edit document (car range) (cdr range) replacement node range)))

(defun set-task-checked (document task-item checked-p)
  (check-type document document)
  (unless (and (task-list-item-p task-item)
               (%node-in-document-p document task-item))
    (error "SET-TASK-CHECKED requires a task-list-item in DOCUMENT."))
  (unless (typep checked-p 'boolean)
    (error 'type-error :datum checked-p :expected-type 'boolean))
  (let* ((source (%edit-document-source document))
         (target-range (%span-range (node-span task-item) source))
         (marker-span (task-list-item-task-marker-span task-item))
         (marker-range (%span-range marker-span source))
         (marker (subseq source (car marker-range) (cdr marker-range)))
         (currently-checked (task-list-item-checked-p task-item)))
    (unless (member marker '("[ ]" "[x]" "[X]") :test #'string=)
      (error "The task marker span does not contain a recognized task marker."))
    (if (eq currently-checked checked-p)
        source
        (%perform-edit document
                       (car marker-range) (cdr marker-range)
                       (if checked-p "[x]" "[ ]")
                       task-item target-range))))
