(in-package #:cl-markdown-kit)

(defun %ip-flanking (text start end run-end)
  (let* ((before (if (> start 0) (char text (1- start)) nil))
         (after (if (< run-end end) (char text run-end) nil))
         (before-space (or (null before) (unicode-whitespace-p before)))
         (after-space (or (null after) (unicode-whitespace-p after)))
         (before-punctuation (and before (%pc-commonmark-punctuation-p before)))
         (after-punctuation (and after (%pc-commonmark-punctuation-p after)))
         (left (and (not after-space)
                    (or (not after-punctuation)
                        before-space before-punctuation)))
         (right (and (not before-space)
                     (or (not before-punctuation)
                         after-space after-punctuation))))
    (values left right before-punctuation after-punctuation)))

(defun %ip-delimiter-bucket-index (delimiter)
  (let ((character (%ip-delimiter-character delimiter))
        (length (%ip-delimiter-length delimiter)))
    (cond
      ((char= character +ip-tilde-delimiter-character+)
       +ip-tilde-delimiter-bucket-index+)
      (t (+ (if (%ip-delimiter-can-close delimiter) 3 0)
            (mod length 3))))))

(defun %ip-rule-of-three-blocked-p (opener closer)
  (and (or (%ip-delimiter-can-close opener)
           (%ip-delimiter-can-open closer))
       (not (zerop (mod (%ip-delimiter-length closer) 3)))
       (zerop (mod (+ (%ip-delimiter-length opener)
                      (%ip-delimiter-length closer))
                   3))))

(defun %ip-delimiter-buckets (openers character)
  (or (gethash character openers)
      (setf (gethash character openers) (make-array +ip-delimiter-bucket-count+ :initial-element nil))))

(defun %ip-best-opener (openers closer on-match on-fail)
  (let* ((character (%ip-delimiter-character closer))
         (buckets (gethash character openers))
         (best nil))
    (when buckets
      (loop for bucket-index below (if (char= character +ip-tilde-delimiter-character+)
                                    +ip-delimiter-bucket-count+
                                    +ip-ordinary-delimiter-bucket-count+)
            for stack = (aref buckets bucket-index)
            for candidate = (first stack)
            when (and candidate
                      (%ip-delimiter-can-open candidate)
                      (or (char= character +ip-tilde-delimiter-character+)
                          (not (%ip-rule-of-three-blocked-p candidate closer)))
                      (or (not (char= character +ip-tilde-delimiter-character+))
                          (and (member (%ip-delimiter-remaining candidate) '(1 2))
                               (= (%ip-delimiter-remaining candidate)
                                  (%ip-delimiter-remaining closer))))
                      (or (null best)
                          (> (%ip-delimiter-start candidate)
                             (%ip-delimiter-start best))))
              do (setf best candidate)))
    (if best
        (funcall on-match best)
        (funcall on-fail))))

(defun %ip-pop-opener-if-empty
    (openers delimiter active-count active-stack total-active-count)
  (when (zerop (%ip-delimiter-remaining delimiter))
    (let* ((buckets (gethash (%ip-delimiter-character delimiter) openers))
           (bucket-index (%ip-delimiter-open-bucket delimiter)))
      ;; The selected opener remains at the head of both stacks until removed.
      (pop (aref buckets bucket-index))
      (pop (car active-stack))
      (setf (%ip-delimiter-open-bucket delimiter) nil)
      (decf (car active-count))
      (decf (car total-active-count)))))

(defun %ip-add-delimiter (start end character can-open can-close
                          openers delimiters pairs active-count active-stack
                          total-active-count bracket-depth depth-limit)
  (let* ((length (- end start))
         (run (%ip-make-delimiter character start length can-open can-close)))
    (setf (%ip-delimiter-remaining run) length)
    (push run delimiters)
    (when can-close
      (loop while (plusp (%ip-delimiter-remaining run))
            do (let ((opener nil))
                 (%ip-best-opener
                  openers run
                  (lambda (candidate) (setf opener candidate))
                  (lambda () nil))
                 (unless opener (return))
                 (let* ((opener-remaining (%ip-delimiter-remaining opener))
                        (closer-remaining (%ip-delimiter-remaining run))
                        (use-count
                          (cond
                            ((char= character +ip-tilde-delimiter-character+)
                             closer-remaining)
                            ((and (>= opener-remaining 2)
                                  (>= closer-remaining 2))
                             2)
                            (t 1)))
                        (open-start
                          (+ (%ip-delimiter-start opener)
                             (%ip-delimiter-open-offset opener)
                             (- opener-remaining use-count)))
                        (close-start (+ start (- length closer-remaining))))
                   (loop until (eq (first (car active-stack)) opener)
                         for skipped = (pop (car active-stack))
                         do (let* ((bucket-index
                                     (%ip-delimiter-open-bucket skipped))
                                   (buckets
                                     (gethash
                                      (%ip-delimiter-character skipped)
                                      openers)))
                              (pop (aref buckets bucket-index))
                              (setf (%ip-delimiter-open-bucket skipped) nil)
                              (decf (car active-count))
                              (decf (car total-active-count))))
                   (push (%ip-make-pair
                          (cond
                            ((char= character +ip-tilde-delimiter-character+)
                             +ip-tilde-run-kind+)
                            ((= use-count 2) :strong)
                            (t :emphasis))
                          character open-start (+ open-start use-count)
                          close-start (+ close-start use-count))
                         pairs)
                   (decf (%ip-delimiter-remaining opener) use-count)
                   (decf (%ip-delimiter-remaining run) use-count)
                   (incf (%ip-delimiter-open-offset run) use-count)
                   (%ip-pop-opener-if-empty
                    openers opener active-count active-stack
                    total-active-count)))))
    (when (and can-open (plusp (%ip-delimiter-remaining run))
               (or (not (char= character +ip-tilde-delimiter-character+))
                   (member (%ip-delimiter-remaining run) '(1 2))))
      (let* ((bucket-index (%ip-delimiter-bucket-index run))
             (buckets (%ip-delimiter-buckets openers character)))
        (%pc-check-depth (+ bracket-depth (car total-active-count) 1) depth-limit :inline)
        (setf (%ip-delimiter-open-bucket run) bucket-index)
        (push run (aref buckets bucket-index))
        (push run (car active-stack))
        (incf (car active-count))
        (incf (car total-active-count))))
    (values delimiters pairs)))

(defun %ip-delimiter-run-scopes (runs pairs)
    (let* ((containers
             (coerce (sort (copy-list pairs) #'< :key #'%ip-pair-inner-start)
                     'vector))
           (container-index 0)
           (container-stack nil)
           (groups (make-hash-table :test #'eq)))
      (dolist (run (sort (copy-list runs) #'< :key #'%ip-delimiter-start))
        (let ((position (%ip-delimiter-start run)))
          (loop while (and container-stack
                           (>= position
                               (%ip-pair-inner-end (first container-stack))))
                do (pop container-stack))
          (loop while (and (< container-index (length containers))
                           (<= (%ip-pair-inner-start
                                (aref containers container-index))
                               position))
                for container = (aref containers container-index)
                do (when (< position (%ip-pair-inner-end container))
                     (push container container-stack))
                   (incf container-index))
          (push run (gethash (first container-stack) groups))))
      groups))

  (defun %ip-delimiter-run-events (groups)
    (let ((events nil))
      (maphash (lambda (scope scoped-runs)
                 (dolist (run scoped-runs)
                   (push (cons scope run) events)))
               groups)
      (sort events #'<
            :key (lambda (event)
                   (%ip-delimiter-start (cdr event))))))

  (defun %ip-resolve-delimiter-runs
      (runs pairs bracket-depths bracket-closes range-start range-end depth-limit)
    (let* ((groups (%ip-delimiter-run-scopes runs pairs))
           (run-events (%ip-delimiter-run-events groups))
           (states (make-hash-table :test #'eq))
           ;; Unmatched records remain counted after their scope ends.
           (total-active-count (list 0))
           (resolved-delimiters nil)
           (resolved-pairs pairs)
           (bracket-count 0)
           (position range-start))
      (labels ((advance-bracket-position ()
                 (when bracket-depths
                   (let ((delta
                           (- (aref bracket-depths (1+ position))
                              (aref bracket-depths position))))
                     (cond
                       ((plusp delta)
                        (%pc-check-depth
                         (+ bracket-count (car total-active-count) 1)
                         depth-limit :inline)
                        (incf bracket-count))
                       ((and (minusp delta)
                             (>= (aref bracket-closes position) range-start))
                        (decf bracket-count))))))
               (process-run (scope run)
                 (let* ((state
                          (or (gethash scope states)
                              (setf (gethash scope states)
                                    (list (make-hash-table :test #'eql)
                                          nil nil (list 0) (list nil)))))
                        (openers (first state))
                        (delimiters (second state))
                        (matched-pairs (third state))
                        (scope-count (fourth state))
                        (scope-stack (fifth state)))
                   (multiple-value-setq (delimiters matched-pairs)
                     (%ip-add-delimiter
                      (%ip-delimiter-start run)
                      (+ (%ip-delimiter-start run)
                         (%ip-delimiter-length run))
                      (%ip-delimiter-character run)
                      (%ip-delimiter-can-open run)
                      (%ip-delimiter-can-close run)
                      openers delimiters matched-pairs scope-count scope-stack
                      total-active-count bracket-count depth-limit))
                   (setf (second state) delimiters
                         (third state) matched-pairs))))
        (dolist (event run-events)
          (let ((run-position (%ip-delimiter-start (cdr event))))
            (loop while (< position run-position)
                  do (advance-bracket-position)
                     (incf position))
            (process-run (car event) (cdr event))
            (advance-bracket-position)
            (incf position)))
        (loop while (< position range-end)
              do (advance-bracket-position)
                 (incf position))
        (maphash (lambda (scope state)
                   (declare (ignore scope))
                   (setf resolved-delimiters
                         (nconc (second state) resolved-delimiters)
                         resolved-pairs
                         (nconc (third state) resolved-pairs)))
                 states))
      (values resolved-delimiters resolved-pairs)))
