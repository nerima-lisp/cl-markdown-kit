(in-package #:cl-markdown-kit/test)

(defun resource-repeat-string (text count)
  (let* ((width (length text))
         (result (make-string (* width count))))
    (dotimes (index count result)
      (replace result text :start1 (* index width)))))

(defun resource-input-nested-lists (count marker)
  (with-output-to-string (stream)
    (dotimes (depth count)
      (dotimes (space (* 2 depth))
        (declare (ignore space))
        (write-char #\Space stream))
      (write-string marker stream)
      (write-char #\Newline stream))))

(defun resource-reference-hash-collision-p (reference)
  (let ((hash 0)
        (mask #xffffffff))
    (loop for character across reference
          for shifted-six = (logand (ash hash 6) mask)
          for shifted-sixteen = (logand (ash hash 16) mask)
          do (setf hash
                   (logand (+ (char-code character)
                              shifted-six shifted-sixteen (- hash))
                           mask)))
    (zerop (mod hash 16))))

(defun resource-input-reference-collisions ()
  (let* ((bad-key nil)
         (collision-count 0)
        (document
          (with-output-to-string (stream)
            (loop with candidate = 0
                  do (let ((key (format nil "x~D" candidate)))
                       (incf candidate)
                       (when (resource-reference-hash-collision-p key)
                         (incf collision-count)
                         (if (= collision-count 1)
                             (setf bad-key key)
                             (format stream "[~A]: /url~%~%[~A]~%~%"
                                     key bad-key))))
                  until (= collision-count 50000)))))
    (assert bad-key)
    (assert (= collision-count 50000))
    document))

(defun resource-input-backticks ()
  (let* ((run-count 4999)
         (total-length (+ run-count
                          (loop for run from 1 to run-count sum run)))
         (input (make-string total-length))
         (index 0))
    (loop for run from 1 to run-count
          do (setf (aref input index) #\e)
             (incf index)
             (dotimes (backtick run)
               (declare (ignore backtick))
               (setf (aref input index) #\`)
               (incf index)))
    (assert (= index total-length))
    input))

(defun resource-input-table-rows ()
  (let ((row (concatenate 'string
                          "aaa" (string #\Return) "bbb" (string #\Newline)
                          "-" (string (code-char 11)) (string #\Newline))))
    (resource-repeat-string row 30000)))
(defun resource-input-composite (kind)
  (ecase kind
    (:nested-strong-emphasis
     (concatenate 'string
                  (resource-repeat-string "*a **a " 65000)
                  "b"
                  (resource-repeat-string " a** a*" 65000)))
    (:delimiter-multiple-of-three
     (concatenate 'string "a**b" (resource-repeat-string "c* " 50000)))
    (:nested-link-brackets
     (concatenate 'string
                  (make-string 50000 :initial-element #\[)
                  "a"
                  (make-string 50000 :initial-element #\])))
    (:nested-block-quotes
     (concatenate 'string (resource-repeat-string "> " 50000) "a"))
    (:nul-input
     (concatenate 'string "abc" (string (code-char 0)) "de"
                  (string (code-char 0))))
    (:unclosed-html-comment
     (concatenate 'string "</" (resource-repeat-string "<!--" 300000)))))
(defun resource-materialize-input (input-form)
  (if (consp input-form)
      (apply (first input-form) (rest input-form))
      input-form))

(defun resource-input-utf8-byte-length (input)
  (loop for character across input
        for code = (char-code character)
        sum (cond ((<= code #x7f) 1)
                  ((<= code #x7ff) 2)
                  ((<= code #xffff) 3)
                  (t 4))))

(defun resource-seconds (ticks)
  (/ (float ticks 1d0) internal-time-units-per-second))

(defun resource-report-pathological-case (name input expected-length expected-result
                                          expected-limit-name expected-limit
                                          expected-actual)
  (let ((characters (length input))
        (bytes (resource-input-utf8-byte-length input)))
    (assert (plusp characters))
    (when expected-length
      (assert (= characters expected-length)))
    (sb-ext:gc :full t)
    (let* ((wall-start (get-internal-real-time))
           (cpu-start (get-internal-run-time))
           (result nil)
           (report
             (with-output-to-string (stream)
               (let ((*standard-output* stream))
                 (setf result
                       (handler-case
                           (let ((document (cl-markdown-kit:parse-string input)))
                             (unless (typep document 'cl-markdown-kit:document)
                               (error "Pathological case ~A returned a non-document."
                                      name))
                             (format t "T-007 resource case=~A chars=~D bytes=~D wall=~,6Fs cpu=~,6Fs result=DOCUMENT~%"
                                     name characters bytes
                                     (resource-seconds
                                      (- (get-internal-real-time) wall-start))
                                     (resource-seconds
                                      (- (get-internal-run-time) cpu-start)))
                             document)
                         (cl-markdown-kit:markdown-resource-limit-error (condition)
                           (format t "T-007 resource case=~A chars=~D bytes=~D wall=~,6Fs cpu=~,6Fs result=RESOURCE-LIMIT name=~A limit=~S actual=~S~%"
                                   name characters bytes
                                   (resource-seconds
                                    (- (get-internal-real-time) wall-start))
                                   (resource-seconds
                                    (- (get-internal-run-time) cpu-start))
                                   (cl-markdown-kit:markdown-resource-limit-error-limit-name condition)
                                   (cl-markdown-kit:markdown-resource-limit-error-limit condition)
                                   (cl-markdown-kit:markdown-resource-limit-error-actual condition))
                           condition)))))))
      (write-string report *standard-output*)
      (ecase expected-result
        (:document
         (assert (typep result 'cl-markdown-kit:document))
         (assert (null expected-limit-name))
         (assert (null expected-limit))
         (assert (null expected-actual))
         (assert (search "result=DOCUMENT" report)))
        (:resource-limit
         (assert (typep result 'cl-markdown-kit:markdown-resource-limit-error))
         (assert (equal expected-limit-name
                        (cl-markdown-kit:markdown-resource-limit-error-limit-name result)))
         (assert (eql expected-limit
                      (cl-markdown-kit:markdown-resource-limit-error-limit result)))
         (assert (eql expected-actual
                      (cl-markdown-kit:markdown-resource-limit-error-actual result)))
         (assert (search
                  (format nil "result=RESOURCE-LIMIT name=~A limit=~S actual=~S"
                          expected-limit-name expected-limit expected-actual)
                  report))))
      result)))

(describe "cmark-gfm pathological inputs: emphasis and delimiters (7 cases)"
  (cl-weave:describe-each (("parses nested strong emphasis delimiters"
      "nested strong emph"
      (resource-input-composite :nested-strong-emphasis)
      910001 :resource-limit :inline-delimiter-depth 4096 4097)
     ("scans unmatched emphasis closers"
      "many emph closers with no openers"
      (resource-repeat-string "a_ " 65000)
      195000 :document nil nil nil)
     ("scans unmatched emphasis openers"
      "many emph openers with no closers"
      (resource-repeat-string "_a " 65000)
      195000 :resource-limit :inline-delimiter-depth 4096 4097)
     ("handles mismatched emphasis openers and closers"
      "mismatched openers and closers"
      (resource-repeat-string "*a_ " 50000)
      200000 :resource-limit :inline-delimiter-depth 4096 4097)
     ("handles delimiter counts that are multiples of three"
      "openers and closers multiple of 3"
      (resource-input-composite :delimiter-multiple-of-three)
      150004 :document nil nil nil)
     ("scans emphasis closers after link openers"
      "link openers and emph closers"
      (resource-repeat-string "[ a_" 50000)
      200000 :resource-limit :inline-delimiter-depth 4096 4097)
     ("handles the hard link and emphasis delimiter case"
      "hard link/emph case"
      "**x [a*b**c*](d)"
      16 :document nil nil nil))
    "~A"
    (case-title name input-form expected-length expected-result
                expected-limit-name expected-limit expected-actual)
    (it "checks the expected parser result and resource report"
      (:timeout-ms 15000)
      (resource-report-pathological-case
       name (resource-materialize-input input-form) expected-length expected-result
       expected-limit-name expected-limit expected-actual))))

(describe "cmark-gfm pathological inputs: links and brackets (7 cases)"
  (cl-weave:describe-each (("scans unmatched link closers"
      "many link closers with no openers"
      (resource-repeat-string "a]" 65000)
      130000 :document nil nil nil)
     ("scans unmatched link openers"
      "many link openers with no closers"
      (resource-repeat-string "[a" 65000)
      130000 :resource-limit :inline-delimiter-depth 4096 4097)
     ("scans repeated bracket and parenthesis patterns"
      "pattern [ (]( repeated"
      (resource-repeat-string "[ (](" 80000)
      400000 :document nil nil nil)
     ("scans repeated image and link delimiter patterns"
      "pattern ![[]() repeated"
      (resource-repeat-string "![[]()" 160000)
      960000 :resource-limit :inline-delimiter-depth 4096 4097)
     ("handles nested link brackets"
      "nested brackets"
      (resource-input-composite :nested-link-brackets)
      100001 :resource-limit :inline-delimiter-depth 4096 4097)
     ("handles the first unclosed link destination shape"
      "unclosed links A"
      (resource-repeat-string "[a](<b" 30000)
      180000 :document nil nil nil)
     ("handles the second unclosed link destination shape"
      "unclosed links B"
      (resource-repeat-string "[a](b" 30000)
      150000 :document nil nil nil))
    "~A"
    (case-title name input-form expected-length expected-result
                expected-limit-name expected-limit expected-actual)
    (it "checks the expected parser result and resource report"
      (:timeout-ms 15000)
      (resource-report-pathological-case
       name (resource-materialize-input input-form) expected-length expected-result
       expected-limit-name expected-limit expected-actual))))

(describe "cmark-gfm pathological inputs: block nesting (2 cases)"
  (cl-weave:describe-each (("handles deeply nested block quotes"
      "nested block quotes"
      (resource-input-composite :nested-block-quotes)
      100001 :resource-limit :nesting-depth 256 257)
     ("handles deeply nested lists"
      "deeply nested lists"
      (resource-input-nested-lists 1000 "* a")
      1003000 :resource-limit :nesting-depth 256 257))
    "~A"
    (case-title name input-form expected-length expected-result
                expected-limit-name expected-limit expected-actual)
    (it "checks the expected parser result and resource report"
      (:timeout-ms 15000)
      (resource-report-pathological-case
       name (resource-materialize-input input-form) expected-length expected-result
       expected-limit-name expected-limit expected-actual))))

(describe "cmark-gfm pathological inputs: other scanning cases (3 cases)"
  (cl-weave:describe-each (("handles U+0000 in the input"
      "U+0000 in input"
      (resource-input-composite :nul-input)
      7 :document nil nil nil)
     ("scans increasing runs of backticks"
      "backticks"
      (resource-input-backticks)
      12502499 :document nil nil nil)
     ("scans an unclosed HTML comment opener sequence"
      "unclosed <!--"
      (resource-input-composite :unclosed-html-comment)
      1200002 :document nil nil nil))
    "~A"
    (case-title name input-form expected-length expected-result
                expected-limit-name expected-limit expected-actual)
    (it "checks the expected parser result and resource report"
      (:timeout-ms 15000)
      (resource-report-pathological-case
       name (resource-materialize-input input-form) expected-length expected-result
       expected-limit-name expected-limit expected-actual))))

(describe "cmark-gfm pathological inputs: GFM table (1 case)"
  (cl-weave:describe-each (("scans repeated carriage-return and vertical-tab table rows"
      "tables"
      (resource-input-table-rows)
      330000 :document nil nil nil))
    "~A"
    (case-title name input-form expected-length expected-result
                expected-limit-name expected-limit expected-actual)
    (it "checks the expected parser result and resource report"
      (:timeout-ms 15000)
      (resource-report-pathological-case
       name (resource-materialize-input input-form) expected-length expected-result
       expected-limit-name expected-limit expected-actual))))

(describe "cmark-gfm pathological inputs: reference hash collision (1 case)"
  (cl-weave:describe-each (("builds and parses all active reference collision definitions"
      "reference collisions"
      (resource-input-reference-collisions)
      1143036 :document nil nil nil))
    "~A"
    (case-title name input-form expected-length expected-result
                expected-limit-name expected-limit expected-actual)
    (it "checks the expected parser result and resource report"
      (:timeout-ms 15000)
      (resource-report-pathological-case
       name (resource-materialize-input input-form) expected-length expected-result
       expected-limit-name expected-limit expected-actual))))

(defun resource-input-for-scaling (kind size)
  (ecase kind
    (:quote
     (concatenate 'string (resource-repeat-string "> " size) "x"))
    (:list
     (resource-input-nested-lists size "- x"))
    (:bracket
     (concatenate 'string (make-string size :initial-element #\[) "x"))
    (:delimiter
     (resource-repeat-string "*a " size))
    (:long-line
     (make-string size :initial-element #\a))))

(defun resource-median (values)
  (nth (floor (length values) 2) (sort (copy-list values) #'<)))

(defconstant +resource-scaling-target-cpu-seconds+ 0.06d0)
(defconstant +resource-scaling-minimum-cpu-seconds+ 0.05d0)
(defconstant +resource-scaling-sample-count+ 3)
(defconstant +resource-scaling-sample-attempts+ 4)

(defun resource-scaling-batch (input iterations)
  (sb-ext:gc :full t)
  (let ((wall-start (get-internal-real-time))
        (cpu-start (get-internal-run-time)))
    (dotimes (iteration iterations)
      (declare (ignore iteration))
      (let ((document
              (cl-markdown-kit:parse-string
               input :max-nesting-depth 2048
                     :max-inline-delimiter-depth 16384)))
        (unless (typep document 'cl-markdown-kit:document)
          (error "Scaling input returned a non-document."))
        (unless (and (= (cl-markdown-kit:document-max-nesting-depth document) 2048)
                     (= (cl-markdown-kit:document-max-inline-delimiter-depth document)
                        16384))
          (error "Scaling parser did not retain its configured resource limits."))))
    (values (resource-seconds (- (get-internal-real-time) wall-start))
            (resource-seconds (- (get-internal-run-time) cpu-start)))))

(defun resource-calibrate-scaling-iterations (input)
  (let ((iterations 1))
    (loop
      (multiple-value-bind (wall cpu)
          (resource-scaling-batch input iterations)
        (declare (ignore wall))
        (when (>= cpu +resource-scaling-target-cpu-seconds+)
          (return iterations))
        (setf iterations
              (if (zerop cpu)
                  (* iterations 2)
                  (max (1+ iterations)
                       (ceiling (/ (* iterations
                                      +resource-scaling-target-cpu-seconds+)
                                   cpu)))))))))

;; Calibration accepts the first batch that reaches the target. When another
;; process slowed that batch, the iteration count is too small and later batches
;; fall below the minimum. Rescale from the shortest batch and sample again so
;; every compared batch uses the same iteration count.
(defun resource-sample-scaling-batches (input)
  (let ((iterations (resource-calibrate-scaling-iterations input)))
    (dotimes (attempt +resource-scaling-sample-attempts+
                      (error "Scaling batches stayed below ~Fs CPU after ~D attempts."
                             +resource-scaling-minimum-cpu-seconds+
                             +resource-scaling-sample-attempts+))
      (declare (ignorable attempt))
      (let ((walls nil)
            (cpus nil))
        (dotimes (trial +resource-scaling-sample-count+)
          (declare (ignore trial))
          (multiple-value-bind (wall cpu)
              (resource-scaling-batch input iterations)
            (push wall walls)
            (push cpu cpus)))
        (let ((shortest (reduce #'min cpus)))
          (when (<= +resource-scaling-minimum-cpu-seconds+ shortest)
            (return (values iterations (nreverse walls) (nreverse cpus))))
          (setf iterations
                (if (zerop shortest)
                    (* iterations 2)
                    (max (1+ iterations)
                         (ceiling (/ (* iterations
                                        +resource-scaling-target-cpu-seconds+)
                                     shortest))))))))))

(defun resource-time-ratio (larger smaller)
  (assert (plusp smaller))
  (/ (float larger 1d0) smaller))
(defun resource-scaling-work-measurement (input)
  (let ((document
          (cl-markdown-kit:parse-string
           input :max-nesting-depth 2048
                 :max-inline-delimiter-depth 16384))
        (prefix-characters 0))
    (unless (typep document 'cl-markdown-kit:document)
      (error "Scaling input returned a non-document."))
    (cl-markdown-kit:walk-node-tree
     document
     (lambda (frame)
       (when (eq :enter (cl-markdown-kit:node-tree-frame-phase frame))
         (let ((node (cl-markdown-kit:node-tree-frame-node frame)))
           (when (cl-markdown-kit:block-node-p node)
             (incf prefix-characters
                   (length
                    (cl-markdown-kit:block-node-continuation-prefix node))))))
       :descend))
    (list (length input)
          prefix-characters
          (+ (length input) prefix-characters))))

(defun resource-measure-scaling-series (kind sizes)
  (let* ((inputs (mapcar (lambda (size)
                           (resource-input-for-scaling kind size))
                         sizes))
         (work-measurements
           (mapcar #'resource-scaling-work-measurement inputs))
         (character-sizes (mapcar #'first work-measurements))
         (prefix-character-sizes (mapcar #'second work-measurements))
         (work-sizes (mapcar #'third work-measurements))
         (wall-samples (make-list +resource-scaling-sample-count+
                                  :initial-element nil))
         (cpu-samples (make-list +resource-scaling-sample-count+
                                 :initial-element nil))
         (iteration-counts (make-list +resource-scaling-sample-count+
                                      :initial-element 0)))
    (assert (= (length sizes) 3))
    (assert (<= 3.8d0
                (/ (second character-sizes)
                   (float (first character-sizes) 1d0))
                4.3d0))
    (assert (<= 3.8d0
                (/ (third character-sizes)
                   (float (second character-sizes) 1d0))
                4.3d0))
    (assert (<= 14.4d0
                (/ (third character-sizes)
                   (float (first character-sizes) 1d0))
                17.6d0))
    (loop for input in inputs
          for index from 0
          do (resource-scaling-batch input 1)
             (multiple-value-bind (iterations walls cpus)
                 (resource-sample-scaling-batches input)
               (setf (nth index iteration-counts) iterations
                     (nth index wall-samples) walls
                     (nth index cpu-samples) cpus)))
    (let* ((median-walls (mapcar #'resource-median wall-samples))
           (median-cpus
             (mapcar (lambda (samples iterations)
                       (/ (resource-median samples) (float iterations 1d0)))
                     cpu-samples iteration-counts))
           (cpu-ratios
             (list (resource-time-ratio (second median-cpus)
                                        (first median-cpus))
                   (resource-time-ratio (third median-cpus)
                                        (second median-cpus))
                   (resource-time-ratio (third median-cpus)
                                        (first median-cpus))))
           (work-ratios
             (list (resource-time-ratio (second work-sizes)
                                        (first work-sizes))
                   (resource-time-ratio (third work-sizes)
                                        (second work-sizes))
                   (resource-time-ratio (third work-sizes)
                                        (first work-sizes))))
           (normalized-ratios
             (mapcar #'/ cpu-ratios work-ratios)))
      (format t "T-007 scaling kind=~A samples=~D chars=~S prefix-chars=~S work=~S batch-iterations=~S batch-wall=~S batch-cpu=~S median-batch-wall=~S median-per-parse-cpu=~S cpu-ratios=~S work-ratios=~S normalized-ratios=~S~%"
              kind +resource-scaling-sample-count+
              character-sizes prefix-character-sizes work-sizes
              iteration-counts wall-samples cpu-samples
              median-walls median-cpus cpu-ratios work-ratios
              normalized-ratios)
      (finish-output)
      (assert (<= (first normalized-ratios) 2.4d0))
      (assert (<= (second normalized-ratios) 2.4d0))
      (assert (<= (third normalized-ratios) 5.3d0)))))

(it "measures five families at approximately 1x, 4x, and 16x source sizes"
  (:timeout-ms 30000)
  (let* ((depth 64)
         (measurement
           (resource-scaling-work-measurement
            (resource-input-for-scaling :quote depth)))
         (expected-prefix-characters (* depth (1+ depth))))
    (expect (second measurement) :to-equal expected-prefix-characters)
    (expect (third measurement)
           :to-equal (+ (first measurement) expected-prefix-characters)))
  (dolist (series '((:quote (64 256 1024))
                    (:list (8 17 35))
                    (:bracket (512 2048 8192))
                    (:delimiter (512 2048 8192))
                    (:long-line (65536 262144 1048576))))
    (resource-measure-scaling-series (first series) (second series))))
