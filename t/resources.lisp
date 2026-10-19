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

(defun resource-input-utf8-byte-length (input)
  (loop for character across input
        for code = (char-code character)
        sum (cond ((<= code #x7f) 1)
                  ((<= code #x7ff) 2)
                  ((<= code #xffff) 3)
                  (t 4))))

(defun resource-seconds (ticks)
  (/ (float ticks 1d0) internal-time-units-per-second))

(defun resource-report-pathological-case (name input expected-length)
  (let ((characters (length input))
        (bytes (resource-input-utf8-byte-length input)))
    (assert (plusp characters))
    (when expected-length
      (assert (= characters expected-length)))
    (sb-ext:gc :full t)
    (let ((wall-start (get-internal-real-time))
          (cpu-start (get-internal-run-time)))
      (handler-case
          (let ((document (cl-markdown-kit:parse-string input)))
            (unless (typep document 'cl-markdown-kit:document)
              (error "Pathological case ~A returned a non-document." name))
            (format t "T-007 resource case=~A chars=~D bytes=~D wall=~,6Fs cpu=~,6Fs result=DOCUMENT~%"
                    name characters bytes
                    (resource-seconds (- (get-internal-real-time) wall-start))
                    (resource-seconds (- (get-internal-run-time) cpu-start)))
            document)
        (cl-markdown-kit:markdown-resource-limit-error (condition)
          (format t "T-007 resource case=~A chars=~D bytes=~D wall=~,6Fs cpu=~,6Fs result=RESOURCE-LIMIT name=~A limit=~S actual=~S~%"
                  name characters bytes
                  (resource-seconds (- (get-internal-real-time) wall-start))
                  (resource-seconds (- (get-internal-run-time) cpu-start))
                  (cl-markdown-kit:markdown-resource-limit-error-limit-name condition)
                  (cl-markdown-kit:markdown-resource-limit-error-limit condition)
                  (cl-markdown-kit:markdown-resource-limit-error-actual condition))
          condition)))))

(describe "cmark-gfm pathological inputs: emphasis and delimiters (7 cases)"
  (it "parses nested strong emphasis delimiters"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "nested strong emph"
     (concatenate 'string
                  (resource-repeat-string "*a **a " 65000)
                  "b"
                  (resource-repeat-string " a** a*" 65000))
     910001))
  (it "scans unmatched emphasis closers"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "many emph closers with no openers"
     (resource-repeat-string "a_ " 65000) 195000))
  (it "scans unmatched emphasis openers"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "many emph openers with no closers"
     (resource-repeat-string "_a " 65000) 195000))
  (it "handles mismatched emphasis openers and closers"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "mismatched openers and closers"
     (resource-repeat-string "*a_ " 50000) 200000))
  (it "handles delimiter counts that are multiples of three"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "openers and closers multiple of 3"
     (concatenate 'string "a**b" (resource-repeat-string "c* " 50000))
     150004))
  (it "scans emphasis closers after link openers"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "link openers and emph closers"
     (resource-repeat-string "[ a_" 50000) 200000))
  (it "handles the hard link and emphasis delimiter case"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "hard link/emph case" "**x [a*b**c*](d)" 16)))

(describe "cmark-gfm pathological inputs: links and brackets (7 cases)"
  (it "scans unmatched link closers"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "many link closers with no openers"
     (resource-repeat-string "a]" 65000) 130000))
  (it "scans unmatched link openers"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "many link openers with no closers"
     (resource-repeat-string "[a" 65000) 130000))
  (it "scans repeated bracket and parenthesis patterns"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "pattern [ (]( repeated"
     (resource-repeat-string "[ (](" 80000) 400000))
  (it "scans repeated image and link delimiter patterns"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "pattern ![[]() repeated"
     (resource-repeat-string "![[]()" 160000) 960000))
  (it "handles nested link brackets"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "nested brackets"
     (concatenate 'string
                  (make-string 50000 :initial-element #\[)
                  "a"
                  (make-string 50000 :initial-element #\]))
     100001))
  (it "handles the first unclosed link destination shape"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "unclosed links A" (resource-repeat-string "[a](<b" 30000) 180000))
  (it "handles the second unclosed link destination shape"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "unclosed links B" (resource-repeat-string "[a](b" 30000) 150000)))

(describe "cmark-gfm pathological inputs: block nesting (2 cases)"
  (it "handles deeply nested block quotes"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "nested block quotes"
     (concatenate 'string (resource-repeat-string "> " 50000) "a")
     100001))
  (it "handles deeply nested lists"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "deeply nested lists" (resource-input-nested-lists 1000 "* a") 1003000)))

(describe "cmark-gfm pathological inputs: other scanning cases (3 cases)"
  (it "handles U+0000 in the input"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "U+0000 in input"
     (concatenate 'string "abc" (string (code-char 0)) "de" (string (code-char 0)))
     7))
  (it "scans increasing runs of backticks"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "backticks" (resource-input-backticks) 12502499))
  (it "scans an unclosed HTML comment opener sequence"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "unclosed <!--"
     (concatenate 'string "</" (resource-repeat-string "<!--" 300000))
     1200002)))

(describe "cmark-gfm pathological inputs: GFM table (1 case)"
  (it "scans repeated carriage-return and vertical-tab table rows"
    (:timeout-ms 15000)
    (resource-report-pathological-case "tables" (resource-input-table-rows) 330000)))

(describe "cmark-gfm pathological inputs: reference hash collision (1 case)"
  (it "builds and parses all active reference collision definitions"
    (:timeout-ms 15000)
    (resource-report-pathological-case
     "reference collisions" (resource-input-reference-collisions) 1143036)))

(defparameter *resource-pathological-category-counts*
  '((:emphasis-and-delimiter . 7)
    (:link-and-bracket . 7)
    (:block-nesting . 2)
    (:other-scanning . 3)
    (:gfm-table . 1)
    (:reference-collision . 1)))

(it "keeps the active pathological input inventory at 21 cases"
  (:timeout-ms 1000)
  (expect (mapcar #'cdr *resource-pathological-category-counts*)
          :to-equal '(7 7 2 3 1 1))
  (expect (reduce #'+ *resource-pathological-category-counts* :key #'cdr)
          :to-equal 21))

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
  (/ larger smaller))

(defun resource-measure-scaling-series (kind sizes)
  (let* ((inputs (mapcar (lambda (size)
                          (resource-input-for-scaling kind size))
                        sizes))
         (character-sizes (mapcar #'length inputs))
         (wall-samples (make-list +resource-scaling-sample-count+
                                 :initial-element nil))
         (cpu-samples (make-list +resource-scaling-sample-count+
                                :initial-element nil))
         (iteration-counts (make-list +resource-scaling-sample-count+
                                      :initial-element 0)))
    (assert (= (length sizes) 3))
    (assert (<= 1.8d0
                (/ (second character-sizes)
                   (float (first character-sizes) 1d0))
                2.2d0))
    (assert (<= 1.8d0
                (/ (third character-sizes)
                   (float (second character-sizes) 1d0))
                2.2d0))
    (assert (<= 3.6d0
                (/ (third character-sizes)
                   (float (first character-sizes) 1d0))
                4.4d0))
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
           (first-second
             (resource-time-ratio (second median-cpus)
                                  (first median-cpus)))
           (second-fourth
             (resource-time-ratio (third median-cpus)
                                  (second median-cpus)))
           (first-fourth
             (resource-time-ratio (third median-cpus)
                                  (first median-cpus))))
      (format t "T-007 scaling kind=~A samples=~D chars=~S batch-iterations=~S batch-wall=~S batch-cpu=~S median-batch-wall=~S median-per-parse-cpu=~S cpu-ratios=~S~%"
              kind +resource-scaling-sample-count+ character-sizes
              iteration-counts wall-samples cpu-samples
              median-walls median-cpus
              (list first-second second-fourth first-fourth))
      (finish-output)
      (assert (<= first-second 5.0d0))
      (assert (<= second-fourth 5.0d0))
      ;; Fourfold input growth below 10x remains separated from quadratic 16x growth.
      (assert (<= first-fourth 10.0d0)))))

(it "measures five families at approximately 1x, 2x, and 4x source sizes"
  (:timeout-ms 30000)
  (dolist (series '((:quote (128 256 512))
                    (:list (32 45 65))
                    (:bracket (2048 4096 8192))
                    (:delimiter (2048 4096 8192))
                    (:long-line (65536 131072 262144))))
    (resource-measure-scaling-series (first series) (second series))))
