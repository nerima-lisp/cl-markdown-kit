(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defpackage #:coverage-gate (:use #:cl) (:export #:run))

(in-package #:coverage-gate)

(defparameter *columns*
  '("file" "line-covered"
           "line-total"
           "branch-covered"
           "branch-total"
           "uncovered-lines"
           "branch-locations"))

(defparameter *default-line-minimum*
  100)

(defparameter *default-branch-minimum*
  100)

(defparameter *default-source-root*
  "src")

(defparameter *default-exclusions*
  "scripts/coverage-exclusions.sexp")

(defparameter *program-name*
  "check-coverage.lisp")

(define-condition coverage-gate-error
  (error)
  ((message :initarg :message :reader coverage-gate-error-message)
   (usage-p :initarg :usage-p :reader coverage-gate-error-usage-p))
  (:report
   (lambda (condition stream)
     (write-string (coverage-gate-error-message condition) stream))))

(defun gate-fail (message &optional usage-p)
  (error 'coverage-gate-error :message message :usage-p usage-p))

(defun usage (stream)
  (format stream
          "usage: sbcl --script scripts/check-coverage.lisp [--input TSV] [--min-line PERCENT] [--min-branch PERCENT]~%             [--source-root DIRECTORY] [--exclusions FILE]~%~%  --input TSV          per-file coverage summary (default: $COVERAGE_SUMMARY,~%                       else the system temporary directory)~%  --min-line PERCENT   required line coverage (default: $CL_MARKDOWN_COVERAGE_MIN_LINE,~%                       else 100)~%  --min-branch PERCENT required branch coverage (default: $CL_MARKDOWN_COVERAGE_MIN_BRANCH,~%                       else 100)~%  --source-root DIRECTORY source files used to classify exclusions (default: src)~%  --exclusions FILE    exclusion data (default: scripts/coverage-exclusions.sexp)~%~%exit 0 when every file meets both thresholds, 1 when a file is below one, and 2~%when the settings are invalid or the summary cannot be read.~%"))

(defun valid-option-name-p (name)
  (and (plusp (length name))
       (every
        (lambda (character)
          (or (and (char<= #\a character #\z)) (char= character #\-)))
        name)
       (char<= #\a (char name 0) #\z)))

(defun parse-arguments (arguments)
  (let ((given (make-hash-table :test #'equal)))
    (loop while arguments
          for argument = (pop arguments)
          do (unless
                 (and (> (length argument) 2) (string= argument "--" :end1 2))
               (gate-fail (format nil "unexpected argument '~A'" argument) t)) (let ((setting
                                                                                      (subseq
                                                                                       argument
                                                                                       2)))
                                                                                 (unless
                                                                                     (valid-option-name-p
                                                                                      setting)
                                                                                   (gate-fail
                                                                                    (format
                                                                                     nil
                                                                                     "unexpected argument '~A'"
                                                                                     argument)
                                                                                    t))
                                                                                 (unless
                                                                                     (member
                                                                                      setting
                                                                                      '("input"
                                                                                        "min-line"
                                                                                        "min-branch"
                                                                                        "source-root"
                                                                                        "exclusions")
                                                                                      :test
                                                                                      #'equal)
                                                                                   (gate-fail
                                                                                    (format
                                                                                     nil
                                                                                     "unknown option --~A"
                                                                                     setting)
                                                                                    t))
                                                                                 (unless
                                                                                     arguments
                                                                                   (gate-fail
                                                                                    (format
                                                                                     nil
                                                                                     "option --~A requires a value"
                                                                                     setting)
                                                                                    t))
                                                                                 (setf (gethash
                                                                                        setting
                                                                                        given) (pop
                                                                                                arguments))))
    given))

(defun resolve-setting (setting given environment default)
  (multiple-value-bind (value present-p) (gethash setting given)
    (when present-p
      (return-from resolve-setting
        (values value "option")))
    (let ((environment-value (uiop:getenv environment)))
      (when (and environment-value (plusp (length environment-value)))
        (return-from resolve-setting
          (values environment-value environment))))
    (values default "default")))

(defun ascii-digit-p (character)
  (and (char<= #\0 character #\9)))

(defun parse-decimal (string)
  (let ((point (position #\. string)))
    (when
        (or (zerop (length string))
            (and point
                 (or (position #\. string :start (1+ point))
                     (= point (1- (length string)))))
            (and (null point) (zerop (length string))))
      (return-from parse-decimal
        nil))
    (let* ((whole
            (if point
                (subseq string 0 point)
                string))
           (fraction (and point (subseq string (1+ point)))))
      (unless
          (and
           (or (plusp (length whole)) (and fraction (plusp (length fraction))))
           (every #'ascii-digit-p whole)
           (or (null fraction) (every #'ascii-digit-p fraction)))
        (return-from parse-decimal
          nil))
      (let ((whole-value
             (if (plusp (length whole))
                 (parse-integer whole)
                 0)))
        (if fraction
            (+ whole-value
               (/ (parse-integer fraction) (expt 10 (length fraction))))
            whole-value)))))

(defun resolve-percent (setting given environment default)
  (multiple-value-bind (raw origin)
      (resolve-setting setting given environment default)
    (let ((number
           (if (realp raw)
               raw
               (parse-decimal raw))))
      (unless (and number (<= 0 number 100))
        (gate-fail
         (format nil
                 "--~A (~A) must be a percentage in 0..100, got '~A'"
                 setting
                 origin
                 raw)))
      number)))

(defun split-on-character (string separator)
  (loop with start = 0
        for
        end = (position separator string :start start)
        collect (subseq string start end)
        while
        end
        do (setf start (1+ end))))

(defun parse-integer-field (path line-number column value)
  (unless (and (plusp (length value)) (every #'ascii-digit-p value))
    (gate-fail
     (format nil
             "~A line ~D: ~A must be a non-negative integer, got '~A'"
             path
             line-number
             column
             value)))
  (parse-integer value))

(defun parse-uncovered-ranges (path line-number value)
  (unless
      (or (string= value "-")
          (and (plusp (length value))
               (every
                (lambda (character)
                  (or (ascii-digit-p character)
                      (char= character #\-)
                      (char= character #\,)))
                value)))
    (gate-fail
     (format nil
             "~A line ~D: invalid uncovered-lines syntax '~A'"
             path
             line-number
             value)))
  (if (string= value "-")
      nil
      (mapcar
       (lambda (range)
         (let ((parts (split-on-character range #\-)))
           (unless
               (and (<= 1 (length parts) 2)
                    (every
                     (lambda (part)
                       (and (plusp (length part)) (every #'ascii-digit-p part)))
                     parts))
             (gate-fail
              (format nil
                      "~A line ~D: invalid uncovered-lines syntax '~A'"
                      path
                      line-number
                      value)))
           (let* ((start (parse-integer (first parts)))
                  (end
                   (if (second parts)
                       (parse-integer (second parts))
                       start)))
             (when (or (< start 1) (< end start))
               (gate-fail
                (format nil
                        "~A line ~D: uncovered-lines range '~A' is backwards"
                        path
                        line-number
                        range)))
             (cons start end))))
       (split-on-character value #\,))))

(defun parse-branch-locations (path line-number value)
  (if (string= value "-")
      (values nil 0 0)
      (let ((seen (make-hash-table))
            (covered-total 0)
            (branch-total 0))
        (let ((locations
               (mapcar
                (lambda (location)
                  (let* ((colon-parts (split-on-character location #\:))
                         (slash-parts
                          (and (= (length colon-parts) 2)
                               (split-on-character (second colon-parts) #\/))))
                    (unless
                        (and (= (length colon-parts) 2)
                             (= (length slash-parts) 2)
                             (every
                              (lambda (part)
                                (and (plusp (length part))
                                     (every #'ascii-digit-p part)))
                              (list (first colon-parts)
                                    (first slash-parts)
                                    (second slash-parts))))
                      (gate-fail
                       (format nil
                               "~A line ~D: invalid branch-locations entry '~A'"
                               path
                               line-number
                               location)))
                    (let ((source-line (parse-integer (first colon-parts)))
                          (covered (parse-integer (first slash-parts)))
                          (total (parse-integer (second slash-parts))))
                      (unless
                          (and (plusp source-line)
                               (plusp total)
                               (<= covered total))
                        (gate-fail
                         (format nil
                                 "~A line ~D: invalid branch-locations entry '~A'"
                                 path
                                 line-number
                                 location)))
                      (when (gethash source-line seen)
                        (gate-fail
                         (format nil
                                 "~A line ~D: duplicate branch-locations line ~D"
                                 path
                                 line-number
                                 source-line)))
                      (setf (gethash source-line seen) t)
                      (incf covered-total covered)
                      (incf branch-total total)
                      (list source-line covered total))))
                (split-on-character value #\,))))
          (values locations covered-total branch-total)))))

(defun trim-empty-p (string)
  (every
   (lambda (character)
     (find character '(#\Space #\Tab #\Return #\Newline)))
   string))

(defun tab-separated (columns)
  (with-output-to-string (stream)
    (loop for remaining on columns
          do (princ (first remaining) stream) (when (rest remaining)
                                                (write-char #\Tab stream)))))

(defun read-rows (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((rows nil)
          (line-number 0)
          (header-p nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (incf line-number) (unless
                                      (and (plusp (length line))
                                           (char= (char line 0) #\#))
                                    (let ((fields
                                           (split-on-character line #\Tab))
                                          (prior-header-p header-p))
                                      (unless header-p
                                        (unless (equal fields *columns*)
                                          (gate-fail
                                           (format nil
                                                   "~A line ~D: expected header '~A', got '~A'"
                                                   path
                                                   line-number
                                                   (tab-separated *columns*)
                                                   line)))
                                        (setf header-p t))
                                      (when
                                          (and prior-header-p
                                               (equal fields *columns*))
                                        (gate-fail
                                         (format nil
                                                 "~A line ~D: duplicate header row"
                                                 path
                                                 line-number)))
                                      (when header-p
                                        (unless (equal fields *columns*)
                                          (when (> line-number 1)
                                            (let ((expected-count
                                                   (length *columns*)))
                                              (unless
                                                  (= (length fields)
                                                     expected-count)
                                                (gate-fail
                                                 (format nil
                                                         "~A line ~D: expected ~D tab-separated columns, got ~D"
                                                         path
                                                         line-number
                                                         expected-count
                                                         (length fields)))))))
                                        (when
                                            (and header-p
                                                 (not (equal fields *columns*)))
                                          (let* ((expected-count
                                                  (length *columns*))
                                                 (field-count (length fields)))
                                            (when (= expected-count field-count)
                                              (let* ((file (first fields))
                                                     (line-covered
                                                      (parse-integer-field path
                                                                           line-number
                                                                           "line-covered"
                                                                           (second
                                                                            fields)))
                                                     (line-total
                                                      (parse-integer-field path
                                                                           line-number
                                                                           "line-total"
                                                                           (third
                                                                            fields)))
                                                     (branch-covered
                                                      (parse-integer-field path
                                                                           line-number
                                                                           "branch-covered"
                                                                           (fourth
                                                                            fields)))
                                                     (branch-total
                                                      (parse-integer-field path
                                                                           line-number
                                                                           "branch-total"
                                                                           (fifth
                                                                            fields)))
                                                     (uncovered (sixth fields)))
                                                (when (trim-empty-p file)
                                                  (gate-fail
                                                   (format nil
                                                           "~A line ~D: file must not be empty"
                                                           path
                                                           line-number)))
                                                (when
                                                    (> line-covered line-total)
                                                  (gate-fail
                                                   (format nil
                                                           "~A line ~D: line-covered cannot exceed line-total"
                                                           path
                                                           line-number)))
                                                (when
                                                    (> branch-covered
                                                       branch-total)
                                                  (gate-fail
                                                   (format nil
                                                           "~A line ~D: branch-covered cannot exceed branch-total"
                                                           path
                                                           line-number)))
                                                (let ((uncovered-ranges
                                                       (parse-uncovered-ranges
                                                        path
                                                        line-number
                                                        uncovered)))
                                                  (multiple-value-bind (branch-locations
                                                                        location-covered
                                                                        location-total)
                                                      (parse-branch-locations
                                                       path
                                                       line-number
                                                       (seventh fields))
                                                    (unless
                                                        (and
                                                         (= location-covered
                                                            branch-covered)
                                                         (= location-total
                                                            branch-total))
                                                      (gate-fail
                                                       (format nil
                                                               "~A line ~D: branch-locations do not match branch counts"
                                                               path
                                                               line-number)))
                                                    (push
                                                     (list :file
                                                           file
                                                           :counts
                                                           (vector line-covered
                                                                   line-total
                                                                   branch-covered
                                                                   branch-total)
                                                           :uncovered
                                                           uncovered
                                                           :uncovered-ranges
                                                           uncovered-ranges
                                                           :branch-locations
                                                           branch-locations
                                                           :ranges
                                                           nil
                                                           :values
                                                           nil)
                                                     rows)))))))))))
      (when (zerop line-number)
        (gate-fail (format nil "~A is empty" path)))
      (unless header-p
        (gate-fail (format nil "~A has no header row" path)))
      (unless rows
        (gate-fail (format nil "~A has no data rows" path)))
      (nreverse rows))))

(defun read-exclusion-rules (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (let ((*read-eval* nil)
          (eof (gensym "EOF"))
          (rules nil))
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (unless (and (consp form) (keywordp (first form)))
                 (gate-fail
                  (format nil
                          "~A: expected an exclusion rule, got ~S"
                          path
                          form))) (let* ((kind-value (getf form :kind))
                                         (head (getf form :head))
                                         (name (getf form :name))
                                         (reproduction
                                          (getf form :reproduction)))
                                    (unless
                                        (and (symbolp kind-value)
                                             (stringp head)
                                             (or (null name) (stringp name))
                                             (stringp reproduction))
                                      (gate-fail
                                       (format nil
                                               "~A: expected :kind and :head in '~S'"
                                               path
                                               form)))
                                    (unless (probe-file reproduction)
                                      (gate-fail
                                       (format nil
                                               "~A: missing reproduction ~A"
                                               path
                                               reproduction)))
                                    (push
                                     (list :kind
                                           (string-downcase
                                            (symbol-name kind-value))
                                           :head
                                           head
                                           :name
                                           name
                                           :reproduction
                                           reproduction)
                                     rules)))
      (unless rules
        (gate-fail (format nil "~A has no exclusion rules" path)))
      (nreverse rules))))

(defun coverage-whitespace-p (character)
  (find character '(#\Space #\Tab #\Return #\Newline #\Page)))

(defun ascii-alpha-p (character)
  (or (char<= #\a (char-downcase character) #\z)))

(defun lisp-token-character-p (character)
  (or (ascii-alpha-p character)
      (ascii-digit-p character)
      (find character '(#\* #\+ #\! #\? #\_ #\% #\- #\:))))

(defun strip-lisp-strings-and-comments (line)
  (with-output-to-string (stream)
    (loop with index = 0
          while (< index (length line))
          for character = (char line index)
          do (cond
               ((char= character #\")
                 (incf index)
                 (loop while (< index (length line))
                       for inner = (char line index)
                       do (cond
                            ((char= inner #\\) (incf index 2))
                            ((char= inner #\")
                              (incf index)
                              (return))
                            (t (incf index)))))
               ((and (char= character #\#)
                     (< (1+ index) (length line))
                     (char= (char line (1+ index)) #\\))
                 (incf index 2)
                 (when (< index (length line))
                   (if (ascii-alpha-p (char line index))
                       (loop while (and (< index (length line))
                                        (or (ascii-alpha-p (char line index))
                                            (ascii-digit-p (char line index))
                                            (char= (char line index) #\-)))
                             do (incf index))
                       (incf index))))
               ((char= character #\;) (return))
               (t
                 (write-char character stream)
                 (incf index))))))

(defun skip-whitespace (string index)
  (loop while (and (< index (length string))
                   (coverage-whitespace-p (char string index)))
        do (incf index)
        finally (return index)))

(defun scan-token (string index predicate)
  (let ((start index))
    (loop while (and (< index (length string))
                     (funcall predicate (char string index)))
          do (incf index))
    (values (and (< start index) (subseq string start index)) index)))

(defun scan-form-name (string index)
  (let ((start index))
    (loop while (and (< index (length string))
                     (not (coverage-whitespace-p (char string index)))
                     (not (find (char string index) "()")))
          do (incf index))
    (values (and (< start index) (subseq string start index)) index)))

(defun first-form-head-and-name (line)
  (loop for open = (position #\( line) then (and open
                                                 (position #\(
                                                           line
                                                           :start
                                                           (1+ open)))
        while open
        for after-open = (skip-whitespace line (1+ open))
        for (head head-end) = (multiple-value-list
                               (scan-token line
                                           after-open
                                           #'lisp-token-character-p))
        when head
          do (let* ((name-start (skip-whitespace line head-end)))
               (multiple-value-bind (name name-end)
                   (scan-form-name line name-start)
                 (declare (ignore name-end))
                 (return (values head name open))))
        finally (return (values nil nil nil))))

(defun find-same-line-lambda-open (line form-open head name)
  (let ((head-start (skip-whitespace line (1+ form-open))))
    (multiple-value-bind (actual-head head-end)
        (scan-token line head-start #'lisp-token-character-p)
      (unless (and actual-head (string= actual-head head))
        (return-from find-same-line-lambda-open
          nil))
      (let ((name-start (skip-whitespace line head-end)))
        (multiple-value-bind (actual-name name-end)
            (scan-form-name line name-start)
          (unless (and actual-name (string= actual-name name))
            (return-from find-same-line-lambda-open
              nil))
          (let ((lambda-open (skip-whitespace line name-end)))
            (and (< lambda-open (length line))
                 (char= (char line lambda-open) #\()
                 lambda-open)))))))

(defun count-character (string character)
  (count character string))

(defun condition-report-line-p (line)
  (loop for open = (position #\( line) then (and open
                                                 (position #\(
                                                           line
                                                           :start
                                                           (1+ open)))
        while open
        for token-start = (skip-whitespace line (1+ open))
        when (and (<= (+ token-start (length ":report")) (length line))
                  (string= line
                           ":report"
                           :start1
                           token-start
                           :end1
                           (+ token-start 7))
                  (or (= (+ token-start 7) (length line))
                      (not
                       (or (ascii-alpha-p (char line (+ token-start 7)))
                           (ascii-digit-p (char line (+ token-start 7)))
                           (char= (char line (+ token-start 7)) #\_)))))
          do (return t)
        finally (return nil)))

(defun matching-rule (rules head name)
  (find-if
   (lambda (rule)
     (and (string= (getf rule :head) head)
          (or (null (getf rule :name))
              (and name (string= (getf rule :name) name)))))
   rules))

(defun source-exclusion-lines (path rules)
  (let ((lines
         (with-open-file (stream path :direction :input :external-format :utf-8)
           (loop for line = (read-line stream nil nil)
                 while line
                 collect line)))
        (ranges nil)
        (depth 0))
    (loop for original in lines
          for index from 0
          for clean = (strip-lisp-strings-and-comments original)
          do (when (zerop depth)
               (multiple-value-bind (head name form-open)
                   (first-form-head-and-name clean)
                 (when head
                   (let ((rule (or (matching-rule rules head name)
                                   (and (string= head "define-node")
                                        (matching-rule rules "defstruct" nil)))))
                     (when rule
                       (let ((start (1+ index)))
                         (if (string= (getf rule :kind) "defun-header")
                             (let ((lambda-line nil)
                                   (lambda-start nil))
                               (loop for candidate from index below (length
                                                                     lines)
                                     for part = (if (= candidate index)
                                                    clean
                                                  (strip-lisp-strings-and-comments
                                                   (nth candidate lines)))
                                     do (if (= candidate index)
                                            (setf lambda-start (find-same-line-lambda-open
                                                                part
                                                                form-open
                                                                head
                                                                name))
                                          (setf lambda-start (position #\(
                                                                       part))) (when
                                                                                   lambda-start
                                                                                 (setf lambda-line candidate)
                                                                                 (return)))
                               (when lambda-start
                                 (let ((balance 0)
                                       (header-end nil))
                                   (loop for candidate from lambda-line below (length
                                                                               lines)
                                         for part = (strip-lisp-strings-and-comments
                                                     (nth candidate lines))
                                         do (when (= candidate lambda-line)
                                              (setf part (subseq part
                                                                 lambda-start))) (incf
                                                                                  balance
                                                                                  (count-character
                                                                                   part
                                                                                   #\()) (decf
                                                                                          balance
                                                                                          (count-character
                                                                                           part
                                                                                           #\))) (when
                                                                                                     (zerop
                                                                                                      balance)
                                                                                                   (setf header-end (1+
                                                                                                                     candidate))
                                                                                                   (return)))
                                   (when (and header-end (<= start header-end))
                                     (push (list start header-end head) ranges)))))
                           (let ((balance 0)
                                 (end (1- (length lines))))
                             (loop for candidate from index below (length
                                                                   lines)
                                   for part = (strip-lisp-strings-and-comments
                                               (nth candidate lines))
                                   do (incf balance
                                            (count-character part #\()) (decf
                                                                         balance
                                                                         (count-character
                                                                          part
                                                                          #\))) (when
                                                                                    (<=
                                                                                     balance
                                                                                     0)
                                                                                  (setf end candidate)
                                                                                  (return)))
                             (let ((excluded-end (1+ end)))
                               (when
                                   (string= (getf rule :kind)
                                            "condition-declaration")
                                 (loop for candidate from index to
                                       end
                                       for part = (strip-lisp-strings-and-comments
                                                   (nth candidate lines))
                                       when (condition-report-line-p part)
                                       do (setf excluded-end candidate) (return)))
                               (when (and (<= start excluded-end)
                                          (source-form-excludable-p lines index form-open excluded-end head name))
                                 (push (list start excluded-end head) ranges)))))))))))
          (incf depth
                (- (count-character clean #\() (count-character clean #\)))))
    (append (nreverse ranges) (nested-form-exclusion-lines lines rules))))
(defun source-form-end-line (lines start-index open)
  (let ((balance 0))
    (loop for candidate from start-index below (length lines)
          for part = (strip-lisp-strings-and-comments (nth candidate lines))
          do (when (= candidate start-index)
               (setf part (subseq part open)))
             (incf balance (count-character part #\())
             (decf balance (count-character part #\)))
             (when (<= balance 0)
               (return-from source-form-end-line (1+ candidate))))
    nil))
(defun source-form-excludable-p (lines start-index start-position end-line head name)
  (unless (member head
                  '("defstruct" "define-node" "defconstant" "defvar" "defparameter" "sb-ext:defglobal")
                  :test (function string=))
    (return-from source-form-excludable-p t))
  (labels ((named-p (value name)
             (and (symbolp value) (string= (symbol-name value) name)))
           (literal-p (value)
             (or (null value) (eq value t) (keywordp value) (numberp value)
                 (characterp value) (stringp value)
                 (and (consp value) (named-p (first value) "QUOTE")
                      (consp (rest value)) (null (cddr value)))
                 (and (vectorp value)
                      (loop for element across value always (literal-p element)))))
           (source-text ()
             (with-output-to-string (stream)
               (loop for index from start-index below end-line
                     do (write-string
                         (if (= index start-index)
                             (subseq (nth index lines) start-position)
                             (nth index lines))
                         stream)
                        (terpri stream))))
           (reader-text (source)
             (with-output-to-string (stream)
               (loop with index = 0
                     while (< index (length source))
                     for character = (char source index)
                     do (if (lisp-token-character-p character)
                            (multiple-value-bind (token end)
                                (scan-token source index #'lisp-token-character-p)
                              (let ((separator
                                      (position #\: token :start 1 :from-end t)))
                                (write-string
                                 (if separator
                                     (subseq source (+ index separator 1) end)
                                     (subseq source index end))
                                 stream))
                              (setf index end))
                            (progn
                              (write-char character stream)
                              (incf index))))))
           (read-form ()
             (let ((*read-eval* nil)
                   (*package* (find-package "CL-USER")))
               (handler-case
                   (read-from-string (reader-text (source-text)) nil :eof)
                 (error () :unreadable)))))
    (cond
      ((string= head "defstruct")
       t)
      ((string= head "define-node")
       (let ((form (read-form)))
         (and (consp form)
              (named-p (first form) "DEFINE-NODE")
              (consp (second form))
              (named-p (first (second form)) "DEFSTRUCT"))))
      ((string= head "defparameter")
       (or (string-equal name "*html5-entities*")
           (let* ((form (read-form))
                  (initializer (and (consp form) (third form))))
             (and (consp form)
                  (symbolp (second form))
                  initializer
                  (literal-p initializer)))))
      (t
       (let* ((form (read-form))
              (initializer (and (consp form) (third form))))
         (and (consp form) (symbolp (second form))
              (or (and (string= head "defvar") (null initializer))
                  (and initializer (literal-p initializer)))))))))

(defun source-defun-header-end-line (lines start-index clean form-open head name)
  (let ((lambda-line nil)
        (lambda-start nil))
    (loop for candidate from start-index below (length lines)
          for part = (if (= candidate start-index)
                         clean
                         (strip-lisp-strings-and-comments (nth candidate lines)))
          do (if (= candidate start-index)
                 (setf lambda-start
                       (find-same-line-lambda-open part form-open head name))
                 (setf lambda-start (position #\( part)))
             (when lambda-start
               (setf lambda-line candidate)
               (return)))
    (when lambda-start
      (let ((balance 0))
        (loop for candidate from lambda-line below (length lines)
              for part = (strip-lisp-strings-and-comments (nth candidate lines))
              do (when (= candidate lambda-line)
                   (setf part (subseq part lambda-start)))
                 (incf balance (count-character part #\())
                 (decf balance (count-character part #\)))
                 (when (zerop balance)
                   (return-from source-defun-header-end-line (1+ candidate))))))
    nil))
(defun nested-form-exclusion-lines (lines rules)
  (let ((stack nil)
        (ranges nil))
    (loop for original in lines
          for index from 0
          for clean = (strip-lisp-strings-and-comments original)
          do (loop for position from 0 below (length clean)
                   for character = (char clean position)
                   do (cond
                       ((char= character #\()
                        (let ((head-start (skip-whitespace clean (1+ position))))
                          (multiple-value-bind (head head-end)
                              (scan-token clean head-start #'lisp-token-character-p)
                            (multiple-value-bind (name name-end)
                                (scan-form-name clean
                                                (skip-whitespace clean head-end))
                              (declare (ignore name-end))
                              (when (and stack
                                         head
                                         (every
                                          (lambda (ancestor)
                                            (member ancestor
                                                    '("eval-when" "progn")
                                                    :test #'string=))
                                          stack))
                                (let ((rule (matching-rule rules head name)))
                                  (when rule
                                    (let ((end (if (string= (getf rule :kind) "defun-header")
                                                   (source-defun-header-end-line lines index clean position head name)
                                                 (source-form-end-line lines index position))))
                                      (when (and end
                                                 (source-form-excludable-p lines index position end head name))
                                        (push (list (1+ index) end head) ranges))))))
                              (push head stack)))))
                       ((char= character #\))
                        (when stack
                          (pop stack))))))
    (nreverse ranges)))

(defun line-is-excluded-p (line ranges)
  (some
   (lambda (range)
     (<= (first range) line (second range)))
   ranges))

(defun branch-counts-outside-exclusions (row ranges)
  (let* ((counts (getf row :counts))
         (covered (aref counts 2))
         (total (aref counts 3)))
    (dolist (location (getf row :branch-locations))
      (when (line-is-excluded-p (first location) ranges)
        (decf covered (second location))
        (decf total (third location))))
    (values covered total)))

(defun uncovered-lines-excluded-p (row ranges)
  (if (string= (getf row :uncovered) "-")
      (not (null ranges))
      (every
       (lambda (range)
         (loop for line from (car range) to (cdr range)
               always (line-is-excluded-p line ranges)))
       (getf row :uncovered-ranges))))

(defun percentage (covered total)
  (when (plusp total)
    (* 100 (/ covered total))))

(defun percent-text (value)
  (if value
      (format nil "~,1F%" (float value 1.0d0))
      "n/a"))

(defun source-path (source-root file)
  (format nil "~A/~A" (string-right-trim "/" source-root) file))

(defun evaluate-rows (rows rules source-root minimum-line minimum-branch)
  (let ((totals (vector 0 0 0 0))
        (failures nil))
    (dolist (row rows)
      (let* ((source (source-path source-root (getf row :file)))
             (ranges
              (if (probe-file source)
                  (source-exclusion-lines source rules)
                  nil)))
        (multiple-value-bind (branch-covered branch-total)
            (branch-counts-outside-exclusions row ranges)
          (let* ((counts (getf row :counts))
                 (line-value (percentage (aref counts 0) (aref counts 1)))
                 (branch-value (percentage branch-covered branch-total))
                 (reasons nil)
                 (line-failed (and line-value (< line-value minimum-line)))
                 (branch-failed
                  (and branch-value (< branch-value minimum-branch))))
            (when line-failed
              (push
               (format nil
                       "line ~A, required ~A"
                       (percent-text line-value)
                       (percent-text minimum-line))
               reasons))
            (when branch-failed
              (push
               (format nil
                       "branch ~A, required ~A"
                       (percent-text branch-value)
                       (percent-text minimum-branch))
               reasons))
            (setf (getf row :ranges) ranges
                  (getf row :values) (list line-value branch-value))
            (when
                (or branch-failed
                    (and line-failed
                         (not (uncovered-lines-excluded-p row ranges))))
              (push (list :file (getf row :file) :reasons (nreverse reasons))
                    failures))
            (incf (aref totals 0) (aref counts 0))
            (incf (aref totals 1) (aref counts 1))
            (incf (aref totals 2) branch-covered)
            (incf (aref totals 3) branch-total)))))
    (values (nreverse failures) totals)))

(defun print-report (rows failures totals output)
  (format output "| file | line | branch | uncovered lines |~%")
  (format output "|---|---:|---:|---|~%")
  (dolist (row rows)
    (format output
            "| ~A | ~A | ~A | ~A |~%"
            (getf row :file)
            (percent-text (first (getf row :values)))
            (percent-text (second (getf row :values)))
            (getf row :uncovered)))
  (format output
          "| total | ~A | ~A |  |~%"
          (percent-text (percentage (aref totals 0) (aref totals 1)))
          (percent-text (percentage (aref totals 2) (aref totals 3))))
  (if failures
      (progn
        (terpri output)
        (dolist (failure failures)
          (format output
                  "  ~A: ~{~A~^; ~}~%"
                  (getf failure :file)
                  (getf failure :reasons)))
        (dolist (row rows)
          (dolist (location (getf row :branch-locations))
            (unless (= (second location) (third location))
              (format output
                      "  ~A line ~D: ~D/~D branches covered~%"
                      (getf row :file)
                      (first location)
                      (second location)
                      (third location)))))
        (format output
                "coverage gate: FAIL (~D of ~D files below threshold)~%"
                (length failures)
                (length rows))
        1)
      (progn
        (format output
                "coverage gate: PASS (all ~D files at or above threshold)~%"
                (length rows))
        0)))

(defun resolve-input (given)
  (multiple-value-bind (value origin)
      (resolve-setting "input"
                       given
                       "COVERAGE_SUMMARY"
                       (merge-pathnames "cl-markdown-kit-coverage/per-file.tsv"
                                        (uiop:ensure-directory-pathname
                                         (uiop:temporary-directory))))
    (declare (ignore origin))
    value))

(defun execute-gate (arguments output)
  (let* ((given (parse-arguments arguments))
         (input (resolve-input given))
         (source-root (gethash "source-root" given *default-source-root*))
         (exclusions (gethash "exclusions" given *default-exclusions*))
         (minimum-line
          (resolve-percent "min-line"
                           given
                           "CL_MARKDOWN_COVERAGE_MIN_LINE"
                           *default-line-minimum*))
         (minimum-branch
          (resolve-percent "min-branch"
                           given
                           "CL_MARKDOWN_COVERAGE_MIN_BRANCH"
                           *default-branch-minimum*))
         (rows (read-rows input))
         (rules (read-exclusion-rules exclusions)))
    (multiple-value-bind (failures totals)
        (evaluate-rows rows rules source-root minimum-line minimum-branch)
      (print-report rows failures totals output))))

(defun run (arguments &key
                      (output *standard-output*)
                      (error-output *error-output*))
  (handler-case (execute-gate arguments output)
    (coverage-gate-error (condition)
      (format error-output
              "~A: ~A~%"
              *program-name*
              (coverage-gate-error-message condition))
      (when (coverage-gate-error-usage-p condition)
        (usage error-output))
      2)
    (error (condition)
      (format error-output "~A: ~A~%" *program-name* condition)
      2)))
