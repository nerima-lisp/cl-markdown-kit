(in-package #:coverage-gate)

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

(defun validate-row-header (path line-number line fields header-p)
  (let ((prior-header-p header-p))
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
    (when (and prior-header-p (equal fields *columns*))
      (gate-fail
       (format nil "~A line ~D: duplicate header row" path line-number)))
    (when header-p
      (unless (equal fields *columns*)
        (when (> line-number 1)
          (validate-row-field-count path line-number fields))))
    (values header-p (and header-p (not (equal fields *columns*))))))

(defun validate-row-field-count (path line-number fields)
  (let ((expected-count (length *columns*)))
    (unless (= (length fields) expected-count)
      (gate-fail
       (format nil
               "~A line ~D: expected ~D tab-separated columns, got ~D"
               path
               line-number
               expected-count
               (length fields))))))

(defun parse-coverage-row-counts (path line-number fields)
  (let ((file (first fields))
        (line-covered
          (parse-integer-field path line-number "line-covered" (second fields)))
        (line-total
          (parse-integer-field path line-number "line-total" (third fields)))
        (branch-covered
          (parse-integer-field path line-number "branch-covered" (fourth fields)))
        (branch-total
          (parse-integer-field path line-number "branch-total" (fifth fields)))
        (uncovered (sixth fields)))
    (when (trim-empty-p file)
      (gate-fail
       (format nil "~A line ~D: file must not be empty" path line-number)))
    (when (> line-covered line-total)
      (gate-fail
       (format nil
               "~A line ~D: line-covered cannot exceed line-total"
               path
               line-number)))
    (when (> branch-covered branch-total)
      (gate-fail
       (format nil
               "~A line ~D: branch-covered cannot exceed branch-total"
               path
               line-number)))
    (values file
            line-covered
            line-total
            branch-covered
            branch-total
            uncovered)))

(defun parse-coverage-row (path line-number fields)
  (multiple-value-bind (file line-covered line-total branch-covered branch-total uncovered)
      (parse-coverage-row-counts path line-number fields)
    (let ((uncovered-ranges
            (parse-uncovered-ranges path line-number uncovered)))
      (multiple-value-bind (branch-locations location-covered location-total)
          (parse-branch-locations path line-number (seventh fields))
        (unless (and (= location-covered branch-covered)
                     (= location-total branch-total))
          (gate-fail
           (format nil
                   "~A line ~D: branch-locations do not match branch counts"
                   path
                   line-number)))
        (list :file file
              :counts
              (vector line-covered line-total branch-covered branch-total)
              :uncovered uncovered
              :uncovered-ranges uncovered-ranges
              :branch-locations branch-locations
              :ranges nil
              :values nil)))))

(defun read-rows-from-stream (path stream)
  (let ((rows nil)
        (line-number 0)
        (header-p nil))
    (loop for line = (read-line stream nil nil)
          while line
          do (incf line-number)
             (unless (and (plusp (length line))
                          (char= (char line 0) #\#))
               (let ((fields (split-on-character line #\Tab)))
                 (multiple-value-bind (next-header-p data-row-p)
                     (validate-row-header path line-number line fields header-p)
                   (setf header-p next-header-p)
                   (when data-row-p
                     (push (parse-coverage-row path line-number fields) rows))))))
    (when (zerop line-number)
      (gate-fail (format nil "~A is empty" path)))
    (unless header-p
      (gate-fail (format nil "~A has no header row" path)))
    (unless rows
      (gate-fail (format nil "~A has no data rows" path)))
    (nreverse rows)))

(defun read-rows (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (read-rows-from-stream path stream)))

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
