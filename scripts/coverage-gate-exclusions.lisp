(in-package #:coverage-gate)

(defun coverage-whitespace-p (character)
  (find character '(#\Space #\Tab #\Return #\Newline #\Page)))

(defun ascii-alpha-p (character)
  (or (char<= #\a (char-downcase character) #\z)))

(defun lisp-token-character-p (character)
  (or (ascii-alpha-p character)
      (ascii-digit-p character)
      (find character '(#\* #\+ #\! #\? #\_ #\% #\- #\: #\. #\/))))

(defun strip-lisp-strings-and-comments (line &optional scanner-state)
  (let ((clean (copy-seq line))
        (block-comment-depth (if scanner-state
                                 (aref scanner-state 0)
                               0))
        (string-open-p (and scanner-state
                            (> (length scanner-state) 1)
                            (aref scanner-state 1))))
    (labels ((mask-range (start end)
                         (loop for index from start below end
                               do (setf (char clean index) #\Space)))
             (string-content-end (start)
                                 (loop with index = start
                                       while (< index (length line))
                                       for character = (char line index)
                                       do (cond
                                           ((char= character #\\)
                                            (if (< (1+ index) (length line))
                                                (incf index 2)
                                              (return (values (length line) nil))))
                                           ((char= character #\")
                                            (return (values (1+ index) t)))
                                           (t (incf index)))
                                       finally (return (values index nil))))
             (character-end (start)
                            (let ((index (+ start 2)))
                              (when (< index (length line))
                                (let ((literal (char line index)))
                                  (incf index)
                                  (when (ascii-alpha-p literal)
                                    (loop while (and (< index (length line))
                                                     (or (ascii-alpha-p (char line index))
                                                         (ascii-digit-p (char line index))
                                                         (char= (char line index) #\-)))
                                          do (incf index)))))
                              index)))
      (loop with index = 0
            while (< index (length line))
            for character = (char line index)
            do (cond
                (string-open-p
                 (multiple-value-bind (end closed-p)
                     (string-content-end index)
                   (mask-range index end)
                   (setf index end
                         string-open-p (not closed-p))))
                ((plusp block-comment-depth)
                 (cond
                  ((and (char= character #\#)
                        (< (1+ index) (length line))
                        (char= (char line (1+ index)) #\|))
                   (mask-range index (+ index 2))
                   (incf block-comment-depth)
                   (incf index 2))
                  ((and (char= character #\|)
                        (< (1+ index) (length line))
                        (char= (char line (1+ index)) #\#))
                   (mask-range index (+ index 2))
                   (decf block-comment-depth)
                   (incf index 2))
                  (t
                   (mask-range index (1+ index))
                   (incf index))))
                ((char= character #\")
                 (mask-range index (1+ index))
                 (multiple-value-bind (end closed-p)
                     (string-content-end (1+ index))
                   (mask-range (1+ index) end)
                   (setf index end
                         string-open-p (not closed-p))))
                ((and (char= character #\#)
                      (< (1+ index) (length line))
                      (char= (char line (1+ index)) #\\))
                 (let ((end (character-end index)))
                   (mask-range index end)
                   (setf index end)))
                ((char= character #\;)
                 (mask-range index (length line))
                 (return))
                ((and (char= character #\#)
                      (< (1+ index) (length line))
                      (char= (char line (1+ index)) #\|))
                 (mask-range index (+ index 2))
                 (setf block-comment-depth 1)
                 (incf index 2))
                (t
                 (incf index))))
      (when scanner-state
        (setf (aref scanner-state 0) block-comment-depth)
        (when (> (length scanner-state) 1)
          (setf (aref scanner-state 1) string-open-p)))
      clean)))

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
  (loop for colon = (position #\: line)
          then (and colon (position #\: line :start (1+ colon)))
        while colon
        when (and (or (zerop colon)
                      (not (lisp-token-character-p (char line (1- colon)))))
                  (or (zerop colon)
                      (not (char= (char line (1- colon)) #\#))))
          do (multiple-value-bind (value end)
                 (handler-case
                     (let ((*read-eval* nil)
                           (*package* (find-package "KEYWORD")))
                       (read-from-string line nil nil :start colon))
                   (error () (values nil colon)))
               (when (and (eq value :report)
                          (> end colon)
                          (or (= end (length line))
                              (not (lisp-token-character-p (char line end)))))
                 (return (values t end))))
        finally (return (values nil nil))))

(defun matching-rule (rules head name)
  (find-if
   (lambda (rule)
     (and (string= (getf rule :head) head)
          (or (null (getf rule :name))
              (and name (string= (getf rule :name) name)))))
   rules))

(defun source-exclusion-lines (path rules)
  (let* ((lines
           (with-open-file (stream path :direction :input :external-format :utf-8)
             (loop for line = (read-line stream nil nil)
                   while line
                   collect line)))
         (constant-symbols
           (source-constant-symbol-keys
            (uiop:pathname-directory-pathname path)))
         (ranges nil)
         (depth 0)
         (scanner-state (vector 0 nil)))
    (loop for original in lines
          for index from 0
          for scanner-state-before = (copy-seq scanner-state)
          for clean = (strip-lisp-strings-and-comments original scanner-state)
          do (when (zerop depth)
               (multiple-value-bind (head name form-open)
                   (first-form-head-and-name clean)
                 (when head
                   (let ((rule (matching-rule rules head name)))
                     (when rule
                       (setf ranges
                             (or (record-source-exclusion
                                  lines index clean form-open head name rule
                                  constant-symbols ranges
                                  scanner-state-before)
                                 ranges)))))))
          (incf depth
                (- (count-character clean #\()
                   (count-character clean #\)))))
    (append (nreverse ranges)
            (nested-form-exclusion-lines lines rules constant-symbols))))

(defun source-form-end-line (lines start-index open
                              &optional (scanner-state (vector 0 nil)))
  (let ((balance 0)
        (scan-state (copy-seq scanner-state)))
    (loop for candidate from start-index below (length lines)
          for part = (strip-lisp-strings-and-comments
                      (nth candidate lines) scan-state)
          do (when (= candidate start-index)
               (setf part (subseq part open)))
             (loop for position below (length part)
                   for character = (char part position)
                   do (cond
                        ((char= character #\() (incf balance))
                        ((char= character #\))
                         (decf balance)
                         (when (zerop balance)
                           (let ((trailing-code-p
                                   (position-if-not
                                    (lambda (trailing)
                                      (or (coverage-whitespace-p trailing)
                                          (char= trailing #\))))
                                    part
                                    :start (1+ position))))
                             (return-from source-form-end-line
                               (values (1+ candidate)
                                       (not (null trailing-code-p))))))))))
    (values nil nil)))

(defun source-form-excludable-p (lines start-index start-position end-line head
                                       &optional constant-symbols)
  (labels ((source-package-name ()
                                (loop with package-name = (package-name (find-package "CL-USER"))
                                      with depth = 0
                                      with scanner-state = (vector 0 nil)
                                      for index below start-index
                                      for clean = (strip-lisp-strings-and-comments
                                                   (nth index lines) scanner-state)
                                      do (when (zerop depth)
                                           (multiple-value-bind (form-head name)
                                               (first-form-head-and-name clean)
                                             (when (and form-head name (string= form-head "in-package"))
                                               (let* ((token (string-upcase name))
                                                      (name (if (and (>= (length token) 2)
                                                                     (string= token "#:" :end1 2))
                                                                (subseq token 2)
                                                              (string-left-trim ":" token)))
                                                      (package (find-package name)))
                                                 (setf package-name
                                                       (if package (package-name package) name))))))
                                      (incf depth
                                            (- (count-character clean #\()
                                               (count-character clean #\))))
                                      finally (return package-name)))
           (source-text ()
                        (with-output-to-string (stream)
                          (loop for index from start-index below end-line
                                do (write-string
                                    (if (= index start-index)
                                        (subseq (nth index lines) start-position)
                                      (nth index lines))
                                    stream)
                                (terpri stream))))
           (qualified-token-parts (token)
                                  (let ((separator (position #\: token :start 1)))
                                    (when (and separator
                                               (plusp separator)
                                               (< (1+ separator) (length token)))
                                      (let ((name-start
                                             (if (and (< (1+ separator) (length token))
                                                      (char= (char token (1+ separator)) #\:))
                                                 (+ separator 2)
                                               (1+ separator))))
                                        (when (< name-start (length token))
                                          (values (subseq token 0 separator)
                                                  (subseq token name-start)))))))
           (reader-text (source)
                        (with-output-to-string (stream)
                          (loop with index = 0
                                while (< index (length source))
                                for character = (char source index)
                                do (cond
                                    ((char= character #\")
                                     (write-char character stream)
                                     (incf index)
                                     (loop while (< index (length source))
                                           for inner = (char source index)
                                           do (write-char inner stream)
                                           (incf index)
                                           (cond
                                            ((char= inner #\\)
                                             (when (< index (length source))
                                               (write-char (char source index) stream)
                                               (incf index)))
                                            ((char= inner #\") (return)))))
                                    ((char= character #\;)
                                     (loop while (and (< index (length source))
                                                      (not (char= (char source index)
                                                                  #\Newline)))
                                           do (write-char (char source index) stream)
                                           (incf index)))
                                    ((and (char= character #\#)
                                          (< (1+ index) (length source))
                                          (char= (char source (1+ index)) #\\))
                                     (write-string "#\\" stream)
                                     (incf index 2)
                                     (when (< index (length source))
                                       (write-char (char source index) stream)
                                       (incf index)
                                       (when (and (ascii-alpha-p (char source (1- index)))
                                                  (< index (length source)))
                                         (loop while (and (< index (length source))
                                                          (or (ascii-alpha-p (char source index))
                                                              (ascii-digit-p (char source index))
                                                              (char= (char source index) #\-)))
                                               do (write-char (char source index) stream)
                                               (incf index))))) ((char= character #\|)
                                                                 (write-char character stream)
                                                                 (incf index)
                                                                 (loop while (< index (length source))
                                                                       for inner = (char source index)
                                                                       do (write-char inner stream)
                                                                       (incf index)
                                                                       (cond
                                                                        ((char= inner #\\)
                                                                         (when (< index (length source))
                                                                           (write-char (char source index) stream)
                                                                           (incf index)))
                                                                        ((char= inner #\|) (return))))) ((lisp-token-character-p character)
                                                                                                         (multiple-value-bind (token end)
                                                                                                             (scan-token source index #'lisp-token-character-p)
                                                                                                           (multiple-value-bind (package-name symbol-name)
                                                                                                               (qualified-token-parts token)
                                                                                                             (if package-name
                                                                                                                 (format stream
                                                                                                                         "#:|%COVERAGE-QUALIFIED%~D:~A~A|"
                                                                                                                         (length package-name)
                                                                                                                         package-name
                                                                                                                         symbol-name)
                                                                                                               (write-string token stream)))
                                                                                                           (setf index end))) (t
                                                                                                                               (write-char character stream)
                                                                                                                               (incf index)))))) (read-form ()
                                                                                                                                                            (let ((*read-eval* nil)
                                                                                                                                                                  (*package* (find-package "CL-USER")))
                                                                                                                                                              (handler-case
                                                                                                                                                                  (read-from-string (reader-text (source-text)) nil :eof)
                                                                                                                                                                (error () :unreadable)))) (symbol-components (value source-package)
                                                                                                                                                                                                             (when (symbolp value)
                                                                                                                                                                                                               (let* ((name (symbol-name value))
                                                                                                                                                                                                                      (marker "%COVERAGE-QUALIFIED%")
                                                                                                                                                                                                                      (package-length-start (length marker)))
                                                                                                                                                                                                                 (if (and (>= (length name) package-length-start)
                                                                                                                                                                                                                          (string= name marker :end1 package-length-start))
                                                                                                                                                                                                                     (let* ((colon (position #\: name :start package-length-start))
                                                                                                                                                                                                                            (package-length
                                                                                                                                                                                                                             (and colon
                                                                                                                                                                                                                                  (ignore-errors
                                                                                                                                                                                                                                   (parse-integer name
                                                                                                                                                                                                                                                  :start package-length-start
                                                                                                                                                                                                                                                  :end colon))))
                                                                                                                                                                                                                            (package-start (and colon (1+ colon)))
                                                                                                                                                                                                                            (symbol-start
                                                                                                                                                                                                                             (and package-start package-length
                                                                                                                                                                                                                                  (+ package-start package-length))))
                                                                                                                                                                                                                       (when (and symbol-start (<= symbol-start (length name)))
                                                                                                                                                                                                                         (let* ((declared-package
                                                                                                                                                                                                                                 (string-upcase
                                                                                                                                                                                                                                  (subseq name package-start symbol-start)))
                                                                                                                                                                                                                                (package (find-package declared-package)))
                                                                                                                                                                                                                           (values (if package (package-name package) declared-package)
                                                                                                                                                                                                                                   (string-upcase (subseq name symbol-start)))))) (let ((package (symbol-package value)))
                                                                                                                                                                                                                                                                                    (when package
                                                                                                                                                                                                                                                                                      (values (if (eq package (find-package "CL-USER"))
                                                                                                                                                                                                                                                                                                  source-package
                                                                                                                                                                                                                                                                                                (package-name package))
                                                                                                                                                                                                                                                                                              (string-upcase name)))))))
                                                                                                                                                                                                             )
           (named-p (value expected &optional (package-name "COMMON-LISP"))
                    (multiple-value-bind (actual-package actual-name)
                        (symbol-components value (source-package-name))
                      (and actual-package actual-name
                           (string-equal actual-package package-name)
                           (string-equal actual-name expected))))
           (proper-list-p (value)
                          (and (listp value)
                               (or (null value) (integerp (list-length value)))))
           (literal-p (value)
                      (or (null value) (eq value t) (keywordp value) (numberp value)
                          (characterp value) (stringp value)
                          (and (consp value) (named-p (first value) "QUOTE")
                               (proper-list-p value) (= (length value) 2))
                          (and (vectorp value)
                               (loop for element across value always (literal-p element)))))
           (safe-value-p (value)
                         (or (literal-p value)
                             (and (symbolp value)
                                  (or (constantp value)
                                      (multiple-value-bind (package-name symbol-name)
                                          (symbol-components value (source-package-name))
                                        (and package-name symbol-name
                                             (member (cons package-name symbol-name)
                                                     constant-symbols
                                                     :test #'equal)))))))
           (parameter-name-p (value)
                             (and (symbolp value)
                                  (not (keywordp value))
                                  (not (and (plusp (length (symbol-name value)))
                                            (char= (char (symbol-name value) 0) #\&)))))
           (optional-parameter-p (parameter)
                                 (or (parameter-name-p parameter)
                                     (and (proper-list-p parameter)
                                          (<= 1 (length parameter) 3)
                                          (parameter-name-p (first parameter))
                                          (or (< (length parameter) 2)
                                              (safe-value-p (second parameter)))
                                          (or (< (length parameter) 3)
                                              (parameter-name-p (third parameter))))))
           (key-parameter-p (parameter)
                            (or (parameter-name-p parameter)
                                (and (proper-list-p parameter)
                                     (<= 1 (length parameter) 3)
                                     (let ((name (first parameter)))
                                       (or (parameter-name-p name)
                                           (and (proper-list-p name)
                                                (= (length name) 2)
                                                (keywordp (first name))
                                                (parameter-name-p (second name)))))
                                     (or (< (length parameter) 2)
                                         (safe-value-p (second parameter)))
                                     (or (< (length parameter) 3)
                                         (parameter-name-p (third parameter))))))
           (aux-parameter-p (parameter)
                            (or (parameter-name-p parameter)
                                (and (proper-list-p parameter)
                                     (<= 1 (length parameter) 2)
                                     (parameter-name-p (first parameter))
                                     (or (< (length parameter) 2)
                                         (safe-value-p (second parameter))))))
           (constructor-lambda-list-p (lambda-list)
                                      (and (proper-list-p lambda-list)
                                           (let ((mode :required))
                                             (and
                                              (loop for parameter in lambda-list
                                                    always
                                                    (cond
                                                     ((named-p parameter "&OPTIONAL")
                                                      (if (member mode '(:required :optional))
                                                          (progn (setf mode :optional) t)
                                                        nil))
                                                     ((named-p parameter "&REST")
                                                      (if (member mode '(:required :optional))
                                                          (progn (setf mode :rest-variable) t)
                                                        nil))
                                                     ((named-p parameter "&KEY")
                                                      (if (member mode '(:required :optional :after-rest))
                                                          (progn (setf mode :key) t)
                                                        nil))
                                                     ((named-p parameter "&ALLOW-OTHER-KEYS")
                                                      (if (eq mode :key)
                                                          (progn (setf mode :allow-other-keys) t)
                                                        nil))
                                                     ((named-p parameter "&AUX")
                                                      (if (member mode '(:required :optional :after-rest :key
                                                                                   :allow-other-keys))
                                                          (progn (setf mode :aux) t)
                                                        nil))
                                                     ((eq mode :rest-variable)
                                                      (if (parameter-name-p parameter)
                                                          (progn (setf mode :after-rest) t)
                                                        nil))
                                                     ((eq mode :required) (parameter-name-p parameter))
                                                     ((eq mode :optional) (optional-parameter-p parameter))
                                                     ((eq mode :key) (key-parameter-p parameter))
                                                     ((eq mode :aux) (aux-parameter-p parameter))
                                                     (t nil)))
                                              (not (eq mode :rest-variable))))))
           (slot-p (slot)
                   (or (parameter-name-p slot)
                       (and (proper-list-p slot)
                            (<= 1 (length slot))
                            (parameter-name-p (first slot))
                            (or (< (length slot) 2)
                                (safe-value-p (second slot)))
                            (evenp (length (cddr slot)))
                            (loop for (option value) on (cddr slot) by #'cddr
                                  always
                                  (and (member option '(:type :read-only))
                                       (or (eq option :type)
                                           (safe-value-p value)))))))
           (constructor-option-p (option)
                                 (and (proper-list-p option)
                                      (<= 1 (length option) 3)
                                      (or (< (length option) 2)
                                          (or (null (second option))
                                              (parameter-name-p (second option))))
                                      (or (< (length option) 3)
                                          (constructor-lambda-list-p (third option)))))
           (defstruct-option-p (option)
                               (and (proper-list-p option)
                                    (consp option)
                                    (case (first option)
                                      (:constructor (constructor-option-p option))
                                      (:include (and (<= 2 (length option))
                                                     (symbolp (second option))
                                                     (every #'slot-p (cddr option))))
                                      ((:conc-name :copier :predicate)
                                       (and (= (length option) 2)
                                            (or (null (second option))
                                                (parameter-name-p (second option)))))
                                      (:named (= (length option) 1))
                                      (:type (and (= (length option) 2)
                                                  (or (member (second option) '(:list :vector))
                                                      (and (consp (second option))
                                                           (member (first (second option))
                                                                   '(vector simple-vector))))))
                                      (:initial-offset (and (= (length option) 2)
                                                            (safe-value-p (second option))))
                                      (otherwise nil))))
           (defstruct-form-p (form)
                             (and (proper-list-p form)
                                  (>= (length form) 2)
                                  (named-p (first form) "DEFSTRUCT")
                                  (let* ((name-spec (second form))
                                         (name (if (symbolp name-spec)
                                                   name-spec
                                                 (and (proper-list-p name-spec)
                                                      (first name-spec))))
                                         (options (if (symbolp name-spec)
                                                      nil
                                                    (rest name-spec)))
                                         (slots (cddr form))
                                         (constructor-options
                                          (remove-if-not
                                           (lambda (option)
                                             (and (consp option)
                                                  (eq (first option) :constructor)))
                                           options)))
                                    (and (parameter-name-p name)
                                         (every #'defstruct-option-p options)
                                         (<= (length constructor-options) 1)
                                         (every #'slot-p slots)))))
           (condition-type-spec-p (value)
                                  (or (symbolp value)
                                      (numberp value)
                                      (stringp value)
                                      (and (proper-list-p value)
                                           (every #'condition-type-spec-p value))))
           (condition-slot-option-p (option value)
                                    (case option
                                      ((:reader :writer :accessor) (symbolp value))
                                      (:initarg (keywordp value))
                                      (:initform (safe-value-p value))
                                      (:type (condition-type-spec-p value))
                                      (:documentation (stringp value))
                                      (otherwise nil)))
           (condition-slot-p (slot)
                             (or (symbolp slot)
                                 (and (proper-list-p slot)
                                      (consp slot)
                                      (symbolp (first slot))
                                      (evenp (length (cdr slot)))
                                      (loop for (option value) on (cdr slot) by #'cddr
                                            always (condition-slot-option-p option value)))))
           (condition-option-p (option)
                               (and (proper-list-p option)
                                    (consp option)
                                    (case (first option)
                                      (:default-initargs
                                       (and (evenp (length (cdr option)))
                                            (loop for (initarg value) on (cdr option) by #'cddr
                                                  always (and (keywordp initarg)
                                                              (safe-value-p value)))))
                                      (:report (= (length option) 2))
                                      (:documentation (and (= (length option) 2)
                                                           (stringp (second option))))
                                      (:metaclass (and (= (length option) 2)
                                                       (symbolp (second option))))
                                      (otherwise nil))))
           (condition-form-p (form)
                             (and (proper-list-p form)
                                  (>= (length form) 4)
                                  (named-p (first form) "DEFINE-CONDITION")
                                  (symbolp (second form))
                                  (proper-list-p (third form))
                                  (every #'symbolp (third form))
                                  (proper-list-p (fourth form))
                                  (every #'condition-slot-p (fourth form))
                                  (every #'condition-option-p (nthcdr 4 form)))))
    (let* ((package-name (source-package-name))
           (form (read-form)))
      (declare (ignore package-name))
      (cond
       ((string= head "defstruct")
        (defstruct-form-p form))
       ((string= head "define-node")
        (and (consp form)
             (named-p (first form) "DEFINE-NODE" (source-package-name))
             (defstruct-form-p (second form))))
       ((string= head "defparameter")
        (and (proper-list-p form)
             (<= 3 (length form) 4)
             (symbolp (second form))
             (literal-p (third form))
             (or (= (length form) 3)
                 (stringp (fourth form)))))
       ((string= head "defun")
        (and (proper-list-p form)
             (>= (length form) 3)
             (named-p (first form) "DEFUN")
             (symbolp (second form))
             (constructor-lambda-list-p (third form))))
       ((string= head "define-condition")
        (condition-form-p form))
       ((not (member head
                     '("defstruct" "define-node" "defconstant" "defvar"
                                   "defparameter" "sb-ext:defglobal" "define-condition"
                                   "defun")
                     :test #'string=))
        (not (eq form :unreadable)))
       (t
        (let ((initializer (and (consp form) (third form))))
          (and (consp form)
               (symbolp (second form))
               (or (and (string= head "defvar") (null initializer))
                   (and initializer (literal-p initializer))))))))))

(defun source-defun-header-end-line
    (lines start-index clean form-open head name
           &optional (scanner-state (vector 0 nil)))
  (declare (ignore clean))
  (let ((lambda-line nil)
        (lambda-start nil)
        (lambda-line-scanner-state nil)
        (scan-state (copy-seq scanner-state)))
    (loop for candidate from start-index below (length lines)
          for scanner-state-before = (copy-seq scan-state)
          for part = (strip-lisp-strings-and-comments
                      (nth candidate lines) scan-state)
          do (if (= candidate start-index)
                 (setf lambda-start
                       (find-same-line-lambda-open part form-open head name))
               (setf lambda-start (position #\( part)))
          (when lambda-start
            (setf lambda-line candidate
                  lambda-line-scanner-state scanner-state-before)
            (return)))
    (when lambda-start
      (let ((header-scan-state (copy-seq lambda-line-scanner-state)))
        (loop with balance = 0
              for candidate from lambda-line below (length lines)
              for part = (strip-lisp-strings-and-comments
                          (nth candidate lines) header-scan-state)
              for scan-start = (if (= candidate lambda-line) lambda-start 0)
              do (loop for position from scan-start below (length part)
                       for character = (char part position)
                       do (cond
                           ((char= character #\() (incf balance))
                           ((char= character #\))
                            (decf balance)
                            (when (zerop balance)
                              (let ((body-start
                                     (position-if-not
                                      (function coverage-whitespace-p)
                                      part
                                      :start (1+ position))))
                                (return-from source-defun-header-end-line
                                             (values
                                              (1+ candidate)
                                              (and body-start
                                                   (not
                                                    (char= (char part body-start)
                                                           (code-char 41))))))))))))))
    (values nil nil)))

(defun source-constant-symbol-keys (directory)
  (labels ((package-token-name (token)
                               (let* ((token (string-upcase token))
                                      (name (cond
                                             ((and (>= (length token) 2)
                                                   (string= token "#:" :end1 2))
                                              (subseq token 2))
                                             ((and (plusp (length token))
                                                   (char= (char token 0) #\:))
                                              (subseq token 1))
                                             (t token)))
                                      (package (find-package name)))
                                 (if package (package-name package) name)))
           (symbol-token-key (token package-name)
                             (let* ((token (string-upcase token))
                                    (separator (position #\: token :start 1)))
                               (cond
                                ((and (>= (length token) 2)
                                      (string= token "#:" :end1 2))
                                 nil)
                                (separator
                                 (let ((symbol-start
                                        (if (and (< (1+ separator) (length token))
                                                 (char= (char token (1+ separator)) #\:))
                                            (+ separator 2)
                                          (1+ separator))))
                                   (when (< symbol-start (length token))
                                     (cons (package-token-name
                                            (subseq token 0 separator))
                                           (subseq token symbol-start)))))
                                (t (cons package-name token))))))
    (let ((keys nil))
      (dolist (path (directory
                     (merge-pathnames
                      "*.lisp"
                      (uiop:ensure-directory-pathname directory))))
        (let ((package-name (package-name (find-package "CL-USER")))
              (depth 0)
              (scanner-state (vector 0 nil)))
          (with-open-file (stream path :direction :input :external-format :utf-8)
            (loop for original = (read-line stream nil nil)
                  while original
                  for clean = (strip-lisp-strings-and-comments
                               original scanner-state)
                  do (when (zerop depth)
                       (multiple-value-bind (head name)
                           (first-form-head-and-name clean)
                         (cond
                          ((and head name (string= head "in-package"))
                           (setf package-name (package-token-name name)))
                          ((and head name (string= head "defconstant"))
                           (let ((key (symbol-token-key name package-name)))
                             (when key (push key keys)))))))
                  (incf depth
                        (- (count-character clean #\()
                           (count-character clean #\))))))))
      (nreverse keys))))

(defun nested-form-exclusion-lines (lines rules constant-symbols)
  (let ((stack nil)
        (ranges nil)
        (scanner-state (vector 0 nil)))
    (loop for original in lines
          for index from 0
          for scanner-state-before = (copy-seq scanner-state)
          for clean = (strip-lisp-strings-and-comments original scanner-state)
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
                                     (setf ranges
                                           (or (record-source-exclusion
                                                lines index clean position head name
                                                rule constant-symbols ranges
                                                scanner-state-before)
                                               ranges)))))
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
  (and (not (string= (getf row :uncovered) "-"))
       (every (lambda (range)
                (loop for line from (car range) to (cdr range)
                      always (line-is-excluded-p line ranges)))
              (getf row :uncovered-ranges))))

(defun record-source-exclusion (lines index clean form-open head name rule
                                constant-symbols ranges
                                &optional (scanner-state (vector 0 nil)))
  (let ((start (1+ index)))
    (if (string= (getf rule :kind) "defun-header")
        (multiple-value-bind (header-end same-line-body-p)
            (source-defun-header-end-line
             lines index clean form-open head name scanner-state)
          (multiple-value-bind (form-end trailing-code-p)
              (source-form-end-line lines index form-open scanner-state)
            (when (and header-end
                       form-end
                       (source-form-excludable-p lines index form-open form-end
                                                 head constant-symbols))
              (let* ((header-end
                       (if same-line-body-p (1- header-end) header-end))
                     (excluded-end
                       (if (and trailing-code-p (= header-end form-end))
                           (1- header-end)
                           header-end)))
                (when (<= start excluded-end)
                  (push (list start excluded-end head) ranges))))))
        (multiple-value-bind (form-end trailing-code-p)
            (source-form-end-line lines index form-open scanner-state)
          (when form-end
            (let ((excluded-end form-end)
                  (scan-state (copy-seq scanner-state)))
              (when (string= (getf rule :kind) "condition-declaration")
                (loop for candidate from index below form-end
                      for part = (strip-lisp-strings-and-comments
                                  (nth candidate lines) scan-state)
                      do (multiple-value-bind (report-line-p report-end)
                             (condition-report-line-p part)
                           (when report-line-p
                             (setf excluded-end
                                   (if (every (lambda (character)
                                                (or (coverage-whitespace-p character)
                                                    (char= character #\))))
                                              (subseq part report-end))
                                       (1+ candidate)
                                       candidate))
                             (return)))))
              (when (and trailing-code-p (= excluded-end form-end))
                (decf excluded-end))
              (when (and (<= start excluded-end)
                         (source-form-excludable-p lines index form-open form-end
                                                   head constant-symbols))
                (push (list start excluded-end head) ranges))))))))
