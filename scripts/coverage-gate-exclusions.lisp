(in-package #:coverage-gate)

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
  (let* ((lines
           (with-open-file (stream path :direction :input :external-format :utf-8)
             (loop for line = (read-line stream nil nil)
                   while line
                   collect line)))
         (constant-symbols
           (source-constant-symbol-names
            (uiop:pathname-directory-pathname path)))
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
                       (setf ranges
                             (or (record-source-exclusion lines index clean
                                                          form-open head name rule
                                                          constant-symbols ranges)
                                 ranges)))))))
          (incf depth
                (- (count-character clean #\() (count-character clean #\)))))
    (append (nreverse ranges)
            (nested-form-exclusion-lines lines rules constant-symbols))))

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

(defun source-form-excludable-p (lines start-index start-position end-line head name &optional constant-symbols)
  (unless (member head
                  '("defstruct" "define-node" "defconstant" "defvar" "defparameter" "sb-ext:defglobal")
                  :test (function string=))
    (return-from source-form-excludable-p t))
  (labels ((named-p (value expected)
             (and (symbolp value)
                  (string= (symbol-name value) expected)))
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
                          (member (symbol-name value) constant-symbols :test #'string-equal)))))
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
       (defstruct-form-p (read-form)))
      ((string= head "define-node")
       (let ((form (read-form)))
         (and (consp form)
              (named-p (first form) "DEFINE-NODE")
              (defstruct-form-p (second form)))))
      ((string= head "defparameter")
       (let* ((form (read-form))
              (initializer (and (consp form) (third form))))
         (and (consp form)
              (symbolp (second form))
              initializer
              (literal-p initializer))))
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
(defun source-constant-symbol-names (directory)
  (let ((names nil))
    (dolist (path (directory
                     (merge-pathnames
                      "*.lisp"
                      (uiop:ensure-directory-pathname directory))))
      (let ((depth 0))
        (with-open-file (stream path :direction :input :external-format :utf-8)
          (loop for original = (read-line stream nil nil)
                while original
                for clean = (strip-lisp-strings-and-comments original)
                do (when (zerop depth)
                     (multiple-value-bind (head name)
                         (first-form-head-and-name clean)
                       (let* ((head-separator
                               (and head (position #\: head :from-end t)))
                              (head-name
                               (if head-separator
                                   (subseq head (1+ head-separator))
                                 head))
                              (name-separator
                               (and name (position #\: name :from-end t)))
                              (name-text
                               (if name-separator
                                   (subseq name (1+ name-separator))
                                 name)))
                         (when (and head-name name-text
                                    (string= head-name "defconstant"))
                           (push name-text names))))
                     (incf depth
                           (- (count-character clean #\()
                              (count-character clean #\)))))))))
    (nreverse names))
  )

(defun nested-form-exclusion-lines (lines rules constant-symbols)
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
                                                 (source-form-excludable-p lines index position end head name constant-symbols))
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

(defun uncovered-lines-excluded-p (row ranges) (and (not (string= (getf row :uncovered) "-")) (every (lambda (range) (loop for line from (car range) to (cdr range) always (line-is-excluded-p line ranges))) (getf row :uncovered-ranges))))

(defun record-source-exclusion (lines index clean form-open head name rule constant-symbols ranges)
  (let ((start (1+ index)))
    (if (string= (getf rule :kind) "defun-header")
        (let ((lambda-line nil)
              (lambda-start nil))
          (loop for candidate from index below (length lines)
                for part = (if (= candidate index)
                               clean
                               (strip-lisp-strings-and-comments
                                (nth candidate lines)))
                do (progn
                     (if (= candidate index)
                         (setf lambda-start
                               (find-same-line-lambda-open
                                part
                                form-open
                                head
                                name))
                         (setf lambda-start (position #\( part)))
                     (when lambda-start
                       (setf lambda-line candidate)
                       (return))))
          (when lambda-start
            (let ((balance 0)
                  (header-end nil))
              (loop for candidate from lambda-line below (length lines)
                    for part = (strip-lisp-strings-and-comments
                                (nth candidate lines))
                    do (progn
                         (when (= candidate lambda-line)
                           (setf part (subseq part lambda-start)))
                         (incf balance (count-character part #\())
                         (decf balance (count-character part #\)))
                         (when (zerop balance)
                           (setf header-end (1+ candidate))
                           (return))))
              (when (and header-end (<= start header-end))
                (push (list start header-end head) ranges)))))
        (let ((balance 0)
              (end (1- (length lines))))
          (loop for candidate from index below (length lines)
                for part = (strip-lisp-strings-and-comments
                            (nth candidate lines))
                do (progn
                     (incf balance (count-character part #\())
                     (decf balance (count-character part #\)))
                     (when (<= balance 0)
                       (setf end candidate)
                       (return))))
          (let ((excluded-end (1+ end)))
            (when (string= (getf rule :kind) "condition-declaration")
              (loop for candidate from index to end
                    for part = (strip-lisp-strings-and-comments
                                (nth candidate lines))
                    when (condition-report-line-p part)
                      do (setf excluded-end candidate)
                         (return)))
            (when (and (<= start excluded-end)
                       (source-form-excludable-p lines index form-open excluded-end head name constant-symbols))
              (push (list start excluded-end head) ranges)))))))
