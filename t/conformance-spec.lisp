(in-package #:cl-markdown-kit/test)

(defstruct spec-example
  id
  section
  markdown
  html)

(defvar *conformance-exclusions* nil)

(defun conformance-fixture-path (environment-variable)
  (let ((value (uiop:getenv environment-variable)))
    (unless value
      (error "~A must name a conformance fixture file" environment-variable))
    (let ((pathname (pathname value)))
      (unless (probe-file pathname)
        (error "~A does not name an existing conformance fixture file"
               environment-variable))
      pathname)))

(defun conformance-file-string (pathname)
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (with-output-to-string (output)
      (loop for character = (read-char stream nil nil)
            while character
            do (write-char character output)))))

(defun conformance-lines (text)
  (let ((start 0)
        (lines nil))
    (loop for end = (position #\Newline text :start start)
          do (push (string-right-trim '(#\Return)
                                      (subseq text start end))
                   lines)
          while end
          do (setf start (1+ end)))
    (nreverse lines)))

(defun conformance-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun conformance-fence-open (line)
  (let ((start (position-if-not #'conformance-horizontal-space-p line)))
    (when (and start (<= start 3) (< start (length line)))
      (let ((marker (char line start)))
        (when (find marker "`~")
          (let ((end start))
            (loop while (and (< end (length line))
                             (char= (char line end) marker))
                  do (incf end))
            (let ((length (- end start)))
              (when (>= length 3)
                (values marker length
                        (string-trim '(#\Space #\Tab)
                                     (subseq line end)))))))))))

(defun conformance-example-info-p (info)
  (let* ((text (string-downcase (string-trim '(#\Space #\Tab) info)))
         (end (position-if #'conformance-horizontal-space-p text)))
    (string= "example" text :end2 end)))

(defun conformance-fence-close-p (line marker minimum-length)
  (multiple-value-bind (line-marker length info)
      (conformance-fence-open line)
    (and line-marker
         (char= line-marker marker)
         (>= length minimum-length)
         (every #'conformance-horizontal-space-p info))))

(defun conformance-heading (line)
  (let ((start (position-if-not #'conformance-horizontal-space-p line)))
    (when (and start (<= start 3) (< start (length line))
               (char= (char line start) #\#))
      (let ((end start))
        (loop while (and (< end (length line))
                         (char= (char line end) #\#))
              do (incf end))
        (when (and (<= (- end start) 6)
                   (or (= end (length line))
                       (conformance-horizontal-space-p (char line end))))
          (let* ((content (string-left-trim '(#\Space #\Tab)
                                            (subseq line end)))
                 (trimmed (string-right-trim '(#\Space #\Tab) content))
                 (last-non-hash (position-if-not (lambda (character)
                                                   (char= character #\#))
                                                 trimmed
                                                 :from-end t)))
            (if (and last-non-hash
                     (< (1+ last-non-hash) (length trimmed))
                     (conformance-horizontal-space-p
                      (char trimmed last-non-hash)))
                (string-right-trim '(#\Space #\Tab)
                                   (subseq trimmed 0 last-non-hash))
                trimmed)))))))

(defun conformance-setext-underline-p (line)
  (let ((text (string-trim '(#\Space #\Tab) line)))
    (and (>= (length text) 3)
         (or (every (lambda (character) (char= character #\=)) text)
             (every (lambda (character) (char= character #\-)) text)))))

(defun conformance-example-text (lines)
  (with-output-to-string (output)
    (dolist (line lines)
      (write-string line output)
      (terpri output))))

(defun conformance-read-example (lines start section prefix number)
  (multiple-value-bind (marker fence-length info)
      (conformance-fence-open (aref lines start))
    (unless (conformance-example-info-p info)
      (error "Expected a spec example fence at line ~D" (1+ start)))
    (let ((markdown-lines nil)
          (html-lines nil)
          (separator-seen nil)
          (markdown-fence nil))
      (block example
        (loop for index from (1+ start) below (length lines)
              for line = (aref lines index)
              do (cond
                   ((conformance-fence-close-p line marker fence-length)
                    (unless separator-seen
                      (error "Spec example has no input/output separator"))
                    (return-from example
                      (values (make-spec-example
                               :id (format nil "~A-~4,'0D" prefix number)
                               :section section
                               :markdown (conformance-example-text
                                          (nreverse markdown-lines))
                               :html (conformance-example-text
                                      (nreverse html-lines)))
                              (1+ index))))
                   ((not separator-seen)
                    (cond
                      (markdown-fence
                       (when (conformance-fence-close-p
                              line (car markdown-fence) (cdr markdown-fence))
                         (setf markdown-fence nil))
                       (push line markdown-lines))
                      ((string= line ".")
                       (setf separator-seen t))
                      (t
                       (multiple-value-bind (inner-marker inner-length)
                           (conformance-fence-open line)
                         (when inner-marker
                           (setf markdown-fence
                                 (cons inner-marker inner-length))))
                       (push line markdown-lines))))
                   (t (push line html-lines))))
        (error "Unterminated spec example beginning at line ~D" (1+ start))))))

(defun conformance-read-examples (environment-variable prefix)
  (let* ((pathname (conformance-fixture-path environment-variable))
         (lines (coerce (conformance-lines
                         (conformance-file-string pathname))
                        'vector))
         (section nil)
         (previous-line nil)
         (fence nil)
         (examples nil)
         (number 0)
         (index 0))
    (loop while (< index (length lines))
          for line = (aref lines index)
          do (multiple-value-bind (marker fence-length info)
                 (conformance-fence-open line)
               (cond
                 (fence
                  (when (conformance-fence-close-p
                         line (car fence) (cdr fence))
                    (setf fence nil))
                  (setf previous-line nil)
                  (incf index))
                 ((and marker (conformance-example-info-p info))
                  (incf number)
                  (multiple-value-bind (example next-index)
                      (conformance-read-example lines index section prefix number)
                    (push example examples)
                    (setf index next-index
                          previous-line nil)))
                 (marker
                  (setf fence (cons marker fence-length)
                        previous-line nil)
                  (incf index))
                 (t
                  (let ((heading (conformance-heading line)))
                    (cond
                      (heading
                       (setf section heading
                             previous-line nil))
                      ((and previous-line
                            (conformance-setext-underline-p line))
                       (setf section
                             (string-trim '(#\Space #\Tab) previous-line)
                             previous-line nil))
                      ((string= (string-trim '(#\Space #\Tab) line) "")
                       (setf previous-line nil))
                      (t (setf previous-line line))))
                  (incf index)))))
    (nreverse examples)))

(defun commonmark-spec-examples ()
  (conformance-read-examples "COMMONMARK_TEST_SUITE" "commonmark"))

(defun gfm-extension-examples ()
  (conformance-read-examples "GFM_TEST_SUITE" "gfm"))

(defun spec-example-valid-p (example)
  (and (stringp (spec-example-id example))
       (> (length (spec-example-id example)) 0)
       (stringp (spec-example-section example))
       (> (length (spec-example-section example)) 0)
       (stringp (spec-example-markdown example))
       (stringp (spec-example-html example))))

(defun conformance-read-exclusions ()
  (let ((pathname
          (merge-pathnames "t/data/conformance-exclusions.lisp"
                           (asdf:system-source-directory :cl-markdown-kit))))
    (unless (probe-file pathname)
      (error "Missing conformance exclusion list"))
    (load pathname)
    *conformance-exclusions*))

(defun conformance-exclusion-valid-p (entry examples)
  (and (consp entry)
       (= (length entry) 3)
       (stringp (first entry))
       (find (first entry) examples :key #'spec-example-id :test #'string=)
       (eq (second entry) :html)
       (stringp (third entry))
       (> (length (string-trim '(#\Space #\Tab) (third entry))) 0)))

(defun conformance-exclusions-valid-p (examples exclusions)
  (and (listp exclusions)
       (= (length exclusions)
          (length (remove-duplicates exclusions :key #'first :test #'string=)))
       (every (lambda (entry)
                (conformance-exclusion-valid-p entry examples))
              exclusions)))
