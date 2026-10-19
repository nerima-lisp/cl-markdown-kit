(in-package #:cl-markdown-kit)

(defstruct (%html-writer-context
             (:constructor %make-html-writer-context
                 (&key stream raw-html unsafe-urls gfm footnote-definitions
                       footnote-numbers footnote-reference-counts
                       footnote-reference-indices footnote-order
                       footnote-ids footnote-reference-ids)))
  stream
  last-character
  raw-html
  unsafe-urls
  gfm
  footnote-definitions
  footnote-numbers
  footnote-reference-counts
  footnote-reference-indices
  footnote-order
  footnote-ids
  footnote-reference-ids
  tree-parent
  tree-grandparent
  tree-child-index
  tree-last-child-p
  table-body-open-p
  plain-alt-root)

(defun %hw-write (context string)
  (unless (zerop (length string))
    (write-string string (%html-writer-context-stream context))
    (setf (%html-writer-context-last-character context)
          (char string (1- (length string)))))
  string)

(defun %hw-write-char (context character)
  (write-char character (%html-writer-context-stream context))
  (setf (%html-writer-context-last-character context) character)
  character)

(defun %hw-newline (context)
  (when (and (%html-writer-context-last-character context)
             (not (char= (%html-writer-context-last-character context)
                         #\Newline)))
    (%hw-write-char context #\Newline)))

(defun %hw-escaped-text (context string)
  (%hw-write context (escape-html-text string)))

(defun %hw-escaped-attribute (context string)
  (%hw-write context (escape-html-attribute string)))

(defun %hw-href (context uri)
  (unless (and (not (%html-writer-context-unsafe-urls context))
               (dangerous-url-p uri))
    (%hw-escaped-attribute context (normalize-uri-percent-encoding uri))))

(defun %hw-tagfilter-match-p (string start)
  (let* ((length (length string))
         (index (1+ start)))
    (when (and (< index length) (char= (char string index) #\/))
      (incf index))
    (some (lambda (tag)
            (let ((end (+ index (length tag))))
              (and (<= end length)
                   (string-equal tag string :start2 index :end2 end)
                   (< end length)
                   (let ((next (char string end)))
                     (or (%pc-html-space-p next)
                         (char= next #\>)
                         (and (char= next #\/)
                              (< (1+ end) length)
                              (char= (char string (1+ end)) #\>)))))))
          +gfm-tagfilter-tags+)))

(defun %hw-filter-html (context string)
  (loop with start = 0
        for index = (position #\< string :start start)
        while index
        do (%hw-write context (subseq string start index))
           (if (%hw-tagfilter-match-p string index)
               (%hw-write context "&lt;")
               (%hw-write-char context #\<))
           (setf start (1+ index))
        finally (%hw-write context (subseq string start))))

(defun %hw-write-raw-html (context string)
  (if (%html-writer-context-gfm context)
      (%hw-filter-html context string)
      (%hw-write context string)))
