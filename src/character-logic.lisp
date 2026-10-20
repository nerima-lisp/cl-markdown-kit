(in-package #:cl-markdown-kit)

(defun %numeric-character-reference-digit (char radix)
  (let ((code (char-code char)))
    (cond
      ((<= #x30 code #x39) (- code #x30))
      ((and (= radix 16) (<= #x41 code #x46)) (+ 10 (- code #x41)))
      ((and (= radix 16) (<= #x61 code #x66)) (+ 10 (- code #x61)))
      (t nil))))

(defun lookup-html5-entity (name)
  (let ((table (or *html5-entities*
                   (setf *html5-entities* (build-html5-entity-table)))))
    (gethash name table)))

(defun decode-numeric-character-reference (digits radix)
  (unless (and (stringp digits)
               (member radix '(10 16) :test #'eql)
               (<= 1 (length digits) (if (= radix 10) 7 6)))
    (return-from decode-numeric-character-reference nil))
  (let ((codepoint 0))
    (loop for char across digits
          for digit = (%numeric-character-reference-digit char radix)
          do (unless digit
               (return-from decode-numeric-character-reference nil))
             (setf codepoint (+ (* codepoint radix) digit)))
    (when (or (zerop codepoint)
              (<= #xD800 codepoint #xDFFF)
              (> codepoint #x10FFFF))
      (setf codepoint #xFFFD))
    (string (code-char codepoint))))

(defun unicode-whitespace-p (character)
  (or (case (char-code character)
        ((#x09 #x0A #x0C #x0D) t))
      (eq (sb-unicode:general-category character) :zs)))

(defun ascii-punctuation-p (char)
  (let ((code (char-code char)))
    (or (<= #x21 code #x2F)
        (<= #x3A code #x40)
        (<= #x5B code #x60)
        (<= #x7B code #x7E))))

(defun ascii-control-p (char)
  (let ((code (char-code char)))
    (or (<= #x00 code #x1F)
        (= code #x7F))))

(defun %pc-link-label-space-p (character)
  (or (char= character #\Space)
      (char= character #\Tab)
      (char= character #\Newline)
      (char= character #\Return)))

(defun normalize-link-label (string)
  (let ((casefolded (sb-unicode:casefold string))
        (pending-space nil)
        (wrote-character nil))
    (with-output-to-string (output)
      (loop for char across casefolded
            do (if (%pc-link-label-space-p char)
                   (when wrote-character
                     (setf pending-space t))
                   (progn
                     (when pending-space
                       (write-char #\Space output))
                     (write-char char output)
                     (setf pending-space nil
                           wrote-character t)))))))

(defun %pc-space-p (character)
  (or (char= character #\Space)
      (char= character #\Tab)))

(defun %pc-html-space-p (character)
  (or (char= character #\Space)
      (char= character #\Tab)
      (char= character #\Newline)
      (char= character #\Return)
      (char= character #\Page)))

(defun %pc-ascii-alpha-p (character)
  (let ((code (char-code character)))
    (or (<= #x41 code #x5A) (<= #x61 code #x7A))))

(defun %pc-ascii-digit-p (character)
  (let ((code (char-code character)))
    (<= #x30 code #x39)))

(defun %pc-ascii-alphanumeric-p (character)
  (or (%pc-ascii-alpha-p character) (%pc-ascii-digit-p character)))

(defun %pc-c1-replacement (codepoint)
  (cdr (assoc codepoint +html-c1-replacements+ :test #'=)))

(defun %pc-decode-numeric-reference (digits radix)
  (let* ((decoded (decode-numeric-character-reference digits radix))
         (replacement
          (and decoded (%pc-c1-replacement (parse-integer digits :radix radix)))))
    (if replacement
        (string (code-char replacement))
        decoded)))

(defun %pc-commonmark-punctuation-p (character)
  (or (ascii-punctuation-p character)
      (case (sb-unicode:general-category character)
        ((:pc :pd :ps :pe :pi :pf :po :sm :sc :sk :so) t)
        (otherwise nil))))

(defun %pc-unicode-domain-character-p (character)
  (let ((codepoint (char-code character))
        (category (sb-unicode:general-category character)))
    (and (> codepoint #x7F)
         (not (eq category :zs))
         (not (case category
                ((:pc :pd :ps :pe :pi :pf :po) t)))
         (not (case category
                ((:cc :cf :cs :co :cn) t))))))
