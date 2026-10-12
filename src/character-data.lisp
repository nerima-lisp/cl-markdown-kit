(in-package #:cl-markdown-kit)

(defun %numeric-character-reference-digit (char radix)
  (let ((code (char-code char)))
    (cond
      ((<= #x30 code #x39) (- code #x30))
      ((and (= radix 16) (<= #x41 code #x46)) (+ 10 (- code #x41)))
      ((and (= radix 16) (<= #x61 code #x66)) (+ 10 (- code #x61)))
      (t nil))))

(defun lookup-html5-entity (name)
  (gethash name *html5-entities*))

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

(defun unicode-whitespace-p (char)
  (or (not (null (member (char-code char)
                         '(#x09 #x0A #x0C #x0D)
                         :test #'=)))
      (eq (sb-unicode:general-category char) :zs)))

(defun unicode-punctuation-p (char)
  (not (null
        (member (sb-unicode:general-category char)
                '(:pc :pd :ps :pe :pi :pf :po :sm :sc :sk :so)
                :test #'eq))))

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

(defun %link-label-space-p (char)
  (not (null
        (member (char-code char) '(#x20 #x09 #x0A #x0D) :test #'=))))

(defun normalize-link-label (string)
  (let ((casefolded (sb-unicode:casefold string))
        (pending-space nil)
        (wrote-character nil))
    (with-output-to-string (output)
      (loop for char across casefolded
            do (if (%link-label-space-p char)
                   (when wrote-character
                     (setf pending-space t))
                   (progn
                     (when pending-space
                       (write-char #\Space output))
                     (write-char char output)
                     (setf pending-space nil
                           wrote-character t)))))))
