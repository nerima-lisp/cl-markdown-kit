(in-package #:cl-markdown-kit)

(defun %escape-html-string (string)
  (with-output-to-string (output)
    (loop for char across string
          do (case char
               (#\& (write-string "&amp;" output))
               (#\< (write-string "&lt;" output))
               (#\> (write-string "&gt;" output))
               (#\" (write-string "&quot;" output))
               (otherwise (write-char char output))))))

(defun escape-html-text (string)
  (%escape-html-string string))

(defun escape-html-attribute (string)
  (%escape-html-string string))

(defun %hexadecimal-digit-p (char)
  (let ((code (char-code char)))
    (or (<= #x30 code #x39)
        (<= #x41 code #x46)
        (<= #x61 code #x66))))

(defun %write-percent-encoded-byte (byte output)
  (let ((hex-digits "0123456789ABCDEF"))
    (write-char #\% output)
    (write-char (char hex-digits (ash byte -4)) output)
    (write-char (char hex-digits (logand byte #x0F)) output)))

(defun %write-percent-encoded-character (char output)
  (let ((code (char-code char)))
    (cond
      ((<= code #x7FF)
       (%write-percent-encoded-byte (+ #xC0 (ash code -6)) output)
       (%write-percent-encoded-byte (+ #x80 (logand code #x3F)) output))
      ((<= code #xFFFF)
       (%write-percent-encoded-byte (+ #xE0 (ash code -12)) output)
       (%write-percent-encoded-byte
        (+ #x80 (logand (ash code -6) #x3F)) output)
       (%write-percent-encoded-byte (+ #x80 (logand code #x3F)) output))
      (t
       (%write-percent-encoded-byte (+ #xF0 (ash code -18)) output)
       (%write-percent-encoded-byte
        (+ #x80 (logand (ash code -12) #x3F)) output)
       (%write-percent-encoded-byte
        (+ #x80 (logand (ash code -6) #x3F)) output)
       (%write-percent-encoded-byte (+ #x80 (logand code #x3F)) output)))))

(defun normalize-uri-percent-encoding (uri)
  (with-output-to-string (output)
    (loop with length = (length uri)
          for index from 0 below length
          for char = (char uri index)
          for code = (char-code char)
          do (cond
               ((and (char= char #\%)
                     (< (+ index 2) length)
                     (%hexadecimal-digit-p (char uri (1+ index)))
                     (%hexadecimal-digit-p (char uri (+ index 2))))
                (write-char char output)
                (write-char (char uri (1+ index)) output)
                (write-char (char uri (+ index 2)) output)
                (incf index 2))
               ((and (< code 128)
                     (or (<= #x30 code #x39)
                         (<= #x41 code #x5A)
                         (<= #x61 code #x7A)
                         (find char ";/?:@&=+$,-_.!~*'()#" :test #'char=)))
                (write-char char output))
               ((< code 128)
                (%write-percent-encoded-byte code output))
               (t
                (%write-percent-encoded-character char output))))))

(defun %ascii-prefix-equal-p (string prefix)
  (and (>= (length string) (length prefix))
       (loop for index below (length prefix)
             for string-code = (char-code (char string index))
             for prefix-code = (char-code (char prefix index))
             always (= (if (<= #x41 string-code #x5A)
                           (+ string-code #x20)
                           string-code)
                       prefix-code))))

(defun dangerous-url-p (uri)
  (or (%ascii-prefix-equal-p uri "javascript:")
      (%ascii-prefix-equal-p uri "vbscript:")
      (%ascii-prefix-equal-p uri "file:")
      (and (%ascii-prefix-equal-p uri "data:")
           (not (some (lambda (image-type)
                        (%ascii-prefix-equal-p uri image-type))
                      '("data:image/png"
                        "data:image/gif"
                        "data:image/jpeg"
                        "data:image/webp"))))))
