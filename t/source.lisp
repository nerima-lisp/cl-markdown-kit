(in-package #:cl-markdown-kit/test)

(defun source-contract-span-coordinates (span)
  (list (cl-parser-kit:span-start span)
        (cl-parser-kit:span-end span)
        (cl-parser-kit:span-start-line span)
        (cl-parser-kit:span-start-column span)
        (cl-parser-kit:span-end-line span)
        (cl-parser-kit:span-end-column span)))

(defun source-contract-boundary (source-buffer index)
  (let ((span (cl-markdown-kit:source-buffer-span source-buffer index index)))
    (list (cl-parser-kit:span-start span)
          (cl-parser-kit:span-start-line span)
          (cl-parser-kit:span-start-column span))))

(describe "Source buffer contract"
  (it "constructs public source formats and spans"
      (let* ((source-format (cl-markdown-kit:make-source-format
                             :input-kind :stream
                             :bom-p t
                             :newline-style :cr
                             :newline-sequence #(:cr)))
             (buffer (cl-markdown-kit:make-source-buffer "x"))
             (span (cl-markdown-kit:source-buffer-span buffer 0 1)))
        (expect (cl-markdown-kit:source-format-p source-format) :to-be-truthy)
        (expect (list (cl-markdown-kit:source-format-input-kind source-format)
                      (cl-markdown-kit:source-format-bom-p source-format)
                      (cl-markdown-kit:source-format-newline-style source-format)
                      (coerce
                       (cl-markdown-kit:source-format-newline-sequence
                        source-format)
                       (quote list)))
                :to-equal (list :stream t :cr (list :cr)))
        (expect (typep span (quote cl-parser-kit:span)) :to-be-truthy)
        (expect (cl-parser-kit:span-source span) :to-equal "x")
        (expect (source-contract-span-coordinates span)
                :to-equal (list 0 1 1 1 1 2))))

  (it "normalizes LF while keeping half-open one-based source coordinates"
    (let* ((source (format nil "alpha~%beta"))
           (buffer (cl-markdown-kit:make-source-buffer source))
           (span (cl-markdown-kit:source-buffer-span buffer 5 6)))
      (expect (cl-markdown-kit:source-buffer-source buffer) :to-equal source)
      (expect (cl-markdown-kit:source-buffer-normalized-text buffer)
              :to-equal source)
      (expect (cl-markdown-kit:source-format-newline-style
               (cl-markdown-kit:source-buffer-format buffer))
              :to-equal :lf)
      (expect (coerce
               (cl-markdown-kit:source-format-newline-sequence
                (cl-markdown-kit:source-buffer-format buffer))
               'list)
              :to-equal '(:lf))
      (expect (typep span 'cl-parser-kit:span) :to-be-truthy)
      (expect (cl-parser-kit:span-source span) :to-equal source)
      (expect (source-contract-span-coordinates span)
              :to-equal '(5 6 1 6 2 1))
      (expect (source-contract-boundary buffer 6) :to-equal '(6 2 1))))

  (it "maps CRLF and CR to one logical LF without losing physical offsets"
    (let* ((crlf-source (format nil "A~C~%B" #\Return))
           (crlf-buffer (cl-markdown-kit:make-source-buffer crlf-source))
           (crlf-span (cl-markdown-kit:source-buffer-span crlf-buffer 1 2))
           (cr-source (format nil "A~CB" #\Return))
           (cr-buffer (cl-markdown-kit:make-source-buffer cr-source))
           (cr-span (cl-markdown-kit:source-buffer-span cr-buffer 1 2)))
      (expect (cl-markdown-kit:source-buffer-normalized-text crlf-buffer)
              :to-equal (format nil "A~%B"))
      (expect (cl-markdown-kit:source-format-newline-style
               (cl-markdown-kit:source-buffer-format crlf-buffer))
              :to-equal :crlf)
      (expect (source-contract-boundary crlf-buffer 2) :to-equal '(3 2 1))
      (expect (source-contract-span-coordinates crlf-span)
              :to-equal '(1 3 1 2 2 1))
      (expect (subseq (cl-parser-kit:span-source crlf-span)
                      (cl-parser-kit:span-start crlf-span)
                      (cl-parser-kit:span-end crlf-span))
              :to-equal (format nil "~C~%" #\Return))
      (expect (cl-markdown-kit:source-buffer-normalized-text cr-buffer)
              :to-equal (format nil "A~%B"))
      (expect (cl-markdown-kit:source-format-newline-style
               (cl-markdown-kit:source-buffer-format cr-buffer))
              :to-equal :cr)
      (expect (source-contract-boundary cr-buffer 2) :to-equal '(2 2 1))
      (expect (source-contract-span-coordinates cr-span)
              :to-equal '(1 2 1 2 2 1))))

  (it "records mixed newline order and reads character streams"
    (let* ((source (format nil "a~C~%b~Cc~%d" #\Return #\Return))
           (buffer (cl-markdown-kit:make-source-buffer source))
           (format (cl-markdown-kit:source-buffer-format buffer))
           (stream-buffer
             (with-input-from-string (stream
                                       (format nil "x~C~%y" #\Return))
               (cl-markdown-kit:make-source-buffer stream))))
      (expect (cl-markdown-kit:source-buffer-normalized-text buffer)
              :to-equal (format nil "a~%b~%c~%d"))
      (expect (cl-markdown-kit:source-format-newline-style format)
              :to-equal :mixed)
      (expect (coerce (cl-markdown-kit:source-format-newline-sequence format)
                      'list)
              :to-equal '(:crlf :cr :lf))
      (expect (cl-markdown-kit:source-format-input-kind format)
              :to-equal :string)
      (expect (cl-markdown-kit:source-buffer-source stream-buffer)
              :to-equal (format nil "x~C~%y" #\Return))
      (expect (cl-markdown-kit:source-format-input-kind
               (cl-markdown-kit:source-buffer-format stream-buffer))
              :to-equal :stream)
      (expect (cl-markdown-kit:source-format-newline-style
               (cl-markdown-kit:source-buffer-format stream-buffer))
              :to-equal :crlf)))

  (it "keeps an initial BOM in the source span and starts visible text at 1:1"
    (let* ((bom (or (code-char #xfeff) (error "No BOM character available")))
           (source (concatenate 'string (string bom) (format nil "A~%B")))
           (buffer (cl-markdown-kit:make-source-buffer source))
           (format (cl-markdown-kit:source-buffer-format buffer))
           (text-span (cl-markdown-kit:source-buffer-span buffer 0 1))
           (document-span
             (cl-markdown-kit:source-buffer-document-span buffer)))
      (expect (cl-markdown-kit:source-buffer-normalized-text buffer)
              :to-equal (format nil "A~%B"))
      (expect (cl-markdown-kit:source-format-bom-p format) :to-be-truthy)
      (expect (source-contract-boundary buffer 0) :to-equal '(1 1 1))
      (expect (source-contract-span-coordinates text-span)
              :to-equal '(1 2 1 1 1 2))
      (expect (source-contract-span-coordinates document-span)
              :to-equal (list 0 (length source) 1 1 2 2))
      (expect (cl-parser-kit:span-source document-span) :to-equal source)))

  (it "decodes BOM-marked UTF-8 octets before mapping source coordinates"
      (let* ((octets (make-array 7 :element-type (quote (unsigned-byte 8))
                                 :initial-contents (list 239 187 191 65 13 10 66)))
             (buffer (cl-markdown-kit:make-source-buffer octets))
             (format (cl-markdown-kit:source-buffer-format buffer))
             (span (cl-markdown-kit:source-buffer-span buffer 1 2))
             (bom (or (code-char #xfeff)
                      (error "No BOM character available")))
             (payload-octets
              (make-array 7 :element-type (quote (unsigned-byte 8))
                          :initial-contents (list 239 187 191 239 187 191 65)))
             (payload-buffer
              (cl-markdown-kit:make-source-buffer payload-octets))
             (payload-span
              (cl-markdown-kit:source-buffer-span payload-buffer 0 2)))
        (expect (cl-markdown-kit:source-format-input-kind format)
                :to-equal :octets)
        (expect (cl-markdown-kit:source-format-bom-p format) :to-be-truthy)
        (expect (cl-markdown-kit:source-format-encoding format)
                :to-equal :utf-8)
        (expect (cl-markdown-kit:source-buffer-normalized-text buffer)
                :to-equal (format nil "A~%B"))
        (expect (cl-markdown-kit:source-format-newline-style format)
                :to-equal :crlf)
        (expect (source-contract-boundary buffer 0) :to-equal (list 1 1 1))
        (expect (source-contract-span-coordinates span)
                :to-equal (list 2 4 1 2 2 1))
        (expect (subseq (cl-parser-kit:span-source span)
                        (cl-parser-kit:span-start span)
                        (cl-parser-kit:span-end span))
                :to-equal (format nil "~C~%" (code-char 13)))
        (expect (cl-markdown-kit:source-buffer-source payload-buffer)
                :to-equal (concatenate (quote string) (string bom) (string bom) "A"))
        (expect (cl-markdown-kit:source-buffer-normalized-text payload-buffer)
                :to-equal (concatenate (quote string) (string bom) "A"))
        (expect (source-contract-span-coordinates payload-span)
                :to-equal (list 1 3 1 1 1 3)))))
