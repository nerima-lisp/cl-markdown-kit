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

(defun source-contract-signals-p (function expected-type)
  (handler-case
      (progn (funcall function) nil)
    (serious-condition (condition)
      (typep condition expected-type))))

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

  (cl-weave:it-each ((:crlf :crlf (format nil "A~C~%B" #\Return)
            (3 2 1)
            (1 3 1 2 2 1)
            (format nil "~C~%" #\Return))
     (:cr :cr (format nil "A~CB" #\Return)
          (2 2 1)
          (1 2 1 2 2 1)
          (string #\Return)))
    "newline style ~A: maps CRLF and CR to one logical LF without losing physical offsets"
    (name newline-style source expected-boundary expected-coordinates physical-newline)
      (let* ((buffer (cl-markdown-kit:make-source-buffer (apply (first source) (rest source))))
             (span (cl-markdown-kit:source-buffer-span buffer 1 2)))
        (expect (cl-markdown-kit:source-buffer-normalized-text buffer)
                :to-equal (format nil "A~%B"))
        (expect (cl-markdown-kit:source-format-newline-style
                 (cl-markdown-kit:source-buffer-format buffer))
                :to-equal newline-style)
        (expect (source-contract-boundary buffer 2) :to-equal expected-boundary)
        (expect (source-contract-span-coordinates span) :to-equal expected-coordinates)
        (expect (subseq (cl-parser-kit:span-source span)
                        (cl-parser-kit:span-start span)
                        (cl-parser-kit:span-end span))
                :to-equal (apply (first physical-newline) (rest physical-newline)))))

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

  (it "stores source boundaries compactly and maps mixed newline offsets"
    (let* ((prefix (make-string 8192 :initial-element #\x))
           (source (concatenate 'string prefix (format nil "~C~%yz~Cq"
                                                       #\Return #\Return)))
           (buffer (cl-markdown-kit:make-source-buffer source))
           (boundary-map
             (cl-markdown-kit::source-buffer-boundary-map buffer)))
      (expect (cl-markdown-kit:source-buffer-normalized-text buffer)
              :to-equal (concatenate 'string prefix (format nil "~%yz~%q")))
      (expect (cl-markdown-kit::boundary-map-boundaries boundary-map)
              :to-equal nil)
      (expect (length
               (cl-markdown-kit::boundary-map-normalized-line-starts
                boundary-map))
              :to-equal 3)
      (expect (length
               (cl-markdown-kit::boundary-map-source-line-starts boundary-map))
              :to-equal 3)
      (expect (cl-markdown-kit::boundary-map-length boundary-map)
              :to-equal 8197)
      (expect (source-contract-boundary buffer 8192) :to-equal '(8192 1 8193))
      (expect (source-contract-boundary buffer 8193) :to-equal '(8194 2 1))
      (expect (source-contract-boundary buffer 8195) :to-equal '(8196 2 3))
      (expect (source-contract-boundary buffer 8196) :to-equal '(8197 3 1))
      (expect (source-contract-boundary buffer 8197) :to-equal '(8198 3 2))))

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
                :to-equal (list 1 3 1 1 1 3))))

  (it "validates boundary maps and rejects unsupported source inputs and spans"
    (let* ((initial (cl-markdown-kit::make-source-boundary))
           (after-character
             (cl-markdown-kit::make-source-boundary :offset 1 :column 2))
           (boundary-map
             (cl-markdown-kit::make-boundary-map
              "x" (list initial after-character)))
           (empty-buffer (cl-markdown-kit:make-source-buffer ""))
           (trailing-cr-buffer
             (cl-markdown-kit:make-source-buffer
              (concatenate 'string "x" (string #\Return))))
           (mapped-buffer
             (cl-markdown-kit:make-source-buffer
              (format nil "A~C~%B" #\Return)))
           (composed-map
             (cl-markdown-kit::%make-boundary-map
              (cl-markdown-kit:source-buffer-source mapped-buffer)
              nil 3 #() #()
              (cl-markdown-kit::source-buffer-boundary-map mapped-buffer)
              #(0 2) #(#(0 1) #(2 3))))
           (default-format (cl-markdown-kit:make-source-format)))
      (expect (list (cl-markdown-kit::source-boundary-offset initial)
                    (cl-markdown-kit::source-boundary-line initial)
                    (cl-markdown-kit::source-boundary-column initial))
              :to-equal '(0 1 1))
      (expect (cl-markdown-kit::boundary-map-length boundary-map) :to-equal 1)
      (expect (cl-markdown-kit::source-boundary-column
               (cl-markdown-kit::boundary-map-boundary boundary-map 1))
              :to-equal 2)
      (expect (list (cl-markdown-kit:source-format-input-kind default-format)
                    (cl-markdown-kit:source-format-bom-p default-format)
                    (cl-markdown-kit:source-format-newline-style default-format))
              :to-equal '(:string nil :none))
      (expect (cl-markdown-kit:source-buffer-normalized-text empty-buffer)
              :to-equal "")
      (expect (source-contract-span-coordinates
               (cl-markdown-kit:source-buffer-span empty-buffer 0 0))
              :to-equal '(0 0 1 1 1 1))
      (expect (cl-markdown-kit:source-format-newline-style
               (cl-markdown-kit:source-buffer-format trailing-cr-buffer))
              :to-equal :cr)
      (expect (source-contract-signals-p
               (lambda () (cl-markdown-kit::make-boundary-map "" #()))
               simple-error)
              :to-be-truthy)
      (expect (source-contract-signals-p
               (lambda ()
                 (cl-markdown-kit::make-boundary-map "x" #(0)))
               type-error)
              :to-be-truthy)
      (expect (source-contract-signals-p
               (lambda () (cl-markdown-kit:make-source-buffer 7))
               type-error)
              :to-be-truthy)
      (expect (source-contract-signals-p
               (lambda () (cl-markdown-kit:source-buffer-span empty-buffer 1 0))
               simple-error)
              :to-be-truthy)
      (expect (cl-markdown-kit::source-boundary-offset
               (cl-markdown-kit::boundary-map-boundary composed-map 2))
              :to-equal 3)
      (expect (source-contract-signals-p
               (lambda () (cl-markdown-kit::boundary-map-boundary (cl-markdown-kit::source-buffer-boundary-map mapped-buffer) 4))
               simple-error)
              :to-be-truthy)
      (expect (source-contract-signals-p
               (lambda () (cl-markdown-kit::boundary-map-boundary composed-map 4))
               simple-error)
              :to-be-truthy)
      (with-open-file (stream "/dev/null" :direction :input
                                       :element-type '(unsigned-byte 8))
        (expect (source-contract-signals-p
               (lambda () (cl-markdown-kit:make-source-buffer stream))
               type-error)
                :to-be-truthy)))))
