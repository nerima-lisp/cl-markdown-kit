;;;; src/source.lisp
(in-package #:cl-markdown-kit)

(defstruct (source-boundary
             (:constructor %make-source-boundary (offset line column)))
  (offset 0 :type (integer 0 *))
  (line 1 :type (integer 1 *))
  (column 1 :type (integer 1 *)))

(defun make-source-boundary (&key (offset 0) (line 1) (column 1))
  (%make-source-boundary offset line column))

(defstruct (boundary-map
             (:constructor %make-boundary-map (source boundaries)))
  (source "" :type string)
  (boundaries #() :type vector))

(defun make-boundary-map (source boundaries)
  "Create a boundary map over SOURCE from a vector of source boundaries.

BOUNDARIES has one entry for each text boundary, including both ends."
  (check-type source string)
  (let ((entries (copy-seq (coerce boundaries 'vector))))
    (when (zerop (length entries))
      (error "A boundary map must contain at least its initial boundary."))
    (loop for boundary across entries
          do (check-type boundary source-boundary))
    (%make-boundary-map source entries)))

(defun boundary-map-length (boundary-map)
  "Return the number of characters represented by BOUNDARY-MAP."
  (1- (length (boundary-map-boundaries boundary-map))))

(defun boundary-map-boundary (boundary-map index)
  "Return the source coordinate at text boundary INDEX."
  (check-type boundary-map boundary-map)
  (aref (boundary-map-boundaries boundary-map) index))

(defstruct (source-format
             (:constructor %make-source-format
                 (input-kind encoding bom-p newline-style newline-sequence)))
  (input-kind :string :type keyword)
  encoding
  (bom-p nil :type boolean)
  (newline-style :none :type keyword)
  (newline-sequence #() :type vector))

(defun make-source-format (&key (input-kind :string) encoding (bom-p nil)
                                (newline-style :none) (newline-sequence #()))
  (%make-source-format input-kind encoding bom-p newline-style newline-sequence))

(defstruct (source-buffer
             (:constructor %make-source-buffer
                 (source normalized-text format boundary-map)))
  (source "" :type string)
  (normalized-text "" :type string)
  (format nil :type source-format)
  (boundary-map nil :type boundary-map))

(defun %bom-character ()
  #+sb-unicode
  (code-char #xfeff)
  #-sb-unicode
  (error "This implementation cannot represent the Unicode BOM."))

(defun %starts-with-bom-p (string)
  (and (plusp (length string))
       (= (char-code (char string 0)) #xfeff)))

(defun %read-character-stream (stream)
  (unless (input-stream-p stream)
    (error 'type-error :datum stream :expected-type '(satisfies input-stream-p)))
  (unless (nth-value 0 (subtypep (stream-element-type stream) 'character))
    (error "Expected a character input stream, got element type ~S."
           (stream-element-type stream)))
  (with-output-to-string (output)
    (loop for character = (read-char stream nil nil)
          while character
          do (write-char character output))))

(defun %decode-source-input (source)
  (cond
    ((stringp source)
     (values (copy-seq source) :string nil nil))
    ((and (vectorp source)
          (typep source '(vector (unsigned-byte 8))))
     (multiple-value-bind (encoding bom-octets)
         (cl-codec-kit:detect-unicode-encoding source)
       (let* ((decoded (cl-codec-kit:octets-to-string
                        source :encoding :auto :errorp t))
              (restored
                (if (plusp bom-octets)
                    (concatenate 'string (string (%bom-character)) decoded)
                    decoded)))
         (values restored :octets encoding (plusp bom-octets)))))
    ((streamp source)
     (values (%read-character-stream source) :stream nil nil))
    (t
     (error 'type-error
            :datum source
            :expected-type '(or string
                                (vector (unsigned-byte 8))
                                (satisfies input-stream-p))))))

(defun %newline-style (sequence)
  (cond
    ((zerop (length sequence)) :none)
    ((loop for ending across sequence
           always (eq ending (aref sequence 0)))
     (aref sequence 0))
    (t :mixed)))

(defun %normalize-source-text (source initial-offset)
  (let ((characters (make-array 0 :element-type 'character
                                :adjustable t :fill-pointer 0))
        (boundaries (make-array 0 :adjustable t :fill-pointer 0))
        (line-endings (make-array 0 :adjustable t :fill-pointer 0))
        (line 1)
        (column 1)
        (index initial-offset))
    (vector-push-extend (%make-source-boundary initial-offset line column)
                        boundaries)
    (loop while (< index (length source))
          for character = (char source index)
          do (cond
               ((char= character #\Return)
                (let ((crlf-p (and (< (1+ index) (length source))
                                   (char= (char source (1+ index)) #\Newline))))
                  (vector-push-extend (if crlf-p :crlf :cr) line-endings)
                  (vector-push-extend #\Newline characters)
                  (incf index (if crlf-p 2 1))
                  (incf line)
                  (setf column 1)
                  (vector-push-extend (%make-source-boundary index line column)
                                      boundaries)))
               ((char= character #\Newline)
                (vector-push-extend :lf line-endings)
                (vector-push-extend #\Newline characters)
                (incf index)
                (incf line)
                (setf column 1)
                (vector-push-extend (%make-source-boundary index line column)
                                    boundaries))
               (t
                (vector-push-extend character characters)
                (incf index)
                (incf column)
                (vector-push-extend (%make-source-boundary index line column)
                                    boundaries))))
    (let* ((text (coerce characters 'string))
           (entries (coerce boundaries 'vector))
           (endings (coerce line-endings 'vector)))
      (values text entries (%newline-style endings) endings))))

(defun make-source-buffer (source)
  "Snapshot SOURCE and build normalized text, format metadata, and coordinates.

SOURCE may be a string, character input stream, or octet vector. An initial BOM
remains in the source snapshot and is omitted from the parser-facing text."
  (multiple-value-bind (decoded input-kind encoding octet-bom-p)
      (%decode-source-input source)
    (let* ((source-text (copy-seq decoded))
           (bom-p (or octet-bom-p (%starts-with-bom-p source-text)))
           (initial-offset (if (%starts-with-bom-p source-text) 1 0)))
      (multiple-value-bind (normalized-text boundaries newline-style newline-sequence)
          (%normalize-source-text source-text initial-offset)
        (let* ((format (%make-source-format input-kind encoding bom-p
                                            newline-style newline-sequence))
               (boundary-map (%make-boundary-map source-text boundaries)))
          (%make-source-buffer source-text normalized-text format boundary-map))))))

(defun make-source-span (boundary-map start end)
  "Create a cl-parser-kit span for [START,END) in BOUNDARY-MAP's text."
  (check-type boundary-map boundary-map)
  (check-type start (integer 0 *))
  (check-type end (integer 0 *))
  (unless (<= start end (boundary-map-length boundary-map))
    (error "Invalid source span boundaries [~D,~D) for a mapped length of ~D."
           start end (boundary-map-length boundary-map)))
  (let* ((start-boundary (boundary-map-boundary boundary-map start))
         (end-boundary (boundary-map-boundary boundary-map end)))
    (cl-parser-kit:make-span
     :source (boundary-map-source boundary-map)
     :start (source-boundary-offset start-boundary)
     :end (source-boundary-offset end-boundary)
     :start-line (source-boundary-line start-boundary)
     :start-column (source-boundary-column start-boundary)
     :end-line (source-boundary-line end-boundary)
     :end-column (source-boundary-column end-boundary))))

(defun source-buffer-span (source-buffer start end)
  "Create a span by normalized character boundaries in SOURCE-BUFFER."
  (check-type source-buffer source-buffer)
  (make-source-span (source-buffer-boundary-map source-buffer) start end))

(defun source-buffer-document-span (source-buffer)
  "Return a span over the full original source, including an initial BOM."
  (check-type source-buffer source-buffer)
  (let* ((source (source-buffer-source source-buffer))
         (end-boundary
           (boundary-map-boundary (source-buffer-boundary-map source-buffer)
                                  (boundary-map-length
                                   (source-buffer-boundary-map source-buffer)))))
    (cl-parser-kit:make-span
     :source source
     :start 0
     :end (length source)
     :start-line 1
     :start-column 1
     :end-line (source-boundary-line end-boundary)
     :end-column (source-boundary-column end-boundary))))
