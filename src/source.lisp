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
             (:constructor %make-boundary-map
                 (source boundaries normalized-length
                  normalized-line-starts source-line-starts parent-map
                  segment-starts segment-maps)))
  (source "" :type string)
  (boundaries nil :type (or null vector))
  (normalized-length 0 :type (integer 0 *))
  (normalized-line-starts #() :type vector)
  (source-line-starts #() :type vector)
  parent-map
  (segment-starts #() :type vector)
  (segment-maps #() :type vector))

(defun make-boundary-map (source boundaries)
  "Create a boundary map over SOURCE from a vector of source boundaries.

BOUNDARIES has one entry for each text boundary, including both ends."
  (check-type source string)
  (let ((entries (copy-seq (coerce boundaries 'vector))))
    (when (zerop (length entries))
      (error "A boundary map must contain at least its initial boundary."))
    (loop for boundary across entries
          do (check-type boundary source-boundary))
    (%make-boundary-map source entries (1- (length entries)) #() #() nil #() #())))

(defun boundary-map-length (boundary-map)
  "Return the number of characters represented by BOUNDARY-MAP."
  (check-type boundary-map boundary-map)
  (if (boundary-map-boundaries boundary-map)
      (1- (length (boundary-map-boundaries boundary-map)))
      (boundary-map-normalized-length boundary-map)))

(defun %boundary-map-line-index (line-starts index)
  (let ((low 0)
        (high (length line-starts)))
    (loop while (< low high)
          for middle = (floor (+ low high) 2)
          do (if (<= (aref line-starts middle) index)
                 (setf low (1+ middle))
                 (setf high middle)))
    (1- low)))

(defun boundary-map-boundary (boundary-map index)
  "Return the source coordinate at text boundary INDEX."
  (check-type boundary-map boundary-map)
  (let ((boundaries (boundary-map-boundaries boundary-map))
        (parent-map (boundary-map-parent-map boundary-map)))
    (cond
      (boundaries
       (aref boundaries index))
      (parent-map
       (let* ((length (boundary-map-normalized-length boundary-map)))
          (check-type index (integer 0 *))
          (unless (<= index length)
            (error "Boundary index ~D is outside the mapped range [0,~D]."
                   index length))
          (let* ((line-index
                   (%boundary-map-line-index
                    (boundary-map-segment-starts boundary-map) index))
                 (fragment-line-start
                   (aref (boundary-map-segment-starts boundary-map) line-index))
                 (source-index
                   (aref (aref (boundary-map-segment-maps boundary-map)
                               line-index)
                         (- index fragment-line-start))))
            (boundary-map-boundary parent-map source-index))))
      (t
       (let* ((length (boundary-map-normalized-length boundary-map)))
         (check-type index (integer 0 *))
         (unless (<= index length)
           (error "Boundary index ~D is outside the mapped range [0,~D]."
                  index length))
         (let* ((line-index
                  (%boundary-map-line-index
                   (boundary-map-normalized-line-starts boundary-map) index))
                (normalized-line-start
                  (aref (boundary-map-normalized-line-starts boundary-map)
                        line-index))
                (source-line-start
                  (aref (boundary-map-source-line-starts boundary-map)
                        line-index))
                (column-offset (- index normalized-line-start)))
           (%make-source-boundary (+ source-line-start column-offset)
                                  (1+ line-index)
                                  (1+ column-offset))))))))

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
     (values source :string nil nil))
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

(defun %source-normalization-counts (source initial-offset)
  (let ((newline-count 0)
        (crlf-count 0)
        (index initial-offset)
        (source-length (length source)))
    (loop while (< index source-length)
          do (case (char-code (char source index))
               (13
                (incf newline-count)
                (if (and (< (1+ index) source-length)
                         (char= (char source (1+ index)) #\Newline))
                    (progn (incf crlf-count) (incf index 2))
                    (incf index)))
               (10
                (incf newline-count)
                (incf index))
               (otherwise (incf index))))
    (values newline-count crlf-count)))

(defun %normalize-source-text (source initial-offset)
  (multiple-value-bind (newline-count crlf-count)
      (%source-normalization-counts source initial-offset)
    (let* ((source-length (length source))
           (normalized-length (- source-length initial-offset crlf-count))
           (text (make-string normalized-length))
           (normalized-line-starts (make-array (1+ newline-count)))
           (source-line-starts (make-array (1+ newline-count)))
           (line-endings (make-array newline-count))
           (normalized-index 0)
           (source-index initial-offset)
           (line-index 0))
      (setf (aref normalized-line-starts 0) 0
            (aref source-line-starts 0) initial-offset)
      (loop while (< source-index source-length)
            for character = (char source source-index)
            do (cond
                 ((char= character #\Return)
                  (let ((crlf-p (and (< (1+ source-index) source-length)
                                     (char= (char source (1+ source-index))
                                            #\Newline))))
                    (setf (aref line-endings line-index)
                          (if crlf-p :crlf :cr))
                    (incf line-index)
                    (setf (aref text normalized-index) #\Newline)
                    (incf normalized-index)
                    (incf source-index (if crlf-p 2 1))
                    (setf (aref normalized-line-starts line-index)
                          normalized-index
                          (aref source-line-starts line-index) source-index)))
                 ((char= character #\Newline)
                  (setf (aref line-endings line-index) :lf)
                  (incf line-index)
                  (setf (aref text normalized-index) #\Newline)
                  (incf normalized-index)
                  (incf source-index)
                  (setf (aref normalized-line-starts line-index)
                        normalized-index
                        (aref source-line-starts line-index) source-index))
                 (t
                  (setf (aref text normalized-index) character)
                  (incf normalized-index)
                  (incf source-index))))
      (assert (= normalized-index normalized-length))
      (assert (= line-index newline-count))
      (values text normalized-line-starts source-line-starts
              (%newline-style line-endings) line-endings))))

(defun make-source-buffer (source)
  "Snapshot SOURCE and build normalized text, format metadata, and coordinates.

SOURCE may be a string, character input stream, or octet vector. An initial BOM
remains in the source snapshot and is omitted from the parser-facing text."
  (multiple-value-bind (decoded input-kind encoding octet-bom-p)
      (%decode-source-input source)
    (let* ((source-text (copy-seq decoded))
           (bom-p (or octet-bom-p (%starts-with-bom-p source-text)))
           (initial-offset (if (%starts-with-bom-p source-text) 1 0)))
      (multiple-value-bind (normalized-text normalized-line-starts
                            source-line-starts newline-style newline-sequence)
          (%normalize-source-text source-text initial-offset)
        (let* ((format (%make-source-format input-kind encoding bom-p
                                            newline-style newline-sequence))
               (boundary-map
                 (%make-boundary-map source-text nil (length normalized-text)
                                     normalized-line-starts source-line-starts
                                     nil #() #())))
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
