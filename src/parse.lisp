;;;; src/parse.lisp
(in-package #:cl-markdown-kit)

(declaim (ftype function parse-blocks parse-inline))

(defstruct (inline-fragment
             (:constructor make-inline-fragment
                 (&key node text boundary-map)))
  node
  (text "" :type string)
  (boundary-map nil :type boundary-map))

(defstruct (block-parse-result
             (:constructor %make-block-parse-result
                 (&key document inline-fragments reference-index)))
  document
  (inline-fragments nil :type list)
  (reference-index nil :type (or null hash-table)))
(defun make-block-parse-result (&key document
                                      inline-fragments
                                      (reference-index nil reference-index-p))
  (unless reference-index-p
    (setf reference-index (make-hash-table :test 'equal)))
  (check-type reference-index hash-table)
  (%make-block-parse-result :document document
                            :inline-fragments inline-fragments
                            :reference-index reference-index))

(defun %positive-limit (value)
  (unless (and (integerp value) (plusp value))
    (error 'type-error :datum value :expected-type '(integer 1 *)))
  value)

(defun %set-inline-children (owner children)
  (typecase owner
    (paragraph (setf (paragraph-children owner) children))
    (heading (setf (heading-children owner) children))
    (table-cell (setf (table-cell-children owner) children))
    (t (error "An inline fragment owner must be a paragraph, heading, or table cell: ~S"
              owner)))
  owner)

(defun %parse-source-buffer (source-buffer front-matter gfm max-nesting-depth
                             max-inline-delimiter-depth max-input-size)
  (check-type gfm boolean)
  (let* ((nesting-limit (%positive-limit max-nesting-depth))
         (inline-limit (%positive-limit max-inline-delimiter-depth))
         (block-result (parse-blocks source-buffer
                                     :front-matter front-matter
                                     :gfm gfm
                                     :max-nesting-depth nesting-limit))
         (document (block-parse-result-document block-result))
         (reference-index (block-parse-result-reference-index block-result)))
    (check-type document document)
    (dolist (fragment (block-parse-result-inline-fragments block-result))
      (check-type fragment inline-fragment)
      (%set-inline-children
       (inline-fragment-node fragment)
       (parse-inline (inline-fragment-text fragment)
                     (inline-fragment-boundary-map fragment)
                     reference-index
                     :gfm gfm
                     :max-inline-delimiter-depth inline-limit)))
    (setf (node-span document) (source-buffer-document-span source-buffer)
          (document-source document) (source-buffer-source source-buffer)
          (document-source-format document) (source-buffer-format source-buffer)
          (document-reference-index document) reference-index
          (document-front-matter-p document) (not (null front-matter))
          (document-gfm-p document) gfm
          (document-max-nesting-depth document) nesting-limit
          (document-max-inline-delimiter-depth document) inline-limit
          (document-max-input-size document) max-input-size)
    document))

(defun parse-string (string &key (front-matter t) (gfm t)
                               (max-nesting-depth +default-max-nesting-depth+)
                               (max-inline-delimiter-depth +default-max-inline-delimiter-depth+)
                               (max-input-size +default-max-input-size+))
  "Parse STRING using the selected parser options.

FRONT-MATTER defaults to T. When enabled, a leading --- delimited block after
an optional BOM becomes a front-matter node. GFM controls extension parsing.
MAX-INPUT-SIZE must be positive, counts characters, and defaults to
+DEFAULT-MAX-INPUT-SIZE+."
  (check-type string string)
  (let ((input-limit (%positive-limit max-input-size)))
    (%parse-source-buffer
     (%build-source-buffer-from-input string input-limit "parse-string")
     front-matter gfm max-nesting-depth max-inline-delimiter-depth input-limit)))

(defun parse-stream (character-stream &key (front-matter t) (gfm t)
                                       (max-nesting-depth +default-max-nesting-depth+)
                                       (max-inline-delimiter-depth +default-max-inline-delimiter-depth+)
                                       (max-input-size +default-max-input-size+))
  "Parse CHARACTER-STREAM using the selected parser options.

FRONT-MATTER defaults to T. When enabled, a leading --- delimited block after
an optional BOM becomes a front-matter node. GFM controls extension parsing.
MAX-INPUT-SIZE must be positive, counts characters, and defaults to
+DEFAULT-MAX-INPUT-SIZE+. On overflow, the parser consumes the first character
beyond the limit, signals markdown-resource-limit-error, and leaves the rest of
the stream unread."
  (check-type character-stream stream)
  (let ((input-limit (%positive-limit max-input-size)))
    (%parse-source-buffer
     (%build-source-buffer-from-input
      character-stream input-limit "parse-stream")
     front-matter gfm max-nesting-depth max-inline-delimiter-depth input-limit)))

(defun parse-octets (octet-vector &key (front-matter t) (gfm t)
                                    (max-nesting-depth +default-max-nesting-depth+)
                                    (max-inline-delimiter-depth +default-max-inline-delimiter-depth+)
                                    (max-input-size +default-max-input-size+))
  "Decode and parse OCTET-VECTOR using the selected parser options.

FRONT-MATTER defaults to T. When enabled, a leading --- delimited block after
an optional BOM becomes a front-matter node. GFM controls extension parsing.
MAX-INPUT-SIZE must be positive, counts octets, and defaults to
+DEFAULT-MAX-INPUT-SIZE+. The size check runs before decoding."
  (unless (typep octet-vector '(vector (unsigned-byte 8)))
    (error 'type-error :datum octet-vector
           :expected-type '(vector (unsigned-byte 8))))
  (let ((input-limit (%positive-limit max-input-size)))
    (%parse-source-buffer
     (%build-source-buffer-from-input octet-vector input-limit "parse-octets")
     front-matter gfm max-nesting-depth max-inline-delimiter-depth input-limit)))

(defun parse (source &key (front-matter t) (gfm t)
                        (max-nesting-depth +default-max-nesting-depth+)
                        (max-inline-delimiter-depth +default-max-inline-delimiter-depth+)
                        (max-input-size +default-max-input-size+))
  "Parse SOURCE, dispatching by type.

FRONT-MATTER defaults to T. When enabled, a leading --- delimited block after
an optional BOM becomes a front-matter node. GFM controls extension parsing.
MAX-INPUT-SIZE must be positive, counts characters for strings and character
streams and octets for byte vectors, and defaults to +DEFAULT-MAX-INPUT-SIZE+."
  (cond
    ((stringp source)
     (parse-string source
                   :front-matter front-matter
                   :gfm gfm
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth
                   :max-input-size max-input-size))
    ((typep source '(vector (unsigned-byte 8)))
     (parse-octets source
                   :front-matter front-matter
                   :gfm gfm
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth
                   :max-input-size max-input-size))
    ((and (streamp source) (input-stream-p source))
     (parse-stream source
                   :front-matter front-matter
                   :gfm gfm
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth
                   :max-input-size max-input-size))
    (t
     (error 'type-error
            :datum source
            :expected-type '(or string
                                (vector (unsigned-byte 8))
                                (satisfies input-stream-p))))))
