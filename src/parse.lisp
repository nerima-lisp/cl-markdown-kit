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
             (:constructor make-block-parse-result
                 (&key document inline-fragments reference-index)))
  document
  (inline-fragments nil :type list)
  (reference-index (make-hash-table :test 'equal) :type hash-table))

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

(defun %parse-source-buffer (source-buffer max-nesting-depth
                             max-inline-delimiter-depth)
  (let* ((nesting-limit (%positive-limit max-nesting-depth))
         (inline-limit (%positive-limit max-inline-delimiter-depth))
         (block-result (parse-blocks source-buffer
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
                     :max-inline-delimiter-depth inline-limit)))
    (setf (node-span document) (source-buffer-document-span source-buffer)
          (document-source document) (source-buffer-source source-buffer)
          (document-source-format document) (source-buffer-format source-buffer)
          (document-reference-index document) reference-index
          (document-max-nesting-depth document) nesting-limit
          (document-max-inline-delimiter-depth document) inline-limit)
    document))

(defun parse-string (string &key (max-nesting-depth 256)
                               (max-inline-delimiter-depth 4096))
  "Parse STRING and return its document AST."
  (check-type string string)
  (%parse-source-buffer (make-source-buffer string)
                        max-nesting-depth max-inline-delimiter-depth))

(defun parse-stream (character-stream &key (max-nesting-depth 256)
                                       (max-inline-delimiter-depth 4096))
  "Parse a character input stream and return its document AST."
  (check-type character-stream stream)
  (%parse-source-buffer (make-source-buffer character-stream)
                        max-nesting-depth max-inline-delimiter-depth))

(defun parse-octets (octet-vector &key (max-nesting-depth 256)
                                    (max-inline-delimiter-depth 4096))
  "Decode and parse an octet vector, retaining its Unicode encoding metadata."
  (unless (typep octet-vector '(vector (unsigned-byte 8)))
    (error 'type-error :datum octet-vector
           :expected-type '(vector (unsigned-byte 8))))
  (%parse-source-buffer (make-source-buffer octet-vector)
                        max-nesting-depth max-inline-delimiter-depth))

(defun parse (source &key (max-nesting-depth 256)
                        (max-inline-delimiter-depth 4096))
  "Parse a string, character input stream, or octet vector."
  (cond
    ((stringp source)
     (parse-string source
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth))
    ((typep source '(vector (unsigned-byte 8)))
     (parse-octets source
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth))
    ((and (streamp source) (input-stream-p source))
     (parse-stream source
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth))
    (t
     (error 'type-error
            :datum source
            :expected-type '(or string
                                (vector (unsigned-byte 8))
                                (satisfies input-stream-p))))))
