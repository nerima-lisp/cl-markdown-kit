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

(defun %parse-source-buffer (source-buffer front-matter max-nesting-depth
                             max-inline-delimiter-depth)
  (let* ((nesting-limit (%positive-limit max-nesting-depth))
         (inline-limit (%positive-limit max-inline-delimiter-depth))
         (block-result (parse-blocks source-buffer
                                     :front-matter front-matter
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

(defun parse-string (string &key (front-matter t)
                               (max-nesting-depth 256)
                               (max-inline-delimiter-depth 4096))
  "Parse STRING and return its document AST; FRONT-MATTER controls leading front matter."
  (check-type string string)
  (%parse-source-buffer (make-source-buffer string)
                        front-matter
                        max-nesting-depth max-inline-delimiter-depth))

(defun parse-stream (character-stream &key (front-matter t)
                                       (max-nesting-depth 256)
                                       (max-inline-delimiter-depth 4096))
  "Parse a character stream; FRONT-MATTER controls leading front matter."
  (check-type character-stream stream)
  (%parse-source-buffer (make-source-buffer character-stream)
                        front-matter
                        max-nesting-depth max-inline-delimiter-depth))

(defun parse-octets (octet-vector &key (front-matter t)
                                    (max-nesting-depth 256)
                                    (max-inline-delimiter-depth 4096))
  "Decode and parse OCTET-VECTOR; FRONT-MATTER controls leading front matter."
  (unless (typep octet-vector '(vector (unsigned-byte 8)))
    (error 'type-error :datum octet-vector
           :expected-type '(vector (unsigned-byte 8))))
  (%parse-source-buffer (make-source-buffer octet-vector)
                        front-matter
                        max-nesting-depth max-inline-delimiter-depth))

(defun parse (source &key (front-matter t)
                        (max-nesting-depth 256)
                        (max-inline-delimiter-depth 4096))
  "Parse SOURCE; FRONT-MATTER controls recognition of a leading front-matter block."
  (cond
    ((stringp source)
     (parse-string source
                   :front-matter front-matter
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth))
    ((typep source '(vector (unsigned-byte 8)))
     (parse-octets source
                   :front-matter front-matter
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth))
    ((and (streamp source) (input-stream-p source))
     (parse-stream source
                   :front-matter front-matter
                   :max-nesting-depth max-nesting-depth
                   :max-inline-delimiter-depth max-inline-delimiter-depth))
    (t
     (error 'type-error
            :datum source
            :expected-type '(or string
                                (vector (unsigned-byte 8))
                                (satisfies input-stream-p))))))
