(in-package #:cl-markdown-kit/yaml)

(define-condition markdown-yaml-error (cl-markdown-kit:markdown-kit-error)
  ((yaml-condition :initarg :yaml-condition
                   :reader markdown-yaml-error-yaml-condition)
   (line :initarg :line :initform nil :reader markdown-yaml-error-line)
   (column :initarg :column :initform nil :reader markdown-yaml-error-column))
  (:report (lambda (condition stream)
             (if (markdown-yaml-error-line condition)
                 (format stream
                         "Invalid YAML front matter at Markdown line ~D, column ~D: ~A"
                         (markdown-yaml-error-line condition)
                         (markdown-yaml-error-column condition)
                         (markdown-yaml-error-yaml-condition condition))
                 (format stream "Invalid YAML front matter: ~A"
                         (markdown-yaml-error-yaml-condition condition))))))

(defun %front-matter-node (node-or-document)
  (typecase node-or-document
    (cl-markdown-kit:front-matter node-or-document)
    (cl-markdown-kit:document
     (find-if #'cl-markdown-kit:front-matter-p
              (cl-markdown-kit:document-children node-or-document)))
    (t (error 'type-error :datum node-or-document
              :expected-type '(or cl-markdown-kit:front-matter
                                  cl-markdown-kit:document)))))

(defun %empty-yaml-content-p (text)
  (every (lambda (character)
           (find character '(#\Space #\Tab #\Return #\Newline)))
         text))

(defun %yaml-error-without-mark-p (condition)
  (let ((line (yaml-kit:yaml-parse-error-line condition))
        (column (yaml-kit:yaml-parse-error-column condition)))
    (or (not (and (integerp line) (integerp column)))
        (and (= line 1) (= column 1)
             (zerop (yaml-kit:yaml-parse-error-offset condition))))))

(defun %markdown-yaml-error-location (front-matter condition)
  (let ((span (cl-markdown-kit:node-span front-matter)))
    (when span
      (if (%yaml-error-without-mark-p condition)
          (values (1+ (cl-parser-kit:span-start-line span)) 1)
          (values (+ 1
                     (cl-parser-kit:span-start-line span)
                     (yaml-kit:yaml-parse-error-line condition))
                  (1+ (yaml-kit:yaml-parse-error-column condition)))))))

(defun parse-front-matter (node-or-document &rest yaml-parse-options)
  "Parse a front-matter node or a document's front matter with YAML-KIT options."
  (let ((front-matter (%front-matter-node node-or-document)))
    (when front-matter
      (let ((raw-text (cl-markdown-kit:front-matter-raw-text front-matter)))
        (handler-case
            (apply #'yaml-kit:parse
                   ;; The YAML scanner requires a character array, not a base string.
                   (make-array (length raw-text) :element-type 'character
                                                :initial-contents raw-text)
                   yaml-parse-options)
          (yaml-kit:yaml-parse-error (condition)
            (multiple-value-bind (line column)
                (%markdown-yaml-error-location front-matter condition)
              (error 'markdown-yaml-error
                     :yaml-condition condition
                     :line line
                     :column column)))
          (yaml-kit:yaml-compose-error (condition)
            (if (and (%empty-yaml-content-p raw-text)
                     (equal (yaml-kit:yaml-compose-error-cause condition)
                            "unsupported node type"))
                nil
                (error condition))))))))
