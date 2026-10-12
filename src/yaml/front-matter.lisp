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
  (loop with text-length = (length text)
        with line-start = 0
        while (< line-start text-length)
        for line-end = (or (position-if (lambda (character)
                                          (find character '(#\Return #\Newline)))
                                        text :start line-start)
                           text-length)
        for first-content = (position-if-not (lambda (character)
                                               (find character '(#\Space #\Tab)))
                                             text :start line-start :end line-end)
        unless (or (null first-content)
                   (char= (char text first-content) #\#))
          do (return nil)
        do (setf line-start
                 (if (< line-end text-length)
                     (if (and (char= (char text line-end) #\Return)
                              (< (1+ line-end) text-length)
                              (char= (char text (1+ line-end)) #\Newline))
                         (+ line-end 2)
                         (1+ line-end))
                     text-length))
        finally (return t)))

(defun %yaml-error-without-mark-p (condition)
  (typecase condition
    (yaml-kit:yaml-parse-error
     (let ((line (yaml-kit:yaml-parse-error-line condition))
           (column (yaml-kit:yaml-parse-error-column condition)))
       (or (not (and (integerp line) (integerp column)))
           (and (= line 1) (= column 1)
                (zerop (yaml-kit:yaml-parse-error-offset condition))))))
    (yaml-kit:yaml-compose-error
     (null (yaml-kit:yaml-compose-error-mark condition)))
    (t t)))

(defun %yaml-error-line-column (condition)
  (typecase condition
    (yaml-kit:yaml-parse-error
     (unless (%yaml-error-without-mark-p condition)
       (values (yaml-kit:yaml-parse-error-line condition)
               (yaml-kit:yaml-parse-error-column condition))))
    (yaml-kit:yaml-compose-error
     (unless (%yaml-error-without-mark-p condition)
       (let ((mark (yaml-kit:yaml-compose-error-mark condition)))
         (values (yaml-kit:mark-line mark)
                 (yaml-kit:mark-column mark)))))))

(defun %markdown-yaml-error-location (front-matter condition)
  (let ((span (cl-markdown-kit:node-span front-matter)))
    (when span
      (multiple-value-bind (line column)
          (%yaml-error-line-column condition)
        (if (and (integerp line) (integerp column))
            (values (+ 1 (cl-parser-kit:span-start-line span) line)
                    (1+ column))
            (values (1+ (cl-parser-kit:span-start-line span)) 1))))))

(defun %wrap-markdown-yaml-error (front-matter condition)
  (multiple-value-bind (line column)
      (%markdown-yaml-error-location front-matter condition)
    (error 'markdown-yaml-error
           :yaml-condition condition
           :line line
           :column column)))

(defun parse-front-matter (node-or-document &rest yaml-parse-options)
  "Parse a front-matter node or a document's front matter with YAML-KIT options."
  (let ((front-matter (%front-matter-node node-or-document)))
    (when front-matter
      (let ((raw-text (cl-markdown-kit:front-matter-raw-text front-matter)))
        (unless (%empty-yaml-content-p raw-text)
          (handler-case
              (apply #'yaml-kit:parse
                     ;; The YAML scanner requires a character array, not a base string.
                     (make-array (length raw-text) :element-type 'character
                                                  :initial-contents raw-text)
                     yaml-parse-options)
            (yaml-kit:yaml-parse-error (condition)
              (%wrap-markdown-yaml-error front-matter condition))
            (yaml-kit:yaml-compose-error (condition)
              (%wrap-markdown-yaml-error front-matter condition))))))))
