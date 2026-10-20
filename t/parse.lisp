(in-package #:cl-markdown-kit/test)

(defvar *parser-contract-block-result* nil)
(defvar *parser-contract-block-calls* nil)
(defvar *parser-contract-inline-calls* nil)

(defun parser-contract-blocks (source-buffer &key max-nesting-depth front-matter gfm)
  (push (list source-buffer max-nesting-depth front-matter gfm)
        *parser-contract-block-calls*)
  (or *parser-contract-block-result*
      (cl-markdown-kit::make-block-parse-result
       :document (cl-markdown-kit:make-document))))

(defun parser-contract-inline (text boundary-map reference-index
                               &key max-inline-delimiter-depth gfm)
  (declare (ignore boundary-map reference-index))
  (push (list text max-inline-delimiter-depth gfm)
        *parser-contract-inline-calls*)
  (list (cl-markdown-kit:make-text :value text)))

(defun call-with-parser-contract-stubs (function)
  (let* ((block-symbol 'cl-markdown-kit::parse-blocks)
         (inline-symbol 'cl-markdown-kit::parse-inline)
         (old-block-function
           (and (fboundp block-symbol) (fdefinition block-symbol)))
         (old-inline-function
           (and (fboundp inline-symbol) (fdefinition inline-symbol))))
    (unwind-protect
         (progn
           (setf (fdefinition block-symbol) #'parser-contract-blocks
                 (fdefinition inline-symbol) #'parser-contract-inline)
           (funcall function))
      (if old-block-function
          (setf (fdefinition block-symbol) old-block-function)
          (when (fboundp block-symbol)
            (fmakunbound block-symbol)))
      (if old-inline-function
          (setf (fdefinition inline-symbol) old-inline-function)
          (when (fboundp inline-symbol)
            (fmakunbound inline-symbol))))))

(defun parser-contract-signals-p (function expected-type)
  (handler-case
      (progn (funcall function) nil)
    (serious-condition (condition)
      (typep condition expected-type))))
(defun parser-contract-call-entry-point (entry-point input option value)
  (labels ((call-with-option (function argument)
             (if (eq option :none)
                 (funcall function argument)
                 (funcall function argument option value))))
    (ecase entry-point
      (:parse-string
       (call-with-option #'cl-markdown-kit:parse-string input))
      (:parse-stream
       (with-input-from-string (stream input)
         (call-with-option #'cl-markdown-kit:parse-stream stream)))
      (:parse-octets
       (call-with-option #'cl-markdown-kit:parse-octets input))
      (:dispatch-string
       (call-with-option #'cl-markdown-kit:parse input))
      (:dispatch-stream
       (with-input-from-string (stream input)
         (call-with-option #'cl-markdown-kit:parse stream)))
      (:dispatch-octets
       (call-with-option #'cl-markdown-kit:parse input)))))

(describe "Parser API contract"
  (it "attaches source metadata and inline children to the parsed document"
    (let* ((buffer (cl-markdown-kit:make-source-buffer "x"))
           (boundary-map
             (cl-markdown-kit::source-buffer-boundary-map buffer))
           (document (cl-markdown-kit:make-document))
           (paragraph (cl-markdown-kit:make-paragraph))
           (heading (cl-markdown-kit:make-heading))
           (table-cell (cl-markdown-kit:make-table-cell))
           (reference-index (make-hash-table :test #'equal))
           (block-result
             (cl-markdown-kit::make-block-parse-result
              :document document
              :inline-fragments
              (list (cl-markdown-kit::make-inline-fragment
                     :node paragraph :text "paragraph"
                     :boundary-map boundary-map)
                    (cl-markdown-kit::make-inline-fragment
                     :node heading :text "heading"
                     :boundary-map boundary-map)
                    (cl-markdown-kit::make-inline-fragment
                     :node table-cell :text "cell"
                     :boundary-map boundary-map))
              :reference-index reference-index)))
      (let ((*parser-contract-block-result* block-result)
            (*parser-contract-block-calls* nil)
            (*parser-contract-inline-calls* nil))
        (call-with-parser-contract-stubs
         (lambda ()
           (let ((parsed (cl-markdown-kit:parse-string
                          "x" :max-nesting-depth 12
                          :max-inline-delimiter-depth 34)))
             (expect (eq parsed document) :to-be-truthy)
             (expect (cl-markdown-kit:document-source parsed) :to-equal "x")
             (expect (eq (cl-markdown-kit:document-reference-index parsed)
                         reference-index)
                     :to-be-truthy)
             (expect (eq (cl-markdown-kit:document-source-format parsed)
                         (cl-markdown-kit:source-buffer-format
                          (first (first *parser-contract-block-calls*))))
                     :to-be-truthy)
             (expect (cl-markdown-kit:document-max-nesting-depth parsed)
                     :to-equal 12)
             (expect (cl-markdown-kit:document-max-inline-delimiter-depth parsed)
                     :to-equal 34)
             (expect (typep (cl-markdown-kit:node-span parsed)
                            'cl-parser-kit:span)
                     :to-be-truthy)
             (expect (mapcar #'cl-markdown-kit:text-value
                             (cl-markdown-kit:paragraph-children paragraph))
                     :to-equal '("paragraph"))
             (expect (mapcar #'cl-markdown-kit:text-value
                             (cl-markdown-kit:heading-children heading))
                     :to-equal '("heading"))
             (expect (mapcar #'cl-markdown-kit:text-value
                             (cl-markdown-kit:table-cell-children table-cell))
                     :to-equal '("cell"))
             (expect (second (first *parser-contract-block-calls*))
                     :to-equal 12)
             (expect (third (first *parser-contract-block-calls*))
                     :to-equal t)
             (expect (reverse *parser-contract-inline-calls*)
                     :to-equal '(("paragraph" 34 t) ("heading" 34 t)
                                 ("cell" 34 t)))))))))
  (it "builds a fresh reference index when omitted"
    (let* ((first-result (cl-markdown-kit::make-block-parse-result))
           (second-result (cl-markdown-kit::make-block-parse-result))
           (first-index
             (cl-markdown-kit::block-parse-result-reference-index first-result))
           (second-index
             (cl-markdown-kit::block-parse-result-reference-index second-result)))
      (expect (hash-table-p first-index) :to-be-truthy)
      (expect (zerop (hash-table-count first-index)) :to-be-truthy)
      (expect (not (eq first-index second-index)) :to-be-truthy)
      (expect (cl-markdown-kit::block-parse-result-inline-fragments first-result)
              :to-equal nil)
      (expect (parser-contract-signals-p
                  (lambda ()
                 (cl-markdown-kit::make-block-parse-result
                  :reference-index nil))
                  (quote type-error))
              :to-be-truthy)))
  (it "enforces input-size limits before parsing, reading further, or decoding octets"
    (let ((*parser-contract-block-result* nil)
          (*parser-contract-block-calls* nil)
          (*parser-contract-inline-calls* nil))
      (labels ((capture-resource-limit-error (function)
                 (handler-case
                     (progn (funcall function) nil)
                   (cl-markdown-kit:markdown-resource-limit-error (condition)
                     condition))))
        (call-with-parser-contract-stubs
         (lambda ()
           (expect
            (cl-markdown-kit:document-max-input-size
             (cl-markdown-kit:parse-string "abc" :max-input-size 3))
            :to-equal 3)
           (let ((condition
                   (capture-resource-limit-error
                    (lambda ()
                      (cl-markdown-kit:parse-string "abcd" :max-input-size 3)))))
             (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
                      condition)
                     :to-equal :input-size)
             (expect (cl-markdown-kit:markdown-resource-limit-error-limit condition)
                     :to-equal 3)
             (expect (cl-markdown-kit:markdown-resource-limit-error-actual condition)
                     :to-equal 4)
             (expect (cl-markdown-kit:markdown-resource-limit-error-context condition)
                     :to-equal "parse-string"))
           (with-input-from-string (stream "abc")
             (expect
              (cl-markdown-kit:document-max-input-size
               (cl-markdown-kit:parse-stream stream :max-input-size 3))
              :to-equal 3))
           (with-input-from-string (stream "abcde")
             (let ((condition
                     (capture-resource-limit-error
                      (lambda ()
                        (cl-markdown-kit:parse-stream stream :max-input-size 3)))))
               (progn
                 (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
                          condition)
                         :to-equal :input-size)
                 (expect (cl-markdown-kit:markdown-resource-limit-error-limit
                          condition)
                         :to-equal 3)
                 (expect (cl-markdown-kit:markdown-resource-limit-error-actual
                          condition)
                         :to-equal 4))
               (expect (cl-markdown-kit:markdown-resource-limit-error-context
                        condition)
                       :to-equal "parse-stream")
               (expect (read-char stream nil nil) :to-equal #\e)))
           (let ((octets
                   (make-array 3 :element-type '(unsigned-byte 8)
                                 :initial-contents '(65 66 67))))
             (expect
              (cl-markdown-kit:document-max-input-size
               (cl-markdown-kit:parse-octets octets :max-input-size 3))
              :to-equal 3))
           (let ((condition
                   (capture-resource-limit-error
                    (lambda ()
                      (cl-markdown-kit:parse-octets
                       (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(255 255 255 255))
                       :max-input-size 3)))))
             (progn
               (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
                        condition)
                       :to-equal :input-size)
               (expect (cl-markdown-kit:markdown-resource-limit-error-limit
                        condition)
                       :to-equal 3)
               (expect (cl-markdown-kit:markdown-resource-limit-error-actual
                        condition)
                       :to-equal 4))
             (expect (cl-markdown-kit:markdown-resource-limit-error-context
                      condition)
                     :to-equal "parse-octets"))
           (expect
            (parser-contract-signals-p
             (lambda ()
               (cl-markdown-kit:parse-string "x" :max-input-size 0))
             (quote type-error))
            :to-be-truthy))))))

  (it "dispatches supported input types and rejects invalid inputs and limits"
    (let ((*parser-contract-block-result* nil)
          (*parser-contract-block-calls* nil)
          (*parser-contract-inline-calls* nil))
      (call-with-parser-contract-stubs
       (lambda ()
         (expect (typep (cl-markdown-kit:parse "x") 'cl-markdown-kit:document)
                 :to-be-truthy)
         (expect (typep (cl-markdown-kit:parse
                   (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-contents '(65))) 'cl-markdown-kit:document)
                 :to-be-truthy)
         (expect (with-input-from-string (stream "x")
                   (typep (cl-markdown-kit:parse stream) 'cl-markdown-kit:document))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse :invalid))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse-string 1))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse-stream 1))
                  (quote type-error))
                 :to-be-truthy)
         (expect (with-output-to-string (stream)
                   (parser-contract-signals-p
                  (lambda ()
                      (cl-markdown-kit:parse-stream stream))
                  (quote type-error)))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse-octets "x"))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string "x"
                                                  :max-nesting-depth 0))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string
                     "x" :max-inline-delimiter-depth 0))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string "" :gfm :invalid))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string
                     "x" :max-nesting-depth "invalid"))
                  (quote type-error))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit::%set-inline-children (cl-markdown-kit:make-front-matter) nil))
                  (quote simple-error))
                 :to-be-truthy)))))

  (cl-weave:it-each ((:parse-string "string")
      (:parse-stream "stream")
      (:parse-octets
      (make-array 1 :element-type (unsigned-byte 8) :initial-contents (111)))
      (:dispatch-string "dispatch")
      (:dispatch-stream "dispatch stream")
      (:dispatch-octets
       (make-array 1 :element-type (unsigned-byte 8) :initial-contents (111))))
    "entry point ~A: uses the documented default limits in every public entry point"
    (entry-point input)
    (let ((input (if (consp input) (apply (first input) (rest input)) input)))
      (let ((*parser-contract-block-result* nil)
            (*parser-contract-block-calls* nil)
            (*parser-contract-inline-calls* nil))
        (let ((document
                (call-with-parser-contract-stubs
                 (lambda ()
                   (parser-contract-call-entry-point
                    entry-point input :none nil)))))
          (expect (cl-markdown-kit:document-max-nesting-depth document)
                  :to-equal 256)
          (progn
            (expect
             (cl-markdown-kit:document-max-inline-delimiter-depth document)
             :to-equal 4096)
            (expect
             (cl-markdown-kit:document-max-input-size document)
             :to-equal cl-markdown-kit:+default-max-input-size+))
          (expect (second (first *parser-contract-block-calls*))
                  :to-equal 256)
          (expect (third (first *parser-contract-block-calls*))
                  :to-equal t)
          (progn
            (expect (fourth (first *parser-contract-block-calls*))
                    :to-equal t)
            (let ((limited-document
                    (call-with-parser-contract-stubs
                     (lambda ()
                       (parser-contract-call-entry-point
                        entry-point input :max-input-size 14)))))
              (expect (cl-markdown-kit:document-max-input-size limited-document)
                      :to-equal 14)))))))

  (cl-weave:it-each ((:parse-string "string")
     (:parse-stream "stream")
     (:parse-octets
      (make-array 1 :element-type (unsigned-byte 8) :initial-contents (120)))
     (:dispatch-string "dispatch string")
     (:dispatch-stream "dispatch stream")
     (:dispatch-octets
      (make-array 1 :element-type (unsigned-byte 8) :initial-contents (120))))
    "entry point ~A: forwards the front-matter option through every public input entry point"
    (entry-point input)
    (let ((input (if (consp input) (apply (first input) (rest input)) input)))
      (let ((*parser-contract-block-result* nil)
            (*parser-contract-block-calls* nil)
            (*parser-contract-inline-calls* nil))
        (call-with-parser-contract-stubs
         (lambda ()
           (parser-contract-call-entry-point
            entry-point input :front-matter nil)))
        (expect (third (first *parser-contract-block-calls*))
                :to-equal nil))))

  (cl-weave:it-each ((:parse-string "string")
     (:parse-stream "stream")
     (:parse-octets
      (make-array 1 :element-type (unsigned-byte 8) :initial-contents (120)))
     (:dispatch-string "dispatch string")
     (:dispatch-stream "dispatch stream")
     (:dispatch-octets
      (make-array 1 :element-type (unsigned-byte 8) :initial-contents (120))))
    "entry point ~A: forwards the GFM option through every public input entry point"
    (entry-point input)
    (let ((input (if (consp input) (apply (first input) (rest input)) input)))
      (let* ((source-buffer (cl-markdown-kit:make-source-buffer "x"))
             (*parser-contract-block-result*
               (cl-markdown-kit::make-block-parse-result
                :document (cl-markdown-kit:make-document)
                :inline-fragments
                (list (cl-markdown-kit::make-inline-fragment
                       :node (cl-markdown-kit:make-paragraph)
                       :text "x"
                       :boundary-map
                       (cl-markdown-kit::source-buffer-boundary-map
                        source-buffer)))))
             (*parser-contract-block-calls* nil)
             (*parser-contract-inline-calls* nil))
        (call-with-parser-contract-stubs
         (lambda ()
           (parser-contract-call-entry-point
            entry-point input :gfm nil)))
        (expect (fourth (first *parser-contract-block-calls*))
                :to-equal nil)
        (expect (third (first *parser-contract-inline-calls*))
                :to-equal nil))))

  (it "recognizes leading front matter by default and can disable it"
    (let* ((source (format nil "---~%title: sample~%---~%# body~%"))
           (default-children
             (cl-markdown-kit:document-children
              (cl-markdown-kit:parse-string source)))
           (enabled-children
             (cl-markdown-kit:document-children
              (cl-markdown-kit:parse-string source :front-matter t)))
           (disabled-children
             (cl-markdown-kit:document-children
              (cl-markdown-kit:parse-string source :front-matter nil))))
      (expect (cl-markdown-kit:front-matter-p (first default-children))
              :to-be-truthy)
      (expect (cl-markdown-kit:front-matter-p (first enabled-children))
              :to-be-truthy)
      (expect (find-if #'cl-markdown-kit:front-matter-p disabled-children)
              :to-equal nil)
      (expect (typep (first disabled-children) 'cl-markdown-kit:thematic-break)
              :to-be-truthy))))
