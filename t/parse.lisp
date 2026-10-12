(in-package #:cl-markdown-kit/test)

(defvar *parser-contract-block-result* nil)
(defvar *parser-contract-block-calls* nil)
(defvar *parser-contract-inline-calls* nil)

(defun parser-contract-blocks (source-buffer &key max-nesting-depth)
  (push (list source-buffer max-nesting-depth)
        *parser-contract-block-calls*)
  (or *parser-contract-block-result*
      (cl-markdown-kit::make-block-parse-result
       :document (cl-markdown-kit:make-document))))

(defun parser-contract-inline (text boundary-map reference-index
                               &key max-inline-delimiter-depth)
  (declare (ignore boundary-map reference-index))
  (push (list text max-inline-delimiter-depth)
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

(defun parser-contract-signals-p (function)
  (handler-case
      (progn (funcall function) nil)
    (serious-condition () t)))

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
             (expect (reverse *parser-contract-inline-calls*)
                     :to-equal '(("paragraph" 34) ("heading" 34) ("cell" 34)))))))))

  (it "dispatches supported input types and rejects invalid inputs and limits"
    (let ((*parser-contract-block-result* nil)
          (*parser-contract-block-calls* nil)
          (*parser-contract-inline-calls* nil))
      (call-with-parser-contract-stubs
       (lambda ()
         (expect (cl-markdown-kit:document-p (cl-markdown-kit:parse "x"))
                 :to-be-truthy)
         (expect (cl-markdown-kit:document-p
                  (cl-markdown-kit:parse
                   (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-contents '(65))))
                 :to-be-truthy)
         (expect (with-input-from-string (stream "x")
                   (cl-markdown-kit:document-p (cl-markdown-kit:parse stream)))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse :invalid)))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse-string 1)))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse-stream 1)))
                 :to-be-truthy)
         (expect (with-output-to-string (stream)
                   (parser-contract-signals-p
                    (lambda ()
                      (cl-markdown-kit:parse-stream stream))))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda () (cl-markdown-kit:parse-octets "x")))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string "x"
                                                  :max-nesting-depth 0)))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string
                     "x" :max-inline-delimiter-depth 0)))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit:parse-string
                     "x" :max-nesting-depth "invalid")))
                 :to-be-truthy)
         (expect (parser-contract-signals-p
                  (lambda ()
                    (cl-markdown-kit::%set-inline-children
                     (cl-markdown-kit:make-front-matter) nil)))
                 :to-be-truthy)))))

  (it "uses the documented default limits in every public entry point"
    (let ((*parser-contract-block-result* nil)
          (*parser-contract-block-calls* nil)
          (*parser-contract-inline-calls* nil))
      (let ((documents
              (call-with-parser-contract-stubs
               (lambda ()
                 (list (cl-markdown-kit:parse-string "string")
                       (with-input-from-string (stream "stream")
                         (cl-markdown-kit:parse-stream stream))
                       (cl-markdown-kit:parse-octets
                        (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-contents '(111)))
                       (cl-markdown-kit:parse "dispatch"))))))
        (expect (mapcar #'cl-markdown-kit:document-max-nesting-depth documents)
                :to-equal '(256 256 256 256))
        (expect
         (mapcar #'cl-markdown-kit:document-max-inline-delimiter-depth documents)
         :to-equal '(4096 4096 4096 4096))
        (expect (mapcar #'second *parser-contract-block-calls*)
                :to-equal '(256 256 256 256))))))
