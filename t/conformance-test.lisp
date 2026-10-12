(in-package #:cl-markdown-kit/test)

(describe "Markdown conformance fixtures"
  (it "reads the pinned CommonMark and GFM extension examples"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm)))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (every #'spec-example-valid-p examples) :to-be-truthy)
      (expect (conformance-exclusions-valid-p
               examples (conformance-read-exclusions))
              :to-be-truthy)
      (format t "CommonMark 0.31.2 examples: ~D~%" (length commonmark))
      (format t "cmark-gfm extension examples: ~D~%" (length gfm))))

  (it "reports parser conditions with and without custom messages"
    (let* ((base-condition
             (make-condition 'cl-markdown-kit:markdown-kit-error))
           (base-message-condition
             (make-condition 'cl-markdown-kit:markdown-kit-error
                             :message "custom parser error"))
           (resource-default-condition
             (make-condition 'cl-markdown-kit:markdown-resource-limit-error))
           (resource-condition
             (make-condition 'cl-markdown-kit:markdown-resource-limit-error
                             :limit-name :nesting-depth
                             :limit 64
                             :actual 65
                             :context "document"))
           (resource-message-condition
             (make-condition 'cl-markdown-kit:markdown-resource-limit-error
                             :limit-name :nesting-depth
                             :limit 64
                             :actual 65
                             :context "document"
                             :message "maximum nesting depth exceeded")))
      (expect (princ-to-string base-condition) :to-equal "Markdown error")
      (expect (princ-to-string base-message-condition)
              :to-equal "custom parser error")
      (expect (cl-markdown-kit:markdown-kit-error-message base-condition)
              :to-equal nil)
      (expect (cl-markdown-kit:markdown-kit-error-message
               base-message-condition)
              :to-equal "custom parser error")
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
               resource-default-condition)
              :to-equal "resource")
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit
               resource-default-condition)
              :to-equal nil)
      (expect (cl-markdown-kit:markdown-resource-limit-error-actual
               resource-default-condition)
              :to-equal nil)
      (expect (cl-markdown-kit:markdown-resource-limit-error-context
               resource-default-condition)
              :to-equal "Markdown input")
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit-name
               resource-condition)
              :to-equal :nesting-depth)
      (expect (cl-markdown-kit:markdown-resource-limit-error-limit
               resource-condition)
              :to-equal 64)
      (expect (cl-markdown-kit:markdown-resource-limit-error-actual
               resource-condition)
              :to-equal 65)
      (expect (cl-markdown-kit:markdown-resource-limit-error-context
               resource-condition)
              :to-equal "document")
      (expect (search "Markdown resource limit exceeded in document"
                      (princ-to-string resource-condition))
              :to-be-truthy)
      (expect (search "; maximum nesting depth exceeded"
                      (princ-to-string resource-message-condition))
              :to-be-truthy))))

(defun %conformance-html-fixture-text (string)
  (substitute #\Tab #\→ string))

(defun %conformance-html-example-p (example)
  (not (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (spec-example-html example))
                "<IGNORE>")))

(defun %conformance-html-render-result (example)
  (let ((document
          (handler-case
              (cl-markdown-kit:parse-string
               (%conformance-html-fixture-text
                (spec-example-markdown example))
               :front-matter nil)
            (error (condition)
              (return-from %conformance-html-render-result
                (values :parse-error condition))))))
    (handler-case
        (let ((html (cl-markdown-kit:write-html
                     document :raw-html :pass-through :unsafe-urls t))
              (expected (%conformance-html-fixture-text
                         (spec-example-html example))))
          (if (string= html expected)
              (values :match nil)
              (values :mismatch html)))
      (error (condition)
        (values :html-writer-error condition)))))

(defun %conformance-html-difference (expected actual)
  (let ((limit (min (length expected) (length actual))))
    (loop for index from 0 below limit
          unless (char= (char expected index) (char actual index))
            do (return index)
          finally (return limit))))

(defun %conformance-html-context (string difference)
  (subseq string (max 0 (- difference 32))
          (min (length string) (+ difference 64))))

(defun %check-html-conformance (name examples exclusions)
  (let* ((failures nil)
         (exclusion-map (make-hash-table :test #'equal))
         (stale-exclusions nil))
    (dolist (exclusion exclusions)
      (setf (gethash (first exclusion) exclusion-map) (third exclusion)))
    (dolist (example examples)
      (multiple-value-bind (status detail)
          (%conformance-html-render-result example)
        (unless (eq status :match)
          (push (list example status detail) failures))))
    (setf failures (nreverse failures))
    (let ((excluded-count 0)
          (unexcluded-failures nil))
      (dolist (failure failures)
        (let* ((example (first failure))
               (id (spec-example-id example))
               (reason (gethash id exclusion-map)))
          (if reason
              (progn
                (incf excluded-count)
                (format t "~A excluded ~A: ~A~%" name id reason))
              (push failure unexcluded-failures))))
      (dolist (exclusion exclusions)
        (unless (find (first exclusion) failures
                      :key (lambda (failure)
                             (spec-example-id (first failure)))
                      :test #'string=)
          (push exclusion stale-exclusions)))
      (setf unexcluded-failures (nreverse unexcluded-failures)
            stale-exclusions (nreverse stale-exclusions))
      (format t "~A HTML conformance: ~D/~D matched, ~D excluded~%"
              name (- (length examples) (length failures))
              (length examples) excluded-count)
      (when unexcluded-failures
        (format t "~A failing example IDs: ~{~A~^, ~}~%"
                name (mapcar (lambda (failure)
                               (spec-example-id (first failure)))
                             unexcluded-failures)))
      (dolist (failure unexcluded-failures)
        (destructuring-bind (example status detail) failure
          (if (member status '(:parse-error :html-writer-error))
              (format t "~A ~A ~A: ~A~%"
                      name status (spec-example-id example) detail)
              (let* ((expected (%conformance-html-fixture-text
                                (spec-example-html example)))
                     (difference (%conformance-html-difference expected detail)))
                (format t "~A HTML mismatch ~A at ~D; expected ~S, actual ~S~%"
                        name (spec-example-id example) difference
                        (%conformance-html-context expected difference)
                        (%conformance-html-context detail difference))))))
      (dolist (exclusion stale-exclusions)
        (format t "~A stale HTML exclusion ~A~%" name (first exclusion)))
      (expect excluded-count :to-equal 0)
      (expect (length unexcluded-failures) :to-equal 0)
      (expect (length stale-exclusions) :to-equal 0))))

(describe "HTML conformance"
  (it "matches all CommonMark 0.31.2 HTML examples"
    (let* ((examples (commonmark-spec-examples))
           (exclusions (remove-if-not
                        (lambda (entry) (find (first entry) examples
                                              :key #'spec-example-id
                                              :test #'string=))
                        (conformance-read-exclusions))))
      (%check-html-conformance "T-001" examples exclusions)))

  (it "matches all GFM extension and footnote HTML examples"
    (let* ((all-examples (gfm-extension-examples))
           (examples (remove-if-not #'%conformance-html-example-p all-examples))
           (exclusions (remove-if-not
                        (lambda (entry) (find (first entry) examples
                                              :key #'spec-example-id
                                              :test #'string=))
                        (conformance-read-exclusions))))
      (format t "T-002 fixture placeholders omitted: ~D~%"
              (- (length all-examples) (length examples)))
      (%check-html-conformance "T-002" examples exclusions))))
