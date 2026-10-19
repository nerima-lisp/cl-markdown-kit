(in-package #:cl-markdown-kit/test)

(describe "Inline conformance fixtures"
  (it "parses inline content in every pinned CommonMark and GFM example"
    (let ((examples (append (commonmark-spec-examples)
                            (gfm-extension-examples))))
      (dolist (example examples)
        (let* ((source (spec-example-markdown example))
               (document (cl-markdown-kit:parse-string source)))
          (expect (inline-parser-test-ranges-valid-p document source)
                  :to-be-truthy)))
      (expect (length examples) :to-equal 682))))

(describe "Inline structure conformance fixtures"
  (it "compares every CommonMark and GFM inline structure with pinned cmark output"
    (let* ((commonmark (commonmark-spec-examples))
           (gfm (gfm-extension-examples))
           (examples (append commonmark gfm))
           (fixtures (inline-parser-test-read-data
                      "inline-structure-fixtures.lisp"))
           (exclusions (inline-parser-test-read-data
                        "inline-structure-exclusions.lisp"))
           (known-ids (mapcar #'spec-example-id examples))
           (fixture-ids (mapcar #'first fixtures))
           (fixture-index (make-hash-table :test 'equal))
           (exclusion-index (make-hash-table :test 'equal))
           (matches 0)
           (failures nil))
      (expect (length commonmark) :to-equal 652)
      (expect (length gfm) :to-equal 30)
      (expect (length fixtures) :to-equal 682)
      (expect fixture-ids :to-equal known-ids)
      (expect (every (lambda (entry)
                       (and (= (length entry) 2)
                            (stringp (first entry))
                            (stringp (second entry))
                            (plusp (length (second entry)))
                            (member (first entry) known-ids :test #'string=)))
                     exclusions)
              :to-be-truthy)
      (expect (length (remove-duplicates (mapcar #'first exclusions)
                                         :test #'string=))
              :to-equal (length exclusions))
      (dolist (entry fixtures)
        (setf (gethash (first entry) fixture-index) (second entry)))
      (dolist (entry exclusions)
        (setf (gethash (first entry) exclusion-index) (second entry)))
      (dolist (example examples)
        (let* ((id (spec-example-id example))
               (reason (gethash id exclusion-index)))
          (unless reason
            (multiple-value-bind (actual error-message)
                (inline-parser-test-structure-signature example)
              (let ((difference
                      (inline-parser-test-structure-difference
                       (gethash id fixture-index) actual error-message)))
                (if difference
                    (push (list id difference) failures)
                    (incf matches)))))))
      (dolist (failure (nreverse failures))
        (format t "inline structure mismatch ~A: ~S~%"
                (first failure) (second failure)))
      (format t "Inline structures: ~D CommonMark + ~D GFM; ~D match, ~D excluded.~%"
              (length commonmark) (length gfm) matches (length exclusions))
      (expect (+ matches (length exclusions)) :to-equal (length examples))
      (expect failures :to-equal nil))))
