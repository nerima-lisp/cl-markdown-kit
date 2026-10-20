(in-package #:cl-markdown-kit/test)

(describe "Block parser conformance"
          (it
           "compares every CommonMark and GFM block structure with pinned cmark output"
           (let* ((commonmark (commonmark-spec-examples))
                  (gfm (gfm-extension-examples))
                  (examples (append commonmark gfm))
                  (fixtures
                   (block-parser-test-read-data "block-structure-fixtures.lisp"))
                  (exclusions
                   (block-parser-test-read-data
                    "block-structure-exclusions.lisp"))
                  (known-ids (mapcar #'spec-example-id examples))
                  (fixture-ids (mapcar #'first fixtures))
                  (fixture-index (make-hash-table :test 'equal))
                  (matches 0)
                  (failures nil))
             (expect (length commonmark) :to-equal 652)
             (expect (length gfm) :to-equal 30)
             (expect (length fixtures) :to-equal 682)
             (expect fixture-ids :to-equal known-ids)
             (expect exclusions :to-equal nil)
             (dolist (entry fixtures)
               (setf (gethash (first entry) fixture-index) (second entry)))
             (dolist (example examples)
               (let* ((id (spec-example-id example))
                      (expected (gethash id fixture-index)))
                 (multiple-value-bind (actual error-message)
                     (block-parser-test-structure-signature example)
                   (let ((difference
                          (block-parser-test-structure-difference expected
                                                                  actual
                                                                  error-message
                                                                  id)))
                     (cond
                       ((null difference) (incf matches))
                       (t (push (list id difference) failures)))))))
             (dolist (failure (nreverse failures))
               (format t
                       "block structure mismatch ~A: ~S~%"
                       (first failure)
                       (second failure)))
             (format t
                     "Block structures: ~D CommonMark + ~D GFM; ~D match, 0 exclusions.~%"
                     (length commonmark)
                     (length gfm)
                     matches)
             (expect failures :to-equal nil)
             (expect matches :to-equal 682)))
          (it
           "checks T-004 block span properties for every CommonMark and GFM example"
           (let* ((commonmark (commonmark-spec-examples))
                  (gfm (gfm-extension-examples))
                  (examples (append commonmark gfm)))
             (dolist (example examples)
               (let* ((source (spec-example-markdown example))
                      (document
                       (block-parser-test-document
                        (block-parser-test-result source))))
                 (expect (block-parser-test-ranges-valid-p document source)
                         :to-be-truthy)))
             (format t
                     "T-004 block range properties: ~D examples (~D CommonMark, ~D GFM)~%"
                     (length examples)
                     (length commonmark)
                     (length gfm)))))
