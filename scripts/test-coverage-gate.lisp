(eval-when (:compile-toplevel :load-toplevel :execute) (require :asdf))
(defparameter *coverage-gate-test-completed* nil)

(let* ((script
        (or *load-truename*
            *compile-file-truename*
            (error "Unable to determine the coverage gate test location")))
       (root
        (uiop:pathname-parent-directory-pathname
         (uiop:pathname-directory-pathname script))))
  (uiop:chdir root)
  (load (merge-pathnames "scripts/coverage-gate.lisp" root))
  (let* ((temporary-directory
          (merge-pathnames
           (format nil "cl-markdown-kit-coverage-gate-~D/"
                   (get-internal-real-time))
           (uiop:ensure-directory-pathname (uiop:temporary-directory))))
         (source-directory (merge-pathnames "source/" temporary-directory))
         (summary-file (merge-pathnames "per-file.tsv" temporary-directory))
         (assertion-count 0))
    (labels ((check (condition message)
               (incf assertion-count)
               (unless condition
                 (error "Coverage gate regression failed: ~A" message)))
             (row (file expression-covered expression-total branch-covered branch-total
                    branch-location)
               (format nil "~A~C~D~C~D~C~D~C~D~C-~C~A"
                       file #\Tab
                       expression-covered #\Tab expression-total #\Tab
                       branch-covered #\Tab branch-total #\Tab #\Tab
                       branch-location))
             (write-summary (rows)
               (with-open-file (stream summary-file
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-line "# aggregate coverage fixture" stream)
                 (format stream
                         "file~Cline-covered~Cline-total~Cbranch-covered~Cbranch-total~Cuncovered-lines~Cbranch-locations~%"
                         #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
                 (dolist (summary-row rows)
                   (write-line summary-row stream))))
             (invoke (rows)
               (write-summary rows)
               (let* ((status nil)
                     (report
                       (with-output-to-string (output)
                         (setf status
                               (uiop:symbol-call
                                :coverage-gate :run
                                (list "--input" (namestring summary-file)
                                      "--source-root" (namestring source-directory))
                                :output output
                                :error-output *error-output*))))) (values status report))) (invoke-error (rows)
               (write-summary rows)
               (let* ((status nil)
                     (errors
                       (with-output-to-string (error-output)
                         (setf status
                               (uiop:symbol-call
                                :coverage-gate :run
                                (list "--input" (namestring summary-file)
                                      "--source-root" (namestring source-directory))
                                :output (make-broadcast-stream)
                                :error-output error-output))))) (values status errors)))) (unwind-protect
          (progn
            (ensure-directories-exist
             (merge-pathnames "nested/b.lisp" source-directory))
            (with-open-file (stream (merge-pathnames "a.lisp" source-directory)
                                    :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-line "(in-package #:cl-user)" stream))
            (with-open-file (stream (merge-pathnames "nested/b.lisp" source-directory)
                                    :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-line "(in-package #:cl-user)" stream))
            (multiple-value-bind (status report)
                (invoke (list (row "a.lisp" 0 1 0 1 "1:0/1")
                              (row "nested/b.lisp" 99 99 99 99 "1:99/99")))
              (check (zerop status) "aggregate threshold control should pass")
              (check (search "a.lisp: expressions 0/1 (0.0%), branches 0/1 (0.0%)"
                             report)
                     "per-file coverage should be informational output")
              (check (search "Aggregate expressions: 99/100 (99.0%), threshold 95.0%"
                             report)
                     "aggregate expression values and threshold should be reported")
              (check (search "Aggregate branches: 99/100 (99.0%), threshold 95.0%"
                             report)
                     "aggregate branch values and threshold should be reported")
              (check (search "coverage gate: PASS" report)
                     "passing aggregate gate should be reported"))
            (multiple-value-bind (status report)
                (invoke (list (row "a.lisp" 94 100 100 100 "1:100/100")
                              (row "nested/b.lisp" 0 0 0 0 "-")))
              (check (= status 1) "aggregate expression below threshold should fail")
              (check (search "Aggregate expressions: 94/100 (94.0%), threshold 95.0%"
                             report)
                     "expression failure should report aggregate values"))
            (multiple-value-bind (status report)
                (invoke (list (row "a.lisp" 100 100 94 100 "1:94/100")
                              (row "nested/b.lisp" 0 0 0 0 "-")))
              (check (= status 1) "aggregate branch below threshold should fail")
              (check (search "Aggregate branches: 94/100 (94.0%), threshold 95.0%"
                             report)
                     "branch failure should report aggregate values"))
            (multiple-value-bind (status errors)
                (invoke-error (list (row "a.lisp" 1 1 1 1 "1:1/1")))
              (check (= status 2) "missing source file should be rejected")
              (check (search "missing [nested/b.lisp]" errors)
                     "missing source file should be named"))
            (multiple-value-bind (status errors)
                (invoke-error (list (row "a.lisp" 1 1 1 1 "1:1/1")
                                    (row "nested/b.lisp" 1 1 1 1 "1:1/1")
                                    (row "../outside.lisp" 1 1 1 1 "1:1/1")))
              (check (= status 2) "manifest-external file should be rejected")
              (check (search "outside source root [../outside.lisp]" errors)
                     "manifest-external file should be named"))
            (multiple-value-bind (status errors)
                (invoke-error (list (row "a.lisp" 1 1 1 1 "1:1/1")
                                    (row "a.lisp" 1 1 1 1 "1:1/1")
                                    (row "nested/b.lisp" 1 1 1 1 "1:1/1")))
              (check (= status 2) "duplicate source file should be rejected")
              (check (search "duplicate [a.lisp]" errors)
                     "duplicate source file should be named"))
            (unless (= assertion-count 15) (error "Expected 15 coverage gate assertions, got ~D" assertion-count))
            (format t
                    "coverage gate regression passed: 4 required failures, one manifest-external failure, and one aggregate PASS control (~D assertions).~%"
                    assertion-count))
        (uiop:delete-directory-tree temporary-directory
                                    :validate t
                                    :if-does-not-exist :ignore)))
) (setf *coverage-gate-test-completed* t))
(unless *coverage-gate-test-completed* (error "Coverage gate test body did not complete"))
