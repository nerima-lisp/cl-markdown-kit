(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

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
           (format nil
                   "cl-markdown-kit-coverage-gate-~D/"
                   (get-internal-real-time))
           (uiop:ensure-directory-pathname (uiop:temporary-directory))))
         (source-directory (merge-pathnames "source/" temporary-directory))
         (source-file (merge-pathnames "branch.lisp" source-directory))
         (summary-file (merge-pathnames "per-file.tsv" temporary-directory))
         (exclusions-file
          (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
    (ensure-directories-exist source-file)
    (with-open-file
        (stream source-file
                :direction
                :output
                :if-exists
                :supersede
                :if-does-not-exist
                :create
                :external-format
                :utf-8)
      (write-line "(defun branch-value (flag)" stream)
      (write-line "  (if flag" stream)
      (write-line "      :covered" stream)
      (write-line "      :uncovered))" stream))
    (with-open-file
        (stream summary-file
                :direction
                :output
                :if-exists
                :supersede
                :if-does-not-exist
                :create
                :external-format
                :utf-8)
      (format stream
              "file~Cline-covered~Cline-total~Cbranch-covered~Cbranch-total~Cuncovered-lines~Cbranch-locations~%"
              #\Tab
              #\Tab
              #\Tab
              #\Tab
              #\Tab
              #\Tab)
      (format stream
              "branch.lisp~C3~C3~C1~C2~C-~C2:1/2~%"
              #\Tab
              #\Tab
              #\Tab
              #\Tab
              #\Tab
              #\Tab))
    (let ((report
           (with-output-to-string (output)
             (let ((status
                    (uiop:symbol-call :coverage-gate
                                      :run
                                      (list "--input"
                                            (namestring summary-file)
                                            "--source-root"
                                            (namestring source-directory)
                                            "--exclusions"
                                            (namestring exclusions-file))
                                      :output
                                      output)))
               (unless (= status 1)
                 (error "Expected the uncovered branch to return 1, got ~D."
                        status))))))
      (unless (search "branch 50.0%, required 100.0%" report)
        (error "The report did not identify the uncovered branch:~%~A" report))
      (unless (search "| branch.lisp | 100.0% | 50.0% |" report)
        (error "The report did not include the per-file coverage:~%~A" report))
      (unless
          (search "coverage gate: FAIL (1 of 1 files below threshold)" report)
        (error "The report did not reject the uncovered branch:~%~A" report))
      (format t "coverage gate regression passed: uncovered branch rejected.~%"))))
