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
         (excluded-file (merge-pathnames "excluded.lisp" source-directory))
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
    ;; The declaim follows a multi-line defstruct, so excluding it requires the
    ;; scanner to track nesting depth across lines.
    (with-open-file
        (stream excluded-file
                :direction
                :output
                :if-exists
                :supersede
                :if-does-not-exist
                :create
                :external-format
                :utf-8)
      (write-line "(in-package #:cl-user)" stream)
      (write-line "" stream)
      (write-line "(defstruct (point" stream)
      (write-line "             (:constructor make-point (x y)))" stream)
      (write-line "  x" stream)
      (write-line "  y)" stream)
      (write-line "" stream)
      (write-line "(declaim (optimize speed))" stream))
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
              #\Tab)
      (format stream
              "excluded.lisp~C0~C6~C0~C0~C1,3-6,8~C-~%"
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
          (search "coverage gate: FAIL (1 of 2 files below threshold)" report)
        (error "The report did not reject the uncovered branch:~%~A" report))
      (when (search "  excluded.lisp:" report)
        (error "The report rejected lines covered by exclusion rules:~%~A" report))
      ;; These compile-time forms should remain excluded even when the file has no branch locations.
      (unless
          (search "| excluded.lisp | 0.0% | n/a | 1,3-6,8 |" report)
        (error "The no-branch summary row was not reported:~%~A" report))
      (format t "coverage gate regression passed: uncovered branch rejected, compile-time lines excluded, no-branch summary reported.~%"))))
