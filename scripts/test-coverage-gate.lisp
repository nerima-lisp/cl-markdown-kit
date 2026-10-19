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
      (unless (and (search "branch 50.0%, required 100.0%" report)
                   (search "branch.lisp line 2: 1/2 branches covered" report))
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

(let* ((script
        (or *load-truename*
            *compile-file-truename*
            (error "Unable to determine the coverage gate test location")))
       (root
        (uiop:pathname-parent-directory-pathname
         (uiop:pathname-directory-pathname script))))
  (uiop:chdir root)
  (let* ((temporary-directory
          (merge-pathnames
           (format nil
                   "cl-markdown-kit-declaration-coverage-~D/"
                   (get-internal-real-time))
           (uiop:ensure-directory-pathname (uiop:temporary-directory))))
         (source-directory (merge-pathnames "source/" temporary-directory))
         (direct-file (merge-pathnames "direct.lisp" source-directory))
         (wrapped-file (merge-pathnames "wrapped.lisp" source-directory))
         (inline-context-file
          (merge-pathnames "inline-context.lisp" source-directory))
         (render-frame-file
          (merge-pathnames "render-frame.lisp" source-directory))
         (block-parse-result-file
          (merge-pathnames "block-parse-result.lisp" source-directory))
         (entities-file
          (merge-pathnames "html-entities-data.lisp" source-directory))
         (escape-data-file
          (merge-pathnames "markdown-writer-escape-data.lisp" source-directory))
         (summary-file (merge-pathnames "per-file.tsv" temporary-directory))
         (exclusions-file
          (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
    (ensure-directories-exist direct-file)
    (flet ((write-fixture (path lines)
             (with-open-file
                 (stream path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :external-format :utf-8)
               (dolist (line lines)
                 (write-line line stream)))))
      (write-fixture direct-file
                     (list
                      "(defstruct (point (:constructor make-point (value))) (value 0 :type (integer 0 *)))"))
      (write-fixture wrapped-file
                     (list
                      "(define-node"
                      "  (defstruct (document (:constructor make-document))"
                      "    (max-nesting-depth +default-max-nesting-depth+ :type (integer 1 *)))"
                      "  :kind :document)"))
      (write-fixture
       inline-context-file
       (list
        "(defstruct (%ip-context"
        "             (:constructor %ip-make-context (text boundary-map reference-index limit)))"
        "  (limit +default-max-inline-delimiter-depth+ :type (integer 1 *)))"))
      (write-fixture
       render-frame-file
       (list
        "(defstruct (%mw-render-frame"
        "             (:constructor %mw-make-render-frame"
        "                 (&key (line-start-p t) (line-indent 0) prefix)))"
        "  (line-start-p t)"
        "  (line-indent 0)"
        "  (prefix \"\" :type string))"))
      (write-fixture
       block-parse-result-file
       (list
        "(defstruct (block-parse-result"
        "             (:constructor make-block-parse-result"
        "                 (&key document inline-fragments reference-index)))"
        "  document"
        "  (inline-fragments nil :type list)"
        "  (reference-index (make-hash-table :test (quote equal)) :type hash-table))"))
      (write-fixture
       entities-file
       (list
        "(defparameter *html5-entities*"
        "  (let ((table (make-hash-table :test (quote equal))))"
        "    (dolist (entry (quote ((\"amp;\" #x26))))"
        "      (setf (gethash (first entry) table) (second entry)))"
        "    table))"))
      (write-fixture
       escape-data-file
       (list
        "(defparameter +mw-escape-bracket-characters+"
        "  (quote (#\\[ #\\])))"
        "(defparameter +mw-escape-punctuation-characters+"
        "  (quote (#\\` #\\* #\\_ #\\< #\\~)))"))
      (with-open-file
          (stream summary-file
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create
                  :external-format :utf-8)
        (format stream
                "file~Cline-covered~Cline-total~Cbranch-covered~Cbranch-total~Cuncovered-lines~Cbranch-locations~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
        (dolist (file
                 (list "direct.lisp" "wrapped.lisp" "inline-context.lisp"
                       "render-frame.lisp" "block-parse-result.lisp"
                       "html-entities-data.lisp"))
          (format stream "~A~C0~C1~C0~C2~C1~C1:0/2~%"
                  file #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
        (format stream "~A~C0~C2~C0~C2~C1,3~C1:0/1,3:0/1~%"
                "markdown-writer-escape-data.lisp"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
      (let* ((status nil)
             (report
               (with-output-to-string (output)
                 (setf status
                       (uiop:symbol-call :coverage-gate
                                         :run
                                         (list "--input"
                                               (namestring summary-file)
                                               "--source-root"
                                               (namestring source-directory)
                                               "--exclusions"
                                               (namestring exclusions-file))
                                         :output
                                         output)))))
        (unless (= status 0)
          (error "Expected coverage exclusions for declarations and static data, got status ~D:~%~A"
                 status
                 report))
        (unless (and (search "| direct.lisp | 0.0% | n/a | 1 |" report)
                     (search "| wrapped.lisp | 0.0% | n/a | 1 |" report)
                     (search "| inline-context.lisp | 0.0% | n/a | 1 |" report)
                     (search "| render-frame.lisp | 0.0% | n/a | 1 |" report)
                     (search "| block-parse-result.lisp | 0.0% | n/a | 1 |" report)
                     (search "| html-entities-data.lisp | 0.0% | n/a | 1 |" report)
                     (search "| markdown-writer-escape-data.lisp | 0.0% | n/a | 1,3 |" report)
                     (search "coverage gate: PASS (all 7 files at or above threshold)" report))
          (error "Coverage exclusions for declarations, entity data, and writer data were not reported as expected:~%~A"
                 report))
        (format t "coverage gate regression passed: declaration forms, entity data, and writer character data are excluded.~%")))))
(let* ((script
        (or *load-truename*
            *compile-file-truename*
            (error "Unable to determine the coverage gate test location")))
       (root
        (uiop:pathname-parent-directory-pathname
         (uiop:pathname-directory-pathname script))))
  (uiop:chdir root)
  (let* ((temporary-directory
          (merge-pathnames
           (format nil
                   "cl-markdown-kit-nested-coverage-~D/"
                   (get-internal-real-time))
           (uiop:ensure-directory-pathname (uiop:temporary-directory))))
         (source-directory (merge-pathnames "source/" temporary-directory))
         (source-file (merge-pathnames "mixed.lisp" source-directory))
         (summary-file (merge-pathnames "per-file.tsv" temporary-directory))
         (exclusions-file
          (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
    (ensure-directories-exist source-file)
    (flet ((write-fixture (path lines)
             (with-open-file
                 (stream path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :external-format :utf-8)
               (dolist (line lines)
                 (write-line line stream)))))
      (write-fixture
       source-file
       (list
        "(defconstant +default-max-nesting-depth+ 256)"
        "(sb-ext:defglobal +html-attribute-start-characters+ \"_:\")"
        "(eval-when (:compile-toplevel :load-toplevel :execute)"
        "  (defvar *node-definitions* nil)"
        "  (progn"
        "    (defun verify-node-dispatches (&key operation (families (quote (:block :inline))))"
        "      (if operation :covered :uncovered))"
        "    (define-node"
        "      (defstruct (wrapped-point (:constructor make-wrapped-point (value)))"
        "        (value 0 :type (integer 0 *)))"
        "      :kind :wrapped-point))"
        "  (if flag :covered :uncovered)"
        "  (verify-node-dispatches-at-compile-time :query (:inline)))"
        "(defun %pc-parse-title (text index end &key"
        "                         (backslash-mode :punctuation))"
        "  (if flag :covered :uncovered))"))
      (progn
        (write-fixture
         (merge-pathnames "compile-time.lisp" source-directory)
         (list
          "(progn (verify-node-dispatches-at-compile-time :query (:inline)))"))
        (write-fixture
         (merge-pathnames "executable-initializers.lisp" source-directory)
         (list
          "(defconstant +computed-coverage-fixture+ (if flag :covered :uncovered))"
          "(sb-ext:defglobal *computed-coverage-fixture* (if flag :covered :uncovered))"
          "(eval-when (:compile-toplevel :load-toplevel :execute)"
          "  (defvar *computed-coverage-fixture* (if flag :covered :uncovered)))")))
      (with-open-file
          (stream summary-file
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create
                  :external-format :utf-8)
        (format stream
                "file~Cline-covered~Cline-total~Cbranch-covered~Cbranch-total~Cuncovered-lines~Cbranch-locations~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
        (format stream
                "mixed.lisp~C15~C16~C3~C20~C13~C1:0/2,2:0/2,4:0/2,6:0/2,7:1/2,10:0/2,12:1/2,13:0/2,15:0/2,16:1/2~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
        (progn
        (format stream
                "compile-time.lisp~C0~C1~C0~C2~C1~C1:0/2~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
        (format stream
                "executable-initializers.lisp~C0~C4~C0~C6~C1-4~C1:0/2,2:0/2,4:0/2~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)))
      (let* ((status nil)
             (report
               (with-output-to-string (output)
                 (setf status
                       (uiop:symbol-call :coverage-gate
                                         :run
                                         (list "--input"
                                               (namestring summary-file)
                                               "--source-root"
                                               (namestring source-directory)
                                               "--exclusions"
                                               (namestring exclusions-file))
                                         :output
                                         output)))))
        (unless (= status 1)
          (error "Expected executable initializer branches to remain, got status ~D:~%~A"
                 status
                 report))
        (unless (and
                 (search "| mixed.lisp | 93.8% | 50.0% | 13 |" report)
                 (search "| compile-time.lisp | 0.0% | n/a | 1 |" report)
                 (not (search "  compile-time.lisp: " report))
                 (search "| executable-initializers.lisp | 0.0% | 0.0% | 1-4 |" report)
                 (search "executable-initializers.lisp line 1: 0/2 branches covered" report)
                 (search "executable-initializers.lisp line 2: 0/2 branches covered" report)
                 (search "executable-initializers.lisp line 4: 0/2 branches covered" report)
                 (search "mixed.lisp line 7: 1/2 branches covered" report)
                 (search "mixed.lisp line 12: 1/2 branches covered" report)
                 (search "mixed.lisp line 16: 1/2 branches covered" report)
                 (search "coverage gate: FAIL (2 of 3 files below threshold)" report))
          (error "Executable initializers or runtime branches were hidden:~%~A" report))
        (format t "coverage gate regression passed: nested declaration exclusions preserve executable initializer and branch coverage.~%")))))
