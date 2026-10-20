(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(let* ((script
         (or *load-truename*
             *compile-file-truename*
             (error "Unable to determine the coverage gate test location")))
       (root
         (uiop:pathname-parent-directory-pathname
          (uiop:pathname-directory-pathname script))))
  (asdf:load-asd (merge-pathnames "cl-markdown-kit.asd" root))
  (asdf:load-system "cl-weave")
  (asdf:load-system "cl-process-kit"))

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
         (dash-file (merge-pathnames "dash-excluded.lisp" source-directory))
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
    ;; The declaim follows a multi-line defstruct, so excluding it requires the
    ;; scanner to track nesting depth across lines.
    (with-open-file
        (stream dash-file
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
              #\Tab)
      (format stream
              "dash-excluded.lisp~C0~C6~C0~C0~C-~C-~%"
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
                 (error "Expected the uncovered branch and dash row to return 1, got ~D."
                        status))))))
      (unless (and (search "branch 50.0%, required 100.0%" report) (search "branch.lisp line 2: 1/2 branches covered" report) (search "dash-excluded.lisp: line 0.0%, required 100.0%" report) (uiop:symbol-call :coverage-gate :source-exclusion-lines (namestring dash-file) (uiop:symbol-call :coverage-gate :read-exclusion-rules (namestring exclusions-file)))) (error "The report did not identify the uncovered branch and low-coverage dash row with fixture exclusion ranges:~%~A" report))
      (progn
        (cl-weave:describe
         "Coverage gate"
         (cl-weave:it-each
          (("line and branch summary" "branch 50.0%, required 100.0%")
           ("per-file row" "| branch.lisp | 100.0% | 50.0% |")
           ("failed gate" "coverage gate: FAIL (2 of 3 files below threshold)")
           ("no-branch row" "| excluded.lisp | 0.0% | n/a | 1,3-6,8 |"))
          "reports ~A: ~A"
          (report-detail expected-substring)
          (cl-weave:expect (search expected-substring report) :to-be-truthy))
         (cl-weave:it
          "exits nonzero when a checked subprocess times out"
          (let* ((timeout-script
                  (merge-pathnames "timeout.lisp" temporary-directory)))
            (with-open-file
                (stream timeout-script
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :external-format :utf-8)
              (write-line "(eval-when (:compile-toplevel :load-toplevel :execute)" stream)
              (write-line "  (require :asdf))" stream)
              (write-line (format nil "(asdf:load-system ~S)" "cl-process-kit") stream)
              (write-line
               (format nil "(process-kit:run/checked ~S (list ~S) :search t :timeout 0.1)"
                       "sleep"
                       "10")
               stream))
            (let* ((result
                    (process-kit:run
                     "sbcl"
                     (list "--no-sysinit"
                           "--no-userinit"
                           "--disable-debugger"
                           "--script"
                           (namestring timeout-script))
                     :search t
                     :timeout 30))
                   (exit-code (process-kit:process-result-exit-code result))
                   (stderr (process-kit:process-result-stderr result)))
              (cl-weave:expect (not (process-kit:process-success-p result))
                               :to-be-truthy)
              (cl-weave:expect (and (integerp exit-code) (plusp exit-code))
                               :to-be-truthy)
              (cl-weave:expect (search "PROCESS-TIMEOUT-ERROR" (string-upcase stderr))
                               :to-be-truthy)))))
        (unless (cl-weave:run-all :reporter :spec :pass-with-no-tests nil)
          (uiop:quit 1)))
      (unless (search "| branch.lisp | 100.0% | 50.0% |" report)
        (error "The report did not include the per-file coverage:~%~A" report))
      (unless
          (search "coverage gate: FAIL (2 of 3 files below threshold)" report)
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
         (summary-file (merge-pathnames "per-file.tsv" temporary-directory))
         (exclusions-file
          (merge-pathnames "scripts/coverage-exclusions.sexp" root))
         (fixtures
           '(("direct-literal.lisp" "1" "1:0/2"
              "(defstruct literal-control (value 7))")
             ("direct-constant.lisp" "2" "2:0/2"
              "(defconstant +fixture-value+ 17)"
              "(defstruct constant-control (value +fixture-value+))")
             ("wrapped-constant.lisp" "1" "1:0/2"
              "(define-node (defstruct wrapped-constant (value +fixture-value+)) :kind :wrapped-constant)")
             ("nested-constant.lisp" "3" "3:0/2"
              "(eval-when (:compile-toplevel :load-toplevel :execute)"
              "  (progn"
              "    (define-node"
              "      (defstruct nested-constant (value +fixture-value+))"
              "      :kind :nested-constant)))")
             ("constructor-literal.lisp" "1" "1:0/2"
              "(defstruct (key-literal (:constructor make-key-literal (&key (value 7)))) value)")
             ("constructor-constant.lisp" "2" "2:0/2"
              "(defconstant +key-default+ 9)"
              "(defstruct (key-constant (:constructor make-key-constant (&key (value +key-default+)))) value)")
             ("runtime-parameter.lisp" "1" "1:0/2"
              "(defparameter +ip-html-close-patterns+ (progn 1))")
             ("literal-parameter.lisp" "1" "1:0/2"
              "(defparameter +ip-html-close-patterns+ (quote (1 2)))")
             ("direct-runtime.lisp" "1" "1:0/2"
              "(defstruct runtime-control (value (if flag 1 2)))")
             ("wrapped-runtime.lisp" "1" "1:0/2"
              "(define-node (defstruct wrapped-runtime (value (let ((n 1)) n))) :kind :wrapped-runtime)")
             ("nested-runtime.lisp" "2,4" "2:0/2,4:0/2"
              "(eval-when (:compile-toplevel :load-toplevel :execute)"
              "  (defstruct eval-runtime (value (if flag 1 2)))"
              "  (progn"
              "    (define-node"
              "      (defstruct progn-runtime (value (let ((n 1)) n)))"
              "      :kind :progn-runtime)))")
             ("constructor-runtime.lisp" "1" "1:0/2"
              "(defstruct (key-runtime (:constructor make-key-runtime (&key (value (make-hash-table))))) value)"))))
    (ensure-directories-exist (merge-pathnames (first (first fixtures)) source-directory))
    (flet ((write-fixture (path lines)
             (with-open-file
                 (stream path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :external-format :utf-8)
               (dolist (line lines)
                 (write-line line stream)))))
      (dolist (fixture fixtures)
        (write-fixture
         (merge-pathnames (first fixture) source-directory)
         (cdddr fixture)))
      (with-open-file
          (stream summary-file
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create
                  :external-format :utf-8)
        (format stream
                "file~Cline-covered~Cline-total~Cbranch-covered~Cbranch-total~Cuncovered-lines~Cbranch-locations~%"
                #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
        (dolist (fixture fixtures)
          (let* ((lines (cdddr fixture))
                 (branch-locations (third fixture))
                 (branch-count (* 2 (1+ (count #\, branch-locations)))))
            (format stream
                    "~A~C0~C~D~C0~C~D~C~A~C~A~%"
                    (first fixture)
                    #\Tab
                    #\Tab
                    (length lines)
                    #\Tab
                    #\Tab
                    branch-count
                    #\Tab
                    (second fixture)
                    #\Tab
                    branch-locations)))))
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
                                         output))))
             (expected-failures
               '("runtime-parameter.lisp"
                 "direct-runtime.lisp"
                 "wrapped-runtime.lisp"
                 "nested-runtime.lisp"
                 "constructor-runtime.lisp"))
             (expected-safe
               '("literal-parameter.lisp"
                 "direct-literal.lisp"
                 "direct-constant.lisp"
                 "wrapped-constant.lisp"
                 "nested-constant.lisp"
                 "constructor-literal.lisp"
                 "constructor-constant.lisp")))
        (unless (= status 1)
          (error "Expected runtime declarations to fail coverage, got status ~D:~%~A"
                 status
                 report))
        (dolist (file expected-failures)
          (unless (search (format nil "  ~A: line " file) report)
            (error "Runtime declaration ~A was not reported uncovered:~%~A"
                   file
                   report)))
        (dolist (file expected-safe)
          (when (search (format nil "  ~A: " file) report)
            (error "Literal or constant declaration ~A was not excluded:~%~A"
                   file
                   report)))
        (unless (and
                 (search "| literal-parameter.lisp | 0.0% | n/a | 1 |" report)
                 (search "| runtime-parameter.lisp | 0.0% | 0.0% | 1 |" report)
                 (search "| nested-constant.lisp | 0.0% | n/a | 3 |" report)
                 (search "coverage gate: FAIL (5 of 12 files below threshold)" report))
          (error "Declaration coverage did not preserve safe controls and reject runtime forms:~%~A"
                 report))
        (format t "coverage gate regression passed: literal and constant declarations excluded; direct, wrapped, nested, and constructor runtime initializers remain uncovered.~%"))))
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
          "  (defvar *computed-coverage-fixture* (if flag :covered :uncovered)))"
          "(defparameter +ip-html-close-patterns+ (if flag :covered :uncovered))")))
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
                "executable-initializers.lisp~C0~C5~C0~C8~C1-5~C1:0/2,2:0/2,4:0/2,5:0/2~%"
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
                 (search "| executable-initializers.lisp | 0.0% | 0.0% | 1-5 |" report)
                 (search "executable-initializers.lisp line 1: 0/2 branches covered" report)
                 (search "executable-initializers.lisp line 2: 0/2 branches covered" report)
                 (search "executable-initializers.lisp line 4: 0/2 branches covered" report)
                 (search "executable-initializers.lisp line 5: 0/2 branches covered" report)
                 (search "mixed.lisp line 7: 1/2 branches covered" report)
                 (search "mixed.lisp line 12: 1/2 branches covered" report)
                 (search "mixed.lisp line 16: 1/2 branches covered" report)
                 (search "coverage gate: FAIL (2 of 3 files below threshold)" report))
          (error "Executable initializers or runtime branches were hidden:~%~A" report))
        (format t "coverage gate regression passed: nested declaration exclusions preserve executable initializer and branch coverage.~%")))))
