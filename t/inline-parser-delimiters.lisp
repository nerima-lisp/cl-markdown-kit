(in-package #:cl-markdown-kit/test)

(describe "Inline parser delimiters"
(it "builds nested emphasis and strong nodes with delimiter-inclusive spans"
    (let* ((node (first (inline-parser-test-parse "***word***")))
           (strong (first (cl-markdown-kit:emphasis-children node)))
           (word (first (cl-markdown-kit:strong-children strong))))
      (expect (and (typep node 'cl-markdown-kit:emphasis)
                   (typep strong 'cl-markdown-kit:strong)
                   (typep word 'cl-markdown-kit:text))
              :to-be-truthy)
      (expect (list (inline-parser-test-span node)
                    (inline-parser-test-span strong)
                    (cl-markdown-kit:text-value word)
                    (inline-parser-test-span word))
              :to-equal '((0 10 1 1 1 11)
                          (1 9 1 2 1 10)
                          "word"
                          (3 7 1 4 1 8)))))

(it "respects underscore flanking rules and unmatched delimiters"
    (let* ((intraword (inline-parser-test-parse "foo_bar_baz"))
           (punctuation (second (inline-parser-test-parse "(_word_)"))))
      (expect (mapcar #'cl-markdown-kit:text-value intraword)
              :to-equal '("foo_bar_baz"))
      (expect (and (typep punctuation 'cl-markdown-kit:emphasis)
                   (string= (cl-markdown-kit:text-value
                             (first (cl-markdown-kit:emphasis-children punctuation)))
                            "word"))
              :to-be-truthy)))

(it "chooses the nearest delimiter across opener buckets"
    (let* ((nodes (inline-parser-test-parse "**outer *inner* tail**"))
           (strong (first nodes))
           (emphasis (second (cl-markdown-kit:strong-children strong))))
      (expect (and (typep strong 'cl-markdown-kit:strong)
                   (typep emphasis 'cl-markdown-kit:emphasis))
              :to-be-truthy)
      (expect (cl-markdown-kit:text-value
               (first (cl-markdown-kit:emphasis-children emphasis)))
              :to-equal "inner")
      (expect (typep (first (inline-parser-test-parse "**strong**")) 'cl-markdown-kit:strong)
              :to-be-truthy)))

(it "keeps code-span contents literal and normalizes line endings"
    (let* ((node (first (inline-parser-test-parse "`a &amp; *b*`")))
           (crlf-nodes (inline-parser-test-parse
                        (format nil "a  ~C~%b" #\Return)))
           (hard-break (second crlf-nodes)))
      (expect (list (cl-markdown-kit:code-span-literal-text node)
                    (cl-markdown-kit:code-span-backtick-run-length node)
                    (inline-parser-test-span node))
              :to-equal '("a &amp; *b*" 1 (0 13 1 1 1 14)))
      (expect (and (= (length crlf-nodes) 3)
                   (typep hard-break 'cl-markdown-kit:hard-break)
                   (equal (inline-parser-test-span hard-break)
                          '(1 5 1 2 2 1)))
              :to-be-truthy)))

(it "closes code spans on backticks preceded by a backslash"
    (let* ((nodes (inline-parser-test-parse "`foo\\`bar`"))
           (code (first nodes)))
      (expect (and (typep code 'cl-markdown-kit:code-span)
                   (string= (cl-markdown-kit:code-span-literal-text code)
                            "foo\\")
                   (equal (mapcar #'cl-markdown-kit:text-value (rest nodes))
                          '("bar`")))
              :to-be-truthy)))

(it "matches single and double tilde runs without accepting unequal runs"
    (let* ((single (first (inline-parser-test-parse "~plain~")))
           (double (first (inline-parser-test-parse "~~gone~~")))
           (mismatched (inline-parser-test-parse "~~~gone~~")))
      (expect (list (typep single 'cl-markdown-kit:strikethrough)
                    (typep double 'cl-markdown-kit:strikethrough)
                    (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value mismatched)))
              :to-equal '(t t "~~~gone~~"))))

(it "normalizes padded code spans and preserves non-escapable backslashes"
    (let* ((padded (first (inline-parser-test-parse "`  code  `")))
           (spaces (first (inline-parser-test-parse "`  `")))
           (plain (inline-parser-test-parse "ordinary text \\q \\")))
      (expect (list (cl-markdown-kit:code-span-literal-text padded)
                    (cl-markdown-kit:code-span-literal-text spaces)
                    (mapcar #'cl-markdown-kit:text-value plain))
              :to-equal '(" code " "  " ("ordinary text \\q \\")))))
(it "calls the delimiter match continuation with the best opener"
  (let* ((openers (make-hash-table :test #'eql))
         (opener (cl-markdown-kit::%ip-make-delimiter #\* 0 1 t nil))
         (closer (cl-markdown-kit::%ip-make-delimiter #\* 2 1 nil t))
         (buckets (cl-markdown-kit::%ip-delimiter-buckets openers #\*))
         (match-calls 0)
         (fail-calls 0)
         (continuation-calls 0))
    (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 1
          (aref buckets 1) (list opener)
          (cl-markdown-kit::%ip-delimiter-remaining closer) 1)
    (cl-weave:with-continuation-result (result next calledp)
      (cl-markdown-kit::%ip-best-opener
       openers closer
       (lambda (candidate)
         (incf match-calls)
         (incf continuation-calls)
         (next candidate))
       (lambda ()
         (incf fail-calls)
         (incf continuation-calls)
         (next :failure)))
      (expect calledp :to-be-truthy)
      (expect result :to-be opener)
      (expect (list match-calls fail-calls continuation-calls)
              :to-equal '(1 0 1)))))
(it "calls the delimiter failure continuation when no opener matches"
  (let* ((openers (make-hash-table :test #'eql))
         (closer (cl-markdown-kit::%ip-make-delimiter #\* 2 1 nil t)))
    (cl-weave:with-continuation-result (result next calledp)
      (cl-markdown-kit::%ip-best-opener
       openers closer
       (lambda (candidate)
         (declare (ignore candidate)))
       (function next))
      (expect calledp :to-be-truthy)
      (expect result :to-be nil))))

(it "selects tilde openers only for matching one or two marker runs"
    (labels ((best-opener (openers closer)
               (cl-weave:with-continuation-result (result next calledp)
                 (cl-markdown-kit::%ip-best-opener
                  openers closer
                  (function next)
                  (function next))
                 (expect calledp :to-be-truthy)
                 result)))
      (let* ((openers (make-hash-table :test #'eql))
             (character (code-char 126))
             (buckets (cl-markdown-kit::%ip-delimiter-buckets openers character))
             (opener (cl-markdown-kit::%ip-make-delimiter
                      character 0 2 t nil))
             (closer (cl-markdown-kit::%ip-make-delimiter
                      character 4 2 nil t))
             (asterisk-buckets
               (cl-markdown-kit::%ip-delimiter-buckets openers #\*))
             (asterisk-opener
               (cl-markdown-kit::%ip-make-delimiter #\* 8 1 t nil))
             (asterisk-closer
               (cl-markdown-kit::%ip-make-delimiter #\* 10 1 nil t)))
        (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 1
              (aref buckets 6) (list opener)
              (cl-markdown-kit::%ip-delimiter-remaining closer) 1)
        (expect (best-opener openers closer)
                :to-be opener)
        (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 3)
        (expect (best-opener openers closer)
                :to-equal nil)
        (setf (cl-markdown-kit::%ip-delimiter-remaining opener) 2)
        (expect (best-opener openers closer)
                :to-equal nil)
        (setf (cl-markdown-kit::%ip-delimiter-remaining closer) 2)
        (expect (best-opener openers closer)
                :to-be opener)
        (setf (cl-markdown-kit::%ip-delimiter-remaining asterisk-opener) 1
              (aref asterisk-buckets 1) (list asterisk-opener)
              (cl-markdown-kit::%ip-delimiter-remaining asterisk-closer) 1)
        (expect (best-opener openers asterisk-closer)
                :to-be asterisk-opener))))

(it "handles leftover delimiter runs without pairing across them"
    (let* ((tilde (inline-parser-test-parse "~~a ~~b~~~"))
           (crossed (inline-parser-test-parse "*a **b* c**")))
      (expect (notany #'(lambda (node)
                          (typep node 'cl-markdown-kit:strikethrough))
                      tilde)
              :to-be-truthy)
      (expect (some #'(lambda (node)
                        (typep node 'cl-markdown-kit:emphasis))
                    crossed)
              :to-be-truthy))))
