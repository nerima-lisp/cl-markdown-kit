(in-package #:cl-markdown-kit/test)

(describe "Inline parser ranges and depth"
(it "checks inline delimiter depth at exact limits"
    (labels ((depth-condition (text limit)
               (handler-case
                   (progn
                     (inline-parser-test-parse
                      text nil :max-inline-delimiter-depth limit)
                     nil)
                 (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded
                     (condition)
                   condition)))
             (depth-condition-matches-p (condition limit actual)
               (and condition
                    (eq (cl-markdown-kit:markdown-resource-limit-error-limit-name
                         condition)
                        :inline-delimiter-depth)
                    (= (cl-markdown-kit:markdown-resource-limit-error-limit
                        condition)
                       limit)
                    (= (cl-markdown-kit:markdown-resource-limit-error-actual
                        condition)
                       actual)))
             (repeated-opener (marker count)
               (with-output-to-string (output)
                 (dotimes (index count)
                   (when (plusp index)
                     (write-char #\Space output))
                   (write-string marker output)
                   (write-char #\a output))))
             (combined-openers (bracket-count)
               (concatenate 'string
                            (make-string bracket-count :initial-element #\[)
                            "*a b"))
             (repeated-brackets (count)
               (concatenate 'string
                            (make-string count :initial-element #\[)
                            "x"
                            (make-string count :initial-element #\])))
             (sequential-openers ()
               (concatenate 'string (repeated-brackets 8) " "
                            (repeated-opener "*" 8))))
      (expect (inline-parser-test-signals-depth-p "*a *b* c*" 1)
              :to-be-truthy)
      (expect (inline-parser-test-signals-depth-p "[[x]]" 1)
              :to-be-truthy)
      (expect (not (inline-parser-test-signals-depth-p "*a*" 1))
              :to-be-truthy)
      (expect (not (inline-parser-test-signals-depth-p "[x]" 1))
              :to-be-truthy)
      (expect (null (depth-condition (repeated-brackets 8) 8))
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition (repeated-brackets 9) 8) 8 9)
              :to-be-truthy)
      (expect (null (depth-condition (combined-openers 7) 8))
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition (combined-openers 8) 8) 8 9)
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition "[*a]" 1) 1 2)
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition "*[a]" 1) 1 2)
              :to-be-truthy)
      (expect (null (depth-condition (sequential-openers) 8))
              :to-be-truthy)
      (expect (depth-condition-matches-p
               (depth-condition "[*a](u) *b *c" 2) 2 3)
              :to-be-truthy)))
(cl-weave:it-each
    (("*" 8 nil)
     ("*" 9 9)
     ("**" 8 nil)
     ("**" 9 9)
     ("~~" 8 nil)
     ("~~" 9 9))
    "checks delimiter depth for marker ~S repeated ~D times"
    (marker count expected-actual)
  (let* ((source (with-output-to-string (output)
                   (dotimes (index count)
                     (when (plusp index)
                       (write-char #\Space output))
                     (write-string marker output)
                     (write-char #\a output))))
         (condition
           (handler-case
               (progn
                 (inline-parser-test-parse
                  source nil :max-inline-delimiter-depth 8)
                 nil)
             (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded
                 (condition)
               condition))))
    (expect (if expected-actual
                (and condition
                     (eq (cl-markdown-kit:markdown-resource-limit-error-limit-name
                          condition)
                         :inline-delimiter-depth)
                     (= (cl-markdown-kit:markdown-resource-limit-error-limit
                         condition)
                        8)
                     (= (cl-markdown-kit:markdown-resource-limit-error-actual
                         condition)
                        expected-actual))
                (null condition))
            :to-be-truthy)))

(it "handles long unmatched bracket and angle sequences"
    (let* ((brackets (make-string 2000 :initial-element #\[))
           (angles (make-string 2000 :initial-element #\<))
           (bracket-nodes (inline-parser-test-parse brackets))
           (angle-nodes (inline-parser-test-parse angles)))
      (expect (mapcar #'cl-markdown-kit:text-value bracket-nodes)
              :to-equal (list brackets))
      (expect (mapcar #'cl-markdown-kit:text-value angle-nodes)
              :to-equal (list angles))))

(it "handles unmatched backtick runs and paired delimiter stress"
    (let* ((backticks (concatenate 'string (make-string 2000 :initial-element #\`)
                                   " text "
                                   (make-string 1999 :initial-element #\`)))
           (backtick-nodes (inline-parser-test-parse backticks))
           (delimiter-text (format nil "~{~A~}" (make-list 600 :initial-element
                                                              "*a **a")))
           (delimiter-nodes (inline-parser-test-parse delimiter-text)))
      (expect (mapcar #'cl-markdown-kit:text-value backtick-nodes)
              :to-equal (list backticks))
      (expect (plusp (length delimiter-nodes)) :to-be-truthy)))

(it "counts unmatched delimiter openers against the depth limit"
    (let* ((limit 4096)
           (text (with-output-to-string (output)
                   (loop repeat (1+ limit) do (write-string "*a " output))))
           (condition
             (handler-case
                 (progn
                   (inline-parser-test-parse
                    text nil :max-inline-delimiter-depth limit)
                   nil)
               (cl-markdown-kit:markdown-inline-delimiter-depth-exceeded
                   (condition)
                 condition))))
      (expect (and condition
                   (eq (cl-markdown-kit:markdown-resource-limit-error-limit-name
                        condition)
                       :inline-delimiter-depth)
                   (= (cl-markdown-kit:markdown-resource-limit-error-limit
                       condition)
                      limit)
                   (= (cl-markdown-kit:markdown-resource-limit-error-actual
                       condition)
                      (1+ limit)))
              :to-be-truthy)))

(it "parses deeply nested bracket links without reparsing each inner range"
    (let* ((count 1000)
           (source (concatenate 'string
                                (make-string count :initial-element #\[)
                                "a"
                                (with-output-to-string (output)
                                  (loop repeat count
                                        do (write-string "](u)" output)))))
           (completed-p
             (handler-case
                 (progn
                   (sb-ext:with-timeout 2
                     (inline-parser-test-parse source))
                   t)
               (sb-ext:timeout () nil))))
      (expect completed-p :to-be-truthy)))

(it "parses soft breaks, backslash hard breaks, and raw HTML comments"
    (let* ((soft (inline-parser-test-parse (format nil "a~%b")))
           (soft-spaced (inline-parser-test-parse (format nil "a ~%b")))
           (hard (inline-parser-test-parse (format nil "a\\~%b")))
           (comment (first (inline-parser-test-parse "<!-- comment -->"))))
      (expect (and (= (length soft) 3)
                   (typep (second soft) 'cl-markdown-kit:soft-break)
                   (typep (second soft-spaced) 'cl-markdown-kit:soft-break)
                   (= (length hard) 3)
                   (typep (second hard) 'cl-markdown-kit:hard-break)
                   (typep comment 'cl-markdown-kit:html-inline)
                   (string= (cl-markdown-kit:html-inline-body comment)
                            "<!-- comment -->"))
              :to-be-truthy)))

(it "trims spaces at inline line boundaries and at the fragment end"
    (let* ((hard (inline-parser-test-parse (format nil "foo  ~%  bar")))
           (backslash-hard
             (inline-parser-test-parse (format nil "foo\\~%     bar")))
           (soft (inline-parser-test-parse (format nil "foo ~%  bar")))
           (trailing (inline-parser-test-parse "foo  ")))
      (expect (mapcar #'inline-parser-test-node-sequence-signature
                      (list hard backslash-hard soft trailing))
              :to-equal '(((:text "foo") (:hard-break) (:text "bar"))
                          ((:text "foo") (:hard-break) (:text "bar"))
                          ((:text "foo") (:soft-break) (:text "bar"))
                          ((:text "foo"))))))

(it "preserves link-like text when parsing disallows autolinks and links"
    (let* ((nodes (inline-parser-test-parse-range
                   "www.example.com <https://example.com> [x](/target)" nil)))
      (expect (notany (lambda (node)
                        (or (typep node 'cl-markdown-kit:autolink)
                            (typep node 'cl-markdown-kit:link)))
                      nodes)
              :to-be-truthy)))

(it "rejects invalid inline parser arguments"
    (let* ((buffer (cl-markdown-kit:make-source-buffer "x"))
           (text (cl-markdown-kit:source-buffer-normalized-text buffer))
           (boundary-map (cl-markdown-kit::source-buffer-boundary-map buffer))
           (index (make-hash-table :test #'equal)))
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline text boundary-map index
                                                        :max-inline-delimiter-depth 0)
                         nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline text boundary-map nil) nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline 1 boundary-map index) nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline text nil index) nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline
                          text boundary-map index :max-inline-delimiter-depth "bad")
                         nil)
                (type-error () t))
              :to-be-truthy)
      (expect (handler-case
                  (progn (cl-markdown-kit::parse-inline "xx" boundary-map index) nil)
                (error () t))
              :to-be-truthy)))

(it "keeps only complete atoms and pairs inside a rendered range"
    (let* ((text "abcdefgh")
           (buffer (cl-markdown-kit:make-source-buffer text))
           (context
             (cl-markdown-kit::%ip-make-context
              text (cl-markdown-kit::source-buffer-boundary-map buffer)
              nil 4096 nil nil nil nil nil nil nil))
           (atoms (list
                   (cl-markdown-kit::%ip-make-atom
                    1 2 (cl-markdown-kit::%ip-text-node context "b" 1 2))
                   (cl-markdown-kit::%ip-make-atom
                    3 4 (cl-markdown-kit::%ip-text-node context "d" 3 4))
                   (cl-markdown-kit::%ip-make-atom
                    5 7 (cl-markdown-kit::%ip-text-node context "fg" 5 7))
                   (cl-markdown-kit::%ip-make-atom
                    6 7 (cl-markdown-kit::%ip-text-node context "g" 6 7))))
           (pairs (list
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 0 0 1 1)
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 2 3 5 6)
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 5 6 6 7)
                   (cl-markdown-kit::%ip-make-pair
                    :emphasis (code-char 42) 6 7 7 8)))
           (nodes (cl-markdown-kit::%ip-build-range context 2 6 atoms pairs 0))
           (emphasis (first nodes)))
      (expect (and (= (length nodes) 1)
                   (typep emphasis 'cl-markdown-kit:emphasis)
                   (string= (apply #'concatenate 'string
                                   (mapcar #'cl-markdown-kit:text-value
                                           (cl-markdown-kit:emphasis-children
                                            emphasis)))
                            "de"))
              :to-be-truthy)))

(it "handles tab breaks in inline parsing"
    (let ((tab-break
            (inline-parser-test-parse (format nil "a~C~%b" #\Tab))))
      (expect (typep (second tab-break) 'cl-markdown-kit:soft-break)
              :to-be-truthy)))

(it "keeps footnotes and text within inline range boundaries"
    (let* ((partial-footnote
             (inline-parser-test-parse-range "![^Note]" nil 0 4))
           (middle (inline-parser-test-parse-range "*word*" nil 1 5))
           (prefix (inline-parser-test-parse-range "*word*" nil 0 5))
           (suffix (inline-parser-test-parse-range "*word*" nil 1 6)))
      (expect (notany #'(lambda (node)
                          (typep node 'cl-markdown-kit:footnote-reference))
                      partial-footnote)
              :to-be-truthy)
      (expect (list (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value middle))
                    (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value prefix))
                    (apply #'concatenate 'string
                           (mapcar #'cl-markdown-kit:text-value suffix)))
              :to-equal '("word" "*word" "word*")))))
