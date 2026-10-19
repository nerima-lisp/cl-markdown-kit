;;;; src/inline-parser.lisp
(in-package #:cl-markdown-kit)

(declaim (ftype function %ip-angle-email-p %ip-reference-label-valid-p %ip-parse-range))

(defun %ip-inline-end (text)
  (loop with end = (length text)
        while (and (plusp end)
                   (%pc-space-p (char text (1- end))))
          do (decf end)
        finally (return end)))

(defun parse-inline (text boundary-map reference-index
                     &key (max-inline-delimiter-depth +default-max-inline-delimiter-depth+) (gfm t))
  "Parse inline Markdown TEXT using its source map and reference index."
  (check-type text string)
  (check-type boundary-map boundary-map)
  (check-type reference-index hash-table)
  (check-type gfm boolean)
  (unless (and (integerp max-inline-delimiter-depth)
               (plusp max-inline-delimiter-depth))
    (error 'type-error :datum max-inline-delimiter-depth
           :expected-type '(integer 1 *)))
  (unless (= (length text) (boundary-map-length boundary-map))
    (error "Inline text length ~D does not match boundary map length ~D."
           (length text) (boundary-map-length boundary-map)))
  (let* ((backtick-next (%ip-build-backtick-index text))
         (html-closes (%ip-build-html-closes text))
         (bracket-indexes (multiple-value-list
                           (%ip-build-bracket-index
                            text backtick-next html-closes
                            max-inline-delimiter-depth)))
         (bare-links (if gfm
                         (%ip-build-bare-links text)
                         (make-hash-table :test #'eql)))
         (context (%ip-make-context text boundary-map reference-index
                                    max-inline-delimiter-depth backtick-next
                                    (first bracket-indexes)
                                    (second bracket-indexes)
                                    (third bracket-indexes)
                                    (fourth bracket-indexes)
                                    bare-links html-closes)))
    (setf (%ip-context-gfm-p context) gfm)
    (%ip-parse-range context 0 (%ip-inline-end text) t 0)))
