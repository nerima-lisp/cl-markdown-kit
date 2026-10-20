(in-package #:cl-markdown-kit)

(declaim (special *mw-line-start-p*
                  *mw-line-indent*
                  *mw-table-cell-p*
                  *mw-text-autolink-breakpoints*
                  *mw-heading-p*
                  *mw-autolink-label-p*
                  *mw-escape-trailing-bang-p*
                  *mw-following-block-p*
                  *mw-minimize-inline-escapes-p*
                  *mw-heading-closing-hash-allowed-p*))

(sb-ext:defglobal +mw-escape-punctuation-characters+ '(#\` #\* #\_ #\< #\~))

(sb-ext:defglobal +mw-escape-bracket-characters+ '(#\[ #\]))
