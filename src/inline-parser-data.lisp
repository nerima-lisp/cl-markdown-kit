(in-package #:cl-markdown-kit)

(defparameter +ip-html-close-patterns+ #(">" "-->" "?>" "]]>"))
(defparameter +ip-html-opener-markers+
              #("<!--" "<!--->" "<?" "<![CDATA[" "<!" "</" "<"))
(defconstant +ip-delimiter-bucket-count+ 7)
(defconstant +ip-ordinary-delimiter-bucket-count+ 6)
(defconstant +ip-tilde-delimiter-bucket-index+ 6)
(defconstant +ip-tilde-delimiter-character+ #\~)
(defconstant +ip-tilde-run-kind+ :strikethrough)
