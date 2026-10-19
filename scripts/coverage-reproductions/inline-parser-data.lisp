(in-package #:cl-markdown-kit)

(defparameter +ip-html-close-patterns+ #(">" "-->" "?>" "]]>"))
(defparameter +ip-html-opener-markers+
              #("<!--" "<!--->" "<?" "<![CDATA[" "<!" "</" "<"))
