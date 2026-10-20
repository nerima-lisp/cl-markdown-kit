(in-package #:cl-markdown-kit)
(defparameter +bp-html-comment-start+ "<!--")
(defparameter +bp-html-processing-instruction-start+ "<?")
(defparameter +bp-html-cdata-start+ "<![CDATA[")
(defparameter +bp-html-declaration-start+ "<!")
(defparameter +bp-setext-marker-characters+ '(#\= #\-))
(defparameter +bp-thematic-break-marker-characters+ '(#\- #\_ #\*))
(defparameter +bp-fence-marker-characters+ (list #\` #\~))
(defparameter +bp-html-attribute-name-start-characters+ '(#\_ #\:))
(defparameter +bp-html-attribute-name-characters+ '(#\_ #\: #\. #\-))
(defparameter +bp-html-quoted-value-delimiters+ (list #\" #\'))
(defparameter +bp-html-unquoted-value-excluded-characters+ (list #\" #\' #\= #\< #\> #\`))
(defparameter +bp-unordered-list-marker-characters+ '(#\- #\+ #\*))
(defparameter +bp-ordered-list-delimiters+ '(#\. #\)))
(defparameter +bp-task-marker-states+ '(#\Space #\x #\X))
(defparameter +bp-blockquote-marker+ #\>)
(defparameter +bp-atx-heading-marker+ #\#)

(defparameter +bp-fence-info-trim-characters+ '(#\Space #\Tab))

(defparameter +bp-html-tag-end-characters+ '(#\> #\/))

(defstruct (%bp-open-frame
             (:constructor %bp-make-open-frame
                 (&key kind node context child-context start-line start-position
                       end-line marker-kind marker-delimiter marker-columns
                       content-indent children items blank-seen has-content)))
  kind node context child-context start-line start-position end-line
  marker-kind marker-delimiter marker-columns content-indent
  (children nil) (items nil) (blank-seen nil) (has-content nil))

(declaim (special *block-inline-fragments* *block-reference-index*))
