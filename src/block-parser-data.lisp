(in-package #:cl-markdown-kit)
(sb-ext:defglobal +bp-html-comment-start+ "<!--")
(sb-ext:defglobal +bp-html-processing-instruction-start+ "<?")
(sb-ext:defglobal +bp-html-cdata-start+ "<![CDATA[")
(sb-ext:defglobal +bp-html-declaration-start+ "<!")
(sb-ext:defglobal +bp-setext-marker-characters+ '(#\= #\-))
(sb-ext:defglobal +bp-thematic-break-marker-characters+ '(#\- #\_ #\*))
(sb-ext:defglobal +bp-fence-marker-characters+ (quote (#\` #\~)))
(sb-ext:defglobal +bp-html-attribute-name-start-characters+ '(#\_ #\:))
(sb-ext:defglobal +bp-html-attribute-name-characters+ '(#\_ #\: #\. #\-))
(sb-ext:defglobal +bp-html-quoted-value-delimiters+ (quote (#\" #\')))
(sb-ext:defglobal +bp-html-unquoted-value-excluded-characters+ (quote (#\" #\' #\= #\< #\> #\`)))
(sb-ext:defglobal +bp-unordered-list-marker-characters+ '(#\- #\+ #\*))
(sb-ext:defglobal +bp-ordered-list-delimiters+ '(#\. #\)))
(sb-ext:defglobal +bp-task-marker-states+ '(#\Space #\x #\X))
(sb-ext:defglobal +bp-blockquote-marker+ #\>)
(sb-ext:defglobal +bp-atx-heading-marker+ #\#)

(sb-ext:defglobal +bp-fence-info-trim-characters+ '(#\Space #\Tab))

(sb-ext:defglobal +bp-html-tag-end-characters+ '(#\> #\/))

(defstruct (%bp-open-frame
             (:constructor %bp-make-open-frame
                 (&key kind node context child-context start-line start-position
                       end-line marker-kind marker-delimiter marker-columns
                       content-indent children items blank-seen has-content)))
  kind node context child-context start-line start-position end-line
  marker-kind marker-delimiter marker-columns content-indent
  (children nil) (items nil) (blank-seen nil) (has-content nil))

(declaim (special *block-inline-fragments* *block-reference-index*))
