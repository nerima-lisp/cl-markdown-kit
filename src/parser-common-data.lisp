(in-package #:cl-markdown-kit)

(defconstant +default-max-nesting-depth+ 256)

(defconstant +default-max-inline-delimiter-depth+ 4096)

(defconstant +max-ordered-list-marker-digits+ 9)

(sb-ext:defglobal +html-block-type-1-tags+ '("script" "pre" "style" "textarea"))

(sb-ext:defglobal +html-block-type-6-tags+ '("address" "article"
              "aside"
              "base"
              "basefont"
              "blockquote"
              "body"
              "caption"
              "center"
              "col"
              "colgroup"
              "dd"
              "details"
              "dialog"
              "dir"
              "div"
              "dl"
              "dt"
              "fieldset"
              "figcaption"
              "figure"
              "footer"
              "form"
              "frame"
              "frameset"
              "h1"
              "h2"
              "h3"
              "h4"
              "h5"
              "h6"
              "head"
              "header"
              "hr"
              "html"
              "iframe"
              "legend"
              "li"
              "link"
              "main"
              "menu"
              "menuitem"
              "nav"
              "noframes"
              "ol"
              "optgroup"
              "option"
              "p"
              "param"
              "search"
              "section"
              "summary"
              "table"
              "tbody"
              "td"
              "tfoot"
              "th"
              "thead"
              "title"
              "tr"
              "track"
              "ul"))

(sb-ext:defglobal +gfm-tagfilter-tags+ '("title" "textarea"
            "style"
            "xmp"
            "iframe"
            "noembed"
            "noframes"
            "script"
            "plaintext"))

(sb-ext:defglobal +html-attribute-start-characters+ "_:")

(sb-ext:defglobal +html-attribute-name-extra-characters+ ":._-")

(sb-ext:defglobal +html-unquoted-attribute-excluded-characters+ (concatenate 'string (string #\") "'=<>" (string (code-char #x60))))

(sb-ext:defglobal +autolink-angle-excluded-characters+ "<>")

(sb-ext:defglobal +uri-scheme-extra-characters+ "+.-")

(sb-ext:defglobal +angle-email-local-part-characters+ (concatenate 'string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.!#$%&'*+/=?^_" (string (code-char #x60)) "{|}~-"))

(sb-ext:defglobal +email-local-part-extra-characters+ ".-_+")

(sb-ext:defglobal +gfm-domain-extra-characters+ "_-")

(sb-ext:defglobal +bare-url-trailing-punctuation-characters+ "?!.,:*_~")

(sb-ext:defglobal +common-quote-characters+ (list #\" #\'))

(sb-ext:defglobal +link-label-unescape-characters+ (list #\\ #\[ #\]))

(sb-ext:defglobal +reference-label-space-characters+ (list #\Space #\Tab #\Newline))

(sb-ext:defglobal +inline-delimiter-characters+ (list #\* #\_ #\~))

(sb-ext:defglobal +inline-text-special-characters+ (list #\\ #\` #\& #\< #\Newline #\Space #\Tab #\! #\[ #\] #\* #\_ #\~))

(sb-ext:defglobal +numeric-reference-hex-prefix-characters+ "xX")

(sb-ext:defglobal +ascii-hexadecimal-letter-characters+ "abcdefABCDEF")

(sb-ext:defglobal +bare-url-preceding-excluded-characters+ ":/@<")

(sb-ext:defglobal +link-title-delimiters+ (list #\" #\' #\())
(sb-ext:defglobal +www-url-prefix+ "www.")
(sb-ext:defglobal +https-url-prefix+ "https://")
(sb-ext:defglobal +http-url-prefix+ "http://")
(sb-ext:defglobal +mailto-url-prefix+ "mailto:")
(sb-ext:defglobal +xmpp-url-prefix+ "xmpp:")
(sb-ext:defglobal +uri-percent-encoding-safe-characters+ ";/?:@&=+$,-_.!~*'()#")
(sb-ext:defglobal +uri-hexadecimal-digits+ "0123456789ABCDEF")
(sb-ext:defglobal +dangerous-uri-scheme-prefixes+ '("javascript:" "vbscript:" "file:"))
(sb-ext:defglobal +data-uri-scheme-prefix+ "data:")
(sb-ext:defglobal +safe-data-image-prefixes+ '("data:image/png" "data:image/gif" "data:image/jpeg" "data:image/webp"))
