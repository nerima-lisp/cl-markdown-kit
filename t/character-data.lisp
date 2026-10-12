(in-package #:cl-markdown-kit/test)

(defun character-data-codepoint (codepoint)
  (or (code-char codepoint)
      (error "SBCL did not provide code point U+~4,'0X" codepoint)))

(defun character-data-codepoint-string (codepoint)
  (string (character-data-codepoint codepoint)))

(defmacro character-data-probe (label function arguments expected)
  `(it ,label
     (expect (funcall #',function ,@arguments) :to-equal ,expected)))

(describe "character data"
  (character-data-probe "probe 01: entity amp;"
      cl-markdown-kit::lookup-html5-entity ("amp;") "&")
  (character-data-probe "probe 02: entity AElig;"
      cl-markdown-kit::lookup-html5-entity ("AElig;")
      (character-data-codepoint-string #x00C6))
  (character-data-probe "probe 03: entity Dcaron;"
      cl-markdown-kit::lookup-html5-entity ("Dcaron;")
      (character-data-codepoint-string #x010E))
  (character-data-probe "probe 04: multi-codepoint entity ngE;"
      cl-markdown-kit::lookup-html5-entity ("ngE;")
      (concatenate 'string (character-data-codepoint-string #x2267)
                   (character-data-codepoint-string #x0338)))
  (character-data-probe "probe 05: entity without semicolon"
      cl-markdown-kit::lookup-html5-entity ("copy") nil)
  (character-data-probe "probe 06: unknown entity"
      cl-markdown-kit::lookup-html5-entity ("MadeUpEntity;") nil)
  (character-data-probe "probe 07: non-string entity name"
      cl-markdown-kit::lookup-html5-entity (nil) nil)
  (character-data-probe "probe 08: decimal 35"
      cl-markdown-kit::decode-numeric-character-reference ("35" 10) "#")
  (character-data-probe "probe 09: decimal 1234"
      cl-markdown-kit::decode-numeric-character-reference ("1234" 10)
      (character-data-codepoint-string #x04D2))
  (character-data-probe "probe 10: decimal 992"
      cl-markdown-kit::decode-numeric-character-reference ("992" 10)
      (character-data-codepoint-string #x03E0))
  (character-data-probe "probe 11: decimal zero"
      cl-markdown-kit::decode-numeric-character-reference ("0" 10)
      (character-data-codepoint-string #xFFFD))
  (character-data-probe "probe 12: hexadecimal quote"
      cl-markdown-kit::decode-numeric-character-reference ("22" 16) "\"")
  (character-data-probe "probe 13: hexadecimal uppercase digits"
      cl-markdown-kit::decode-numeric-character-reference ("D06" 16)
      (character-data-codepoint-string #x0D06))
  (character-data-probe "probe 14: hexadecimal lowercase digits"
      cl-markdown-kit::decode-numeric-character-reference ("cab" 16)
      (character-data-codepoint-string #x0CAB))
  (character-data-probe "probe 15: decimal control value"
      cl-markdown-kit::decode-numeric-character-reference ("1" 10)
      (character-data-codepoint-string #x0001))
  (character-data-probe "probe 16: decimal above Unicode maximum"
      cl-markdown-kit::decode-numeric-character-reference ("1114112" 10)
      (character-data-codepoint-string #xFFFD))
  (character-data-probe "probe 17: decimal surrogate"
      cl-markdown-kit::decode-numeric-character-reference ("55296" 10)
      (character-data-codepoint-string #xFFFD))
  (character-data-probe "probe 18: hexadecimal above Unicode maximum"
      cl-markdown-kit::decode-numeric-character-reference ("110000" 16)
      (character-data-codepoint-string #xFFFD))
  (character-data-probe "probe 19: empty numeric reference"
      cl-markdown-kit::decode-numeric-character-reference ("" 10) nil)
  (character-data-probe "probe 20: overlong decimal reference"
      cl-markdown-kit::decode-numeric-character-reference ("12345678" 10) nil)
  (character-data-probe "probe 21: overlong hexadecimal reference"
      cl-markdown-kit::decode-numeric-character-reference ("1234567" 16) nil)
  (character-data-probe "probe 22: letter in decimal reference"
      cl-markdown-kit::decode-numeric-character-reference ("A" 10) nil)
  (character-data-probe "probe 23: non-hex digit in hexadecimal reference"
      cl-markdown-kit::decode-numeric-character-reference ("G" 16) nil)
  (character-data-probe "probe 24: unsupported radix"
      cl-markdown-kit::decode-numeric-character-reference ("1" 8) nil)
  (character-data-probe "probe 25: non-string numeric digits"
      cl-markdown-kit::decode-numeric-character-reference (nil 10) nil)
  (character-data-probe "probe 26: tab is Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p
      ((character-data-codepoint #x0009)) t)
  (character-data-probe "probe 27: line feed is Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p
      ((character-data-codepoint #x000A)) t)
  (character-data-probe "probe 28: form feed is Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p
      ((character-data-codepoint #x000C)) t)
  (character-data-probe "probe 29: carriage return is Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p
      ((character-data-codepoint #x000D)) t)
  (character-data-probe "probe 30: no-break space is Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p
      ((character-data-codepoint #x00A0)) t)
  (character-data-probe "probe 31: em space is Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p
      ((character-data-codepoint #x2003)) t)
  (character-data-probe "probe 32: letter is not Unicode whitespace"
      cl-markdown-kit::unicode-whitespace-p (#\A) nil)

  (it "rejects non-CommonMark Unicode whitespace separators"
    (expect (mapcar #'cl-markdown-kit::unicode-whitespace-p
                    (mapcar #'character-data-codepoint
                            '(#x0085 #x2028 #x2029 #x180E)))
            :to-equal '(nil nil nil nil)))

  (character-data-probe "probe 33: connector punctuation"
      cl-markdown-kit::unicode-punctuation-p
      ((character-data-codepoint #x203F)) t)
  (character-data-probe "probe 34: dash punctuation"
      cl-markdown-kit::unicode-punctuation-p
      ((character-data-codepoint #x2014)) t)
  (character-data-probe "probe 35: open punctuation"
      cl-markdown-kit::unicode-punctuation-p (#\() t)
  (character-data-probe "probe 36: final quote punctuation"
      cl-markdown-kit::unicode-punctuation-p
      ((character-data-codepoint #x2019)) t)
  (character-data-probe "probe 37: math symbol"
      cl-markdown-kit::unicode-punctuation-p (#\+) t)
  (character-data-probe "probe 38: other symbol"
      cl-markdown-kit::unicode-punctuation-p
      ((character-data-codepoint #x2603)) t)
  (character-data-probe "probe 39: letter is not Unicode punctuation"
      cl-markdown-kit::unicode-punctuation-p (#\A) nil)

  (it "decodes the named references in CommonMark Example 25"
    (expect (mapcar #'cl-markdown-kit::lookup-html5-entity
                    '("nbsp;" "amp;" "copy;" "AElig;" "Dcaron;"
                      "frac34;" "HilbertSpace;" "DifferentialD;"
                      "ClockwiseContourIntegral;" "ngE;"))
            :to-equal
            (list (character-data-codepoint-string #x00A0)
                  "&"
                  (character-data-codepoint-string #x00A9)
                  (character-data-codepoint-string #x00C6)
                  (character-data-codepoint-string #x010E)
                  (character-data-codepoint-string #x00BE)
                  (character-data-codepoint-string #x210B)
                  (character-data-codepoint-string #x2146)
                  (character-data-codepoint-string #x2232)
                  (concatenate 'string
                               (character-data-codepoint-string #x2267)
                               (character-data-codepoint-string #x0338)))))

  (it "decodes the decimal numeric references in CommonMark Example 26"
    (expect (mapcar (lambda (digits)
                      (cl-markdown-kit::decode-numeric-character-reference
                       digits 10))
                    '("35" "1234" "992" "0"))
            :to-equal
            (list "#"
                  (character-data-codepoint-string #x04D2)
                  (character-data-codepoint-string #x03E0)
                  (character-data-codepoint-string #xFFFD))))

  (it "decodes the hexadecimal numeric references in CommonMark Example 27"
    (expect (mapcar (lambda (digits)
                      (cl-markdown-kit::decode-numeric-character-reference
                       digits 16))
                    '("22" "D06" "cab"))
            :to-equal
            (list "\""
                  (character-data-codepoint-string #x0D06)
                  (character-data-codepoint-string #x0CAB))))

  (it "rejects invalid references from CommonMark Examples 28 through 30"
    (expect (mapcar #'cl-markdown-kit::lookup-html5-entity
                    '("copy" "MadeUpEntity;" "hi?;"))
            :to-equal '(nil nil nil))
    (expect (mapcar (lambda (digits)
                      (cl-markdown-kit::decode-numeric-character-reference
                       digits 10))
                    '("" "87654321" "abcdef0"))
            :to-equal '(nil nil nil)))

  (it "uses the entity values in CommonMark Examples 31 through 34"
    (let ((umlaut (cl-markdown-kit::lookup-html5-entity "ouml;")))
      (expect umlaut :to-equal (character-data-codepoint-string #x00F6))
      (expect (cl-markdown-kit::normalize-uri-percent-encoding
               (concatenate 'string "/f" umlaut umlaut ".html"))
              :to-equal "/f%C3%B6%C3%B6.html")
      (expect (concatenate 'string "f" umlaut umlaut) :to-equal "föö")))

  (it "keeps entity spellings literal when HTML text is escaped"
    (expect (cl-markdown-kit::escape-html-text "f&ouml;f&ouml;")
            :to-equal "f&amp;ouml;f&amp;ouml;")
    (expect (cl-markdown-kit::decode-numeric-character-reference "42" 10)
            :to-equal "*")
    (expect (cl-markdown-kit::decode-numeric-character-reference "10" 10)
            :to-equal (character-data-codepoint-string #x000A))
    (expect (cl-markdown-kit::decode-numeric-character-reference "9" 10)
            :to-equal (character-data-codepoint-string #x0009))
    (expect (cl-markdown-kit::escape-html-attribute
             (cl-markdown-kit::decode-numeric-character-reference "34" 10))
            :to-equal "&quot;"))

  (it "checks character-reference values for CommonMark Examples 37 through 41"
    (expect (cl-markdown-kit::decode-numeric-character-reference "42" 10)
            :to-equal "*")
    (expect (concatenate
             'string
             (cl-markdown-kit::decode-numeric-character-reference "10" 10)
             (cl-markdown-kit::decode-numeric-character-reference "10" 10))
            :to-equal (concatenate 'string
                                   (character-data-codepoint-string #x000A)
                                   (character-data-codepoint-string #x000A)))
    (expect (cl-markdown-kit::decode-numeric-character-reference "9" 10)
            :to-equal (character-data-codepoint-string #x0009))
    (expect (cl-markdown-kit::lookup-html5-entity "quot;")
            :to-equal "\""))

  (it "matches the [ẞ] and [SS]: /url labels from CommonMark Example 540"
    (let ((link-label
            (cl-markdown-kit::normalize-link-label
             (character-data-codepoint-string #x1E9E)))
          (reference-label
            (cl-markdown-kit::normalize-link-label "SS")))
      (expect link-label :to-equal "ss")
      (expect reference-label :to-equal "ss")
      (expect (string= link-label reference-label) :to-be-truthy))
    (expect (cl-markdown-kit::normalize-link-label
             (format nil "  Foo~cBAR~cBAZ  " #\Tab #\Newline))
            :to-equal "foo bar baz")
    (expect (cl-markdown-kit::normalize-link-label
             (concatenate 'string " " (string #\Tab) (string #\Newline)
                          (string #\Return)))
            :to-equal "")
    (expect (cl-markdown-kit::normalize-link-label
             (concatenate 'string "a"
                          (character-data-codepoint-string #x00A0)
                          "b"))
            :to-equal (concatenate 'string "a"
                                   (character-data-codepoint-string #x00A0)
                                   "b")))

  (it "classifies all ASCII punctuation ranges and control boundaries"
    (expect (mapcar #'cl-markdown-kit::ascii-punctuation-p
                    '(#\! #\: #\[ #\{ #\A #\Space))
            :to-equal '(t t t t nil nil))
    (expect (mapcar #'cl-markdown-kit::ascii-control-p
                    (list (character-data-codepoint #x0000)
                          (character-data-codepoint #x001F)
                          (character-data-codepoint #x007F)
                          #\Space #\A
                          (character-data-codepoint #x0080)))
            :to-equal '(t t t nil nil nil)))

  (it "escapes HTML text and attributes and preserves ordinary characters"
    (expect (cl-markdown-kit::escape-html-text "A&<>\"")
            :to-equal "A&amp;&lt;&gt;&quot;")
    (expect (cl-markdown-kit::escape-html-attribute "A&<>\"")
            :to-equal "A&amp;&lt;&gt;&quot;")
    (expect (cl-markdown-kit::escape-html-text "plain text")
            :to-equal "plain text"))

  (it "normalizes URI percent encoding without changing valid triplets"
    (expect (cl-markdown-kit::normalize-uri-percent-encoding
             "azAZ09-._~;/?:@&=+$,-_.!~*'()#")
            :to-equal "azAZ09-._~;/?:@&=+$,-_.!~*'()#")
    (expect (cl-markdown-kit::normalize-uri-percent-encoding "%2f%aF%09")
            :to-equal "%2f%aF%09")
    (expect (cl-markdown-kit::normalize-uri-percent-encoding "% %x0")
            :to-equal "%25%20%25x0")
    (expect (cl-markdown-kit::normalize-uri-percent-encoding "%a")
            :to-equal "%25a")
    (expect (cl-markdown-kit::normalize-uri-percent-encoding
             (character-data-codepoint-string #x00E9))
            :to-equal "%C3%A9")
    (expect (cl-markdown-kit::normalize-uri-percent-encoding
             (character-data-codepoint-string #x20AC))
            :to-equal "%E2%82%AC")
    (expect (cl-markdown-kit::normalize-uri-percent-encoding
             (character-data-codepoint-string #x1F680))
            :to-equal "%F0%9F%9A%80"))

  (it "marks dangerous URL schemes case-insensitively"
    (expect (mapcar #'cl-markdown-kit::dangerous-url-p
                    '("javascript:alert(1)" "JAVASCRIPT:alert(1)"
                      "vbscript:msgbox(1)" "VbScRiPt:msgbox(1)"
                      "file:///example/path" "FILE:///example/path"))
            :to-equal '(t t t t t t))
    (expect (mapcar #'cl-markdown-kit::dangerous-url-p
                    '("data:text/html,<script>" "DATA:TEXT/HTML,x"
                      "data:image/svg+xml,<svg onload=alert(1)>"))
            :to-equal '(t t t))
    (expect (mapcar #'cl-markdown-kit::dangerous-url-p
                    '("data:image/png;base64,x" "data:image/gif;base64,x"
                      "data:image/jpeg;base64,x" "data:image/webp;base64,x"
                      "DATA:IMAGE/PNG;base64,x" "https://example.test/"
                      "/relative/path" ""))
            :to-equal '(nil nil nil nil nil nil nil nil))))
