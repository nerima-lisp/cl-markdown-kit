(in-package #:fixture-generator)

(defparameter *commonmark-inline-overrides*
  '(("commonmark-0028"
     ("&nbsp &x; &#; &#x;"
      "&#87654321;"
      "&#abcdef0;"
      "&ThisIsNotDefined; &hi?;")
     "CommonMark 0.31.2 Example 28 expects the invalid 8-digit decimal reference to remain literal; cmark-gfm 0.29.0.gfm.13 replaces it with U+FFFD.")))
