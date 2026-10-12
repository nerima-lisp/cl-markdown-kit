(defun parse-string (source &key
                            (limit 256))
  source)

(defun parse-stream (source &key
                            (limit 256))
  source)

(defun parse-octets (source &key
                            (limit 256))
  source)

(defun parse (source &key
                     (limit 256))
  source)

(defun make-source-boundary (&key (offset 0) (line 1) (column 1))
  (list offset line column))

(defun make-source-format (&key
                             (newline-style :none))
  newline-style)
