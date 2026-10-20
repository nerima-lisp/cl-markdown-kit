(defconstant +default-max-input-size+ 1048576)
(defconstant +default-max-nesting-depth+ 256)
(defun make-source-buffer (source
                           &key
                             (max-input-size +default-max-input-size+))
  source)
(defun parse-blocks (source-buffer
                     &key
                       (front-matter t)
                       (gfm t)
                       (max-nesting-depth +default-max-nesting-depth+))
  source-buffer)
