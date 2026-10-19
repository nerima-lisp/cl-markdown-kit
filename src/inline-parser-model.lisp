(in-package #:cl-markdown-kit)

(defstruct (%ip-context
             (:constructor %ip-make-context
                 (text boundary-map reference-index limit backtick-next
                  bracket-opens bracket-closes bracket-prefix bracket-depths
                  bare-links html-closes)))
  (text "" :type string)
  boundary-map
  reference-index
  (limit +default-max-inline-delimiter-depth+ :type (integer 1 *))
  backtick-next
  bracket-opens
  bracket-closes
  bracket-prefix
  bracket-depths
  bare-links
  html-closes
  (rightmost-unescaped-close-paren :unspecified
                                   :type (or (integer -1 *)
                                             (member :unspecified)))
  (gfm-p t :type boolean))

(defstruct (%ip-delimiter
             (:constructor %ip-make-delimiter
                 (character start length can-open can-close)))
  character
  (start 0 :type (integer 0 *))
  (length 0 :type (integer 1 *))
  (remaining 0 :type (integer 0 *))
  (open-offset 0 :type (integer 0 *))
  (can-open nil :type boolean)
  (can-close nil :type boolean)
  (open-bucket nil))

(defstruct (%ip-pair
             (:constructor %ip-make-pair
                 (kind delimiter-character start inner-start inner-end end
                  &optional destination title reference-kind reference-label)))
  kind
  delimiter-character
  (start 0 :type (integer 0 *))
  (inner-start 0 :type (integer 0 *))
  (inner-end 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  destination
  title
  reference-kind
  reference-label)

(defstruct (%ip-atom (:constructor %ip-make-atom (start end node)))
  (start 0 :type (integer 0 *))
  (end 0 :type (integer 0 *))
  node)

(defstruct (%ip-bracket
             (:constructor %ip-make-bracket (open close image-p)))
  (open 0 :type (integer 0 *))
  close
  (image-p nil :type boolean))
