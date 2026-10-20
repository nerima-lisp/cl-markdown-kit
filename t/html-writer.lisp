(in-package #:cl-markdown-kit/test)
(describe "Dispatch verification"
  (it "verifies HTML node dispatches"
    (expect (cl-markdown-kit::%hw-verify-node-dispatches)
            :to-equal t)))

(defmacro html-writer-it (name &body body)
  `(cl-weave:it ,name
     (let ((cl-weave:*snapshot-directory*
            (asdf:system-relative-pathname
             "cl-markdown-kit"
             "t/snapshots/")))
       ,@body)))

(defun html-writer-test-document (&rest children)
  (cl-markdown-kit:make-document :children children))

(defun html-writer-test-render (children &rest options)
  (apply #'cl-markdown-kit:write-html
         (cl-markdown-kit:make-document :children children)
         options))

(defun html-writer-test-count-substring (needle string)
  (loop with offset = 0
        for position = (search needle string :start2 offset)
        while position
        count position
        do (setf offset (+ position (length needle)))))

(defun html-writer-test-attribute-values (attribute html)
  (let ((needle (format nil "~A=\"" attribute))
        (offset 0)
        (values nil))
    (loop for start = (search needle html :start2 offset)
          while start
          do (let* ((value-start (+ start (length needle)))
                    (end (position #\" html :start value-start)))
               (push (subseq html value-start end) values)
               (setf offset (1+ end))))
    (nreverse values)))

(defun html-writer-test-footnote-links-valid-p (html)
  (let ((ids (html-writer-test-attribute-values "id" html))
        (hrefs (html-writer-test-attribute-values "href" html)))
    (and (= (length ids) (length (remove-duplicates ids :test #'equal)))
         (every (lambda (href)
                  (and (> (length href) 1)
                       (char= (char href 0) #\#)
                       (= 1 (count (subseq href 1) ids :test #'equal))))
                hrefs))))
