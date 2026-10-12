(in-package #:cl-markdown-kit/test)

(defun run-tests ()
  (run-all :reporter :spec :pass-with-no-tests nil))
