;;;; run-tests.lisp
(require :asdf)

(defun script-directory ()
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename*
                               (error "Unable to determine script location"))))

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(script-directory)) :inherit-configuration))

(defun compile-warning-check ()
  (let* ((root (script-directory))
         (source-directory (merge-pathnames "src/" root))
         (warnings nil))
    (flet ((record-warning (condition)
             (when *compile-file-truename*
               (push (list *compile-file-truename* condition) warnings)
               (muffle-warning condition))))
      (handler-bind ((warning #'record-warning))
        (asdf:operate 'asdf:compile-op "cl-markdown-kit" :force t)
        (asdf:operate 'asdf:compile-op "cl-markdown-kit/test" :force t)
        (compile-file (merge-pathnames "run-tests.lisp" root)
                      :output-file
                      (merge-pathnames "run-tests-check.fasl"
                                       (uiop:temporary-directory)))))
    (let ((non-style (remove-if (lambda (entry)
                                  (typep (second entry) 'style-warning))
                                warnings))
          (source-style (remove-if-not
                         (lambda (entry)
                           (and (typep (second entry) 'style-warning)
                                (uiop:string-prefix-p
                                 (namestring source-directory)
                                 (namestring (first entry)))))
                         warnings)))
      (format t "compile warning check: non-style=~D src-style=~D~%"
              (length non-style) (length source-style))
      (dolist (entry (append non-style source-style))
        (format *error-output* "~&compile warning in ~A: ~A~%"
                (namestring (first entry)) (second entry)))
      (when (or non-style source-style)
        (error "Compilation warning check failed: non-style=~D src-style=~D"
               (length non-style) (length source-style)))))
  t)

(compile-warning-check)
(asdf:test-system "cl-markdown-kit")
(uiop:quit 0)
