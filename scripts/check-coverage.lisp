(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(let ((script
       (or *load-truename*
           *compile-file-truename*
           (error "Unable to determine the coverage gate script location"))))
  (load
   (merge-pathnames "coverage-gate.lisp"
                    (uiop:pathname-directory-pathname script))))

(uiop:quit (uiop:symbol-call :coverage-gate :run (uiop:command-line-arguments)))
