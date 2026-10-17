(require :asdf)

(load
 (merge-pathnames "fixture-generator-support.lisp"
                  (uiop:pathname-directory-pathname *load-truename*)))

(sb-ext:exit :code (fixture-generator:run-inline-generator))
